library(readr)
library(shiny)
library(shiny.semantic)
library(highcharter)
library(tidyr)
library(wesanderson)
library(shinycssloaders)
library(hrbrthemes)
library(dplyr)
library(shinyWidgets)
library(shinyjs)
library(maps)
library(DT)
library(RPostgres)
library(pool)

myGridTemplate <- grid_template(
  default = list(
    areas = rbind(
      c("title", "title"),
      c("user", "map"),
      c("user", "mfd")
    ),
    cols_width = c(
      "350px",
      "1fr"
    ),
    rows_height = c(
      "auto",
      "0.5fr",
      "0.5fr"
    )
  )
)

bar_charts_grid <- grid_template(
  default = list(
    areas = cbind(
      c("tab_1"),
      c("tab_2")
    ),
    cols_width = c("0.5fr", "0.5fr"),
    rows_height = c("1fr")
  )
)

hierarchical_grid <- grid_template(
  default = list(
    areas = rbind(
      c("sunburst"),
      c("treemap")
    ),
    cols_width = c("0.5fr", "0.5fr"),
    rows_height = c("1fr")
  )
)

# display_grid(myGridTemplate)

# Connect to PostgreSQL database with connection pooling
# Uses environment variables for configuration (set in Render/Docker)
db_pool <- dbPool(
  drv = Postgres(),
  host = Sys.getenv("POSTGRES_HOST", "localhost"),
  port = as.integer(Sys.getenv("POSTGRES_PORT", "5432")),
  dbname = Sys.getenv("POSTGRES_DB", "postgres"),
  user = Sys.getenv("POSTGRES_USER", "postgres"),
  password = Sys.getenv("POSTGRES_PASSWORD", ""),
  minSize = 1,
  maxSize = 5,
  idleTimeout = 60000  # Close idle connections after 60 seconds
)

# Alias for backward compatibility with existing code
db_con <- db_pool

# Clean up pool on app shutdown
onStop(function() {
  poolClose(db_pool)
})

# Helper function to query the database
query_data <- function(con, causes, sex, measure = NULL) {
  # Build SQL query with parameterized filters
  cause_list <- paste0("'", gsub("'", "''", causes), "'", collapse = ", ")
  sex_escaped <- gsub("'", "''", sex)

  if (is.null(measure)) {
    sql <- sprintf("
      SELECT * FROM ihme_data_geo
      WHERE cause_name IN (%s)
      AND sex_label = '%s'
    ", cause_list, sex_escaped)
  } else {
    measure_escaped <- gsub("'", "''", measure)
    sql <- sprintf("
      SELECT * FROM ihme_data_geo
      WHERE cause_name IN (%s)
      AND sex_label = '%s'
      AND measure_name = '%s'
    ", cause_list, sex_escaped, measure_escaped)
  }

  dbGetQuery(con, sql)
}

# Optimized function for SHS aggregation - pushes GROUP BY to DuckDB
query_shs_aggregated <- function(con, causes, sex) {
  cause_list <- paste0("'", gsub("'", "''", causes), "'", collapse = ", ")
  sex_escaped <- gsub("'", "''", sex)

  # Do the aggregation in SQL (much faster than R)
  sql <- sprintf("
    SELECT
      location_name, location_id, year_id, age_group_name, age_id,
      sex_label, cause_name, income_group, continent, subregion, metric_name,
      SUM(CASE WHEN measure_name = 'Deaths' THEN val ELSE 0 END) as deaths_val,
      SUM(CASE WHEN measure_name = 'Prevalence' THEN val ELSE 0 END) as prev_val,
      SUM(CASE WHEN measure_name = 'Incidence' THEN val ELSE 0 END) as inc_val
    FROM ihme_data_geo
    WHERE cause_name IN (%s)
    AND sex_label = '%s'
    GROUP BY
      location_name, location_id, year_id, age_group_name, age_id,
      sex_label, cause_name, income_group, continent, subregion, metric_name
  ", cause_list, sex_escaped)

  dbGetQuery(con, sql)
}

# Define all SHS causes (used for filtering when SHS toggle is on)
all_shs_causes <- c(
  "Acute glomerulonephritis",
  "Alzheimer's disease and other dementias",
  "Chronic kidney disease",
  "Cirrhosis and other chronic liver diseases",
  "Congenital birth defects",
  "Diabetes mellitus",
  "Extensively drug-resistant tuberculosis",
  "HIV/AIDS",
  "Injuries",
  "Intracerebral hemorrhage",
  "Ischemic stroke",
  "Leukemia",
  "Multidrug-resistant tuberculosis without extensive drug resistance",
  "Multiple sclerosis",
  "Musculoskeletal disorders",
  "Neonatal encephalopathy due to birth asphyxia and trauma",
  "Neonatal preterm birth",
  "Neoplasms",
  "Other digestive diseases",
  "Other neglected tropical diseases",
  "Other neoplasms",
  "Parkinson's disease",
  "Schistosomiasis",
  "Sickle cell disorders",
  "Subarachnoid hemorrhage",
  "Tetanus",
  "Thalassemias"
)

# ============================================
# OPTIMIZED QUERY FUNCTIONS (Memory-efficient)
# Each chart queries only what it needs
# ============================================

# Helper to build cause filter based on health condition selection
build_cause_filter <- function(health_condition, shs_toggle) {
  if (health_condition == "All health conditions") {
    return(all_shs_causes)
  }

  # Map grouped conditions to their components
  condition_map <- list(
    "Stroke" = c("Intracerebral hemorrhage", "Ischemic stroke", "Subarachnoid hemorrhage"),
    "Liver diseases" = c("Cirrhosis and other chronic liver diseases", "Other digestive diseases", "Schistosomiasis"),
    "Tuberculosis" = c("Multidrug-resistant tuberculosis without extensive drug resistance", "Extensively drug-resistant tuberculosis"),
    "Kidney diseases" = c("Acute glomerulonephritis", "Chronic kidney disease")
  )

  if (health_condition %in% names(condition_map)) {
    return(condition_map[[health_condition]])
  }

  return(health_condition)
}

# Escape SQL string values
sql_escape <- function(x) gsub("'", "''", x)

# Build cause list for SQL IN clause
build_cause_list <- function(causes) {
  paste0("'", sql_escape(causes), "'", collapse = ", ")
}

# MAP QUERY: Returns ~201 rows (16 KB) instead of 2.5M rows (424 MB)
query_map_data <- function(con, causes, sex, year, age, measure, shs_toggle) {
  cause_list <- build_cause_list(causes)
  sex_escaped <- sql_escape(sex)
  age_escaped <- sql_escape(age)

  if (isTRUE(shs_toggle)) {
    # SHS calculation pushed to SQL for memory efficiency
    sql <- sprintf("
      WITH base_data AS (
        SELECT
          location_name,
          cause_name,
          age_id,
          income_group,
          SUM(CASE WHEN measure_name = 'Deaths' THEN val ELSE 0 END) as deaths_val,
          SUM(CASE WHEN measure_name = 'Prevalence' THEN val ELSE 0 END) as prev_val,
          SUM(CASE WHEN measure_name = 'Incidence' THEN val ELSE 0 END) as inc_val
        FROM ihme_data_geo
        WHERE cause_name IN (%s)
          AND sex_label = '%s'
          AND year_id = %d
          AND age_group_name = '%s'
          AND metric_name = 'Rate'
        GROUP BY location_name, cause_name, age_id, income_group
      )
      SELECT
        location_name,
        SUM(
          CASE
            WHEN cause_name = 'Other neglected tropical diseases' THEN deaths_val / 3.0 * 0.05
            WHEN cause_name = 'Multidrug-resistant tuberculosis without extensive drug resistance' THEN (inc_val - deaths_val / 6.0) * 0.5
            WHEN cause_name = 'Extensively drug-resistant tuberculosis' THEN (inc_val - deaths_val / 6.0) * 1.0
            WHEN cause_name = 'HIV/AIDS' THEN prev_val / 3.0 * 0.5
            WHEN cause_name = 'Alzheimer''s disease and other dementias' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Tetanus' THEN deaths_val / 3.0 * 0.5
            WHEN cause_name = 'Parkinson''s disease' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Multiple sclerosis' THEN prev_val / 3.0 * 0.017
            WHEN cause_name = 'Congenital birth defects' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Injuries' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Musculoskeletal disorders' THEN deaths_val / 3.0 * 1.4
            WHEN cause_name = 'Sickle cell disorders' THEN prev_val / 3.0 * 0.6
            WHEN cause_name IN ('Cirrhosis and other chronic liver diseases', 'Other digestive diseases', 'Schistosomiasis')
              THEN deaths_val / 9.0 * 0.5 * 1.5
            WHEN cause_name IN ('Chronic kidney disease', 'Acute glomerulonephritis')
              THEN deaths_val / 6.0 * 1.0
            WHEN cause_name IN ('Intracerebral hemorrhage', 'Ischemic stroke', 'Subarachnoid hemorrhage')
              THEN deaths_val / 9.0 * 1.0 * 1.5
            WHEN cause_name = 'Diabetes mellitus' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Thalassemias' THEN prev_val / 3.0 * 0.4
            WHEN cause_name = 'Neonatal preterm birth' THEN prev_val / 3.0 * 0.01
            WHEN cause_name = 'Neonatal encephalopathy due to birth asphyxia and trauma' THEN prev_val / 3.0 * 0.15
            ELSE 0
          END
        ) as val
      FROM base_data
      GROUP BY location_name
    ", cause_list, sex_escaped, year, age_escaped)
  } else {
    # Standard measure query
    measure_escaped <- sql_escape(measure)
    sql <- sprintf("
      SELECT location_name, SUM(val) as val
      FROM ihme_data_geo
      WHERE cause_name IN (%s)
        AND sex_label = '%s'
        AND year_id = %d
        AND age_group_name = '%s'
        AND metric_name = 'Rate'
        AND measure_name = '%s'
      GROUP BY location_name
    ", cause_list, sex_escaped, year, age_escaped, measure_escaped)
  }

  dbGetQuery(con, sql)
}

# HISTORICAL CHART QUERY: Returns ~4 rows per country
query_historical_data <- function(con, causes, sex, locations, age, metric, measure, shs_toggle) {
  cause_list <- build_cause_list(causes)
  sex_escaped <- sql_escape(sex)
  location_list <- build_cause_list(locations)  # Reuse for location escaping
  age_escaped <- sql_escape(age)
  metric_escaped <- sql_escape(metric)

  if (isTRUE(shs_toggle)) {
    sql <- sprintf("
      WITH base_data AS (
        SELECT
          location_name,
          year_id,
          cause_name,
          age_id,
          income_group,
          SUM(CASE WHEN measure_name = 'Deaths' THEN val ELSE 0 END) as deaths_val,
          SUM(CASE WHEN measure_name = 'Prevalence' THEN val ELSE 0 END) as prev_val,
          SUM(CASE WHEN measure_name = 'Incidence' THEN val ELSE 0 END) as inc_val
        FROM ihme_data_geo
        WHERE cause_name IN (%s)
          AND sex_label = '%s'
          AND location_name IN (%s)
          AND age_group_name = '%s'
          AND metric_name = '%s'
        GROUP BY location_name, year_id, cause_name, age_id, income_group
      )
      SELECT
        year_id as year,
        location_name,
        SUM(
          CASE
            WHEN cause_name = 'Other neglected tropical diseases' THEN deaths_val / 3.0 * 0.05
            WHEN cause_name = 'Multidrug-resistant tuberculosis without extensive drug resistance' THEN (inc_val - deaths_val / 6.0) * 0.5
            WHEN cause_name = 'Extensively drug-resistant tuberculosis' THEN (inc_val - deaths_val / 6.0) * 1.0
            WHEN cause_name = 'HIV/AIDS' THEN prev_val / 3.0 * 0.5
            WHEN cause_name = 'Alzheimer''s disease and other dementias' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Tetanus' THEN deaths_val / 3.0 * 0.5
            WHEN cause_name = 'Parkinson''s disease' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Multiple sclerosis' THEN prev_val / 3.0 * 0.017
            WHEN cause_name = 'Congenital birth defects' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Injuries' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Musculoskeletal disorders' THEN deaths_val / 3.0 * 1.4
            WHEN cause_name = 'Sickle cell disorders' THEN prev_val / 3.0 * 0.6
            WHEN cause_name IN ('Cirrhosis and other chronic liver diseases', 'Other digestive diseases', 'Schistosomiasis')
              THEN deaths_val / 9.0 * 0.5 * 1.5
            WHEN cause_name IN ('Chronic kidney disease', 'Acute glomerulonephritis')
              THEN deaths_val / 6.0 * 1.0
            WHEN cause_name IN ('Intracerebral hemorrhage', 'Ischemic stroke', 'Subarachnoid hemorrhage')
              THEN deaths_val / 9.0 * 1.0 * 1.5
            WHEN cause_name = 'Diabetes mellitus' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Thalassemias' THEN prev_val / 3.0 * 0.4
            WHEN cause_name = 'Neonatal preterm birth' THEN prev_val / 3.0 * 0.01
            WHEN cause_name = 'Neonatal encephalopathy due to birth asphyxia and trauma' THEN prev_val / 3.0 * 0.15
            ELSE 0
          END
        ) as val
      FROM base_data
      GROUP BY year_id, location_name
      ORDER BY location_name, year_id
    ", cause_list, sex_escaped, location_list, age_escaped, metric_escaped)
  } else {
    measure_escaped <- sql_escape(measure)
    sql <- sprintf("
      SELECT year_id as year, location_name, SUM(val) as val
      FROM ihme_data_geo
      WHERE cause_name IN (%s)
        AND sex_label = '%s'
        AND location_name IN (%s)
        AND age_group_name = '%s'
        AND metric_name = '%s'
        AND measure_name = '%s'
      GROUP BY year_id, location_name
      ORDER BY location_name, year_id
    ", cause_list, sex_escaped, location_list, age_escaped, metric_escaped, measure_escaped)
  }

  dbGetQuery(con, sql)
}

# TOP 20/BOTTOM 20 RANKING QUERY: Returns exactly 20 rows
query_ranking_data <- function(con, causes, sex, year, age, metric, measure, order_type, shs_toggle) {
  cause_list <- build_cause_list(causes)
  sex_escaped <- sql_escape(sex)
  age_escaped <- sql_escape(age)
  metric_escaped <- sql_escape(metric)
  order_sql <- if (order_type == "top_20") "DESC" else "ASC"

  if (isTRUE(shs_toggle)) {
    sql <- sprintf("
      WITH base_data AS (
        SELECT
          location_name,
          cause_name,
          age_id,
          income_group,
          SUM(CASE WHEN measure_name = 'Deaths' THEN val ELSE 0 END) as deaths_val,
          SUM(CASE WHEN measure_name = 'Prevalence' THEN val ELSE 0 END) as prev_val,
          SUM(CASE WHEN measure_name = 'Incidence' THEN val ELSE 0 END) as inc_val
        FROM ihme_data_geo
        WHERE cause_name IN (%s)
          AND sex_label = '%s'
          AND year_id = %d
          AND age_group_name = '%s'
          AND metric_name = '%s'
        GROUP BY location_name, cause_name, age_id, income_group
      )
      SELECT
        location_name,
        SUM(
          CASE
            WHEN cause_name = 'Other neglected tropical diseases' THEN deaths_val / 3.0 * 0.05
            WHEN cause_name = 'Multidrug-resistant tuberculosis without extensive drug resistance' THEN (inc_val - deaths_val / 6.0) * 0.5
            WHEN cause_name = 'Extensively drug-resistant tuberculosis' THEN (inc_val - deaths_val / 6.0) * 1.0
            WHEN cause_name = 'HIV/AIDS' THEN prev_val / 3.0 * 0.5
            WHEN cause_name = 'Alzheimer''s disease and other dementias' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Tetanus' THEN deaths_val / 3.0 * 0.5
            WHEN cause_name = 'Parkinson''s disease' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Multiple sclerosis' THEN prev_val / 3.0 * 0.017
            WHEN cause_name = 'Congenital birth defects' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Injuries' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Musculoskeletal disorders' THEN deaths_val / 3.0 * 1.4
            WHEN cause_name = 'Sickle cell disorders' THEN prev_val / 3.0 * 0.6
            WHEN cause_name IN ('Cirrhosis and other chronic liver diseases', 'Other digestive diseases', 'Schistosomiasis')
              THEN deaths_val / 9.0 * 0.5 * 1.5
            WHEN cause_name IN ('Chronic kidney disease', 'Acute glomerulonephritis')
              THEN deaths_val / 6.0 * 1.0
            WHEN cause_name IN ('Intracerebral hemorrhage', 'Ischemic stroke', 'Subarachnoid hemorrhage')
              THEN deaths_val / 9.0 * 1.0 * 1.5
            WHEN cause_name = 'Diabetes mellitus' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Thalassemias' THEN prev_val / 3.0 * 0.4
            WHEN cause_name = 'Neonatal preterm birth' THEN prev_val / 3.0 * 0.01
            WHEN cause_name = 'Neonatal encephalopathy due to birth asphyxia and trauma' THEN prev_val / 3.0 * 0.15
            ELSE 0
          END
        ) as val
      FROM base_data
      GROUP BY location_name
      ORDER BY val %s
      LIMIT 20
    ", cause_list, sex_escaped, year, age_escaped, metric_escaped, order_sql)
  } else {
    measure_escaped <- sql_escape(measure)
    sql <- sprintf("
      SELECT location_name, SUM(val) as val
      FROM ihme_data_geo
      WHERE cause_name IN (%s)
        AND sex_label = '%s'
        AND year_id = %d
        AND age_group_name = '%s'
        AND metric_name = '%s'
        AND measure_name = '%s'
      GROUP BY location_name
      ORDER BY val %s
      LIMIT 20
    ", cause_list, sex_escaped, year, age_escaped, metric_escaped, measure_escaped, order_sql)
  }

  dbGetQuery(con, sql)
}

# SUNBURST/TREEMAP QUERY: Returns ~5400 rows grouped by geography
query_sunburst_data <- function(con, causes, sex, year, age, measure, shs_toggle) {
  cause_list <- build_cause_list(causes)
  sex_escaped <- sql_escape(sex)
  # For sunburst, always use "All ages" if Age-standardized selected and Number metric
  age_for_query <- if (age == "Age-standardized") "All ages" else age
  age_escaped <- sql_escape(age_for_query)

  if (isTRUE(shs_toggle)) {
    sql <- sprintf("
      WITH base_data AS (
        SELECT
          continent,
          subregion,
          location_name,
          cause_name,
          age_id,
          income_group,
          SUM(CASE WHEN measure_name = 'Deaths' THEN val ELSE 0 END) as deaths_val,
          SUM(CASE WHEN measure_name = 'Prevalence' THEN val ELSE 0 END) as prev_val,
          SUM(CASE WHEN measure_name = 'Incidence' THEN val ELSE 0 END) as inc_val
        FROM ihme_data_geo
        WHERE cause_name IN (%s)
          AND sex_label = '%s'
          AND year_id = %d
          AND age_group_name = '%s'
          AND metric_name = 'Number'
        GROUP BY continent, subregion, location_name, cause_name, age_id, income_group
      )
      SELECT
        continent,
        subregion,
        location_name,
        SUM(
          CASE
            WHEN cause_name = 'Other neglected tropical diseases' THEN deaths_val / 3.0 * 0.05
            WHEN cause_name = 'Multidrug-resistant tuberculosis without extensive drug resistance' THEN (inc_val - deaths_val / 6.0) * 0.5
            WHEN cause_name = 'Extensively drug-resistant tuberculosis' THEN (inc_val - deaths_val / 6.0) * 1.0
            WHEN cause_name = 'HIV/AIDS' THEN prev_val / 3.0 * 0.5
            WHEN cause_name = 'Alzheimer''s disease and other dementias' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Tetanus' THEN deaths_val / 3.0 * 0.5
            WHEN cause_name = 'Parkinson''s disease' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Multiple sclerosis' THEN prev_val / 3.0 * 0.017
            WHEN cause_name = 'Congenital birth defects' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Injuries' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Musculoskeletal disorders' THEN deaths_val / 3.0 * 1.4
            WHEN cause_name = 'Sickle cell disorders' THEN prev_val / 3.0 * 0.6
            WHEN cause_name IN ('Cirrhosis and other chronic liver diseases', 'Other digestive diseases', 'Schistosomiasis')
              THEN deaths_val / 9.0 * 0.5 * 1.5
            WHEN cause_name IN ('Chronic kidney disease', 'Acute glomerulonephritis')
              THEN deaths_val / 6.0 * 1.0
            WHEN cause_name IN ('Intracerebral hemorrhage', 'Ischemic stroke', 'Subarachnoid hemorrhage')
              THEN deaths_val / 9.0 * 1.0 * 1.5
            WHEN cause_name = 'Diabetes mellitus' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Thalassemias' THEN prev_val / 3.0 * 0.4
            WHEN cause_name = 'Neonatal preterm birth' THEN prev_val / 3.0 * 0.01
            WHEN cause_name = 'Neonatal encephalopathy due to birth asphyxia and trauma' THEN prev_val / 3.0 * 0.15
            ELSE 0
          END
        ) as val
      FROM base_data
      GROUP BY continent, subregion, location_name
      ORDER BY continent, subregion, location_name
    ", cause_list, sex_escaped, year, age_escaped)
  } else {
    measure_escaped <- sql_escape(measure)
    sql <- sprintf("
      SELECT continent, subregion, location_name, SUM(val) as val
      FROM ihme_data_geo
      WHERE cause_name IN (%s)
        AND sex_label = '%s'
        AND year_id = %d
        AND age_group_name = '%s'
        AND metric_name = 'Number'
        AND measure_name = '%s'
      GROUP BY continent, subregion, location_name
      ORDER BY continent, subregion, location_name
    ", cause_list, sex_escaped, year, age_escaped, measure_escaped)
  }

  dbGetQuery(con, sql)
}

# BUBBLE/PACKED BUBBLE QUERY: Returns data grouped by breakdown category
query_bubble_data <- function(con, causes, sex, year, age, measure, breakdown, shs_toggle) {
  cause_list <- build_cause_list(causes)
  sex_escaped <- sql_escape(sex)
  age_for_query <- if (age == "Age-standardized") "All ages" else age
  age_escaped <- sql_escape(age_for_query)
  # Don't escape breakdown - it's a column name, not a value
  # Use COALESCE to handle NULL values in region_wb
  breakdown_col_expr <- sprintf("COALESCE(%s, 'Other')", breakdown)

  if (isTRUE(shs_toggle)) {
    sql <- sprintf("
      WITH base_data AS (
        SELECT
          location_name,
          %s as breakdown_col,
          cause_name,
          age_id,
          income_group,
          SUM(CASE WHEN measure_name = 'Deaths' THEN val ELSE 0 END) as deaths_val,
          SUM(CASE WHEN measure_name = 'Prevalence' THEN val ELSE 0 END) as prev_val,
          SUM(CASE WHEN measure_name = 'Incidence' THEN val ELSE 0 END) as inc_val
        FROM ihme_data_geo
        WHERE cause_name IN (%s)
          AND sex_label = '%s'
          AND year_id = %d
          AND age_group_name = '%s'
          AND metric_name = 'Number'
        GROUP BY location_name, %s, cause_name, age_id, income_group
      )
      SELECT
        location_name,
        breakdown_col,
        SUM(
          CASE
            WHEN cause_name = 'Other neglected tropical diseases' THEN deaths_val / 3.0 * 0.05
            WHEN cause_name = 'Multidrug-resistant tuberculosis without extensive drug resistance' THEN (inc_val - deaths_val / 6.0) * 0.5
            WHEN cause_name = 'Extensively drug-resistant tuberculosis' THEN (inc_val - deaths_val / 6.0) * 1.0
            WHEN cause_name = 'HIV/AIDS' THEN prev_val / 3.0 * 0.5
            WHEN cause_name = 'Alzheimer''s disease and other dementias' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Tetanus' THEN deaths_val / 3.0 * 0.5
            WHEN cause_name = 'Parkinson''s disease' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Multiple sclerosis' THEN prev_val / 3.0 * 0.017
            WHEN cause_name = 'Congenital birth defects' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Injuries' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Musculoskeletal disorders' THEN deaths_val / 3.0 * 1.4
            WHEN cause_name = 'Sickle cell disorders' THEN prev_val / 3.0 * 0.6
            WHEN cause_name IN ('Cirrhosis and other chronic liver diseases', 'Other digestive diseases', 'Schistosomiasis')
              THEN deaths_val / 9.0 * 0.5 * 1.5
            WHEN cause_name IN ('Chronic kidney disease', 'Acute glomerulonephritis')
              THEN deaths_val / 6.0 * 1.0
            WHEN cause_name IN ('Intracerebral hemorrhage', 'Ischemic stroke', 'Subarachnoid hemorrhage')
              THEN deaths_val / 9.0 * 1.0 * 1.5
            WHEN cause_name = 'Diabetes mellitus' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Thalassemias' THEN prev_val / 3.0 * 0.4
            WHEN cause_name = 'Neonatal preterm birth' THEN prev_val / 3.0 * 0.01
            WHEN cause_name = 'Neonatal encephalopathy due to birth asphyxia and trauma' THEN prev_val / 3.0 * 0.15
            ELSE 0
          END
        ) as val
      FROM base_data
      GROUP BY location_name, breakdown_col
      ORDER BY location_name
    ", breakdown_col_expr, cause_list, sex_escaped, year, age_escaped, breakdown_col_expr)
  } else {
    measure_escaped <- sql_escape(measure)
    sql <- sprintf("
      SELECT location_name, %s as breakdown_col, SUM(val) as val
      FROM ihme_data_geo
      WHERE cause_name IN (%s)
        AND sex_label = '%s'
        AND year_id = %d
        AND age_group_name = '%s'
        AND metric_name = 'Number'
        AND measure_name = '%s'
      GROUP BY location_name, %s
      ORDER BY location_name
    ", breakdown_col_expr, cause_list, sex_escaped, year, age_escaped, measure_escaped, breakdown_col_expr)
  }

  dbGetQuery(con, sql)
}

# TABLE QUERY: Returns ~201 rows
query_table_data <- function(con, causes, sex, year, age, metric, measure, shs_toggle) {
  cause_list <- build_cause_list(causes)
  sex_escaped <- sql_escape(sex)
  age_escaped <- sql_escape(age)
  metric_escaped <- sql_escape(metric)

  if (isTRUE(shs_toggle)) {
    sql <- sprintf("
      WITH base_data AS (
        SELECT
          location_name,
          cause_name,
          age_id,
          income_group,
          SUM(CASE WHEN measure_name = 'Deaths' THEN val ELSE 0 END) as deaths_val,
          SUM(CASE WHEN measure_name = 'Prevalence' THEN val ELSE 0 END) as prev_val,
          SUM(CASE WHEN measure_name = 'Incidence' THEN val ELSE 0 END) as inc_val
        FROM ihme_data_geo
        WHERE cause_name IN (%s)
          AND sex_label = '%s'
          AND year_id = %d
          AND age_group_name = '%s'
          AND metric_name = '%s'
        GROUP BY location_name, cause_name, age_id, income_group
      )
      SELECT
        location_name,
        ROUND(SUM(
          CASE
            WHEN cause_name = 'Other neglected tropical diseases' THEN deaths_val / 3.0 * 0.05
            WHEN cause_name = 'Multidrug-resistant tuberculosis without extensive drug resistance' THEN (inc_val - deaths_val / 6.0) * 0.5
            WHEN cause_name = 'Extensively drug-resistant tuberculosis' THEN (inc_val - deaths_val / 6.0) * 1.0
            WHEN cause_name = 'HIV/AIDS' THEN prev_val / 3.0 * 0.5
            WHEN cause_name = 'Alzheimer''s disease and other dementias' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Tetanus' THEN deaths_val / 3.0 * 0.5
            WHEN cause_name = 'Parkinson''s disease' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Multiple sclerosis' THEN prev_val / 3.0 * 0.017
            WHEN cause_name = 'Congenital birth defects' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Injuries' THEN deaths_val / 3.0 * 0.6
            WHEN cause_name = 'Musculoskeletal disorders' THEN deaths_val / 3.0 * 1.4
            WHEN cause_name = 'Sickle cell disorders' THEN prev_val / 3.0 * 0.6
            WHEN cause_name IN ('Cirrhosis and other chronic liver diseases', 'Other digestive diseases', 'Schistosomiasis')
              THEN deaths_val / 9.0 * 0.5 * 1.5
            WHEN cause_name IN ('Chronic kidney disease', 'Acute glomerulonephritis')
              THEN deaths_val / 6.0 * 1.0
            WHEN cause_name IN ('Intracerebral hemorrhage', 'Ischemic stroke', 'Subarachnoid hemorrhage')
              THEN deaths_val / 9.0 * 1.0 * 1.5
            WHEN cause_name = 'Diabetes mellitus' THEN prev_val / 3.0 * 0.1
            WHEN cause_name = 'Thalassemias' THEN prev_val / 3.0 * 0.4
            WHEN cause_name = 'Neonatal preterm birth' THEN prev_val / 3.0 * 0.01
            WHEN cause_name = 'Neonatal encephalopathy due to birth asphyxia and trauma' THEN prev_val / 3.0 * 0.15
            ELSE 0
          END
        ), 3) as val
      FROM base_data
      GROUP BY location_name
      ORDER BY location_name
    ", cause_list, sex_escaped, year, age_escaped, metric_escaped)
  } else {
    measure_escaped <- sql_escape(measure)
    sql <- sprintf("
      SELECT location_name, ROUND(SUM(val), 3) as val
      FROM ihme_data_geo
      WHERE cause_name IN (%s)
        AND sex_label = '%s'
        AND year_id = %d
        AND age_group_name = '%s'
        AND metric_name = '%s'
        AND measure_name = '%s'
      GROUP BY location_name
      ORDER BY location_name
    ", cause_list, sex_escaped, year, age_escaped, metric_escaped, measure_escaped)
  }

  dbGetQuery(con, sql)
}

# Set highcharter options
options(highcharter.theme = hc_theme_economist(tooltip = list(valueDecimals = 3)))

# Get dropdown values from DuckDB (fast queries)
sexes <- as.matrix(dbGetQuery(db_con, "SELECT DISTINCT sex_label FROM ihme_data ORDER BY sex_label")$sex_label)

ages <- c(
  "Age-standardized",
  "<1 year",
  "1-4 years",
  "5-9 years",
  "10-14 years",
  "15-19 years",
  "20-24 years",
  "25-29 years",
  "30-34 years",
  "35-39 years",
  "40-44 years",
  "45-49 years",
  "50-54 years",
  "55-59 years",
  "60-64 years",
  "65-69 years",
  "70-74 years",
  "75-79 years",
  "80-84",
  "85-89",
  "90-94",
  "95+ years",
  "All ages"
)

ages_thalassemias <- c(
  "<1 year",
  "1-4 years",
  "5-9 years",
  "10-14 years",
  "15-19 years"
)

measures <- as.list(dbGetQuery(db_con, "SELECT DISTINCT measure_name FROM ihme_data ORDER BY measure_name")$measure_name)

years <- as.list(dbGetQuery(db_con, "SELECT DISTINCT year_id FROM ihme_data ORDER BY year_id")$year_id)

health_conditions <- c(
  "All health conditions",
  "Acute glomerulonephritis",
  "Alzheimer's disease and other dementias",
  "Chronic kidney disease",
  "Cirrhosis and other chronic liver diseases",
  "Congenital birth defects",
  "Diabetes mellitus",
  "Drug-susceptible tuberculosis",
  "Extensively drug-resistant tuberculosis",
  "HIV/AIDS",
  "Injuries",
  "Intracerebral hemorrhage",
  "Ischemic stroke",
  "Leukemia",
  "Multidrug-resistant tuberculosis without extensive drug resistance",
  "Multiple sclerosis",
  "Musculoskeletal disorders",
  "Neonatal encephalopathy due to birth asphyxia and trauma",
  "Neonatal preterm birth",
  "Neoplasms",
  "Other digestive diseases",
  "Other neglected tropical diseases",
  "Other neoplasms",
  "Parkinson's disease",
  "Schistosomiasis",
  "Sickle cell disorders",
  "Subarachnoid hemorrhage",
  "Tetanus",
  "Thalassemias"
)

health_conditions_shs <- c(
  "All health conditions",
  "Alzheimer's disease and other dementias",
  "Congenital birth defects",
  "Diabetes mellitus",
  "HIV/AIDS",
  "Kidney diseases",
  "Injuries",
  "Leukemia",
  "Liver diseases",
  "Multiple sclerosis",
  "Musculoskeletal disorders",
  "Neonatal encephalopathy due to birth asphyxia and trauma",
  "Neonatal preterm birth",
  "Neoplasms",
  "Other neglected tropical diseases",
  "Other neoplasms",
  "Parkinson's disease",
  "Sickle cell disorders",
  "Stroke",
  "Tetanus",
  "Thalassemias",
  "Tuberculosis"
)

metrics <- as.list(dbGetQuery(db_con, "SELECT DISTINCT metric_name FROM ihme_data ORDER BY metric_name")$metric_name)

# Use cached map data for coords (geographic data is in DuckDB via ihme_data_geo view)
if (file.exists("mapdata.rds")) {
  mapdata <- read_rds("mapdata.rds")
} else {
  mapdata <- get_data_from_map(download_map_data("custom/world-highres"))
  saveRDS(mapdata, "mapdata.rds")
}

# Note: ihme_data_population.R join with mapdata is now done in DuckDB
# The ihme_data_geo view already has continent, subregion, region_wb columns

coords_data <- mapdata %>%
  rename(
    x = "hc-middle-x",
    y = "hc-middle-y",
    names = "name"
  ) %>%
  select(
    x,
    y,
    names
  )

ui <- semanticPage(
  tags$style(HTML(".btn  { margin-bottom: 10px;}")),
  tags$script('
   document.addEventListener("click", function(e) {
      Shiny.setInputValue("shiftclick", e.shiftKey);
   });
 '),
  tags$script('$(document).on("shiny:sessioninitialized",function(){$Intl.DateTimeFormat().resolvedOptions().timeZone, function(response) {Shiny.setInputValue("getIP", response);});})'),
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$script('
  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
    Shiny.onInputChange("geolocation", false);
    }

   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
          console.log(coords.latitude + ", " + coords.longitude);
          Shiny.onInputChange("geolocation", true);
          Shiny.onInputChange("lat", coords.latitude);
          Shiny.onInputChange("long", coords.longitude);
      }, 1100)
  }
  });
'),
  useShinyjs(),
  margin = "20px",
  # CSS replaces deprecated setSliderColor
  tags$style(HTML("
    .irs-bar, .irs-bar-edge, .irs-single, .irs-to, .irs-from {
      background: #6794A7 !important;
      border-color: #6794A7 !important;
    }
    /* Style the SHS toggle with Zissou1 blue (#3B9AB2) */
    .ui.toggle.checkbox input:checked ~ label:before,
    .ui.toggle.checkbox input:checked ~ .box:before,
    .ui.toggle.checkbox input:focus:checked ~ label:before,
    .ui.toggle.checkbox input:focus:checked ~ .box:before {
      background-color: #3B9AB2 !important;
    }
  ")),
  # Responsive CSS
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0"),
    # JavaScript to handle responsive grid at runtime
    tags$script(HTML("
      $(document).ready(function() {
        function adjustGridForScreenSize() {
          var screenWidth = window.innerWidth;
          var gridContainer = $('.ss-grid').first();

          if (screenWidth <= 1024) {
            // Stack vertically on smaller screens
            gridContainer.css({
              'display': 'block',
              'grid-template-columns': 'none',
              'grid-template-areas': 'none'
            });
            gridContainer.children().css({
              'width': '100%',
              'max-width': '100%',
              'grid-area': 'unset',
              'margin-bottom': '10px'
            });
          } else {
            // Restore grid on larger screens
            gridContainer.css({
              'display': 'grid',
              'grid-template-columns': 'minmax(280px, 350px) 1fr',
              'grid-template-areas': '\"user map\" \"user mfd\"'
            });
            gridContainer.children().each(function() {
              $(this).css({
                'width': '',
                'max-width': ''
              });
            });
          }
        }

        // Run on load and resize
        adjustGridForScreenSize();
        $(window).resize(function() {
          adjustGridForScreenSize();
        });
      });
    ")),
    tags$style(HTML("
      /* Base responsive styles - prevent horizontal overflow */
      html, body {
        overflow-x: hidden !important;
        max-width: 100vw !important;
        box-sizing: border-box;
      }

      *, *::before, *::after {
        box-sizing: border-box;
      }

      .ui.grid {
        margin: 0 !important;
        max-width: 100% !important;
      }

      /* Chart cards - make them flexible */
      .ui.card, .ui.cards > .card {
        box-sizing: border-box;
        max-width: 100% !important;
      }

      /* Semantic page container */
      .pusher, .ui.container {
        max-width: 100% !important;
        overflow-x: hidden !important;
      }

      /* shiny.semantic grid */
      .ss-grid {
        max-width: 100% !important;
        overflow-x: hidden !important;
      }

      .ss-grid > div {
        max-width: 100% !important;
        overflow-x: hidden !important;
      }

      /* Standard monitors: 1024px - 1400px */
      @media screen and (max-width: 1400px) {
        /* Make grid template more flexible */
        .ss-grid {
          display: flex !important;
          flex-wrap: wrap !important;
        }

        .ss-grid > div[style*='grid-area: user'] {
          flex: 0 0 300px !important;
          max-width: 300px !important;
        }

        .ss-grid > div[style*='grid-area: map'],
        .ss-grid > div[style*='grid-area: mfd'] {
          flex: 1 1 calc(100% - 320px) !important;
          max-width: calc(100% - 320px) !important;
        }
      }

      /* Tablet: 768px - 1024px */
      @media screen and (max-width: 1024px) {
        /* Stack grid vertically */
        .ss-grid {
          display: block !important;
        }

        /* Settings panel: full width on tablets */
        .ss-grid > div[style*='grid-area: user'],
        .ss-grid > div[style*='grid-area: map'],
        .ss-grid > div[style*='grid-area: mfd'] {
          width: 100% !important;
          max-width: 100% !important;
          margin: 5px 0 !important;
        }

        /* Chart cards: side by side but smaller */
        .ui.card[style*='width: 48%'] {
          width: 48% !important;
          min-width: 280px;
        }

        /* Reduce margins */
        .ui.form .field {
          margin-bottom: 0.5em !important;
        }

        /* Smaller headers */
        .ui.header {
          font-size: 1.1em !important;
        }

        h3.ui.header {
          font-size: 1em !important;
        }
      }

      /* Mobile: below 768px */
      @media screen and (max-width: 768px) {
        /* Full width for all major sections */
        .ss-grid > div {
          width: 100% !important;
          max-width: 100% !important;
          margin: 5px 0 !important;
        }

        /* Chart cards: stack vertically */
        .ui.card[style*='width: 48%'],
        .cards .ui.card,
        .ui.card {
          width: 100% !important;
          max-width: 100% !important;
          margin: 10px 0 !important;
        }

        /* Cards container: stack children */
        .ui.cards {
          flex-direction: column !important;
        }

        /* Reduce page margin */
        body {
          padding: 5px !important;
          margin: 5px !important;
        }

        /* Radio buttons: stack if needed */
        .inline.fields {
          flex-wrap: wrap !important;
        }

        .inline.fields .field {
          margin-bottom: 5px !important;
        }

        /* Smaller fonts */
        .ui.header {
          font-size: 1em !important;
        }

        h3.ui.header {
          font-size: 0.95em !important;
        }

        /* Make dropdowns full width */
        .ui.dropdown, .ui.selection.dropdown {
          width: 100% !important;
          max-width: 100% !important;
        }

        /* Adjust chart heights for mobile */
        .highcharts-container {
          min-height: 250px !important;
        }

        /* Tab menu: scrollable on mobile */
        .ui.tabular.menu {
          overflow-x: auto;
          flex-wrap: nowrap;
        }

        .ui.tabular.menu .item {
          flex-shrink: 0;
        }

        /* Ensure tables don't overflow */
        .dataTables_wrapper {
          max-width: 100% !important;
          overflow-x: auto !important;
        }

        table.dataTable {
          width: 100% !important;
        }
      }

      /* Small mobile: below 480px */
      @media screen and (max-width: 480px) {
        /* Even more compact */
        .ui.form .field {
          margin-bottom: 0.3em !important;
        }

        /* Smaller padding */
        .ui.card > .content {
          padding: 0.8em !important;
        }

        /* Radio labels smaller */
        .ui.radio.checkbox label {
          font-size: 0.85em !important;
        }

        /* DataTable adjustments */
        .dataTables_wrapper {
          font-size: 0.85em;
        }

        table.dataTable {
          font-size: 0.8em !important;
        }
      }

      /* Ensure highcharts are responsive */
      .highcharts-container, .highchart {
        width: 100% !important;
        max-width: 100% !important;
      }
    "))
  ),
  grid(
    myGridTemplate,
    area_styles = list(
      title = "margin: 10px",
      mfd = "margin: 10px",
      user = "margin: 10px",
      map = "margin: 10px",
      bubble = "margin: 10px",
      sunburst = "margin: 10px"
    ),
    title = card(
      style = "border-radius: 0; width: 100%; background: #ffffff",
      div(
        class = "content",
tags$h1(
          style = "margin: 0; font-size: 1.8em; color: #333;",
          "Serious Health-Related Suffering (SHS)"
        )
      )
    ),
    mfd = card(
      style = "border-radius: 0; width: 100%; background: #ffffff",
      div(
        class = "content",
        tabset(
          tabs = list(
            list(
              menu =
                h3(
                  list(
                    icon("bar chart icon"),
                    " ",
                    "SHS Bar Charts"
                  )
                ),
              content =
                div(
                  class = "ui cards",
                  style = "border-radius: 0; width: 100%; background: #ffffff",
                  card(
                    class = "ui card",
                    style = "border-radius: 0; width: 48%; background: #ffffff",
                    div(
                      class = "content",
                      form(
                        class = "ui form",
                        field(
                          class = "inline fields",
                          tags$h3(
                            class = "ui header",
                            "SHS Historical Chart"
                          ),
                          icon("ruler horizontal icon", style = "margin-left: 10px; margin-right: 5px; color: #666;"),
                          multiple_radio(
                            input_id = "radio_indicator",
                            label = "",
                            choices = c(
                              "per 100k",
                              "Absolute"
                            ),
                            choices_value = c(
                              "Rate",
                              "Number"
                            ),
                            selected = "Rate",
                            position = "inline",
                            type = "radio"
                          )
                        ),
                        withSpinner(
                          highchartOutput(
                            "shs_historical_chart"
                          ),
                          type = 8,
                          color = "#6794A7",
                          hide.ui = TRUE
                        )
                      )
                    )
                  ),
                  card(
                    style = "border-radius: 0; width: 48%; background: #ffffff",
                    div(
                      class = "content",
                      form(
                        class = "ui form",
                        field(
                          class = "inline fields",
                          tags$h3(
                            class = "ui header",
                            "SHS Country Ranking"
                          ),
                          multiple_radio(
                            input_id = "order",
                            label = "",
                            choices = c(
                              "Top 20",
                              "Bottom 20"
                            ),
                            choices_value = c(
                              "top_20",
                              "bottom_20"
                            ),
                            selected = "top_20",
                            position = "inline",
                            type = "radio"
                          )
                        ),
                        withSpinner(
                          ui_element = highchartOutput(
                            "shs_top_ten"
                          ),
                          type = 8,
                          color = "#6794A7",
                          hide.ui = TRUE
                        )
                      )
                    )
                  )
                ),
              id = "shs_history_tab"
            ),
            list(
              menu =
                h3(
                  list(
                    icon("sitemap icon"),
                    " ",
                    "SHS Hierarchy"
                  )
                ),
              content =
                div(
                  class = "ui cards",
                  style = "border-radius: 0; width: 100%; background: #ffffff",
                  card(
                    style = "border-radius: 0; width: 48%; background: #ffffff",
                    div(
                      class = "content",
                      form(
                        class = "ui form",
                        field(
                          class = "inline fields",
                          tags$h3(
                            class = "ui header",
                            "SHS Hierarchy"
                          ),
                          div(),
                          multiple_radio(
                            input_id = "sunburst_treemap",
                            label = "",
                            choices = c(
                              "Sunburst",
                              "Treemap"
                            ),
                            choices_value = c(
                              "sunburst",
                              "treemap"
                            ),
                            selected = "sunburst",
                            position = "inline",
                            type = "radio"
                          )
                        ),
                        withSpinner(
                          ui_element = highchartOutput(
                            "shs_sunburst"
                          ),
                          type = 8,
                          color = "#6794A7",
                          hide.ui = TRUE
                        )
                      )
                    )
                  ),
                  card(
                    style = "border-radius: 0; width: 48%; background: #ffffff",
                    div(
                      class = "content",
                      form(
                        class = "ui form",
                        field(
                          class = "inline fields",
                          tags$h3(
                            class = "ui header",
                            "SHS Circle Packing"
                          ),
                          multiple_radio(
                            input_id = "breakdown",
                            label = "",
                            choices = c(
                              "Continents",
                              "Regions",
                              "Subregions",
                              "Income"
                            ),
                            choices_value = c(
                              "continent",
                              "region_wb",
                              "subregion",
                              "income_group"
                            ),
                            selected = "continent",
                            position = "inline",
                            type = "radio"
                          )
                        ),
                        withSpinner(
                          ui_element = highchartOutput(
                            "shs_bubble"
                          ),
                          type = 8,
                          color = "#6794A7",
                          hide.ui = TRUE
                        )
                      )
                    )
                  )
                ),
              id = "shs_ranking_tab"
            ),
            list(
              menu =
                h3(
                  list(
                    icon("table icon"),
                    " ",
                    "SHS Table"
                  )
                ),
              content =
                div(
                  class = "ui cards",
                  style = "border-radius: 0; width: 100%; background: #ffffff",
                  div(
                    class = "ui card",
                    style = "border-radius: 0; width: 100%; background: #ffffff",
                    div(
                      class = "content",
                      form(
                        class = "ui form",
                        field(
                          class = "inline fields",
                          tags$h3(
                            class = "ui header",
                            "SHS Table"
                          ),
                          div()
                        )
                      ),
                      div(
                        class = "header",
                        textOutput("shs_table_title"),
                        div(
                          class = "meta",
                          textOutput("shs_table_sub_title")
                        )
                      ),
                      withSpinner(
                        ui_element = semantic_DTOutput(
                          "shs_table"
                        ),
                        type = 8,
                        color = "#6794A7",
                        hide.ui = TRUE
                      )
                    )
                  )
                ), id = "shs_table_tab"
            )
          ),
          menu_class = "ui secondary pointing menu"
        )
      )
    ),
    map = card(
      style = "border-radius: 0; height: 100%; width: 100%; background: #ffffff",
      div(
        class = "content",
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h3(
              class = "ui header",
              icon("globe icon"),
              "SHS World Map"
            ),
            multiple_radio(
              input_id = "radio_scale",
              label = "",
              choices = c(
                "Linear",
                "Logarithmic"
              ),
              choices_value = c(
                "linear",
                "logarithmic"
              ),
              selected = "linear",
              position = "inline",
              type = "radio",
              icon = "ruler horizontal icon"
            )
          ),
          withSpinner(
            ui_element = highchartOutput(
              "shs_map",
              height = "48vh"
            ),
            type = 8,
            color = "#6794A7",
            hide.ui = FALSE
          )
        )
      )
    ),
    user = card(
      style = "border-radius: 0; height = 100%; width: 100%; background: #F5F5F5",
      div(
        class = "content",
        tags$h3(
          class = "ui header",
          icon("cog icon"),
          "SHS World Map Settings"
        ),
        actionButton(
          "reset_settings",
          "Reset to defaults",
          class = "ui mini basic button",
          style = "margin-bottom: 10px;"
        ),
        br(),
        form(
          class = "ui equal width form",
          field(
            class = "inline fields",
            tags$h5(
              class = "ui header",
              icon("weight icon"),
              div(
                class = "content",
                "Measure"
              )
            ),
            dropdown_input(
              input_id = "shs_measure_name",
              choices = measures,
              value = "Prevalence",
              type = "ui fluid search selection dropdown"
            )
          ),
          field(
            class = "inline fields",
            shiny.semantic::toggle(
              input_id = "shs_toggle",
              label = tags$h5("Calculate SHS"),
              is_marked = FALSE
            )
          )
        ),
        br(),
        form(
          class = "ui equal width form",
          field(
            class = "inline fields",
            tags$h5(
              class = "ui header",
              icon("stethoscope icon"),
              div(
                class = "content",
                "Cause"
              )
            ),
            dropdown_input(
              "shs_health_condition_name",
              choices = health_conditions,
              value = "All health conditions",
              type = "selection fluid"
            )
          )
        ),
        br(),
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h5(
              class = "ui header",
              icon("user clock icon"),
              div(
                class = "content",
                "Age"
              )
            ),
            dropdown_input(
              input_id = "shs_age_group_name",
              choices = ages,
              default_text = "Select age group",
              value = "Age-standardized",
              type = "selection fluid"
            )
          )
        ),
        br(),
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h5(
              class = "ui header",
              icon("mars venus icon"),
              div(
                class = "content",
                "Sex"
              )
            ),
            multiple_radio(
              input_id = "shs_sex_label",
              label = "",
              choices = c(
                "Both",
                "Male",
                "Female"
              ),
              choices_value = c(
                "Both",
                "Male",
                "Female"
              ),
              selected = "Both",
              position = "inline",
              type = "radio"
            )
          )
        ),
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h5(
              class = "ui header",
              icon("history icon"),
              div(
                class = "content",
                "Year"
              )
            ),
            sliderTextInput(
              inputId = "shs_year_id",
              label = NULL,
              choices = as.character(years),
              selected = as.character(max(unlist(years))),
              animate = TRUE,
              grid = TRUE,
              force_edges = TRUE
            )
          )
        )
      )
    ),
    info = card(
      style = "border-radius: 0; width: 100%; background: #ffffff",
      div(
        class = "content",
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h3(
              class = "ui header",
              icon("bar chart icon"),
              "SHS Historical Chart"
            ),
            tags$span(
              style = "color: #666; font-size: 12px; margin-left: 10px;",
              "Shift+click on map to compare countries"
            )
          ),
          withSpinner(
            highchartOutput(
              "shs_historical_chart"
            ),
            type = 8,
            color = "#6794A7",
            hide.ui = TRUE
          )
        )
      )
    ),
    top_ten = card(
      style = "border-radius: 0; width: 100%; background: #ffffff",
      div(
        class = "content",
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h3(
              class = "ui header",
              icon("sort icon"),
              "SHS Country Ranking"
            ),
            multiple_radio(
              input_id = "order",
              label = "",
              choices = c(
                "Top 20",
                "Bottom 20"
              ),
              choices_value = c(
                "top_20",
                "bottom_20"
              ),
              selected = "top_20",
              position = "inline",
              type = "radio"
            )
          ),
          withSpinner(
            ui_element = highchartOutput(
              "shs_top_ten"
            ),
            type = 8,
            color = "#6794A7",
            hide.ui = TRUE
          )
        )
      )
    ),
    bubble = card(
      style = "border-radius: 0; width:100%; background: #ffffff",
      div(
        class = "content",
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h3(
              class = "ui header",
              icon("bowling ball icon"),
              "SHS Circle Packing"
            ),
            multiple_radio(
              input_id = "breakdown",
              label = "",
              choices = c(
                "Continents",
                "Regions",
                "Subregions",
                "Income"
              ),
              choices_value = c(
                "continent",
                "region_wb",
                "subregion",
                "income_group"
              ),
              selected = "continent",
              position = "inline",
              type = "radio"
            )
          ),
          withSpinner(
            ui_element = highchartOutput(
              "shs_bubble",
              height = "90vh"
            ),
            type = 8,
            color = "#6794A7",
            hide.ui = TRUE
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Map
  click_js <- JS("function(event) {Shiny.setInputValue('mapclick',event.point.name);}")

  observeEvent(input$controller, {
    updateTabsetPanel(session, "hidden_tabs", selected = paste0("panel", input$controller))
  })

  output$browser_height <- renderText({
    paste0(shinybrowser::get_height())
  })

  observeEvent(input$chartclick, {
    output$chartclick <- renderPrint({
      input$chartclick
    })
  })

  observeEvent(input$mapclick, {
    output$mapclick <- renderPrint({
      input$mapclick
    })
  })

  observeEvent(input$shiftclick, {
    output$shiftclick <- renderPrint({
      input$shiftclick
    })
  })

  # Reset settings button handler
  observeEvent(input$reset_settings, {
    # Reset all settings to defaults
    update_dropdown_input(session, "shs_measure_name", value = "Prevalence")
    update_dropdown_input(session, "shs_health_condition_name",
                          choices = health_conditions, value = "All health conditions")
    update_dropdown_input(session, "shs_age_group_name",
                          choices = ages, value = "Age-standardized")
    shinyWidgets::updateSliderTextInput(session, "shs_year_id", selected = as.character(max(unlist(years))))
    # Reset sex radio to "Both" using JavaScript
    shinyjs::runjs("$('#shs_sex_label input[value=\"Both\"]').prop('checked', true).trigger('change');")
    # Reset indicator radio to "Rate" using JavaScript
    shinyjs::runjs("$('#radio_indicator input[value=\"Rate\"]').prop('checked', true).trigger('change');")
    # Disable SHS toggle (turn it off)
    shinyjs::runjs("$('#shs_toggle').checkbox('uncheck');")
    shinyjs::enable("shs_measure_name")
  })

  # SHS toggle observer - disable measure dropdown when SHS is on
  observeEvent(input$shs_toggle, {
    if (isTRUE(input$shs_toggle)) {
      shinyjs::disable("shs_measure_name")
      # Update health conditions to SHS-specific list
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions_shs,
        value = if (input$shs_health_condition_name %in% health_conditions_shs) {
          input$shs_health_condition_name
        } else {
          "All health conditions"
        }
      )
    } else {
      shinyjs::enable("shs_measure_name")
      # Restore full health conditions list
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions,
        value = if (input$shs_health_condition_name %in% health_conditions) {
          input$shs_health_condition_name
        } else {
          "All health conditions"
        }
      )
    }
  })

  observeConditions <- reactive({
    list(
      input$shs_measure_name,
      input$shs_health_condition_name
    )
  })

  observeEvent(observeConditions(), {
    if (input$shs_measure_name == "SHS" & input$shs_health_condition_name == "Thalassemias") {
      update_dropdown_input(
        session,
        "shs_age_group_name",
        choices = ages_thalassemias,
        value = case_when(
          input$shs_age_group_name %in% ages_thalassemias ~ input$shs_age_group_name,
          TRUE ~ "<1 year"
        )
      )
    } else {
      update_dropdown_input(
        session,
        "shs_age_group_name",
        choices = ages,
        value = input$shs_age_group_name
      )
    }
  })

  observeEvent(input$shs_measure_name, {
    if (input$shs_measure_name == "SHS") {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions_shs,
        value = case_when(
          input$shs_health_condition_name %in% c(
            "Drug-susceptible tuberculosis",
            "Multidrug-resistant tuberculosis without extensive drug resistance",
            "Extensively drug-resistant tuberculosis"
          ) ~ "Tuberculosis",
          TRUE ~ input$shs_health_condition_name
        )
      )
    } else {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions,
        value = case_when(
          input$shs_health_condition_name == "Tuberculosis" ~ "Drug-susceptible tuberculosis",
          TRUE ~ input$shs_health_condition_name
        )
      )
    }
  })

  observeEvent(input$shs_measure_name, {
    if (input$shs_measure_name == "SHS") {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions_shs,
        value = case_when(
          input$shs_health_condition_name %in% c(
            "Intracerebral hemorrhage",
            "Ischemic stroke",
            "Subarachnoid hemorrhage"
          ) ~ "Stroke",
          TRUE ~ input$shs_health_condition_name
        )
      )
    } else {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions,
        value = case_when(
          input$shs_health_condition_name == "Stroke" ~ "Ischemic stroke",
          TRUE ~ input$shs_health_condition_name
        )
      )
    }
  })

  observeEvent(input$shs_measure_name, {
    if (input$shs_measure_name == "SHS") {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions_shs,
        value = case_when(
          input$shs_health_condition_name %in% c(
            "Cirrhosis and other chronic liver diseases",
            "Other digestive diseases",
            "Schistosomiasis"
          ) ~ "Liver diseases",
          TRUE ~ input$shs_health_condition_name
        )
      )
    } else {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions,
        value = case_when(
          input$shs_health_condition_name == "Liver diseases" ~ "Cirrhosis and other chronic liver diseases",
          TRUE ~ input$shs_health_condition_name
        )
      )
    }
  })

  observeEvent(input$shs_measure_name, {
    if (input$shs_measure_name == "SHS") {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions_shs,
        value = case_when(
          input$shs_health_condition_name %in% c(
            "Acute glomerulonephritis",
            "Chronic kidney disease"
          ) ~ "Kidney diseases",
          TRUE ~ input$shs_health_condition_name
        )
      )
    } else {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions,
        value = case_when(
          input$shs_health_condition_name == "Kidney diseases" ~ "Chronic kidney disease",
          TRUE ~ input$shs_health_condition_name
        )
      )
    }
  })

  # printing output
  # we are using cat() instead of print function
  # so that we can use new line
  ip_country <- reactive({
    sub(
      pattern = ":.*",
      map.where(
        database = "world",
        x = input$long,
        y = input$lat
      ),
      replacement = ""
    )
  })

  output$ip_country <- renderPrint({
    ip_country()
  })

  select_location <- reactive({
    # Only depend on map click and initial IP country, not on input$shiftclick
    # This prevents the reactive from re-running when other inputs change
    click <- ifelse(is.null(input$mapclick),
      ip_country(),
      input$mapclick
    )

    # Return current selections if they exist and no new click was made
    if (length(selected_ids$ids) > 0) {
      return(selected_ids$ids)
    }

    click
  })

  # Track the last processed mapclick to avoid re-processing on map re-render
  last_processed_click <- reactiveVal(NULL)

  # Handle map clicks separately with observeEvent to properly manage state
  observeEvent(input$mapclick, {
    click <- input$mapclick

    # Skip if this is the same click as last time (can happen when map re-renders)
    if (!is.null(last_processed_click()) && identical(click, last_processed_click())) {
      return()
    }
    last_processed_click(click)

    clicked_ids$ids <- unique(c(
      click,
      first(clicked_ids$ids)
    ))

    if (isFALSE(input$shiftclick)) {
      # Regular click - reset to single country
      shift_clicked_ids$ids <- NULL
      selected_ids$ids <- NULL
    } else {
      # Shift+click - accumulate countries
      shift_clicked_ids$ids <- unique(c(
        clicked_ids$ids,
        shift_clicked_ids$ids
      ))

      selected_ids$ids <- unique(c(
        shift_clicked_ids$ids,
        clicked_ids$ids
      )) %>%
        na.omit()
    }
  }, ignoreInit = TRUE)

  output$select_location <- renderPrint({
    select_location()
  })



  output$health_condition <- renderPrint({
    input$shs_health_condition_name
  })

  output$age_name <- renderPrint({
    input$shs_age_group_name
  })

  output$sex_name <- renderPrint({
    input$shs_sex_label
  })

  output$year <- renderPrint({
    input$shs_year_id
  })

  output$metric <- renderPrint({
    input$radio_indicator
  })

  output$breakdown <- renderPrint({
    input$breakdown
  })

  # create empty vector to hold all click ids
  clicked_ids <- reactiveValues(ids = vector())
  selected_ids <- reactiveValues(ids = vector())
  shift_clicked_ids <- reactiveValues(ids = vector())

  toListen <- reactive({
    list(
      input$mapclick,
      input$getIP
    )
  })

  adapt_type <- reactive({
    case_when(
      length(selected_ids$ids) > 1 ~ "line",
      .default = "column"
    )
  })

  # ============================================
  # MEMORY-OPTIMIZED DATA REACTIVES
  # Each chart queries only what it needs from DuckDB
  # Replaces the old shs() and shs_calculated() that cached 424MB
  # ============================================

  # Reactive for selected causes (used by all chart queries)
  selected_causes <- reactive({
    build_cause_filter(input$shs_health_condition_name, input$shs_toggle)
  })

  # MAP DATA: ~201 rows, ~16KB (was 424MB)
  shs_map_data <- reactive({
    query_map_data(
      db_con,
      selected_causes(),
      input$shs_sex_label,
      as.integer(input$shs_year_id),
      input$shs_age_group_name,
      input$shs_measure_name,
      input$shs_toggle
    )
  })

  # HISTORICAL CHART DATA: ~4 rows per country
  shs_historical_data <- reactive({
    age <- if (input$radio_indicator == "Number" && input$shs_age_group_name == "Age-standardized") {
      "All ages"
    } else {
      input$shs_age_group_name
    }
    query_historical_data(
      db_con,
      selected_causes(),
      input$shs_sex_label,
      select_location(),
      age,
      input$radio_indicator,
      input$shs_measure_name,
      input$shs_toggle
    )
  })

  # RANKING DATA: exactly 20 rows
  shs_ranking_data <- reactive({
    age <- if (input$radio_indicator == "Number" && input$shs_age_group_name == "Age-standardized") {
      "All ages"
    } else {
      input$shs_age_group_name
    }
    query_ranking_data(
      db_con,
      selected_causes(),
      input$shs_sex_label,
      as.integer(input$shs_year_id),
      age,
      input$radio_indicator,
      input$shs_measure_name,
      input$order,
      input$shs_toggle
    )
  })

  # SUNBURST DATA: ~201 rows grouped by geography
  shs_sunburst_data <- reactive({
    query_sunburst_data(
      db_con,
      selected_causes(),
      input$shs_sex_label,
      as.integer(input$shs_year_id),
      input$shs_age_group_name,
      input$shs_measure_name,
      input$shs_toggle
    )
  })

  # BUBBLE CHART DATA: ~201 rows grouped by breakdown
  shs_bubble_data <- reactive({
    query_bubble_data(
      db_con,
      selected_causes(),
      input$shs_sex_label,
      as.integer(input$shs_year_id),
      input$shs_age_group_name,
      input$shs_measure_name,
      input$breakdown,
      input$shs_toggle
    )
  })

  # TABLE DATA: ~201 rows
  shs_table_data <- reactive({
    age <- if (input$radio_indicator == "Number" && input$shs_age_group_name == "Age-standardized") {
      "All ages"
    } else {
      input$shs_age_group_name
    }
    query_table_data(
      db_con,
      selected_causes(),
      input$shs_sex_label,
      as.integer(input$shs_year_id),
      age,
      input$radio_indicator,
      input$shs_measure_name,
      input$shs_toggle
    )
  })

  # Sunburst module ----
  # Data is already filtered by query_sunburst_data()

  output$shs_sunburst <- renderHighchart({
    shs <- shs_sunburst_data()

    # Handle empty data
    if (nrow(shs) == 0) {
      return(highchart() %>% hc_title(text = "No data available"))
    }

    sunburst <- data_to_hierarchical(
      data = shs,
      group_vars = c(continent, subregion, location_name),
      size_var = val,
      colors = wes_palette(
        name = "Zissou1",
        n = max(1, length(unique(shs$continent))),
        type = "continuous"
      )
    )

    hchart(
      sunburst,
      type = input$sunburst_treemap,
      allowDrillToNode = TRUE,
      animationLimit = 1000,
      dataLabels = list(
        enabled = FALSE
      ),
      levels = list(
        level = 1,
        dataLabels = list(
          enabled = FALSE
        )
      )
    ) %>%
      hc_title(
        text = paste0(
          if (isTRUE(input$shs_toggle)) "SHS" else input$shs_measure_name,
          ": ",
          input$shs_health_condition_name
        ),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_subtitle(
        text = paste0("Age: ", input$shs_age_group_name, ", ", "Sex: ", input$shs_sex_label, ", ", "Year: ", input$shs_year_id),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_tooltip(valueDecimals = 0) %>%
      hc_exporting(
        enabled = FALSE,
        filename = "custom-file-name"
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_loading() %>%
      hc_boost(enabled = TRUE)
  })

  # bubble module ----
  # Data is already filtered by query_bubble_data()

  output$shs_bubble <- renderHighchart({
    shs <- shs_bubble_data()

    # Handle empty data
    if (nrow(shs) == 0) {
      return(highchart() %>% hc_title(text = "No data available"))
    }

    # Rename breakdown_col to the actual breakdown name for proper grouping
    breakdown <- input$breakdown

    highchart() %>%
      hc_add_series(
        data = shs,
        type = "packedbubble",
        hcaes(
          location_name,
          value = val,
          group = breakdown_col
        ),
        legendIndex = if (breakdown == "income_group") {
          c(4, 1, 2, 3)
        } else {
          NA
        }
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "<b>{point.name}:</b> {point.value}"
      ) %>%
      hc_plotOptions(
        packedbubble = list(
          maxSize = "150%",
          zMin = 0,
          layoutAlgorithm = list(
            gravitationalConstant = 0.05,
            splitSeries = TRUE,
            seriesInteraction = FALSE,
            dragBetweenSeries = TRUE,
            parentNodeLimit = TRUE
          ),
          dataLabels = list(
            enabled = TRUE,
            format = "{point.name}",
            filter = list(
              property = "y",
              operator = ">",
              value = 250
            )
          )
        )
      ) %>%
      hc_title(
        text = paste0(
          if (isTRUE(input$shs_toggle)) "SHS" else input$shs_measure_name,
          ": ",
          input$shs_health_condition_name
        ),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_subtitle(
        text = paste0("Age: ", input$shs_age_group_name, ", ", "Sex: ", input$shs_sex_label, ", ", "Year: ", input$shs_year_id),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_tooltip(valueDecimals = 0) %>%
      hc_exporting(
        enabled = FALSE,
        filename = "custom-file-name"
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_loading() %>%
      hc_boost(enabled = TRUE) %>%
      hc_colors(wes_palette(name = "Zissou1", n = max(1, length(unique(shs$breakdown_col))), type = "continuous")[1:max(1, length(unique(shs$breakdown_col)))])
  })

  # map module ----
  # Data is already filtered by query_map_data()

  output$shs_map <- renderHighchart({
    shs <- shs_map_data()

    # Handle empty data
    if (nrow(shs) == 0) {
      return(highchart() %>% hc_title(text = "No data available"))
    }

    hcmap(
      map = "custom/world-highres",
      data = shs,
      value = "val",
      joinBy = c(
        "name",
        "location_name"
      ),
      name = if (isTRUE(input$shs_toggle)) "SHS" else input$shs_measure_name,
      download_map_data = TRUE
    ) %>%
      hc_colorAxis(
        stops = color_stops(
          colors = wes_palette("IsleofDogs2")
        ),
        type = input$radio_scale,
        title = if (isTRUE(input$shs_toggle)) "SHS" else input$shs_measure_name
      ) %>%
      hc_title(
        text = paste0(
          if (isTRUE(input$shs_toggle)) "SHS" else input$shs_measure_name,
          ": ",
          input$shs_health_condition_name
        ),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_subtitle(
        text = paste0("Age: ", input$shs_age_group_name, ", ", "Sex: ", input$shs_sex_label, ", ", "Year: ", input$shs_year_id),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_tooltip(valueDecimals = 3) %>%
      hc_mapNavigation(
        enabled = TRUE,
        enableMouseWheelZoom = TRUE,
        enableDoubleClickZoom = TRUE
      ) %>%
      hc_exporting(
        enabled = FALSE,
        filename = "custom-file-name"
      ) %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(
              click = JS("function(event) {Shiny.setInputValue('mapclick', event.point.name);}")
            )
          )
        ),
        allowSelectPoints = TRUE
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_loading() %>%
      hc_boost(enabled = TRUE)
  })

  # historical chart module ----
  # Data is already filtered by query_historical_data()

  output$shs_historical_chart <- renderHighchart({
    shs <- shs_historical_data()

    # Handle empty data
    if (nrow(shs) == 0) {
      return(highchart() %>% hc_title(text = "No data available"))
    }

    # Convert year to factor for proper display
    shs <- shs %>%
      mutate(year = factor(year)) %>%
      arrange(location_name, year)

    chart_type <- adapt_type()
    metric <- input$radio_indicator

    hchart(
      shs,
      type = chart_type,
      showInLegend = TRUE,
      mapping = hcaes(
        x = as.character(year),
        y = val,
        group = location_name
      )
    ) %>%
      hc_title(
        text = paste0(
          if (isTRUE(input$shs_toggle)) "SHS" else input$shs_measure_name,
          ": ",
          input$shs_health_condition_name
        ),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_subtitle(
        text = paste0("Age: ", input$shs_age_group_name, ", ", "Sex: ", input$shs_sex_label),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_tooltip(valueDecimals = 3) %>%
      hc_xAxis(
        title = list(text = "Year"),
        categories = sort(unique(shs$year))
      ) %>%
      hc_exporting(
        enabled = FALSE,
        filename = "custom-file-name"
      ) %>%
      hc_yAxis(
        title = list(
          text = case_when(
            metric == "Number" ~ "Absolute number of cases",
            metric == "Rate" ~ "Cases per 100,000 people"
          )
        )
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_loading() %>%
      hc_boost(enabled = TRUE)
  })

  # Top Ten module ----
  # Data is already filtered and sorted by query_ranking_data()

  output$shs_top_ten <- renderHighchart({
    shs <- shs_ranking_data()

    # Handle empty data
    if (nrow(shs) == 0) {
      return(highchart() %>% hc_title(text = "No data available"))
    }

    metric <- input$radio_indicator

    hchart(
      object = shs,
      type = "bar",
      showInLegend = FALSE,
      mapping = hcaes(
        x = location_name,
        y = val
      )
    ) %>%
      hc_title(
        text = paste0(
          if (isTRUE(input$shs_toggle)) "SHS" else input$shs_measure_name,
          ": ",
          input$shs_health_condition_name
        ),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_subtitle(
        text = paste0("Age: ", input$shs_age_group_name, ", ", "Sex: ", input$shs_sex_label, ", ", "Year: ", input$shs_year_id),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_xAxis(
        type = "category",
        title = list(text = "Country or territory"),
        labels = list(
          align = "right",
          useHTML = TRUE,
          formatter = JS(
            "function(){
            return this.value + ' <i class=\"' + this.value.toLowerCase() + ' flag\"></i>';
            }"
          )
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = case_when(
            metric == "Number" ~ "Absolute number of cases",
            metric == "Rate" ~ "Cases per 100,000 people"
          )
        )
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_loading() %>%
      hc_exporting(
        enabled = FALSE,
        filename = "custom-file-name"
      ) %>%
      hc_boost(enabled = TRUE)
  })

  # Table module ----
  # Data is already filtered by query_table_data()

  output$shs_table_title <- renderText({
    paste0(
      if (isTRUE(input$shs_toggle)) "SHS" else input$shs_measure_name,
      ": ",
      input$shs_health_condition_name
    )
  })

  output$shs_table_sub_title <- renderText({
    paste0(
      "Age: ",
      input$shs_age_group_name,
      ", ",
      "Sex: ",
      input$shs_sex_label,
      ", ",
      "Year: ",
      input$shs_year_id
    )
  })

  output$shs_table <- DT::renderDataTable({
    shs <- shs_table_data()

    # Return empty table if no data
    if (nrow(shs) == 0) {
      return(datatable(data.frame(location_name = character(), val = numeric()),
                       colnames = c("Country or territory", "Value")))
    }

    datatable(
      shs,
      options = list(
        pageLength = 10,
        lengthChange = FALSE
      ),
      colnames = c("Country or territory", "Value")
    )
  })

  output$cols <- renderPrint({
    input$breakdown
  })

  iso_a2 <- reactive({
    ifelse(is.null(input$mapclick),
      mapdata %>%
        dplyr::filter(name == "Germany") %>%
        dplyr::select("iso-a2") %>%
        as.character(),
      mapdata %>%
        dplyr::filter(name == input$mapclick) %>%
        dplyr::select("iso-a2") %>%
        as.character()
    )
  })

  output$flag <- renderUI({
    flag(iso_a2())
  })

  output$condition <- reactive({
    length(select_location()) == 1
  })

  outputOptions(
    output,
    "condition",
    suspendWhenHidden = FALSE
  )

  # Clicked country
  output$country <- renderText({
    ifelse(is.null(input$mapclick),
      ip_country(),
      mapdata %>%
        dplyr::filter(name == input$mapclick) %>%
        dplyr::select("name") %>%
        as.character()
    )
  })


  output$responsive <- renderPrint({
    ifelse(
      input$isMobile,
      "grouped",
      "inline"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
