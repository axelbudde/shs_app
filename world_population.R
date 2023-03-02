world_population_filtered <- world_population %>%
  filter(
    Time == 1990 |
      Time == 2000 |
      Time == 2010 |
      Time == 2019,
    Location %in% ihme_data.R$location_name
  )

world_population_filtered <- world_population_filtered %>%
  mutate(
    age_group_name = case_when(
      AgeGrp == "0" ~ "<1 year",
      between(as.numeric(AgeGrp), 1, 4) ~ "1 to 4",
      between(as.numeric(AgeGrp), 5, 9) ~ "5 to 9",
      between(as.numeric(AgeGrp), 10, 14) ~ "10 to 14",
      between(as.numeric(AgeGrp), 15, 19) ~ "15 to 19",
      between(as.numeric(AgeGrp), 20, 24) ~ "20 to 24",
      between(as.numeric(AgeGrp), 25, 29) ~ "25 to 29",
      between(as.numeric(AgeGrp), 30, 34) ~ "30 to 34",
      between(as.numeric(AgeGrp), 35, 39) ~ "35 to 39",
      between(as.numeric(AgeGrp), 40, 44) ~ "40 to 44",
      between(as.numeric(AgeGrp), 45, 49) ~ "45 to 49",
      between(as.numeric(AgeGrp), 50, 54) ~ "50 to 54",
      between(as.numeric(AgeGrp), 55, 59) ~ "55 to 59",
      between(as.numeric(AgeGrp), 60, 64) ~ "60 to 64",
      between(as.numeric(AgeGrp), 65, 69) ~ "65 to 69",
      between(as.numeric(AgeGrp), 70, 74) ~ "70 to 74",
      between(as.numeric(AgeGrp), 75, 79) ~ "75 to 79",
      between(as.numeric(AgeGrp), 80, 84) ~ "80 to 84",
      between(as.numeric(AgeGrp), 85, 89) ~ "85 to 89",
      between(as.numeric(AgeGrp), 90, 94) ~ "90 to 94",
      as.numeric(AgeGrp) > 94 | AgeGrp == "100+" ~ "95 plus"
    ),
    .before = AgeGrp
  ) %>%
  mutate(age_group_name = as.factor(age_group_name)) %>%
  mutate(
    AgeGrp = NULL,
    AgeGrpStart = NULL,
    AgeGrpSpan = NULL,
    PopTotal = NULL
  ) %>%
  pivot_longer(
    cols = PopMale:PopFemale,
    names_to = "sex_label",
    names_prefix = "Pop",
    values_to = "population"
  )

world_population_filtered <- world_population_filtered %>%
  group_by(
    Location,
    Time,
    sex_label,
    age_group_name
  ) %>%
  summarise(population = sum(population)) %>%
  rename(
    location_name = Location,
    year_id = Time
  )

ihme_data_population.R <- left_join(
  ihme_data.R,
  world_population_filtered
) %>%
  relocate(population,
    .after = val
  ) %>%
  mutate(
    rate = case_when(
      population == 0 ~ 0,
      TRUE ~ val / population
    ),
    .after = population
  )

ihme_data_population.R <- ihme_data_population.R %>%
  select(
    sex_label,
    age_group_name,
    year_id,
    cause_name,
    measure_name,
    location_name,
    rate
  )

ihme_data_population.R <- ihme_data_population.R %>%
  filter(measure_name == "Prevalence")

ihme_data_population.R %>%
  filter(location_name == "Czechia")

saveRDS(
  ihme_data_population.R,
  "ihme_data_population.rds"
)
