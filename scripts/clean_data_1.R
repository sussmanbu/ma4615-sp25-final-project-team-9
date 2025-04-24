library(tidyverse)
library(here)
library(janitor)

harassment_data <- read_csv(here::here("dataset", "Harassment and Bullying.csv"))

reserved_codes <- c(-3, -4, -5, -6, -9, -12, -13)

harassment_data_clean <- harassment_data |>
  mutate(across(where(is.numeric), ~ replace(., . %in% reserved_codes, NA)))

names(harassment_data_clean) <- names(harassment_data_clean) |>
  str_replace("^SCH_H", "")

harassment_data_clean <- harassment_data_clean |>
  clean_names()

# Save cleaned dataset
write_rds(harassment_data_clean, file = here::here("dataset", "harassment_bullying_clean.rds"))

