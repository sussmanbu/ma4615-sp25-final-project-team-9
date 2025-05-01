library(tidyverse)
library(here)

# Reserved codes that should be treated as missing
reserved_codes <- c(-3, -4, -5, -6, -9, -12, -13)

# Load datasets using here::here()
harassment_data <- read_csv(here("dataset", "Harassment and Bullying.csv"), show_col_types = FALSE)
enrollment_data <- read_csv(here("dataset", "Enrollment.csv"), show_col_types = FALSE)
support_data <- read_csv(here("dataset", "School Support.csv"), show_col_types = FALSE)

# Variables to extract
harassment_vars <- c(
  "SCH_HBALLEGATIONS_SEX", "SCH_HBALLEGATIONS_RAC", "SCH_HBALLEGATIONS_DIS", "SCH_HBALLEGATIONS_REL",
  "SCH_HBREPORTED_RAC_HI_M", "SCH_HBREPORTED_RAC_HI_F",
  "SCH_HBREPORTED_RAC_AM_M", "SCH_HBREPORTED_RAC_AM_F",
  "SCH_HBREPORTED_RAC_AS_M", "SCH_HBREPORTED_RAC_AS_F",
  "SCH_HBREPORTED_RAC_HP_M", "SCH_HBREPORTED_RAC_HP_F",
  "SCH_HBREPORTED_RAC_BL_M", "SCH_HBREPORTED_RAC_BL_F",
  "SCH_HBREPORTED_RAC_WH_M", "SCH_HBREPORTED_RAC_WH_F",
  "SCH_HBREPORTED_RAC_TR_M", "SCH_HBREPORTED_RAC_TR_F"
)

enrollment_vars <- c(
  "SCH_ENR_HI_M", "SCH_ENR_HI_F", "SCH_ENR_AM_M", "SCH_ENR_AM_F",
  "SCH_ENR_AS_M", "SCH_ENR_AS_F", "SCH_ENR_HP_M", "SCH_ENR_HP_F",
  "SCH_ENR_BL_M", "SCH_ENR_BL_F", "SCH_ENR_WH_M", "SCH_ENR_WH_F",
  "SCH_ENR_TR_M", "SCH_ENR_TR_F"
)

support_vars <- c("SCH_FTECOUNSELORS", "SCH_FTESERVICES_PSY", "SCH_FTESECURITY_GUA")

# Clean harassment data
harassment_clean <- harassment_data %>%
  select(all_of(harassment_vars)) %>%
  rename_with(~ str_remove(., "^SCH_")) %>%
  mutate(across(everything(), ~ ifelse(.x %in% reserved_codes, NA, .x)))

# Clean enrollment data
enrollment_clean <- enrollment_data %>%
  select(all_of(enrollment_vars)) %>%
  rename_with(~ str_remove(., "^SCH_")) %>%
  mutate(across(everything(), ~ ifelse(.x %in% reserved_codes, NA, .x)))

# Clean support data
support_clean <- support_data %>%
  select(all_of(support_vars)) %>%
  rename_with(~ str_remove(., "^SCH_")) %>%
  mutate(across(everything(), ~ ifelse(.x %in% reserved_codes, NA, .x)))

# Merge datasets together (row-wise binding by row order)
final_clean_data <- bind_cols(harassment_clean, enrollment_clean, support_clean)

# Save the cleaned dataset
write_rds(final_clean_data, here("dataset", "final_clean_data.rds"))
write_csv(final_clean_data, here("dataset", "final_clean_data.csv"))
