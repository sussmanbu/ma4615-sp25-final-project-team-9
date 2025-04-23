library(tidyverse)
library(janitor)


bullying <- read_csv("../dataset/Harassment and Bullying.csv") %>% clean_names()

# Filter to Massachusetts only
ma_bullying <- bullying %>%
  filter(lea_state_name == "Massachusetts")

# Check number of rows
nrow(ma_bullying)

# Check if relevant columns are present
names(ma_bullying)[names(ma_bullying) %in% c(
  "lea_name", 
  "sch_hballegations_sex", 
  "sch_hballegations_rac", 
  "sch_hballegations_ori", 
  "sch_hballegations_dis", 
  "sch_hballegations_rel")]
# Basic summary plot for each type of allegation
ma_long <- ma_bullying %>%
  select(lea_name, 
         sex = sch_hballegations_sex,
         race = sch_hballegations_rac,
         orientation = sch_hballegations_ori,
         disability = sch_hballegations_dis,
         religion = sch_hballegations_rel) %>%
  pivot_longer(cols = -lea_name, names_to = "allegation_type", values_to = "count")

# Check for missing or zero values
summary(ma_long$count)

# Bar plot to visualize totals by type
ma_long %>%
  group_by(allegation_type) %>%
  summarise(total = sum(count, na.rm = TRUE)) %>%
  ggplot(aes(x = allegation_type, y = total, fill = allegation_type)) +
  geom_col() +
  labs(title = "Total Allegations by Type in Massachusetts",
       x = "Allegation Type",
       y = "Total Reported") +
  theme_minimal()
ma_long %>%
  group_by(lea_name, allegation_type) %>%
  summarise(total = sum(count, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total)) %>%
  filter(total > 0) %>%
  head(10)
shiny::runApp("ma_bullying_app")
