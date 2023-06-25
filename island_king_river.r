#1 LOADING LIBRARIES
library(tidyverse)
library(ggplot2)

#2 IMPORTING DATA
data <- read.csv("ngo_data.csv")

#3 EXPLORING DATA
summary(data)

#4 CLEANING DATA
data_clean <- data %>%
  filter(!is.na(issue)) %>%
  select(issue, category, score)

#5 CREATING AGGREGATION COLUMNS
data_agg <- data_clean %>%
  group_by(category) %>% 
  summarise(score_sum = sum(score))

#6 VISUALIZING DATA
ggplot(data_agg, 
       aes(x = category, 
           y = score_sum)) +
  geom_col() +
  scale_y_continuous(name = "Aggregated Score") +
  ggtitle("Category vs Aggregated Score")

#7 ESTABLISHING MODEL
model <- lm(score_sum ~ category, data = data_agg)

#8 ANALYZING MODEL
summary(model)

#9 CREATING INTERVENTION CATEGORIES
intervention_cat <- data_agg %>%
  filter(score_sum >= 0.7 * mean(score_sum))

#10 VISUALIZING INTERVENTION CATEGORIES
ggplot(intervention_cat, 
       aes(x = category, 
           y = score_sum)) +
  geom_col() +
  scale_y_continuous(name = "Aggregated Score") +
  ggtitle("Categories Eligible for Intervention")

#11 NOTE TAKING
notes <- data.frame(
  category = intervention_cat$category, 
  notes = "Eligible for efforts to address pressing social and environmental issues through advocacy and community-based interventions"
)

#12 REPORTING ON INTERVENTION CATEGORIES
final_report <- notes %>%
  merge(intervention_cat, by = "category") %>%
  select(category, notes, score_sum)

#13 SAVING FINAL REPORT
write.csv(final_report, "final_report.csv")

#14 EXPORTING RESULTS
print(paste0("An NGO that works to address pressing social and environmental issues through advocacy and community-based interventions has identified the following categories as eligible for intervention:", 
              paste(unique(final_report$category), collapse = ", ")))