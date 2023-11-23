setwd("F:/Programming/Data Analysis/R")
csv_file_path <- "F:/Programming/Data Analysis/R/suicide-rates-by-age-detailed-who.csv"
# Read the CSV file into a data frame
data <- read.csv(csv_file_path)
library(tidyverse)

# Filter data for the top 20 entities based on the average suicide rate
# Calculate the average suicide rate for each entity
average_suicide_rates <- data %>%
  select(Entity, average) %>%
  arrange(desc(average))  # Arrange in descending order by average

# Identify the top 20 entities based on the highest average scores
top_entities <- head(average_suicide_rates$Entity, 20)

# Specify the columns
age_columns <- colnames(data)[grep("^Age", colnames(data))]
age_data_long_filtered <- data %>%
  select(Entity, age_columns) %>%
  filter(Entity %in% top_entities) %>%
  pivot_longer(
    cols = -Entity,
    names_to = "AgeCategory",
    values_to = "SuicideRate"
  )

# Replace unwanted characters
age_data_long_filtered$AgeCategory <- gsub("\\.", " ", age_data_long_filtered$AgeCategory)
age_data_long_filtered$AgeCategory <- gsub("\\Age Group", "", age_data_long_filtered$AgeCategory)

# 
#  BAR PLOT for the top 20 entities
#
print(
  ggplot(age_data_long_filtered, aes(x = AgeCategory, y = SuicideRate, fill = Entity)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Suicide Rate by Age Category (Top 20 Entities)",
      x = "Age Category",
      y = "Suicide Rate",
      fill = "Entity"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
)
# Adjust the size of the plot and save it
ggsave("suicide_rate_plot_top20.png", width = 36, height = 5, units = "in")

#
#   LINE PLOT for the top 20 entities with rotated x-axis labels
#
print(
  ggplot(age_data_long_filtered, aes(x = AgeCategory, y = SuicideRate, color = Entity, group = Entity)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Suicide Rate by Age Category (Top 20 Entities)",
      x = "Age Category",
      y = "Suicide Rate",
      color = "Entity"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
)
# Adjust the size of the plot and save it
ggsave("suicide_rate_line_plot_top20_rotated.png", width = 12, height = 12, units = "in")

# Filter data for the top 20 entities based on the average suicide rate across all age categories
top_entities_all_age <- age_data_long_filtered %>%
  group_by(Entity) %>%
  summarize(SumSuicideRate = sum(SuicideRate)) %>%
  top_n(20, wt = SumSuicideRate) %>%
  pull(Entity)

#
#   PIE CHART for average suicide rates across age categories without labels
#
print(
  ggplot(age_data_long_filtered %>% filter(Entity %in% top_entities_all_age), 
         aes(x = "", y = SuicideRate, fill = AgeCategory)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(
      title = "Distribution of Average Suicide Rates Across Age Categories",
      fill = "Age Category"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
)
# Adjust the size of the plot and save it
ggsave("suicide_rate_pie_chart_top20_no_labels.png", width = 8, height = 8, units = "in")
