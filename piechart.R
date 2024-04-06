# Load necessary libraries
library(dplyr)
library(ggplot2)

# Define the years of interest
years <- c("1990", "2000", "2010", "2020", "2030")

# Create an empty list to store pie charts
pie_chart_list <- list()

# Iterate through each year
for (year in years) {
  # Find the top 10 countries for the current year
  top_countries <- data %>%
    select(Country.Name, Country.Code, everything()) %>%
    mutate(GDP = !!sym(paste0("GDP_", year))) %>%
    arrange(desc(GDP)) %>%
    head(10)
  
  # Create a pie chart for the current year
  pie_chart <- ggplot(top_countries, aes(x = "", y = GDP, fill = Country.Code)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste("Top 10 Countries GDP Distribution for", year), x = NULL, y = NULL) +
    theme_void() +
    theme(legend.position = "bottom") +
    scale_fill_brewer(palette = "Set3")  # Use a color palette for better differentiation
  
  # Store the pie chart in the list
  pie_chart_list[[year]] <- pie_chart
}

# Output the pie charts in a grid layout
library(gridExtra)
grid.arrange(grobs = pie_chart_list, ncol = 3)

colnames(data)
