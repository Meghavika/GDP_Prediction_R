# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

data<-read.csv("C:/Users/megha/Downloads/GDP Prediction/Prediction_GDP1960_2030.csv")
# Assuming you want to visualize GDP distribution for the year 2011
year <- "GDP_2011"

# Extract GDP data for the selected year
gdp_2011 <- data[[year]]

# Remove NA values (if any)
gdp_2011 <- gdp_2011[!is.na(gdp_2011)]

# Create a summary table for GDP distribution by quantiles
quantiles <- quantile(gdp_2011, probs = c(0, 0.25, 0.5, 0.75, 1))
labels <- c("0-25%", "25-50%", "50-75%", "75-100%")
quantile_table <- cut(gdp_2011, breaks = quantiles, labels = labels, include.lowest = TRUE)
quantile_counts <- table(quantile_table)

# Calculate percentage of GDP distribution for each quantile
quantile_percentages <- round(prop.table(quantile_counts) * 100, digits = 1)
quantile_labels <- paste(labels, ": ", quantile_percentages, "%", sep = "")

# Pie Chart with labels
pie(quantile_counts, main = "GDP Distribution for 2011", col = rainbow(length(labels)), labels = quantile_labels)

# Bar Plot
# Create a bar plot for top 10 countries by GDP in 2011
top_10_gdp_2011 <- data.frame(Country.Name = data$Country.Name[order(data[[year]], decreasing = TRUE)][1:10], GDP = sort(data[[year]], decreasing = TRUE)[1:10])
barplot(top_10_gdp_2011$GDP, names.arg = top_10_gdp_2011$Country.Name, main = "Top 10 Countries by GDP in 2011", xlab = "Country", ylab = "GDP", col = rainbow(10))

# Histogram with ggplot2
ggplot(data.frame(GDP = gdp_2011), aes(x = GDP)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 20) +
  labs(title = "GDP Distribution for 2011", x = "GDP", y = "Frequency") +
  theme_minimal()

# Scatter Plot
# Assuming you want to visualize GDP_2011 vs GDP_2001
gdp_2001 <- data[["GDP_2001"]]
scatter_data <- data.frame(GDP_2001 = gdp_2001, GDP_2011 = gdp_2011)
ggplot(scatter_data, aes(x = GDP_2001, y = GDP_2011)) +
  geom_point(color = "purple", alpha = 0.6) +
  labs(title = "GDP Comparison: 2001 vs 2011", x = "GDP 2001", y = "GDP 2011") +
  theme_minimal()

# Box Plot
# Box plot for GDP distribution over the years
gdp_years <- data[, grep("^GDP", colnames(data))]
gdp_years_long <- gdp_years %>% pivot_longer(cols = everything(), names_to = "Year", values_to = "GDP")
ggplot(gdp_years_long, aes(x = Year, y = GDP)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "GDP Distribution Over the Years", x = "Year", y = "GDP") +
  theme_minimal()

#Top 3 countries from 2000-2030

# Define the years of interest
years <- c("2000", "2005", "2010", "2015", "2020", "2025", "2030")

# Create an empty list to store data frames
data_list <- list()

# Iterate through each year
for (year in years) {
  # Extract GDP data for the current year
  gdp_year <- data[[paste0("GDP_", year)]]
  
  # Remove NA values (if any)
  gdp_year <- gdp_year[!is.na(gdp_year)]
  
  # Find the top 3 countries for the current year
  top_countries <- data %>%
    select(Country.Name, Country.Code, everything()) %>%
    mutate(GDP = !!sym(paste0("GDP_", year))) %>%
    arrange(desc(GDP)) %>%
    head(3)
  
  # Add year information
  top_countries$Year <- year
  
  # Store the top countries data frame in the list
  data_list[[year]] <- top_countries
}

# Combine data frames into one
top_countries_df <- bind_rows(data_list)

# Create a new column for x-axis labels (country code)
top_countries_df$XLabel <- top_countries_df$Country.Code

# Create the plot
ggplot(top_countries_df, aes(x = XLabel, y = GDP, fill = Country.Name)) +
  geom_bar(stat = "identity") +
  labs(title = "GDP Distribution for Top 3 Countries", x = "Country Code", y = "GDP (in billions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = scales::comma(GDP / 1e12), y = GDP), vjust = -0.25, size = 4, color = "black") + # Scale GDP values by 10^9
  scale_fill_brewer(palette = "Pastel1") +  # Use mild colors
  facet_wrap(~Year, nrow = 1)  # Facet by year in a single row


