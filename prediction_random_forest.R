library(randomForest)
library(dplyr)

# Read the data
data <- read.csv("C:/Users/megha/Downloads/GDP Prediction/FinalGDP.csv")

# Train the random forest model
rf_model <- randomForest(X1960 ~ ., data = data, ntree = 500)

# Make predictions for GDP from 2023 to 2030
years <- 2023:2030

# Create a dataframe to store predictions
predictions <- data.frame(Country.Name = unique(data$Country.Name))

# Function to calculate median of last three years
calculate_last_three_median <- function(country_data) {
  last_three_years <- head(country_data, 3)
  median_value <- median(last_three_years, na.rm = TRUE)
  return(median_value)
}

# Predict GDP for each country
for (i in 1:nrow(predictions)) {
  # Subset data for the current country
  country_data <- subset(data, Country.Name == predictions[i, "Country.Name"])
  
  # Predict GDP for the current country using the trained random forest model
  country_rf_predictions <- predict(rf_model, newdata = country_data)
  
  # Calculate median of last three years
  median_last_three <- calculate_last_three_median(tail(unlist(country_data[, -c(1:2)]), 3))
  
  # Combine random forest predictions and median of last three years
  combined_predictions <- c(country_rf_predictions, rep(median_last_three, length(years) - length(country_rf_predictions)))
  
  # Add GDP prediction columns to the predictions dataframe
  col_names <- paste("GDP_", years, sep = "")
  predictions[i, col_names] <- combined_predictions
}

increase_GDP_recursive <- function(data, start_year, end_year) {
  if (start_year > end_year) {
    return(data)
  }
  
  col_name <- paste0("GDP_", start_year)
  data[[col_name]] <- data[[col_name]] + 1000000000
  
  return(increase_GDP_recursive(data, start_year + 1, end_year))
}

predictions <- increase_GDP_recursive(predictions, 2023, 2030)

data1 <- read.csv("C:/Users/megha/Downloads/GDP Prediction/FinalGDP.csv")
data2 <- predictions

# Merge the datasets by Country.Name
merged_data <- merge(data1, data2, by = "Country.Name", all.x = TRUE)

# Rename the columns
new_colnames <- colnames(merged_data)
new_colnames <- gsub("^X", "GDP_", new_colnames) # Replace 'X' with 'GDP_' for GDP data columns
colnames(merged_data) <- new_colnames

# Keep only distinct rows
final_data <- distinct(merged_data)

# Write the final dataset to a new CSV file
write.csv(final_data, file = "Prediction_GDP1960_2030.csv", row.names = FALSE)
