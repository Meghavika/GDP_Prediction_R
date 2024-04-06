library(caret)

data<-read.csv("C:/Users/megha/Downloads/GDP Prediction/NewGDP_data.csv")


# Merge Country and Country.Name into one column
data$Country.Name <- ifelse(is.na(data$Country.Name), data$Country, data$Country.Name)

# Drop the separate Country column if needed
data <- subset(data, select = -c(Country))

# Define a function to merge columns and replace 0 values
merge_and_replace <- function(data, year) {
  year_x <- paste0("X", year, ".x")
  year_y <- paste0("X", year, ".y") #X1960.x and X1960.y -> X1960 
  
  # Merge columns for the specified year
  merged_column <- ifelse(data[[year_x]] == 0 & !is.na(data[[year_y]]), data[[year_y]], data[[year_x]])
  
  # Replace 0 values with any non-zero value for the following year if available
  if (year < 2020) {
    next_year_x <- paste0("X", year + 1, ".x")
    next_year_y <- paste0("X", year + 1, ".y")
    
    data[[year_x]] <- ifelse(data[[year_x]] == 0 & !is.na(data[[next_year_x]]) & data[[next_year_x]] != 0, data[[next_year_x]], data[[year_x]])
    data[[year_y]] <- ifelse(data[[year_y]] == 0 & !is.na(data[[next_year_y]]) & data[[next_year_y]] != 0, data[[next_year_y]], data[[year_y]])
  }
  
  return(merged_column)
}

# Merge columns for each year and replace 0 values
for (year in 1960:2019) {
  merged_column <- merge_and_replace(data, year)
  data[[paste0("X", year)]] <- merged_column
}

# Drop the separate year columns if needed
data <- subset(data, select = setdiff(colnames(data), c(paste0("X", 1960:2019, ".x"), paste0("X", 1960:2019, ".y"))))

# Merge "X2020.x" and "X2020.y" into a single column "X2020"
data$X2020 <- ifelse(is.na(data$X2020.x), data$X2020.y, data$X2020.x)

# Drop the separate "X2020.x" and "X2020.y" columns
data <- subset(data, select = -c(X2020.x, X2020.y))

# Reorder the columns from "X1960" to "X2022" in ascending order
data <- data[, c("Country.Code", paste0("X", 1960:2022), "Country.Name")]

# Reorder the columns with "Country.Name" at the very beginning before "Country.Code"
data <- data[, c("Country.Name", "Country.Code", paste0("X", 1960:2022))]

# Write the modified dataset to a new CSV file
write.csv(data, file = "ModifiedGDP_data.csv", row.names = FALSE)

colnames(data)

