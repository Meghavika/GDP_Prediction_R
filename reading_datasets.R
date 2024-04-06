library(dplyr)

#Preprocessing (P1)
#reading datasets
data1<-read.csv("C:/Users/megha/Downloads/archive/GDP 1.csv") #GPD 1
data2<-read.csv("C:/Users/megha/Downloads/archive/Countries GDP 1960-2020.csv") #Countries GDP 1960-2020.csv
data3<-read.csv("C:/Users/megha/Downloads/archive/gdp-countries.csv") #gdp-countries
data4<-read.csv("C:/Users/megha/Downloads/archive/gdp.csv") #gdp

#Transposing "gdp-countries" data set
library(tidyr)

# Transpose the data frame
data3 <- t(data3)

# Write the transposed data to a new CSV file -> data 3
write.csv(data3, "transposed_data.csv", row.names = FALSE)

# Data cleaning (P2)

# Define a function to replace NA values and empty strings with 0
clean_data <- function(df) {
  # Replace NA values with 0
  df[is.na(df)] <- 0
  
  # Replace empty strings with 0
  df[df == ""] <- 0
  
  return(df)
}

# Clean each dataset
data1 <- clean_data(data1)
data2 <- clean_data(data2)
data3 <- clean_data(data3)
data4 <- clean_data(data4)


#Remove Outliers (P3)
# Check if "Country.Code" is in the correct position in the first row
check_country_code_position <- function(df) {
  ncol <- ncol(df)
  nrow <- nrow(df)
  
  # Ensure there are at least two columns and one row
  if (ncol >= 2 && nrow >= 1) {
    df[1, 2] == "Country.Code"
  } else {
    FALSE
  }
}

# Filter datasets that have "Country.Code" column
data_list_filtered <- lapply(list(data1, data2, data3, data4), function(df) {
  if ("Country.Code" %in% colnames(df) || check_country_code_position(df)) {
    df
  }
})

# If there are no datasets with "Country.Code" column, display a message
if (all(sapply(data_list_filtered, is.null))) {
  cat("None of the datasets contain a 'Country.Code' column or 'Country.Code' is not in the correct position.")
} else {
  # Remove NULL entries (datasets without "Country.Code" column)
  data_list_filtered <- Filter(Negate(is.null), data_list_filtered)
  
  # Reassign filtered datasets to original variables if any
  if (length(data_list_filtered) > 0) {
    data1 <- data_list_filtered[[1]]
    if (length(data_list_filtered) > 1) data2 <- data_list_filtered[[2]]
    if (length(data_list_filtered) > 2) data3 <- data_list_filtered[[3]]
    if (length(data_list_filtered) > 3) data4 <- data_list_filtered[[4]]
  }
  
  # Display filtered datasets
  for (i in seq_along(data_list_filtered)) {
    cat("Dataset", i, ":\n")
    print(head(data_list_filtered[[i]], 10))
    cat("\n")
  }
}

#Merging datasets (Dataset 1 and Dataset 2) and distinct value entriesassurance

# Merge datasets with "Country.Code" column
merged_data <- NULL

for (i in seq_along(data_list_filtered)) {
  if (i == 1) {
    merged_data <- data_list_filtered[[i]]
  } else {
    merged_data <- merge(merged_data, data_list_filtered[[i]], by = "Country.Code", all = TRUE)
  }
}


# Rename the merged dataset as 'data'
data <- distinct(merged_data) #distinct

# Create a new dataset with only distinct country codes
distinct_countries <- unique(data$Country.Code)

# Display the new dataset
print(distinct_countries)


# Count the number of unique country codes
num_cc2 <- length(unique(data$Country.Code)) #266

# Display the count
print(num_cc2)

# Write the dataset to a new CSV file
write.csv(data, file = "NewGDP_data.csv", row.names = FALSE)

# Confirm that the file has been created
if (file.exists("NewGDP_data.csv")) {
  print("NewGDP_data.csv has been created successfully.")
} else {
  print("Error: Failed to create NewGDP_data.csv")
}

