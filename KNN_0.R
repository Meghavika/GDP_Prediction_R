library(caret) #KNN
library(dplyr)
library(zoo) #Linear Interpolation

# Read the data
data <- read.csv("C:/Users/megha/Downloads/GDP Prediction/ModifiedGDP_data.csv")

# Identify columns with zeros
columns_with_zeros <- colnames(data)[apply(data, 2, function(x) sum(x == 0) > 0)]

# Perform KNN imputation for columns with zeros
for (col in columns_with_zeros) {
  data[[col]] <- ifelse(data[[col]] == 0, NA, data[[col]])  # Convert 0s to NAs
  data[[col]] <- na.approx(data[[col]], na.rm = FALSE)  # Apply linear interpolation
}

# Function to replace zero GDP values with country median
replace_zero_with_median <- function(x) {
  non_zero_values <- x[x != 0]
  if (length(non_zero_values) >= 3) {  #Minimum non-zero values required for median calculation
    median_value <- median(non_zero_values)
    x[x == 0] <- median_value
  }
  return(x)
}

# Apply the function to each row (country)
data_processed <- data %>%
  mutate_at(vars(starts_with("X")), ~ replace_zero_with_median(.))

# Convert the data into exponential form with 3 decimal places
data_processed <- format(data_processed, scientific = TRUE, digits = 3)

# Write the processed data to a new CSV file
write.csv(data_processed, file = "FinalGDP.csv", row.names = FALSE)

colnames(data_processed)

# Confirmation message
cat("Processed data has been saved to FinalGDP.csv\n")
