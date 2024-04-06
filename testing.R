data <- read.csv("C:/Users/megha/Downloads/GDP Prediction/Prediction_GDP1960_2030.csv")

if (any(data == 0)) {
  cat("Warning: The data contains zero values.\n")
} 

if (any(is.na(data))) {
  cat("Warning: The data contains NA values.\n")
} 

if (any(data == "")) {
  cat("Warning: The data contains empty values.\n")
}

cat("The data is set to be used.\n")
