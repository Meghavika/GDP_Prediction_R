library(dplyr)
library(tidyr)
library(kableExtra)
library(ggplot2)
library(caret)
library(cluster)
library(grid)
library(graphics)

data <- read.csv("C:/Users/megha/Downloads/GDP Prediction/Prediction_GDP1960_2030.csv")

# Define the list of Country.Names to be removed
countries_to_remove <- c("World","IDA only","IBRD only","Late-demographic dividend","Other small states",
                         "Pacific island small states","Pre-demographic dividend","Small states",
                         "Sub-Saharan Africa","Sub-Saharan Africa (excluding high income)",
                         "West Bank and Gaza","Upper middle income" ,
                         "Sub-Saharan Africa (IDA & IBRD countries)","Low & middle income",
                         "Low income", "Lower middle income" ,"IDA blend",
                         "Heavily indebted poor countries (HIPC)","IBRD only", 
                         "Heavily indebted poor countries (HIPC)",
                         "Europe & Central Asia (excluding high income)",
                         "Europe & Central Asia (IDA & IBRD countries)",
                         "Europe & Central Asia","Arab World","Central Europe and the Baltics",
                         "South Asia","Fragile and conflict affected situations",
                         "Sub-Saharan Africa (excluding high income)",
                         "Sub-Saharan Africa (IDA & IBRD countries)","IDA total","Arab World",
                         "Sub-Saharan Africa","East Asia & Pacific",
                         "Europe & Central Asia (excluding high income)","IDA & IBRD total",
                         "OECD members","Middle East & North Africa","South Asia (IDA & IBRD)",
                         "Europe & Central Asia (IDA & IBRD countries)",
                         "Latin America & the Caribbean (IDA & IBRD countries)" ,
                         "Latin America & Caribbean","East Asia & Pacific (IDA & IBRD countries)",
                         "East Asia & Pacific (excluding high income)", "High income",
                         "Latin America & Caribbean (excluding high income)", "Lower middle income",
                         "Late-demographic dividend", "Early-demographic dividend" ,
                         "Post-demographic dividend", "North America", "Europe & Central Asia",
                         "European Union", "Euro area", "Low & middle income", "Middle income",
                         "IBRD only", "Upper middle income")

# Filter out rows with specified Country.Names
data <- data %>%
  filter(!Country.Name %in% countries_to_remove)

# Step 2: Print the count of rows
cat("Number of rows:", nrow(data), "\n")

# Step 3: Print Country.Name vs Country.Code
print(data[, c("Country.Name", "Country.Code")])

# Compute summary statistics for the GDP columns
summary_data <- summary(data[, -c(1, 2)]) # Exclude Country.Name and Country.Code columns

# Print summary statistics
print(summary_data)

# Select relevant columns for each year
data_1970 <- data %>% select(Country.Name, GDP_1970) %>% arrange(desc(GDP_1970)) %>% top_n(5)
data_2000 <- data %>% select(Country.Name, GDP_2000) %>% arrange(desc(GDP_2000)) %>% top_n(5)
data_2030 <- data %>% select(Country.Name, GDP_2030) %>% arrange(desc(GDP_2030)) %>% top_n(5)

# Join the data for each year
top_countries <- full_join(data_1970, data_2000, by = "Country.Name") %>%
  full_join(data_2030, by = "Country.Name") %>%
  rename(GDP_1970 = GDP_1970, GDP_2000 = GDP_2000, GDP_2030 = GDP_2030)

# Print table
top_countries %>%
  kable("html") %>%
  kable_styling(full_width = FALSE)

# Selecting country names and GDP for 2030 and excluding "World"
gdp_2030 <- data[data$Country.Name != "World", c("Country.Name", "GDP_2030")]

# Finding the country with the highest GDP in 2030
richest_2030 <- gdp_2030[which.max(gdp_2030$GDP_2030), ]

# Printing the richest country in 2030
print(richest_2030)

#Linear Regression
# Assuming you want to predict GDP_2030 based on GDP_2020
lm_model <- lm(GDP_2030 ~ GDP_2020, data = data)
summary(lm_model)

#Multiple Regression
# Assuming you want to predict GDP_2030 based on GDP_2020-2025
lm_model_multiple <- lm(GDP_2030 ~ GDP_2020 + GDP_2021 + GDP_2022 + GDP_2023 + GDP_2024 + GDP_2025, data = data)
summary(lm_model_multiple)

#KMeans
set.seed(123)
kmeans_model <- kmeans(data[, -c(1, 2)], centers = 10)
kmeans_model

#KMedoids
# Assuming you want to cluster based on GDP values using pam function from cluster package
set.seed(123)
kmedoids_model <- pam(data[, -c(1, 2)], k = 10)
kmedoids_model

#Hierarchical Clustering
# Assuming you want to perform hierarchical clustering based on GDP values
hierarchical_model <- hclust(dist(data[, -c(1, 2)]))
plot(hierarchical_model)

# Visualizing K-means clustering
KMeans_Cluster <- as.factor(kmeans_model$cluster)
ggplot(data, aes(GDP_2030, GDP_2000, color = KMeans_Cluster)) +
  geom_point() +
  labs(title = "K-means Clustering") +
  theme_minimal()

# Visualizing K-medoids clustering
KMedoids_Cluster <- as.factor(kmedoids_model$cluster)
ggplot(data, aes(GDP_2030, GDP_2000, color = KMedoids_Cluster)) +
  geom_point() +
  labs(title = "K-medoids Clustering") +
  theme_minimal()

# Visualizing Hierarchical Clustering
plot(hierarchical_model, hang = -1, main = "Hierarchical Clustering")


colnames(data)

