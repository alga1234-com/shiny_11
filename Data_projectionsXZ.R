setwd("C:/Users/kaggwa/Desktop/projects/class_project")

list.files()

# Step 1: Install and Load Required Packages
install.packages(c("dplyr", "caret", "xgboost", "Matrix", "data.table"))
install.packages("tidyverse")
install.packages("ggplot2", dependencies = TRUE)
install.packages("RColorBrewer")
install.packages("randomForest")
install.packages("caTools")
library(caTools)
library(randomForest)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(xgboost)
library(Matrix)
library(data.table)

#AUnified Data Integration and Cleaning

# Load required packages

# 1. Read both datasetslibrary(janitor)
install.packages("readr")  # Install readr package (if not already installed)
library(readr)  

Californiahousing <- read_csv("Californiahousing.csv", show_col_types = FALSE)

AmesHousing <- read_csv("AmesHousing.csv", show_col_types = FALSE)


#Step 1: View Column Names
names(AmesHousing)
names(Californiahousing)

# create Subset to common columns
Ames_common <- AmesHousing[, common_cols]
California_common <- Californiahousing[, common_cols]

#Add sourcename column
Ames_common$sourcename <- "AmesHousing.csv"
California_common$sourcename <- "Californiahousing.csv"

# Step 2: Keep only common columns between the two
common_cols <- intersect(names(Ames_common), names(California_common))
Ames_common <- Ames_common[, common_cols]
California_common <- California_common[, common_cols]

common_cols <- intersect(names(AmesHousing), names(Californiahousing))

#COMBINE DATASETS
library(dplyr)
combined_data <- bind_rows(Ames_common, California_common)


#Summarize by Neighborhood and Source

combined_summary <- combined_data %>%
  group_by(Neighborhood, Source) %>%
  summarise(SalePrice = mean(SalePrice, na.rm = TRUE), .groups = "drop")

#View Structure and Summary
# View the structure of the full combined data
str(combined_data)

# Basic summary statistics
summary(combined_data)


#View Grouped Summary Table
# Show structure and content of the grouped summary
head(combined_data)
colnames(combined_data)
str(combined_summary)
head(combined_summary)
table(combined_data$sourcename)

combined_data %>%
  group_by(sourcename) %>%
  summarize(count = n()) %>%
  print()

#Save Combined dataset as CSV_dataset

write.csv(combined_data, "combined_data.csv", row.names = FALSE)


#To pens the data frames in the RStudio viewer
# Basic summary

------------------------------------------------------------------

#DATA CLEANING AND PREPROCESSING
------------------------------------------------------------------
# Ensure Neighborhood is a factor
combined_data$Neighborhood <- as.factor(combined_data$Neighborhood)

# Step 1: Remove rows with missing LivArea

combined_data <- combined_data %>% filter(!is.na(LivArea))

# Step 2: Remove rows with unseen Neighborhoods
common_neighs <- intersect(unique(combined_data$Neighborhood), unique(combined_data$Neighborhood))
combined_data <- combined_data %>% filter(Neighborhood %in% common_neighs)

# Step 3: Confirm there are no issues left
summary(combined_data)
colSums(is.na(combined_data))  
# Should all be 0

--------------------------------------------------------------------------------
#DATA EXPLORATION (EDA)
--------------------------------------------------------------------------------
# Get the list of known neighborhoods from training data
known_neighs <- unique(combined_data$Neighborhood)


#Reading

summary(combined_data)
colnames(combined_data)

  
#Step 3 VISUALIZE DATA
#Plot Average Sale Price by Neighborhood
  install.packages("ggplot2")  # Install ggplot2 (if not already installed)
library(ggplot2)             # Load ggplot2 package

  
  ggplot(combined_summary, aes(x = reorder(Neighborhood, -SalePrice), y = SalePrice, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.9) +
  labs(title = "SalePrice by Neighborhood",
       x = "Neighborhood", y = "SalePrice") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("Ames" = "steelblue", "California" = "darkorange"))

#Scutter plot to find relationship of SalePrice by LivArea

ggplot(combined_data, aes(x = LivArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Sale Price vs Living Area", x = "Living Area (sq ft)", y = "Sale Price")

#Box Plot for SalePrice by Bedrooms
ggplot(combined_data, aes(x = as.factor(Total_bedrooms), y = SalePrice)) +
  geom_boxplot(fill = "green") +
  labs(title = "Sale Price by Bedroom Count", x = "Total Bedrooms", y = "Sale Price")

#Living Area by Neighborhood

ggplot(combined_data, aes(x = Neighborhood, y = LivArea)) +
  geom_boxplot(fill = "wheat") +
  labs(title = "Living Area by Neighborhood", x = "Neighborhood", y = "Living Area (sq ft)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#A Bar plot plot comparing price vs. predicted sale prices 

library(ggplot2)

# Sample: take top 20 rows for clarity (optional, to avoid overcrowding)

combined_data <- results %>% slice(1:20)


# Convert to long format for grouped bar plot
plot_combined_data_long <- plot_combined_dataa %>%
  select(ID, SalePrice, PredictedPrice) %>%
  pivot_longer(cols = c(SalePrice, PredictedPrice), names_to = "Type", values_to = "Price")

# Plot
ggplot(plot_data_long, aes(x = reorder(ID, -Price), y = Price, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Actual vs Predicted Sale Prices", x = "House ID", y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("SalePrice" = "steelblue", "PredictedPrice" = "orange"))

#Correlation Matrix (Optional Advanced)
cor_data <- combined_data %>%
  select(SalePrice, LivArea, Total_bedrooms) %>%  # Correct column name
  cor(use = "complete.obs")  # Compute correlation for selected columns

print(cor_data)  # Display correlation matrix


#Model Building, Fitting & Testing in R
#Step 1: Data Preparation

library(dplyr)
library(caret)
objects()


#Define Data to be predicted 

install.packages("caTools")   # Run this once
library(caTools)              # Then load the package
## 5. Linear Regression Modeling
# Split data

set.seed(123)
train_idx <- createDataPartition(combined_data$SalePrice, p = 0.7, list = FALSE)
train_data <- combined_data[train_idx, ]
test_data <- combined_data[-train_idx, ]

# Linear model
lm_model <- lm(SalePrice ~ LivArea + Source, data = train_data)
summary(lm_model)

# Predict and evaluate
lm_preds <- predict(lm_model, newdata = test_data)
lm_rmse <- sqrt(mean((lm_preds - test_data$SalePrice)^2))
lm_rmse

install.packages("rsample")
library(rsample)

set.seed(123)
split <- initial_split(combined_data, prop = 0.8)
train_data <- training(split)
test_data  <- testing(split)


# Extract training and testing data using rsample's functions
train_data <- training(split)
test_data <- testing(split)

#Linear Regression:
  
model <- lm(SalePrice ~ ., data = train_data)


# Convert data to a numeric matrix (ensuring all columns are numeric)

train_matrix <- model.matrix(SalePrice ~ . - 1, data = train_data)  

# the -1 removes intercept column

# Train the XGBoost model
library(xgboost)

model <- xgboost(data = train_matrix, 
                 label = train_data$SalePrice, 
                 nrounds = 100, 
                 objective = "reg:squarederror")


#Testing the Model
# Convert test data to numeric matrix
test_matrix <- model.matrix(SalePrice ~ . - 1, data = test_data)

# Make predictions
predictions <- predict(model, newdata = test_matrix)

# Evaluate performance (e.g., RMSE)
actual <- test_data$SalePrice
rmse <- sqrt(mean((predictions - actual)^2))
print(paste("RMSE:", rmse))

# Plot feature importance without using the pipe operator
importance <- xgb.importance(model = model)
xgb.plot.importance(importance, top_n = 10)

# Shows the top 5 most important features
head(importance, 5)  

#Visualize Feature Relationships
library(ggplot2)

library(ggplot2)
names(combined_data)

ggplot(combined_data, aes(x = LivArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "SalePrice vs LivArea",
       x = "LivArea", y = "SalePrice")


# Load necessary libraries
library(ggplot2)
library(reshape2)
library(corrplot)

# Step 1: Load the dataset
data <- read.csv("model_predictions.csv")

# Step 2: Select only numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Step 3: Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Step 4: Plot using ggplot2
# Melt the correlation matrix
melted_cor <- melt(cor_matrix)

#Correlation Heat map between Var1 and Va2
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed()

# Optional: Plot using corrplot for an alternative style
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


# Export predictions
results <- data.frame(ID = 1:nrow(test_data),
                      Actual = actual,
                      Predicted = predictions)

write.csv(results, "model_predictions.csv", row.names = FALSE)

# Export feature importance
write.csv(importance, "feature_importance.csv", row.names = FALSE)

#Save the model with a timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
xgb.save(model, paste0("xgboost_model_", timestamp, ".model"))

save.image(file = "learning_model.RData")

saveRDS(model, file = "learning_model.rds")



