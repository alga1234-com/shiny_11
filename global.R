# global.R

# Set working directory
setwd("C:/Users/kaggwa/Desktop/projects/class_project")

# Load libraries
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(caret)
library(xgboost)
library(Matrix)
library(randomForest)
library(data.table)
library(tidyr)
library(reshape2)
library(corrplot)

# Load datasets
AmesHousing <- read_csv("AmesHousing.csv", show_col_types = FALSE)
Californiahousing <- read_csv("Californiahousing.csv", show_col_types = FALSE)

# Identify common columns and prepare combined data
common_cols <- intersect(names(AmesHousing), names(Californiahousing))
Ames_common <- AmesHousing[, common_cols]
California_common <- Californiahousing[, common_cols]
Ames_common$sourcename <- "Ames"
California_common$sourcename <- "California"
combined_data <- bind_rows(Ames_common, California_common)

# Clean data
combined_data <- combined_data %>%
  filter(!is.na(LivArea)) %>%
  filter(Neighborhood %in% unique(Neighborhood)) %>%
  mutate(Neighborhood = as.factor(Neighborhood))

# Combined summary for plotting
combined_summary <- combined_data %>%
  group_by(Neighborhood, sourcename) %>%
  summarise(SalePrice = mean(SalePrice, na.rm = TRUE), .groups = "drop")

# Train/Test Split
set.seed(123)
train_idx <- createDataPartition(combined_data$SalePrice, p = 0.7, list = FALSE)
train_data <- combined_data[train_idx, ]
test_data <- combined_data[-train_idx, ]

# Linear Model
lm_model <- lm(SalePrice ~ LivArea + sourcename, data = train_data)
lm_preds <- predict(lm_model, newdata = test_data)
lm_rmse <- sqrt(mean((lm_preds - test_data$SalePrice)^2))

# XGBoost Model
train_matrix <- model.matrix(SalePrice ~ . -1, data = train_data)
test_matrix <- model.matrix(SalePrice ~ . -1, data = test_data)

xgb_model <- xgboost(
  data = train_matrix,
  label = train_data$SalePrice,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

predictions <- predict(xgb_model, newdata = test_matrix)
xgb_rmse <- sqrt(mean((predictions - test_data$SalePrice)^2))
importance <- xgb.importance(model = xgb_model)

