# Load libraries
library(caret)
library(ranger)

# Define training control
control <- trainControl(method = "cv", number = 5, savePredictions = "final") 

# Define the grid of parameters
tune_grid <- expand.grid(
  mtry = c(5, 9, 12, 15, 18, 25, 35, 40),              # Range of predictors to try
  splitrule = c("gini"),                # Splitting criterion
  min.node.size = c(1, 3, 5, 10, 20)#,            # Minimum node size
 # max.depth = c(5, 10, 15, NA)
)

# Train the Random Forest
set.seed(123)
rf_model <- train(
  thirtyday_expire_flag ~ ., 
  data = train_data_na_omit, 
  method = "ranger", 
  trControl = control, 
  tuneGrid = tune_grid,
  num.trees = 200                      # Moderate number of trees for tuning
)

# Print results
print(rf_model)

# Extract and plot performance metrics
plot(rf_model)

# Extract the results dataframe
results <- rf_model$results
print(results)

# Visualize performance across `mtry` and `min.node.size`
library(ggplot2)

ggplot(results, aes(x = mtry, y = Accuracy, color = as.factor(min.node.size))) +
  geom_line() + geom_point() +
  labs(
    title = "Random Forest Tuning Results",
    x = "Number of Predictors (mtry)",
    y = "Accuracy",
    color = "Min Node Size"
  ) +
  theme_minimal()
