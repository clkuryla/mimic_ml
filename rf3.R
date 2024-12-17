model_data <- data_vartypes %>%
  select(-all_of(exclude_cols)) %>% 
  select(-all_of(nonpredictor_cols)) %>% 
  na.omit()

#---------------------------
# Train-Test Split
#---------------------------

# Split the data into training and test sets
set.seed(1248)
train_index <- createDataPartition(model_data$thirtyday_expire_flag, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]


#############################################
# Setup
#############################################

# install.packages(c("caret", "ranger", "dplyr", "ggplot2", "tidyr"))
library(caret)
library(ranger)
library(dplyr)
library(ggplot2)
library(tidyr)

set.seed(123)

# Custom F1 summary function (as shown before)
f1_summary <- function(data, lev = NULL, model = NULL) {
  positive_class <- lev[2]
  precision <- posPredValue(data$pred, data$obs, positive_class)
  recall <- sensitivity(data$pred, data$obs, positive_class)
  f1 <- 2 * (precision * recall) / (precision + recall + .Machine$double.eps)
  c(F1 = f1)
}

#############################################
# Define Training Control and Tuning Grid
#############################################

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,        # If you need probabilities for some metrics
  summaryFunction = f1_summary,
  savePredictions = TRUE,
  verboseIter = TRUE
)

# Example tuning grid for ranger (Random Forest):
# Parameters:
# - mtry: number of variables available for splitting at each tree node
# - min.node.size: minimal node size
# - max.depth: maximum tree depth
# - splitrule: splitting rule (e.g., "gini" or "extratrees")
tune_grid <- expand.grid(
  mtry = c(5, 9, 12, 15, 18, 25, 35, 40), 
  min.node.size = c(1, 5, 10, 20),
  splitrule = c("gini", "extratrees")#,
 # max.depth = c(5, 10, 15, NA)  # NA means no max depth limit in ranger
)

#############################################
# Train the Model
#############################################

set.seed(123)
model_f1 <- train(
  thirtyday_expire_flag ~ .,
  data = train_data,
  method = "ranger",
  metric = "F1",
  trControl = ctrl,
  tuneGrid = tune_grid,
  importance = "impurity" # example, adjust if needed
)

#############################################
# Examine Results
#############################################
model_f1
model_f1$bestTune  # Best parameters found

# The results data frame contains all the metric values for each parameter set.
results_df <- model_f1$results
head(results_df)

#############################################
# Plotting the Change in F1 by Parameter
#############################################

# The results data frame typically contains columns:
# mtry, splitrule, min.node.size, max.depth, F1, and maybe other metrics.

# We want to create plots to show how F1 changes as each parameter changes.
# Because we have multiple parameters, we can fix some parameters and vary others,
# or use facets to show multiple dimensions.

# One approach:
# - We'll "gather" or pivot the parameters into a longer format
# - Then we can plot F1 vs parameter value, and facet by other parameters

results_long <- results_df %>%
  select(mtry, min.node.size, splitrule, F1) %>%
  # Turn parameters into long format
  pivot_longer(cols = c(mtry, min.node.size, splitrule, max.depth),
               names_to = "Parameter",
               values_to = "Value")

# For numeric parameters (mtry, min.node.size, max.depth), we can plot F1 against value.
# For categorical parameters like splitrule, we can use a different geom or facet.

# Let's separate numeric and categorical parameters for clarity:
results_numeric <- results_long %>% 
  filter(Parameter %in% c("mtry", "min.node.size", "max.depth"))

results_categorical <- results_long %>% 
  filter(Parameter == "splitrule")

# Plot numeric parameters (mtry, min.node.size, max.depth)
# We'll facet by Parameter to get one subplot per parameter.
ggplot(results_numeric, aes(x = Value, y = F1, color = as.factor(Value))) +
  geom_point() +
  geom_line(aes(group = 1), color = "black", alpha = 0.5) +
  facet_wrap(~ Parameter, scales = "free_x") +
  theme_minimal() +
  labs(title = "Effect of Numeric Parameters on F1 Score",
       x = "Parameter Value",
       y = "F1 Score",
       color = "Parameter Value") +
  theme(legend.position = "none")

# For the categorical parameter (splitrule), we can do a grouped boxplot or points:
ggplot(results_categorical, aes(x = Value, y = F1)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Effect of splitrule on F1 Score",
       x = "Split Rule",
       y = "F1 Score")

#############################################
# Alternative: 3D or Heatmap plots for two parameters at a time
#############################################

# Sometimes it's informative to visualize two parameters at once using contour/heatmaps.
# Example: F1 score as a function of mtry and min.node.size, averaging over other parameters.

agg_data <- results_df %>%
  group_by(mtry, min.node.size) %>%
  summarize(mean_F1 = mean(F1), .groups = "drop")

ggplot(agg_data, aes(x = mtry, y = min.node.size, fill = mean_F1)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Mean F1 Score by mtry and min.node.size",
       x = "mtry",
       y = "min.node.size",
       fill = "Mean F1") +
  theme_minimal()