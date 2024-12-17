model_data <- data_vartypes %>%
  select(-all_of(exclude_cols)) %>% 
  select(-all_of(nonpredictor_cols)) %>% 
  na.omit()

model_data$thirtyday_expire_flag <- factor(model_data$thirtyday_expire_flag, 
                                     levels = c(0,1), 
                                     labels = c("Survived", "Died"))

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

# Install if not already installed:
# install.packages(c("caret", "ranger", "pROC", "PRROC", "ggplot2", "dplyr", "tidyr"))
library(caret)
library(ranger)
library(pROC)
library(PRROC)
library(ggplot2)
library(dplyr)
library(tidyr)

# Assume train_data and test_data are already defined, and Class is a factor
# with levels: "Survived" and "Died" (positive = "Died").

#############################################
# Custom summary function returning multiple metrics
#############################################


multiSummary <- function(data, lev = NULL, model = NULL) {
  positive_class <- lev[2] # "Died"
  pred_probs <- data[[positive_class]]
  
  precision <- posPredValue(data$pred, data$obs, positive_class)
  recall <- sensitivity(data$pred, data$obs, positive_class)
  f1 <- 2 * (precision * recall) / (precision + recall + .Machine$double.eps)
  
  roc_obj <- pROC::roc(response = data$obs, predictor = pred_probs, levels = rev(lev))
  auc_val <- pROC::auc(roc_obj)
  
  pos_scores <- pred_probs[data$obs == positive_class]
  neg_scores <- pred_probs[data$obs != positive_class]
  pr_obj <- PRROC::pr.curve(scores.class0 = pos_scores, scores.class1 = neg_scores, curve = FALSE)
  pr_auc_val <- pr_obj$auc.integral
  
  accuracy <- mean(data$obs == data$pred)
  
  c(F1 = f1,
    Precision = precision,
    Recall = recall,
    ROC_AUC = as.numeric(auc_val),
    PR_AUC = pr_obj$auc.integral,
    Accuracy = accuracy)
}


# 
# multiSummary <- function(data, lev = NULL, model = NULL) {
#   # data has columns: obs (true outcome), pred (predicted class), and probs if classProbs=TRUE
#   # lev[2] should be the positive class, here "Died"
#   
#   positive_class <- lev[2] # "Died"
#   pred_probs <- data[[positive_class]]
#   
#   # Extract predicted probabilities for the positive class if available
#   if (!is.null(data$prob)) {
#     pred_probs <- data$prob[, positive_class]
#   } else {
#     # If probabilities not available, we cannot compute AUC reliably
#     stop("Class probabilities are required for AUC calculation.")
#   }
#   
#   # Compute confusion matrix-based stats:
#   precision <- posPredValue(data$pred, data$obs, positive_class)
#   recall <- sensitivity(data$pred, data$obs, positive_class)
#   f1 <- 2 * (precision * recall) / (precision + recall + .Machine$double.eps)
#   
#   # Compute AUC (ROC):
#   roc_obj <- roc(response = data$obs, predictor = pred_probs, levels = rev(lev))
#   auc_val <- auc(roc_obj)
#   
#   # You could also compute PR AUC if desired:
#   # PR curve expects the predicted scores for class1 vs class0
#   pos_scores <- pred_probs[data$obs == positive_class]
#   neg_scores <- pred_probs[data$obs != positive_class]
#   pr_obj <- pr.curve(scores.class0 = pos_scores, scores.class1 = neg_scores, curve = FALSE)
#   pr_auc_val <- pr_obj$auc.integral
#   
#   # Return a named vector of metrics:
#   c(F1 = f1,
#     Precision = precision,
#     Recall = recall,
#     ROC_AUC = as.numeric(auc_val),
#     PR_AUC = pr_auc_val)
# }

#############################################
# Define training control and tuning grid
#############################################

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = multiSummary,
  savePredictions = TRUE,
  verboseIter = TRUE
)

# Example grid (adjust as needed):
tune_grid <- expand.grid(
  mtry = c(5, 9, 12, 15, 18, 25, 35, 40), 
  min.node.size = c(1, 5, 10, 15, 20),
  splitrule = c("gini")#, "extratrees")#,
#  max.depth = c(5, 10, 15, NA) # NA means no limit in ranger
)

#############################################
# Train the model across the grid
#############################################

set.seed(123)
model_res <- train(
  thirtyday_expire_flag ~ .,
  data = train_data,
  method = "ranger",
  metric = "F1",            # Metric to select best model, but we still get all metrics
  trControl = ctrl,
  tuneGrid = tune_grid,
  importance = "impurity"
)

model_res$bestTune

head(as.data.frame(model_res$results) %>% arrange(desc(Accuracy)), n = 1)
head(as.data.frame(model_res$results) %>% arrange(desc(F1)), n = 1)
head(as.data.frame(model_res$results) %>% arrange(desc(PR_AUC)), n = 1)
head(as.data.frame(model_res$results) %>% arrange(desc(Precision)), n = 1)
head(as.data.frame(model_res$results) %>% arrange(desc(Recall)), n = 1)
head(as.data.frame(model_res$results) %>% arrange(desc(ROC_AUC)), n = 1)
#############################################
# Inspect results
#############################################

# model_res$results contains columns: mtry, min.node.size, splitrule, max.depth,
# plus F1, Precision, Recall, ROC_AUC, PR_AUC, and possibly others.

head(model_res$results)

#############################################
# Convert results to long format for plotting
#############################################

results_long <- model_res$results %>%
  pivot_longer(cols = c(Accuracy, F1, Precision, Recall, ROC_AUC, PR_AUC),
               names_to = "Metric",
               values_to = "Value")

#############################################
# Example Plots
#############################################

# 1. Compare metrics vs. mtry for a fixed splitrule and max.depth:
# Filter for a specific combination to simplify:
subset_data <- results_long %>%
  filter(splitrule == "gini", min.node.size == 10)

ggplot(subset_data, aes(x = mtry, y = Value, color = Metric, group = Metric)) +
  geom_line() +
  geom_point() +
  labs(title = "Metrics vs. mtry (splitrule=gini, max.depth=10, min.node.size=5)",
       x = "mtry", y = "Metric Value") +
  theme_minimal()


results_long %>%
  filter(splitrule == "gini") %>% 
  ggplot(aes(x = mtry, y = Value, color = Metric, group = Metric)) +
  geom_line() +
  geom_point() +
  facet_wrap(~min.node.size) +
  labs(title = "Metrics vs. mtry)",
       x = "mtry", y = "Metric Value") +
  theme_minimal()

results_long %>%
  filter(splitrule == "gini") %>% 
  ggplot(aes(x = min.node.size, y = Value, color = Metric, group = Metric)) +
  geom_line() +
  geom_point() +
  facet_wrap(~mtry) +
  labs(title = "Metrics vs. mtry (splitrule=gini, max.depth=10, min.node.size=5)",
       x = "mtry", y = "Metric Value") +
  theme_minimal()

results_long %>%
  filter(splitrule == "gini") %>% 
  ggplot(aes(x = mtry, y = Value, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Metric, scales = "free", ncol = 2) +
  labs(title = "Random Forest: Metrics vs. mtry (for different min.node.size)",
       x = "mtry", y = "Metric Value") +
  theme_minimal()

# 2. Heatmap of F1 score for min.node.size vs. mtry:
f1_data <- model_res$results %>%
  select(mtry, min.node.size, F1) %>%
  group_by(mtry, min.node.size) %>%
  summarize(MeanF1 = mean(F1), .groups = "drop")

ggplot(f1_data, aes(x = factor(mtry), y = factor(min.node.size), fill = MeanF1)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "F1 by mtry and min.node.size",
       x = "mtry",
       y = "min.node.size",
       fill = "F1") +
  theme_minimal()

# 3. Facet by metric to see all metrics across a parameter:
# Let's look at how all metrics change with mtry (for a given min.node.size, splitrule, max.depth)
facet_data <- results_long %>%
  filter(min.node.size == 10)#, splitrule == "extratrees", max.depth == 5)

ggplot(facet_data, aes(x = mtry, y = Value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "All Metrics vs. mtry (min.node.size=10, splitrule=extratrees, max.depth=5)",
       x = "mtry",
       y = "Value") +
  theme_minimal()

#############################################
# Interpretation & Next Steps
#############################################

# - The 'results_long' dataframe is now your hub for various comparisons.
# - You can create additional plots, comparing different parameters side-by-side,
#   focusing on the metric that matters most (F1, ROC_AUC, PR_AUC, etc.).
# - Consider filtering by top-performing configurations and analyzing differences in metrics.
# - Since everything is in a tidy format, you can leverage dplyr to group by parameters,
#   summarize results (e.g., mean F1 per mtry), and plot distributions or boxplots.