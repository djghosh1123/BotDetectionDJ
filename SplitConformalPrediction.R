library(e1071)  # For SVM
library(caret)  # For data partitioning
library(dplyr)  # For data manipulation

setwd("D:/PostDoc/allMyPapers/Bot Detection")
dat = read.csv("Test_Features_Aug24.csv")
dat = dat[,-1]
dat$label_used = as.factor(dat$label_used)

####  Split Conformal Prediction

set.seed(123)
trainIndex <- createDataPartition(dat$Label, p = 0.5, list = FALSE)
train_data <- dat[trainIndex, ]
temp_data <- dat[-trainIndex, ]


calibIndex <- createDataPartition(temp_data$Label, p = 0.5, list = FALSE)
calib_data <- temp_data[calibIndex, ]
test_data <- temp_data[-calibIndex, ]


svm_model <- svm(Label ~ ., data = train_data, probability = TRUE)

# Get probability predictions on calibration data
calib_pred <- predict(svm_model, calib_data, probability = TRUE)
calib_probs <- attr(calib_pred, "probabilities")[, 2]  # Extract bot probabilities

# Compute nonconformity scores for calibration set
calib_scores <- 1 - calib_probs  # Nonconformity: Lower probability = Higher uncertainty

# Get probability predictions on test data
test_pred <- predict(svm_model, test_data, probability = TRUE)
test_probs <- attr(test_pred, "probabilities")[, 2]

# Compute p-values for test set
p_values <- sapply(test_probs, function(prob) {
  (sum(calib_scores >= (1 - prob)) + 1) / (length(calib_scores) + 1)
})

# Set significance level (e.g., 90% confidence)
alpha <- 0.05
prediction_sets <- ifelse(p_values > alpha, "Bot", "Non-Bot")

results_df <- data.frame(P_Value = p_values, Actual = test_data$Label, Predicted = prediction_sets)
# Plot histogram of p-values
p1 = ggplot(results_df, aes(x = P_Value, fill = Actual)) +
  geom_histogram(binwidth = 0.05, alpha = 0.7, position = "identity") +
  labs(title = "Histogram of Split Conformal Prediction P-Values",
       x = "P-Value", y = "Frequency") +
  theme_minimal()

# Output results
results <- data.frame(Actual = test_data$Label, Predicted = prediction_sets, P_Value = p_values)
print(results)

library(pROC)
roc_curve <- roc(test_data$Label, p_values)
plot(roc_curve, col="blue", main="ROC Curve for Conformal Prediction")
