# libraries
library("caret")
library("ggplot2")
library("ROCR")
set.seed(42)

#####
# read and process data
my_df <- read.csv("diabetes_prediction_dataset.csv")
my_df[, c(1, 3, 4, 5)] <- lapply(my_df[, c(1, 3, 4, 5)], as.factor)
my_df <- unique(my_df)
# scale
my_df[, -c(1, 3, 4, 5, 9)] <-  lapply(my_df[, -c(1, 3, 4, 5, 9)], scale)
# delete extra
Q1 <- quantile(my_df$bmi, 0.25)
Q3 <- quantile(my_df$bmi, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
my_df <- my_df[my_df$bmi >= lower_bound & my_df$bmi <= upper_bound, ]

# split data
train_index <- createDataPartition(my_df$diabetes, p = 0.7, list = FALSE)
train_data <- my_df[train_index, ]
test_data <- my_df[-train_index, ]

# train model on split data
model <- glm(diabetes ~ ., data = train_data, family = "binomial")

predictions <- predict(model, newdata = test_data[, c(-9)], type = "response")

predicted_classes <- ifelse(predictions > 0.07, 1, 0)

correct <- ifelse(predicted_classes == test_data$diabetes, 1, 0)

predicted_classes <- factor(predicted_classes)
test_data$diabetes <- factor(test_data$diabetes)

confusion_matrix <- confusionMatrix(predicted_classes, test_data$diabetes)
print(confusion_matrix)
#####

mean(correct)
confusion_matrix$byClass["F1"]

mean(correct)
0.9588122
F1 
0.9776813 

accuracy <- mean(my_df$correct)
f1_score <- confusion_matrix$byClass["F1"]

#####
pred_fit <- prediction(as.numeric(predictions), as.numeric(test_data$diabetes))
perf_fit <- performance(pred_fit, "tpr", "fpr")
plot(perf_fit)
# Add a diagonal reference line
abline(a = 0, b = 1, col = "red", lty = 2)

#####
perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd = 2)
plot(add=T, perf4 , col = "green", lwd = 2)
plot(add=T, perf5, lwd = 2)

abline(v= 0.07, lwd = 2)

threshold = 0.07
save(model, threshold, file = "model.RData")
