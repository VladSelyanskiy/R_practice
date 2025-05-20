##### libraries
library(ggplot2)
library(ROCR)

# 0.88
# 0.89

##### data frame
my_df <- read.csv("diabetes_prediction_dataset.csv")

my_df[, c(1, 3, 4, 5, 9)] <- lapply(my_df[, c(1, 3, 4, 5, 9)], as.factor)

any(is.na(my_df))

my_df <- unique(my_df)

##### Overview
head(my_df)
str(my_df)
summary(my_df)

my_df[, -c(1, 3, 4, 5, 9)] <- scale(my_df[, -c(1, 3, 4, 5, 9)])

##### train model
fit  <- glm(diabetes ~ ., my_df, family = "binomial")
summary(fit)
head(predict(object = fit, type = "response"))
my_df$prob  <- predict(object = fit, type = "response")

#####
pred_fit <- prediction(my_df$prob, my_df$diabetes)
perf_fit <- performance(pred_fit,"tpr","fpr")
auc  <- performance(pred_fit, measure = "auc")
str(auc)

perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd = 2)
plot(add=T, perf4 , col = "green", lwd = 2)
plot(add=T, perf5, lwd = 2)

legend(x = 0.6,y = 0.5, c("spec", "sens", "accur"),
       col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

abline(v= 0.08, lwd = 2)
#####

my_df$pred_resp  <- factor(ifelse(my_df$prob > 0.08, 1, 0))

my_df$correct  <- ifelse(my_df$pred_resp == my_df$diabetes, 1, 0)

mean(my_df$correct)


test_df  <- my_df
test_df$diabetes  <- NA

test_df$diabetes  <- predict(fit, newdata = test_df, type = "response")
View(test_df)
