# Load libraries
library(tidyverse)
library(caret)
library(pROC)
library(car)
library(ResourceSelection)

# Load data
bank <- read.csv("~/Downloads/bank.csv")
bank_new <- read.csv("~/Downloads/bank_new.csv")

# Convert outcome to binary numeric
bank$y <- ifelse(bank$y == "yes", 1, 0)
bank_new$y <- ifelse(bank_new$y == "yes", 1, 0)

#Question 1- Initial Logistic Regression Using All Predictors
full_model <- glm(y ~ ., data = bank, family = binomial)
summary(full_model)

vif(full_model)

#Model Reduction Process
#Step 1: Remove High Multicollinearity Variables

reduced_model_1 <- glm(
  y ~ . - pdays - previous,
  data = bank,
  family = binomial
)
alias(reduced_model_1)


#Step 2: Remove aliased factor levels
reduced_model_2 <- glm(
  y ~ . - pdays - previous - loan,
  data = bank,
  family = binomial
)
vif(reduced_model_2)

#Step 3: Remove Non-Significant Predictors

final_model <- glm(
  y ~ age + job + marital + education + default +
    X + housing + loan + contact +
    duration + campaign,
  data = bank,
  family = binomial
)
summary(final_model)

#ROC Curve
bank$pred_prob <- predict(final_model, type = "response")

roc_obj <- roc(bank$y, bank$pred_prob)
plot(roc_obj, main = "ROC Curve – Final Model")
auc(roc_obj)

#Calibration plot
cal_df <- bank %>%
  mutate(bin = ntile(pred_prob, 10)) %>%
  group_by(bin) %>%
  summarise(
    mean_pred = mean(pred_prob),
    mean_actual = mean(y)
  )

plot(cal_df$mean_pred, cal_df$mean_actual,
     xlab = "Predicted Probability",
     ylab = "Observed Proportion",
     main = "Calibration Plot")
abline(0,1)

#Refit without leakage
no_leak_model <- glm(
  y ~ age + job + marital + education + default +
    X + housing + loan + contact + campaign,
  data = bank,
  family = binomial
)

bank$pred_prob_nl <- predict(no_leak_model, type = "response")
roc_nl <- roc(bank$y, bank$pred_prob_nl)
plot(roc_nl, main = "ROC – No Leakage Model")
auc(roc_nl)

#Question 2: Forward Selection vs Backward Elimination

#Forward Selection
null_model <- glm(y ~ 1, data = bank, family = binomial)

fs_model <- step(
  null_model,
  scope = formula(final_model),
  direction = "forward"
)
summary(fs_model)

#Backward Selection
be_model <- step(
  final_model,
  direction = "backward"
)
summary(be_model)

# Question 3: LOCO Variable Importance Using Brier Score

loco_results <- data.frame(
  variable = vars,
  delta_brier = NA
)

for (i in seq_along(vars)) {
  formula_loco <- as.formula(
    paste("y ~", paste(setdiff(vars, vars[i]), collapse = " + "))
  )
  
  model_loco <- glm(formula_loco, data = bank, family = binomial)
  pred_loco <- predict(model_loco, type = "response")
  
  loco_results$delta_brier[i] <-
    brier_score(bank$y, pred_loco) - base_brier
}


#Question 4: Cumulative Gains Chart (bank_new.csv)

bank_new$pred_prob <- predict(final_model, bank_new, type = "response")

bank_new <- bank_new %>%
  arrange(desc(pred_prob)) %>%
  mutate(
    rank = row_number(),
    cum_success = cumsum(y)
  )
plot(
  bank_new$rank,
  bank_new$cum_success,
  type = "l",
  xlab = "Number of Customers Targeted",
  ylab = "Cumulative Subscriptions",
  main = "Cumulative Gains Chart"
)
bank_new$cum_success[1000]
