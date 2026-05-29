############################################################
# CHAPTER 5: REGRESSION MODELS (TRAIN-TEST BASED)
############################################################

library(lmtest)
library(car)

############################################################
# LOAD DATA (FIXED - Excel not CSV)
############################################################
library(readxl)

train_data <- read_excel("Chapter 5 Simu Data Set.xlsx",
                         sheet = "Train Data")

test_data  <- read_excel("Chapter 5 Simu Data Set.xlsx",
                         sheet = "Test Data")

############################################################
# MODEL 1: a^(1/2)
############################################################
model_a <- lm(I(sqrt(a)) ~ sqrt(m) + sqrt(ASN0) + sqrt(ANSS0),
              data = train_data)

############################################################
# MODEL 2: b
############################################################
model_b <- lm(b ~ log(m) + log(ASN0) + log(ANSS0),
              data = train_data)

############################################################
# MODEL SUMMARIES
############################################################
summary(model_a)
summary(model_b)

############################################################
# VIF CHECK
############################################################
vif(model_a)
vif(model_b)

############################################################
# TRAIN PERFORMANCE
############################################################
train_pred_a <- predict(model_a, newdata = train_data)
train_pred_b <- predict(model_b, newdata = train_data)

train_rmse_a <- sqrt(mean((sqrt(train_data$a) - train_pred_a)^2))
train_rmse_b <- sqrt(mean((train_data$b - train_pred_b)^2))

############################################################
# TEST PERFORMANCE
############################################################
test_pred_a <- predict(model_a, newdata = test_data)
test_pred_b <- predict(model_b, newdata = test_data)

test_rmse_a <- sqrt(mean((sqrt(test_data$a) - test_pred_a)^2))
test_rmse_b <- sqrt(mean((test_data$b - test_pred_b)^2))

############################################################
# MAPE
############################################################
test_mape_a <- mean(abs((sqrt(test_data$a) - test_pred_a) / sqrt(test_data$a))) * 100
test_mape_b <- mean(abs((test_data$b - test_pred_b) / test_data$b)) * 100

############################################################
# ADJUSTED R Square
############################################################
adj_r2_a <- summary(model_a)$adj.r.squared
adj_r2_b <- summary(model_b)$adj.r.squared

############################################################
# FINAL RESULTS TABLE
############################################################
final_results <- data.frame(
  Model = c("a^(1/2) Model (Eq 5.4)",
            "b Model (Eq 5.5)"),
  
  Train_RMSE = c(train_rmse_a, train_rmse_b),
  Test_RMSE  = c(test_rmse_a, test_rmse_b),
  
  Test_MAPE  = c(test_mape_a, test_mape_b),
  
  Adj_R2 = c(adj_r2_a, adj_r2_b)
)

print(final_results)

############################################################
# DETAILED SUMMARY OUTPUT
############################################################
cat("\n================ MODEL A =================\n")
summary(model_a)

cat("\n================ MODEL B =================\n")
summary(model_b)

############################################################
# VIF OUTPUT
############################################################
cat("\nVIF MODEL A:\n")
print(vif(model_a))

cat("\nVIF MODEL B:\n")
print(vif(model_b))