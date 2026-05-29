rm(list = ls())

############################################################
# LIBRARIES
############################################################
library(readxl)
library(caret)
library(Metrics)
library(xgboost)
library(e1071)
library(randomForest)
library(gbm)
library(FNN)
library(rpart)
library(lmtest)
library(car)
library(tseries)

############################################################
# CUSTOM R2 FUNCTION
############################################################
R2_fun <- function(actual, pred) {
  1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
}

############################################################
# LOAD DATA
############################################################
df <- read_excel("Chapter 3 Simu Data Set Updated.xlsx",
                 sheet = "Data")

############################################################
# FUNCTION FOR FULL MODEL PIPELINE
############################################################
run_models <- function(target_var) {
  
  cat("\n=================================================\n")
  cat("RUNNING MODELS FOR:", target_var, "\n")
  cat("=================================================\n")
  
  X <- df[, c("alpha", "ASN")]
  y <- df[[target_var]]
  
  ########################################################
  # OLS LOG MODEL
  ########################################################
  X_log <- log(X)
  ols_model <- lm(y ~ ., data = X_log)
  
  print(summary(ols_model))
  
  ########################################################
  # TRAIN TEST SPLIT
  ########################################################
  set.seed(42)
  trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
  
  X_train <- X[trainIndex, ]
  X_test  <- X[-trainIndex, ]
  
  y_train <- y[trainIndex]
  y_test  <- y[-trainIndex]
  
  ########################################################
  # XGBOOST
  ########################################################
  dtrain <- xgb.DMatrix(as.matrix(X_train), label = y_train)
  dtest  <- xgb.DMatrix(as.matrix(X_test), label = y_test)
  
  xgb_model <- xgboost(
    data = dtrain,
    nrounds = 100,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  ########################################################
  # MODELS
  ########################################################
  models <- list(
    OLS = lm(y_train ~ ., data = X_train),
    SVM = svm(X_train, y_train),
    RandomForest = randomForest(X_train, y_train),
    GBM = gbm(
      y_train ~ .,
      data = data.frame(X_train, y_train),
      distribution = "gaussian",
      n.trees = 100,
      interaction.depth = 3,
      shrinkage = 0.1,
      verbose = FALSE
    ),
    DecisionTree = rpart(y_train ~ ., data = data.frame(X_train, y_train))
  )
  
  models$XGBoost <- xgb_model
  
  ########################################################
  # EVALUATION
  ########################################################
  results_train <- data.frame()
  results_test  <- data.frame()
  
  for(name in names(models)) {
    
    if(name == "GBM") {
      
      train_pred <- predict(models[[name]], X_train, n.trees = 100)
      test_pred  <- predict(models[[name]], X_test, n.trees = 100)
      
    } else if(name == "XGBoost") {
      
      train_pred <- predict(models[[name]], dtrain)
      test_pred  <- predict(models[[name]], dtest)
      
    } else {
      
      train_pred <- predict(models[[name]], X_train)
      test_pred  <- predict(models[[name]], X_test)
    }
    
    results_train <- rbind(results_train, data.frame(
      Model = name,
      R2 = R2_fun(y_train, train_pred),
      RMSE = rmse(y_train, train_pred),
      MAPE = mean(abs((y_train - train_pred) / y_train)) * 100
    ))
    
    results_test <- rbind(results_test, data.frame(
      Model = name,
      R2 = R2_fun(y_test, test_pred),
      RMSE = rmse(y_test, test_pred),
      MAPE = mean(abs((y_test - test_pred) / y_test)) * 100
    ))
  }
  
  ########################################################
  # OUTPUT
  ########################################################
  cat("\nTRAIN RESULTS:\n")
  print(results_train)
  
  cat("\nTEST RESULTS:\n")
  print(results_test)
  
  ########################################################
  # FINAL OLS + XGBOOST PREDICTION
  ########################################################
  alpha_values <- c(0.01,0.05,0.01,0.05,0.01,0.05,0.05)
  ASN_values   <- c(10,10,20,20,50,50,9)
  
  newdata <- data.frame(
    alpha = log(alpha_values),
    ASN = log(ASN_values)
  )
  
  pred_ols <- predict(ols_model, newdata)
  
  new_ml <- data.frame(alpha = 0.05, ASN = 9)
  pred_xgb <- predict(xgb_model, xgb.DMatrix(as.matrix(new_ml)))
  
  cat("\nOLS Predictions:\n")
  print(data.frame(alpha_values, ASN_values, pred_ols))
  
  cat("\nXGBoost Prediction:\n")
  print(pred_xgb)
}

############################################################
# RUN FOR a
############################################################
run_models("a")

############################################################
# RUN FOR b
############################################################
run_models("b")