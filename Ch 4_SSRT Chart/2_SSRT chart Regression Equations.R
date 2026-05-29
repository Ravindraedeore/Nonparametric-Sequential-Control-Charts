rm(list = ls())

############################################################
# LIBRARIES
############################################################
library(readxl)
library(car)
library(caret)
library(Metrics)
library(xgboost)
library(e1071)
library(randomForest)
library(gbm)
library(FNN)
library(rpart)

############################################################
# LOAD DATA
############################################################
df <- read_excel("Chapter 4 Simu Data Set.xlsx",
                 sheet = "Sheet1")

############################################################
# =========================
# PART 1: THEORY MODELS
# =========================
############################################################

# LOG VARIABLES
df$ln_ASN0  <- log(df$ASN0)
df$ln_ANSS0 <- log(df$ANSS0)

df$b2 <- df$b^2

############################################################
# MODEL A: ln(a)
############################################################
model_a_lm <- lm(log(a) ~ ln_ASN0 + ln_ANSS0, data = df)
summary(model_a_lm)
vif(model_a_lm)

############################################################
# MODEL B: b^2
############################################################
model_b_lm <- lm(b2 ~ ln_ASN0 + ln_ANSS0, data = df)
summary(model_b_lm)
vif(model_b_lm)

############################################################
# =========================
# PART 2: ML MODELS (FOR a and b)
# =========================
############################################################

run_ml <- function(df, response_var) {
  
  X <- df[, c("ASN0","ANSS0")]
  y <- df[[response_var]]
  
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
  # RESULTS STORAGE
  ########################################################
  results_train <- data.frame()
  results_test  <- data.frame()
  
  ########################################################
  # LOOP
  ########################################################
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
    
    results_train <- rbind(results_train,
                           data.frame(
                             Model = name,
                             R2 = 1 - sum((y_train - train_pred)^2) /
                               sum((y_train - mean(y_train))^2),
                             RMSE = sqrt(mean((y_train - train_pred)^2)),
                             MAPE = mean(abs((y_train - train_pred)/y_train))*100
                           ))
    
    results_test <- rbind(results_test,
                          data.frame(
                            Model = name,
                            R2 = 1 - sum((y_test - test_pred)^2) /
                              sum((y_test - mean(y_test))^2),
                            RMSE = sqrt(mean((y_test - test_pred)^2)),
                            MAPE = mean(abs((y_test - test_pred)/y_test))*100
                          ))
  }
  
  return(list(
    train = results_train,
    test = results_test,
    xgb = xgb_model
  ))
}

############################################################
# =========================
# PART 3: RUN FOR a
# =========================
############################################################

res_a <- run_ml(df, "a")

cat("\n================ ML RESULTS FOR a (TRAIN) ================\n")
print(round(res_a$train, 4))

cat("\n================ ML RESULTS FOR a (TEST) ================\n")
print(round(res_a$test, 4))

############################################################
# =========================
# PART 4: RUN FOR b
# =========================
############################################################

res_b <- run_ml(df, "b")

cat("\n================ ML RESULTS FOR b (TRAIN) ================\n")
print(round(res_b$train, 4))

cat("\n================ ML RESULTS FOR b (TEST) ================\n")
print(round(res_b$test, 4))

############################################################
# =========================
# PART 5: PREDICTIONS (THEORY + ML)
# =========================
############################################################

newdata <- data.frame(
  ASN0 = 25,
  ANSS0 = 300
)

# THEORY MODEL PREDICTIONS
pred_ln_a <- predict(model_a_lm,
                     data.frame(ln_ASN0 = log(25), ln_ANSS0 = log(300)))

pred_b2 <- predict(model_b_lm,
                   data.frame(ln_ASN0 = log(25), ln_ANSS0 = log(300)))

cat("\nTHEORY MODEL RESULTS:\n")
cat("Predicted ln(a):", pred_ln_a, "\n")
cat("Predicted a:", exp(pred_ln_a), "\n")
cat("Predicted b:", sqrt(pred_b2), "\n")

# ML PREDICTIONS (XGBOOST)
pred_a_ml <- predict(res_a$xgb, xgb.DMatrix(as.matrix(newdata)))
pred_b_ml <- predict(res_b$xgb, xgb.DMatrix(as.matrix(newdata)))

cat("\nML MODEL RESULTS (XGBoost):\n")
cat("Predicted a:", pred_a_ml, "\n")
cat("Predicted b:", pred_b_ml, "\n")