library(tidyverse)
library(tidymodels)

# Read data set, create partitions, define metrics.

bank <- read_csv("bank.csv")

bank$y <- as_factor(bank$y)
bank$y <- fct_relevel(bank$y,c("yes","no"))

set.seed(1234)
bank <- bank %>%
  sample_n(2000)

bank_split <- initial_split(bank, prop=0.8, strata="y")

bank_train <- training(bank_split)
bank_test <- testing(bank_split)

fold <- vfold_cv(bank_train, v=10, strata="y")
metric <- metric_set(roc_auc,accuracy)

# Model : Logistic Regression

rf_spec <- 
  rand_forest(
    mode="classification",
    trees = tune("trees"),
    #mtry = tune("mtry"),
    engine = "ranger"
  )

rf_rec <- 
  recipe(y ~ ., data=bank_train) %>% 
  step_dummy(all_nominal_predictors()) 

rf_wflow <-
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(rf_rec)

rf_grid <- expand.grid(trees=c(5,10,100,200))

rf_res <- 
  tune_grid(
    rf_wflow,
    resamples = fold,
    metrics = metric,
    grid = rf_grid,
    #mtry = rf_grid
  )

collect_metrics(rf_res)
show_best(rf_res,"roc_auc")

# Select best model automatically. Estimate performance on test set

best_rf_params <- select_best(rf_res,"roc_auc")
best_rf_wflow <- finalize_workflow(rf_wflow,best_rf_params)

best_rf_fit <- last_fit(best_rf_wflow,bank_split )
collect_metrics(best_rf_fit)

# Predicting new observations (classes & probabilities)

best_rf_model <- fit(best_rf_wflow,data=bank_train)

pred_class_rf <- predict(best_rf_model, new_data= bank_test, type="class")
pred_prob_rf <- predict(best_rf_model, new_data = bank_test, type="prob")
