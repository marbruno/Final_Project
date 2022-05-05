# Before running modeling code, run code that cleans data in final_project.Rmd

# Loading libraries
library(tidymodels)
library(survey)
library(parsnip)

# New modeling script
# Create tibble with 2019 and 2020 data for model training, testing
# Removes variable we're trying to predict (employed)
asec_2019_2020 <- cps_svy %>%
  filter(year == 2019 | year == 2020) %>%
  filter(!is.na(employed))
  

# ------------------------------------Creating dataframe for step_pca------------------------------------

# Kept yrimmig and strechlk with 0 variable for NIU
# Recoded NIU values for wksunem1 (99) and wksunem2  (9) to zero

# Select relevant variables from data set
asec_pca_2019_2020 <- asec_2019_2020 %>%
  select(-year, -serial, -cpsid, -immigrant) %>% # deselect variables we don't want to include in PCA analysis
  select(-region, -county, -metro, -metarea, -metfips) %>% # deselect all location variables other than state
  select(-empstat, -labforce, -asecwtcvd) %>% # deselect variables that are unuseful (labforce) or redundant with employed variable
  mutate_at(vars(race, unitsstr, citizen, hispan,
            occ, ind, educ, classwly,
            strechlk, spmmort, health, paidgh, whymove, statefip), list(~ as.factor(.)))

asec_pca_2019_2020 <- recipe(~ ., asec_pca_2019_2020) %>%
  step_dummy(race, unitsstr, citizen, hispan,
             occ, ind, educ, classwly,
             strechlk, spmmort, whymove, health, paidgh, statefip) %>%
  prep() %>%
  bake(asec_pca_2019_2020)
# Note: did not include in step_dummy: hhincome, age, yrimmig, existing indicator variables (sex,
# offpov, disabwrk, himcarenw, caidnw, anycovly, prvtcovnw, grpcovnw, mrkcovnw, 
# mrkscovnw, inhcovnw, schipnw), wksunem1, wksunem2, ftotval, inctto, incwelfr, 
# incunemp, ctccrd, eitcred, moop, hipval

asec_pca_2019_2020 <- asec_pca_2019_2020 %>%
  mutate_at(
    vars(
      offpov, himcarenw, caidnw, anycovly, prvtcovnw, grpcovnw, mrkcovnw,
      mrkscovnw, inhcovnw, mrkucovnw, sex, starts_with("spmmort")
    ),
    list( ~ case_when(. == 1 ~ (1/sqrt(2)),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("citizen"), starts_with("health")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(4),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("unitsstr")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(5),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("classwly")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(7),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("hispan")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(8),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("educ")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(15),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("race")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(25),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("state")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(50),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_)) 
    ) %>%
  mutate_at(
    vars(
      starts_with("ind")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(279),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("occ")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(633),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("classwly")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(7),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("whymove")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(20),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("paidgh")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(3),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  )

# asec_pca_2019_2020 can be run by the models and apply PCA to them (using step_pca)

# ---------------------------------Model prep: non-PCA---------------------------------

# Preparing data for models
asec_models_2019_2020 <- asec_2019_2020 %>%
  filter(year == 2019 | year == 2020) %>%
  filter(!is.na(employed)) %>%
  mutate(employed = as.factor(employed)) %>% # Make our y variable a factor
  select(-year, -serial, -cpsid, -immigrant) %>% # deselect variables we don't want to include as predictors
  select(-region, -county, -metro, -metarea, -metfips) %>% # deselect most location variables other than county
  select(-empstat, -labforce) %>% # deselect variables that are unuseful (labforce)
  mutate_at(vars(race, unitsstr, citizen, hispan,
                 occ, ind, educ, classwly,
                 strechlk, spmmort, whymove, health, paidgh, statefip), list(~ as.factor(.)))


# Set seed so that selection of training/testing data is consistent between runs
# of the code chunk
set.seed(20201020)

# Split into training and testing data
split <- initial_split(data = asec_models_2019_2020, prop = 0.8)

asec_train <- remove_val_labels(training(split))
asec_test <- remove_val_labels(testing(split))

# Set up 10 v-folds
folds <- vfold_cv(data = asec_train, v = 10)

# Create recipe
asec_rec <-
  recipe(employed ~ ., data = asec_train) %>%
  step_dummy(race, unitsstr, citizen, hispan,
             occ, ind, educ, classwly,
             strechlk, spmmort, whymove, health, paidgh, statefip) %>%
  step_center(all_predictors()) %>% # center predictors
  step_scale(all_predictors()) %>% # scale predictors
  step_nzv(all_predictors()) %>%   # drop near zero variance predictors
  themis::step_downsample(employed) %>% # subsampling due to class imbalances between employment class 
  step_other() 

# Create rf recipe
rf_rec <-
  recipe(employed ~ ., data = asec_train) %>%
  step_dummy(race, unitsstr, citizen, hispan, educ, 
             classwly, strechlk, spmmort, whymove, 
             health, paidgh, statefip) %>% #Dummy select categorical variables
  step_center(all_numeric_predictors()) %>% # center predictors
  step_scale(all_numeric_predictors()) %>% # scale predictors
  step_nzv(all_numeric_predictors()) %>%   # drop near zero variance predictors
  themis::step_downsample(employed) %>% # subsampling due to class imbalances between employment class 
  step_other() #%>%
# prep() %>%
# bake(asec_train)


# ---------------------------------Model prep: PCA---------------------------------

# Preparing data for models
#USE asec_pca_2019_2020

# Set seed so that selection of training/testing data is consistent between runs
# of the code chunk
set.seed(20201020)

# Split into training and testing data
split <- initial_split(data = asec_pca_2019_2020, prop = 0.8)

asec_pca_train <- remove_val_labels(training(split))
asec_pca_test <- remove_val_labels(testing(split))

# Set up 10 v-folds
folds_pca <- vfold_cv(data = asec_pca_train, v = 10)

# Create recipe
asec_pca_rec <-
  recipe(employed ~ ., data = asec_pca_train) %>%
  step_center(all_numeric_predictors()) %>% # center predictors
  step_scale(all_numeric_predictors()) %>% # scale predictors
  step_nzv(all_numeric_predictors()) %>%   # drop near zero variance predictors
  step_pca(all_numeric(), num_comp = 20) %>%
  themis::step_downsample(employed) %>% # subsampling due to class imbalances between employment class 
  step_other() 

# See the engineered training data
bake(prep(asec_pca_rec, training = asec_pca_train), new_data = asec_pca_train)

# -------------------------Model 1: Random forest-------------------------------

# Build a random forest model (hyperparameter tuning for no. of trees and predictors sampled at each split)
rf_mod <- rand_forest(mtry = 10, min_n = tune(), trees = 100) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# Create a workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_rec)

# Create a grid of the parameters we're tuning for
rf_grid <- grid_regular(
  min_n(range = c(2, 8)),
  levels = 4)

# Execute hyperparameter tuning using the grid and the cross_validation folds
rf_cv <- tune_grid(rf_workflow,
                   resamples = folds,
                   grid = rf_grid,
                   metrics = metric_set(roc_auc))

# # Build a random forest that will incorporate principal components as predictors
# rf_mod_pca <- rand_forest(mtry = 10, min_n = tune(), trees = 100) %>%
#   set_engine("ranger", importance = "impurity") %>%
#   set_mode("classification")
# 
# # Create a workflow
# rf_workflow_pca <- 
#   workflow() %>% 
#   add_model(rf_mod_pca) %>% 
#   add_recipe(asec_pca_rec)
# 
# # Create a grid of the parameters we're tuning for
# rf_grid_pca <- grid_regular(
#   min_n(range = c(2, 8)),
#   levels = 4)
# 
# # Execute hyperparameter tuning using the grid and the cross_validation folds
# rf_cv_pca <- tune_grid(rf_workflow_pca,
#                    resamples = folds_pca,
#                    grid = rf_grid_pca,
#                    metrics = metric_set(roc_auc))

#Calculate ROC_AUC and accuracy for each fold 
collect_metrics(rf_cv, summarize = TRUE) %>%
  filter(.metric == "roc_auc")

# Select best model based on rmse (MARLYN note: we can choose to do it based on best roc_auc?)
rf_best <- rf_cv %>%
  select_best(metric = "roc_auc")

# Finalize model
rf_final_model <- finalize_model(rf_mod, rf_best)

# Look at (plot) feature importance
library(vip)
rf_final_model %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(employed ~ ., data = asec_train) %>%
  vip(geom = "point")

# # Finalize workflow with best model (1st way)
# rf_last_workflow <- rf_workflow %>%
#   finalize_workflow(parameters = rf_best)

# Finalize workflow (2nd way)
rf_last_workflow <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_final_model)

# Fit to the all training data
set.seed(20220429) #MARLYN: is it best practice to set a seed before last fit?
rf_last_fit <- rf_last_workflow %>%
  last_fit(split = split, metrics = metric_set(roc_auc))

# Look at feature importance
rf_last_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20)
# ------------------Train model on all training data --------------------------
# THIS ALSO DIDN'T WORK
# # the last model
# last_rf_mod <- 
#   rand_forest(mtry = 10, min_n = 2, trees = 100) %>% 
#   set_engine("ranger", importance = "impurity") %>% 
#   set_mode("classification")
# 
# # the last workflow
# last_rf_workflow <- 
#   rf_workflow %>% 
#   update_model(last_rf_mod)
# 
# # the last fit
# set.seed(345)
# last_rf_fit <- 
#   last_rf_workflow %>% 
#   last_fit(split)


# -------------------------Model 2: Logistic Regression (PCA)------------------------------

# Model 2: Logistic regression 

# Build the model
logistic_pca_mod <- logistic_reg(penalty = 1) %>% 
  set_engine("glmnet")

# Create a workflow
logistic_pca_workflow <- 
  workflow() %>% 
  add_model(logistic_pca_mod) %>% 
  add_recipe(asec_pca_rec)

# Fit to folds
logistic_pca_cv <- logistic_pca_workflow %>% 
  fit_resamples(resamples = folds_pca)

# Calculate RMSE and MAE for each fold 
collect_metrics(logistic_pca_cv, summarize = FALSE) 

# Select best model based on rmse (MARLYN note: we can choose to do it based on best roc_auc?)
logistic_pca_best <- logistic_pca_cv %>%
  select_best(metric = "roc_auc")

# Finalize workflow with best model
logistic_last_pca_workflow <- logistic_pca_workflow %>%
  finalize_workflow(parameters = logistic_pca_best)

# Fit to the all training data and check feature importance
set.seed(20220428) #MARLYN: is it best practice to set a seed before last fit?

logistic_last_fit <- logistic_last_pca_workflow %>%
  fit(data = asec_pca_train)

predict(logistic_last_fit, asec_pca_train) %>% group_by(.pred_class) %>% summarize(n = n())

# -------------Running the best model specification on the 2021 data------------



# -------------------------Run model on immigrant data--------------------------
