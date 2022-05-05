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
# Recoded NIU values for wksunem1 (99) and wksunem2 and (9) to zero

# Select relevant variables from data set
asec_pca_2019_2020 <- asec_2019_2020 %>%
  select(-year, -serial, -cpsid, -immigrant) %>% # deselect variables we don't want to include in PCA analysis
  select(-region, -county, -metro, -metarea, -metfips) %>% # deselect all location variables other than state
  select(-empstat, -labforce) %>% # deselect variables that are unuseful (labforce)
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

# ---------------------------------Model prep---------------------------------

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

# Reapply ASEC weights and save as tibble
asec_train_weighted <- asec2019_2020_train %>%
  as_survey_design(weights = asecwtcvd)
#asec_train <- as_tibble(asec_train_weighted)
asec_test_weighted <- asec2019_2020_test %>%
  as_survey_design(weights = asecwtcvd)
#asec_test <- as_tibble(asec_test_weighted)

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

# -------------------------Model 1: Random forest-------------------------------

# Build a random forest model (hyperparametr tuning for no. of trees and predictors sampled at each split)
rf_mod <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# Create a workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(asec_rec)

# Create a grid of the parameters we're tuning for
rf_grid <- grid_regular(
  mtry(range = c(10, 20)), #MARLYN: I don't think we even have 50 predictors?
  min_n(range = c(2, 8)),
  levels = 5)

# Execute hyperparameter tuning using the grid and the cross_validation folds
rf_cv <- rf_workflow %>% 
  tune_grid(rf_workflow,
            resamples = folds,
            grid = rf_grid,
            metrics = metric_set(roc_auc, accuracy))

#Calculate RMSE and MAE for each fold 
collect_metrics(rf_cv, summarize = FALSE) 

# Select best model based on rmse (MARLYN note: we can choose to do it based on best roc_auc?)
rf_best <- rf_cv %>%
  select_best(metric = "roc_auc")

# Finalize workflow with best model
rf_last_workflow <- rf_workflow %>%
  finalize_workflow(parameters = rf_best)

# Fit to the all training data
set.seed(20220429) #MARLYN: is it best practice to set a seed before last fit?
rf_last_fit <- rf_last_workflow %>%
  last_fit(split = asec2019_2020_train)

# Look at feature importance
rf_last_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20)

# -------------------------Model 2: Logistic Regression------------------------------

# Model 2: Logistic regression 

# Build the model (hyperparameter tuning for penalty)
logistic_mod <- logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

# Create a workflow
logistic_workflow <- 
  workflow() %>% 
  add_model(logistic_mod) %>% 
  add_recipe(asec_rec)

# Create a grid of penalty values to tune
logistic_grid <- grid_regular(penalty(), levels = 10)

# Execute hyperparameter tuning using the grid and the cross_validation folds
logistic_cv <- logistic_workflow %>% 
  tune_grid(resamples = folds,
            grid = logistic_grid,
            metrics = metric_set(roc_auc, accuracy))

# Calculate RMSE and MAE for each fold 
collect_metrics(logistic_cv, summarize = FALSE) 

# Select best model based on rmse (MARLYN note: we can choose to do it based on best roc_auc?)
logistic_best <- logistic_cv %>%
  select_best(metric = "roc_auc")

# Finalize workflow with best model
logistic_last_workflow <- logistic_workflow %>%
  finalize_workflow(parameters = logistic_best)

# Fit to the all training data and check feature importance
set.seed(20220428) #MARLYN: is it best practice to set a seed before last fit?
logistic_last_fit <- logistic_last_workflow %>%
  last_fit(data = asec2019_2020_train) %>% 
  extract_fit_parsnip() %>%
  vi(lambda = logistic_best$penalty)

vip(num_features = 20) #looking at feature importance


# -------------Running the best model specification on the 2021 data------------



# -------------------------Run model on immigrant data--------------------------
