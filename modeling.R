# Before running modeling code, run code that cleans data in final_project.Rmd

# Loading libraries
library(tidymodels)

# New modeling script
# Create tibble with 2019 and 2020 data for model training, testing
# Removes variable we're trying to predict (employed)
asec_2019_2020 <- cps_svy %>%
  filter(year == 2019 | year == 2020) %>%
  filter(!is.na(employed))
  

# ------------------------------------PCA------------------------------------
# Select numeric variables from data set
asec_pca_2019_2020 <- asec_2019_2020 %>%
  select(-year, -serial, -cpsid, -immigrant) %>% # deselect variables we don't want to include in PCA analysis
  select(-region, -statefip, -metro, -metarea, -metfips, -statefip) %>% # deselect all location variables other than county
  select(-empstat, -labforce) %>% # deselect variables that are unuseful (labforce)
  mutate_at(vars(race, unitsstr, citizen, hispan,
            occ, ind, educ, classwly,
            strechlk, spmmort, whymove, health, paidgh), list(~ as.factor(.)))

# SYLVIA: need to double check that this works properly
asec_pca_2019_2020 <- recipe(~ ., asec_pca_2019_2020) %>%
  step_dummy(race, unitsstr, citizen, hispan,
             occ, ind, educ, classwly,
             strechlk, spmmort, whymove, health, paidgh) %>%
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
      mrkscovnw, inhcovnw, mrkucovnw, sex
    ),
    list( ~ case_when(. == 1 ~ sqrt(2),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  

# as_survey_design(weights = asecwtcvd)

# create a correlation matrix on asec_pca_2019_2020
cor(asec_pca_2019_2020)

# conduct PCA on the asec_pca_2019_2020 data
  # code below will center and scale the data
asec_pca <- prcomp(asec_pca_2019_2020, scale. = TRUE)

# obtain summary metrics
summary(asec_pca)

# obtain loadings
asec_pca$rotation

# obtain component values for each observation
pca_data <- as_tibble(asec_pca$x) %>%
  select(PC1:PC10)

# extract the PCs
asec_pcs <- asec_pca %>%
  .$x %>%
  as_tibble()

# combine the pcs to the names and parties
asec_pcs <- bind_cols(
  asec2019_2020,
  asec_pcs
)

# ---------------------------------Model prep---------------------------------
# Set seed so that selection of training/testing data is consistent between runs
# of the code chunk
set.seed(20201020)

# Split into training and testing data
split <- initial_split(data = asec2019_2020, prop = 0.8)

asec2019_2020_train <- training(asec2019_2020)
asec2019_2020_test <- testing(asec2019_2020)

# Set up 10 v-folds
folds <- vfold_cv(data = asec2019_2020_train, v = 10)

asec_rec <- 
  recipe(employed ~ ., data = asec2019_2020train) %>%
  update_role(serial, cpsid, cpsidp, new_role = "ID") %>%
  step_dummy(all_nominal_predictors()) %>% # dummy encode categorical predictors 
  step_center(all_predictors()) %>% # center predictors
  step_scale(all_predictors()) %>% # scale predictors
  step_nzv(all_predictors()) %>%   # drop near zero variance predictors
  step_downsample(employed) # subsampling due to class imbalances between employment class 

# -------------------------Model 1: Random forest-------------------------------

# Build a random forest model (hyperparametr tuning for no. of trees and predictors sampled at each split)
rf_mod <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# Create a workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(asec_rec)

# Create a grid of the parameters we're tuning for
rf_grid <- grid_regular(
  mtry(range = c(10, 50)), #MARLYN: I don't think we even have 50 predictors?
  min_n(range = c(2, 8)),
  levels = 5)

# Execute hyperparameter tuning using the grid and the cross_validation folds
rf_cv <- rf_workflow %>% 
  tune_grid(rf_workflow,
            resamples = folds,
            grid = rf_grid,
            metrics = metric_set(roc_auc, rmse))

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
logistic_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
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
            metrics = metric_set(roc_auc, rmse))

# Calculate RMSE and MAE for each fold 
collect_metrics(logistic_cv, summarize = FALSE) 

# Select best model based on rmse (MARLYN note: we can choose to do it based on best roc_auc?)
logistic_best <- logistic_cv %>%
  select_best(metric = "roc_auc")

# Finalize workflow with best model
logistic_last_workflow <- logistic_wf %>%
  finalize_workflow(parameters = logistic_best)

# Fit to the all training data and check feature importance
set.seed(20220428) #MARLYN: is it best practice to set a seed before last fit?
logistic_last_fit <- logistic_last_workflow %>%
  last_fit(data = asec2019_2020_train) %>% 
  extract_fit_parsnip() %>%
  #vi(lambda = logistic_best$penalty) #Do we need this line for logistic reg.?
  vip(num_features = 20) #looking at feature importance

# -------------------------Model 3: KNN? Decision tree?-------------------------------


# -------------------------Running the best model on the 2021 data--------------

# -------------------------Run model on immigrant data--------------------------