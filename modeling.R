# Before running modeling code, run code that cleans data in final_project.Rmd

# Loading libraries
library(tidymodels)

# New modeling script
# Create tibble with 2019 and 2020data for model training, testing
asec2019_2020 <- asec_allyears %>%
  filter(year == 2019 | year == 2020) %>%
  select(-starts_with("q"), -asecwt) # Drop quality flags
# STILL NEEDS MORE CLEANING
# Drop year, serial, month, asecflag
# Don't predict on asecwtd
# ^ ABOVE FOR SYLVIA TO DO

# Set seed so that selection of training/testing data is consistent between runs
# of the code chunk
set.seed(20201020)

# Split into training and testing data
split <- initial_split(data = asec2019_2020, prop = 0.8)

asec2019_2020_train <- training(asec2019_2020)
asec2019_2020_test <- testing(asec2019_2020)

asec_rec <- 
  recipe(employed ~ ., data = asec2019_2020train)
# dummy encode categorical predictors
step_dummy(all_nominal_predictors()) %>%
  # center and scale predictors
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  # drop near zero variance predictors
  step_nzv(all_predictors()) %>%
  # subsampling due to class imbalances between employment class 
  step_downsample(employed)

#PCA
# Select numeric variables from data set
asec_numeric_2019_2020 <- asec2019_2020 %>%
  #Want to select only the variables from asecwht to immigrant to be predictors
  select(-year, -serial, -month, -cpsid, -asecflag) %>%
  #Include only numeric variables as required to do PCA
  select_if(is.numeric) %>%
  #Want to rescale all numeric variables - HELP
  mutate(across(everything(), ~ scales::rescale()))#,
v2 = scales::rescale(v2),
v3 = scales::rescale(v3)
)

# create a recipe with no outcome variable and all predictors
pca_rec <- recipe(~., data = asec_numeric_2019_2020) %>%
  # center and scale all predictors
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  # run prep to prepare recipe
  prep()

# apply recipe to data
employment_clust <- pca_rec %>%
  bake(new_data = NULL)

# create a correlation matrix on employment_clust
cor(employment_clust)

# conduct PCA on the employment_clust data
principal_components <- prcomp(employment_clust)

# obtain summary metrics
summary(principal_components)

# obtain loadings
principal_components$rotation

# obtain component values for each observation
pca_data <- as_tibble(principal_components$x) %>%
  select(PC1:PC10)

# Model 1: Random forest with hypertuning no. of trees, N_estimators

# Model 2: Logistic regression 

# Model 3: K Nearest Neighbor? Decision Tree?