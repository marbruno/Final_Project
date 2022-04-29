# Before running modeling code, run code that cleans data in final_project.Rmd

# Loading libraries
library(tidymodels)

# New modeling script
# Create tibble with 2019 and 2020 data for model training, testing
asec_2019_2020 <- asec_allyears %>%
  filter(year == 2019 | year == 2020)
  

# ------------------------------------PCA------------------------------------
# Select numeric variables from data set
asec_pca_2019_2020 <- asec_2019_2020 %>%
  select(-year, -serial, -cpsid, -immigrant, -asecwtcvd)
  # SYLVIA NOTE: we don't need the line of code below because all of the variable types are numeric (integer or numeric)
  # select_if(is.numeric)
  # HERE IS WHERE WE SHOULD RESCALE FROM 0 TO SQRT(N)
  
  # mutate_at(vars(offpov, himcarenw, caidnw, anycovly, prvtcovnw, grpcovnw, mrkcovnw, mrkscovnw, inhcovnw, sex), list(~ case_when(
  #   . == 1 ~ sqrt(2),
  #   . == 0 ~ 0,
  #   TRUE ~ NA_real_
  # ))) 

asec_pca_2019_2020 <- recipe(~ ., asec_pca_2019_2020) %>%
  step_dummy(region, statefip, metro, metarea, metfips, ) %>%
  prep() %>%
  bake(data = NULL)
  

# SYLVIA NOTE: the code below can be deleted, because we can center and scale the variables in prcomp--we don't need a recipe!
# # create a recipe with no outcome variable and all predictors
# pca_rec <- recipe(~., data = asec_numeric_2019_2020) %>%
#   # center and scale all predictors
#   step_center(all_predictors()) %>%
#   step_scale(all_predictors()) %>%
#   # run prep to prepare recipe
#   prep()
# 
# # apply recipe to data
# employment_clust <- pca_rec %>%
#   bake(new_data = NULL)

# create a correlation matrix on employment_clust
cor(asec_pca_2019_2020)

# conduct PCA on the employment_clust data
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

asec_rec <- 
  recipe(employed ~ ., data = asec2019_2020train) %>%
  update_role(serial, cpsid, cpsidp, new_role = "ID") %>%
  step_dummy(all_nominal_predictors()) %>% # dummy encode categorical predictors 
  step_center(all_predictors()) %>% # center predictors
  step_scale(all_predictors()) %>% # scale predictors
  step_nzv(all_predictors()) %>%   # drop near zero variance predictors
  step_downsample(employed) # subsampling due to class imbalances between employment class 

# Model 1: Random forest with hypertuning no. of trees, N_estimators

# Model 2: Logistic regression 

# Model 3: K Nearest Neighbor? Decision Tree?