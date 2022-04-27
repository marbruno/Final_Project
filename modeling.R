# Before running modeling code, run code that cleans data in final_project.Rmd

# New modeling script
# Create tibble with 2019 and 2020data for model training, testing
asec2019_2020 <- asec_allyears %>%
  filter(year == 2019 | year == 2020) %>%
  select(-starts_with("q")) # Drop quality flags
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
    step_nzv(all_predictors())
    #MARLYN; CAN ADD MORE STEPS HERE