# Final Project
# Authors: Sylvia Brown, Pierina Forastieri, Marlyn Bruno, Julia Buschmann
 
## Objective:
This project's objective is to develop a model for predicting an individual's economic precarity as a product of the recessionary impacts of a pandemic, and could possibly be generalized to natural disasters with similar effects.  We do this by first finding the most important variables for predicting unemployment using Principal Components Analysis (PCA). We integrate these insights into the development of a random forest model and a logistic regression model using CPS data from 2019 and 2020. We then implement these models with data from 2021 to test its accuracy with appropriately determining when an individual was, indeed, unemployed and test to see if we can accurately generalize the model to a subset of the population. With a model like this, we can identify correlations between individual characteristics (e.g., income, state, type of health insurance) and unemployment so that, when a future pandemic or natural disaster occurs, policymakers will be better equipped to understand risk factors for experiencing economic hardship.

## Files in the repository:
- **Final_Project.Rmd**: This file contains the R code to carry out the analysis for the final project. Plain text and comments in the code chunks clarify what is being done in each part of the code. It was separated into chunks for readability and to clearly identify which exercise is being completed.

- **Final_Project.html**: This file contains the result of the R code after knitting the Final_Project.Rmd file.

- **.gitignore**: This file contains the files used for this code that we choose to exclude from the GitHub repository.

- **README.md**: This README.md file contains a brief overview of the objective of the code, the files included in the repository, and instructions for replicating our analysis. 


## Instructions for this Final Project:
To replicate our analysis, run this code as follows:

1. Create an .Rproject and a data folder inside that project folder.

2. Download the data .... Save all the data files in the `data` `folder.

3. Please download the required R Libraries: ** REMEMBER TO UPDATE**
- `ipumsr`
- `srvyr`
- `ggplot2`
- `haven`
- `formattable`
- `tidymodels`
- `tidytext`
- `tidyverse`
- `textrecipes`
- `ggalt`
- `labelled`
- `sjlabelled`

4. Run every code chunk in the .Rmd file. Then, optionally, knit to get an .html file to view the results of the code.

### Contact:
Contact Sylvia Brown at scb136 [at] georgetown.edu, Pierina Forastieri at pnf9 [at] georgetown.edu, Marlyn Bruno at mb2545 [at] georgetown.edu or Julia Buschmann at jrb361 [at] georgetown.edu

*This README.md was last updated on May 6th, 2022*
