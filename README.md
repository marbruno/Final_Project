# Final Project
# Authors: Sylvia Brown, Pierina Forastieri, Marlyn Bruno, Julia Buschmann

### Pages [here](https://sbrown5x.github.io/Final_Project/)
 
## Objective:
This project's objective is to develop a model for predicting an individual's economic precarity as a product of the recessionary impacts of a pandemic, which could possibly be generalized to natural disasters with similar effects. We identify the variables to include as predictors in our predictive model with Principal Components Analysis (PCA) to generate uncorrelated components that capture a portion of the variance within these variables. Using both our original predictors and the resulting PCs, we generate a random forest model and a logistic regression model trained on ASEC data from 2019 and 2020 to predict whether an individual was employed. We then implement the best model (selected by identifying the model with the highest area under the ROC curve) on data from 2021 to test its accuracy in predicting whether an individual was employed in the second year of the pandemic. We also test the model on a subset of the population (i.e., immigrants) to see if the model, which was developed with both immigrants and non-immigrants, is generalizable to only immigrants. With a model like this, we can predict the employment status for future individuals for whom we have data on their characteristics (e.g., income, state, type of health insurance) so that when a future pandemic or natural disaster happens, policymakers will be better equipped to understand who to target support towards.

## Files in the repository:
- **Final_Project.Rmd**: This file contains the R code to carry out the analysis for the final project. Plain text and comments in the code chunks clarify what is being done in each part of the code. It was separated into chunks for readability and to clearly identify which exercise is being completed.

- **Final_Project.html**: This file contains the result of the R code after knitting the Final_Project.Rmd file.

- **modeling.R**: This file contains specific archived code for building and running our machine learning models. All code was eventually transferred to our Final_Project.Rmd file and finalized there.

- **README.md**: This README.md file contains a brief overview of the objective of the code, the files included in the repository, and instructions for replicating our analysis. 

- **gitignore**: This file contains the files used for this code that we choose to exclude from the GitHub repository.

- **doc/**: This directory includes the files used to create the Pages website.

- **\_sites.yml**: This text file includes details of how the .html files are compiled in Pages to create a website.


## Instructions for this Final Project:
To replicate our analysis, run this code as follows:

1. Create an .Rproject and a data folder inside that project folder.

2. Download the following varibales from IPUMS CPS (https://cps.ipums.org/cps-action/variables/group) and save all the data files in the `data` `folder:
YEAR, SERIAL, MONTH, CPSID, ASECFLAG, ASECWTH, REGION, STATEFIP, METRO, METAREA, COUNTY, STATECENSUS, METFIPS, HHINCOME, UNITSSTR, PERNUM, CPSIDP, ASECWT, ASECWTCVD, AGE, SEX, RACE, YRIMMIG, CITIZEN, HISPAN, EMPSTAT, LABFORCE, OCC, IND, EDUC, CLASSWLY, WKSUNEM, WKSUNEM2, STRECHLK, FTOTVAL, INCTOT, INCWELFR, INCUNEMP, CTCCRD, EITCRED, OFFPOV, SPMMORT, WHYMOVE, DISABWRK, HEALTH, PAIDGH, HIMCARENW, CAIDNW, MOOP, HIPVAL, ANYCOVLY, PRVTCOVNW, GRPCOVN, MRKCOVNW, MRKSCOVNW, MRKUCOVNW, INHCOVNW, SCHIPNW.

3. Please download the required R Libraries:
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
- `sf`
- `tigris`

4. Run every code chunk in the .Rmd file. Then, optionally, knit to get an .html file to view the results of the code.

### Contact:
Contact Sylvia Brown at scb136 [at] georgetown.edu, Pierina Forastieri at pnf9 [at] georgetown.edu, Marlyn Bruno at mb2545 [at] georgetown.edu or Julia Buschmann at jrb361 [at] georgetown.edu

*This README.md was last updated on May 6th, 2022*
