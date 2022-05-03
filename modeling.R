# Before running modeling code, run code that cleans data in final_project.Rmd

# Loading libraries
library(tidymodels)
library(survey)

# New modeling script
# Create tibble with 2019 and 2020 data for model training, testing
# Removes variable we're trying to predict (employed)
asec_2019_2020 <- cps_svy %>%
  filter(year == 2019 | year == 2020) %>%
  filter(!is.na(employed))
  

# ------------------------------------PCA------------------------------------

# IMPUTATION: impute ind and oct?
# Select numeric variables from data set
asec_pca_2019_2020 <- asec_2019_2020 %>%
  select(-year, -serial, -cpsid, -immigrant) %>% # deselect variables we don't want to include in PCA analysis
  select(-region, -county, -metro, -metarea, -metfips) %>% # deselect all location variables other than state
  select(-yrimmig, -wksunem1, wksunem2, -whymove) %>% # deselect variables that many people have not responded to resulting in many NAs; also excluded from PCA
  select(-empstat, -labforce) %>% # deselect variables that are unuseful (labforce)
  mutate_at(vars(race, unitsstr, citizen, hispan,
            occ, ind, educ, classwly,
            strechlk, spmmort, whymove, health, paidgh), list(~ as.factor(.)))

# AARON: can we include values that were not ansewrd by everyone in the PCA and
# in the modeling and if so, how?

asec_pca_2019_2020 <- recipe(~ ., asec_pca_2019_2020) %>%
  step_dummy(race, unitsstr, citizen, hispan,
             occ, ind, educ, classwly,
             strechlk, spmmort, whymove, health, paidgh, one_hot = TRUE) %>%
  prep() %>%
  bake(asec_pca_2019_2020)
# Note: did not include in step_dummy: hhincome, age, yrimmig, existing indicator variables (sex,
# offpov, disabwrk, himcarenw, caidnw, anycovly, prvtcovnw, grpcovnw, mrkcovnw, 
# mrkscovnw, inhcovnw, schipnw), wksunem1, wksunem2, ftotval, inctto, incwelfr, 
# incunemp, ctccrd, eitcred, moop, hipval
# QUESTION FOR AARON: should we use one-hot encoding for PCA instead of step_dummy?

asec_pca_2019_2020 <- asec_pca_2019_2020 %>%
  mutate_at(
    vars(
      offpov, himcarenw, caidnw, anycovly, prvtcovnw, grpcovnw, mrkcovnw,
      mrkscovnw, inhcovnw, mrkucovnw, sex, starts_with("strechlk"), starts_with("spmmort")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(3),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("citizen"), starts_with("health")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(5),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("unitsstr")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(7),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("classwly")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(8),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("hispan")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(9),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("educ")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(16),
                      . == 0 ~ 0,
                      TRUE ~ NA_real_))
  ) %>%
  mutate_at(
    vars(
      starts_with("race")
    ),
    list( ~ case_when(. == 1 ~ 1/sqrt(26),
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
  )

asec_pca_2019_2020 <- asec_pca_2019_2020 %>%
  as_survey_design(weights = asecwtcvd)

# conduct PCA on the asec_pca_2019_2020 data
  # code below will center and scale the data
  # MARLYN/AARON: this won't run without having imputed values where the NAs are

# AARON: WAY TO DO THIS MORE ELEGANTLY?
asec_pca <-
  svyprcomp(
    ~ county + hhincome + asecwtcvd + age + sex +
      ftotval + inctot + incwelfr + incunemp + ctccrd + eitcred + offpov + disabwrk +
      himcarenw + caidnw + moop + hipval + anycovly + prvtcovnw + grpcovnw + mrkcovnw +
      mrkscovnw + mrkucovnw + inhcovnw + schipnw + employed + race_X100 + race_X200 + race_X300 +
      race_X651 + race_X652 + race_X801 + race_X802 + race_X803 + race_X804 +
      race_X805 + race_X806 + race_X807 + race_X808 + race_X809 + race_X810 +
      race_X811 + race_X812 + race_X813 + race_X814 + race_X815 + race_X816 +
      race_X817 + race_X818 + race_X819 + race_X820 + race_X830 + unitsstr_X1 + unitsstr_X5 +
      unitsstr_X6 + unitsstr_X7 + unitsstr_X11 + unitsstr_X12 + citizen_X1 + citizen_X2 + citizen_X3 +
      citizen_X4 + citizen_X5 + hispan_X100 + hispan_X200 + hispan_X300 + hispan_X400 +
      hispan_X500 + hispan_X600 + hispan_X611 + hispan_X0 + hispan_X612 + occ_X10 + occ_X20 + occ_X40 +
      occ_X50 + occ_X51 + occ_X52 + occ_X60 + occ_X100 + occ_X101 + occ_X102 +
      occ_X110 + occ_X120 + occ_X135 + occ_X136 + occ_X137 + occ_X140 + occ_X150 +
      occ_X160 + occ_X205 + occ_X220 + occ_X230 + occ_X300 + occ_X310 + occ_X330 +
      occ_X335 + occ_X340 + occ_X350 + occ_X360 + occ_X410 + occ_X420 + occ_X425 +
      occ_X430 + occ_X440 + occ_X500 + occ_X510 + occ_X520 + occ_X530 + occ_X540 +
      occ_X565 + occ_X600 + occ_X630 + occ_X640 + occ_X650 + occ_X700 + occ_X705 +
      occ_X710 + occ_X725 + occ_X726 + occ_X735 + occ_X740 + occ_X750 + occ_X800 +
      occ_X810 + occ_X820 + occ_X830 + occ_X840 + occ_X845 + occ_X850 + occ_X860 +
      occ_X900 + occ_X910 + occ_X930 + occ_X940 + occ_X950 + occ_X960 + occ_X1005 +
      occ_X1006 + occ_X1007 + occ_X1010 + occ_X1020 + occ_X1021 + occ_X1022 +
      occ_X1030 + occ_X1031 + occ_X1032 + occ_X1050 + occ_X1060 + occ_X1065 +
      occ_X1105 + occ_X1106 + occ_X1107 + occ_X1108 + occ_X1200 + occ_X1220 +
      occ_X1240 + occ_X1300 + occ_X1305 + occ_X1306 + occ_X1310 + occ_X1320 +
      occ_X1340 + occ_X1350 + occ_X1360 + occ_X1400 + occ_X1410 + occ_X1420 +
      occ_X1430 + occ_X1440 + occ_X1450 + occ_X1460 + occ_X1500 + occ_X1510 +
      occ_X1520 + occ_X1530 + occ_X1540 + occ_X1541 + occ_X1545 + occ_X1550 +
      occ_X1551 + occ_X1555 + occ_X1560 + occ_X1600 + occ_X1610 + occ_X1640 +
      occ_X1650 + occ_X1700 + occ_X1710 + occ_X1720 + occ_X1740 + occ_X1745 +
      occ_X1750 + occ_X1760 + occ_X1800 + occ_X1820 + occ_X1821 + occ_X1822 +
      occ_X1825 + occ_X1840 + occ_X1860 + occ_X1900 + occ_X1910 + occ_X1920 +
      occ_X1930 + occ_X1935 + occ_X1965 + occ_X1970 + occ_X1980 + occ_X2000 +
      occ_X2001 + occ_X2002 + occ_X2003 + occ_X2004 + occ_X2005 + occ_X2006 +
      occ_X2010 + occ_X2011 + occ_X2012 + occ_X2013 + occ_X2014 + occ_X2015 +
      occ_X2016 + occ_X2025 + occ_X2040 + occ_X2050 + occ_X2060 + occ_X2100 +
      occ_X2105 + occ_X2145 + occ_X2160 + occ_X2170 + occ_X2180 + occ_X2200 +
      occ_X2205 + occ_X2300 + occ_X2310 + occ_X2320 + occ_X2330 + occ_X2340 +
      occ_X2350 + occ_X2360 + occ_X2400 + occ_X2430 + occ_X2435 + occ_X2440 +
      occ_X2540 + occ_X2545 + occ_X2550 + occ_X2555 + occ_X2600 + occ_X2630 +
      occ_X2631 + occ_X2632 + occ_X2633 + occ_X2634 + occ_X2635 + occ_X2636 +
      occ_X2640 + occ_X2700 + occ_X2710 + occ_X2720 + occ_X2721 + occ_X2722 +
      occ_X2723 + occ_X2740 + occ_X2750 + occ_X2751 + occ_X2752 + occ_X2755 +
      occ_X2760 + occ_X2770 + occ_X2800 + occ_X2805 + occ_X2810 + occ_X2825 +
      occ_X2830 + occ_X2840 + occ_X2850 + occ_X2860 + occ_X2861 + occ_X2862 +
      occ_X2865 + occ_X2900 + occ_X2905 + occ_X2910 + occ_X2920 + occ_X3000 +
      occ_X3010 + occ_X3030 + occ_X3040 + occ_X3050 + occ_X3060 + occ_X3090 +
      occ_X3100 + occ_X3110 + occ_X3120 + occ_X3140 + occ_X3150 + occ_X3160 +
      occ_X3200 + occ_X3210 + occ_X3220 + occ_X3230 + occ_X3245 + occ_X3250 +
      occ_X3255 + occ_X3256 + occ_X3258 + occ_X3260 + occ_X3261 + occ_X3270 +
      occ_X3300 + occ_X3310 + occ_X3320 + occ_X3321 + occ_X3322 + occ_X3323 +
      occ_X3324 + occ_X3330 + occ_X3400 + occ_X3401 + occ_X3402 + occ_X3420 +
      occ_X3421 + occ_X3422 + occ_X3423 + occ_X3424 + occ_X3430 + occ_X3500 +
      occ_X3510 + occ_X3515 + occ_X3520 + occ_X3535 + occ_X3540 + occ_X3545 +
      occ_X3550 + occ_X3600 + occ_X3601 + occ_X3602 + occ_X3603 + occ_X3605 +
      occ_X3610 + occ_X3620 + occ_X3630 + occ_X3640 + occ_X3645 + occ_X3646 +
      occ_X3647 + occ_X3648 + occ_X3649 + occ_X3655 + occ_X3700 + occ_X3710 +
      occ_X3720 + occ_X3725 + occ_X3730 + occ_X3740 + occ_X3750 + occ_X3800 +
      occ_X3801 + occ_X3802 + occ_X3820 + occ_X3840 + occ_X3850 + occ_X3870 +
      occ_X3900 + occ_X3910 + occ_X3930 + occ_X3940 + occ_X3945 + occ_X3946 +
      occ_X3955 + occ_X3960 + occ_X4000 + occ_X4010 + occ_X4020 + occ_X4030 +
      occ_X4040 + occ_X4050 + occ_X4055 + occ_X4060 + occ_X4110 + occ_X4120 +
      occ_X4130 + occ_X4140 + occ_X4150 + occ_X4160 + occ_X4200 + occ_X4210 +
      occ_X4220 + occ_X4230 + occ_X4240 + occ_X4250 + occ_X4251 + occ_X4252 +
      occ_X4255 + occ_X4300 + occ_X4320 + occ_X4330 + occ_X4340 + occ_X4350 +
      occ_X4400 + occ_X4420 + occ_X4430 + occ_X4435 + occ_X4460 + occ_X4461 +
      occ_X4465 + occ_X4500 + occ_X4510 + occ_X4520 + occ_X4521 + occ_X4522 +
      occ_X4525 + occ_X4530 + occ_X4540 + occ_X4600 + occ_X4610 + occ_X4620 +
      occ_X4621 + occ_X4622 + occ_X4640 + occ_X4650 + occ_X4655 + occ_X4700 +
      occ_X4710 + occ_X4720 + occ_X4740 + occ_X4750 + occ_X4760 + occ_X4800 +
      occ_X4810 + occ_X4820 + occ_X4830 + occ_X4840 + occ_X4850 + occ_X4900 +
      occ_X4920 + occ_X4930 + occ_X4940 + occ_X4950 + occ_X4965 + occ_X5000 +
      occ_X5010 + occ_X5020 + occ_X5030 + occ_X5040 + occ_X5100 + occ_X5110 +
      occ_X5120 + occ_X5130 + occ_X5140 + occ_X5150 + occ_X5160 + occ_X5165 +
      occ_X5200 + occ_X5220 + occ_X5230 + occ_X5240 + occ_X5250 + occ_X5260 +
      occ_X5300 + occ_X5310 + occ_X5320 + occ_X5330 + occ_X5340 + occ_X5350 +
      occ_X5360 + occ_X5400 + occ_X5410 + occ_X5420 + occ_X5500 + occ_X5510 +
      occ_X5520 + occ_X5521 + occ_X5522 + occ_X5530 + occ_X5540 + occ_X5550 +
      occ_X5560 + occ_X5600 + occ_X5610 + occ_X5620 + occ_X5630 + occ_X5700 +
      occ_X5710 + occ_X5720 + occ_X5730 + occ_X5740 + occ_X5800 + occ_X5810 +
      occ_X5820 + occ_X5840 + occ_X5850 + occ_X5860 + occ_X5900 + occ_X5910 +
      occ_X5920 + occ_X5940 + occ_X6005 + occ_X6010 + occ_X6040 + occ_X6050 +
      occ_X6100 + occ_X6115 + occ_X6120 + occ_X6130 + occ_X6200 + occ_X6210 +
      occ_X6220 + occ_X6230 + occ_X6240 + occ_X6250 + occ_X6260 + occ_X6300 +
      occ_X6305 + occ_X6320 + occ_X6330 + occ_X6355 + occ_X6360 + occ_X6400 +
      occ_X6410 + occ_X6420 + occ_X6440 + occ_X6441 + occ_X6442 + occ_X6460 +
      occ_X6500 + occ_X6515 + occ_X6520 + occ_X6530 + occ_X6540 + occ_X6600 +
      occ_X6660 + occ_X6700 + occ_X6710 + occ_X6720 + occ_X6730 + occ_X6740 +
      occ_X6750 + occ_X6765 + occ_X6800 + occ_X6820 + occ_X6825 + occ_X6830 +
      occ_X6835 + occ_X6840 + occ_X6850 + occ_X6920 + occ_X6940 + occ_X6950 +
      occ_X7000 + occ_X7010 + occ_X7020 + occ_X7030 + occ_X7040 + occ_X7100 +
      occ_X7110 + occ_X7120 + occ_X7130 + occ_X7140 + occ_X7150 + occ_X7160 +
      occ_X7200 + occ_X7210 + occ_X7220 + occ_X7240 + occ_X7260 + occ_X7300 +
      occ_X7315 + occ_X7320 + occ_X7330 + occ_X7340 + occ_X7350 + occ_X7360 +
      occ_X7410 + occ_X7420 + occ_X7430 + occ_X7510 + occ_X7540 + occ_X7550 +
      occ_X7560 + occ_X7610 + occ_X7630 + occ_X7640 + occ_X7700 + occ_X7710 +
      occ_X7720 + occ_X7730 + occ_X7740 + occ_X7750 + occ_X7800 + occ_X7810 +
      occ_X7830 + occ_X7840 + occ_X7850 + occ_X7855 + occ_X7900 + occ_X7905 +
      occ_X7920 + occ_X7925 + occ_X7940 + occ_X7950 + occ_X8000 + occ_X8010 +
      occ_X8025 + occ_X8030 + occ_X8040 + occ_X8100 + occ_X8130 + occ_X8140 +
      occ_X8200 + occ_X8210 + occ_X8220 + occ_X8225 + occ_X8250 + occ_X8255 +
      occ_X8256 + occ_X8300 + occ_X8310 + occ_X8320 + occ_X8330 + occ_X8335 +
      occ_X8350 + occ_X8365 + occ_X8400 + occ_X8410 + occ_X8420 + occ_X8450 +
      occ_X8460 + occ_X8465 + occ_X8500 + occ_X8510 + occ_X8530 + occ_X8540 +
      occ_X8550 + occ_X8555 + occ_X8600 + occ_X8610 + occ_X8620 + occ_X8630 +
      occ_X8640 + occ_X8650 + occ_X8710 + occ_X8720 + occ_X8730 + occ_X8740 +
      occ_X8750 + occ_X8760 + occ_X8800 + occ_X8810 + occ_X8830 + occ_X8850 +
      occ_X8860 + occ_X8910 + occ_X8920 + occ_X8930 + occ_X8940 + occ_X8950 +
      occ_X8965 + occ_X8990 + occ_X9000 + occ_X9005 + occ_X9030 + occ_X9040 +
      occ_X9050 + occ_X9110 + occ_X9120 + occ_X9121 + occ_X9122 + occ_X9130 +
      occ_X9140 + occ_X9141 + occ_X9142 + occ_X9150 + occ_X9200 + occ_X9210 +
      occ_X9240 + occ_X9260 + occ_X9265 + occ_X9300 + occ_X9310 + occ_X9350 +
      occ_X9360 + occ_X9365 + occ_X9410 + occ_X9415 + occ_X9420 + occ_X9430 +
      occ_X9510 + occ_X9520 + occ_X9560 + occ_X9570 + occ_X9600 + occ_X9610 +
      occ_X9620 + occ_X9630 + occ_X9640 + occ_X9645 + occ_X9650 + occ_X9720 +
      occ_X9750 + occ_X9760 + occ_X9840 + ind_X170 + ind_X180 + ind_X190 + ind_X270 + ind_X280 +
      ind_X290 + ind_X370 + ind_X380 + ind_X390 + ind_X470 + ind_X490 + ind_X570 +
      ind_X580 + ind_X590 + ind_X670 + ind_X680 + ind_X690 + ind_X770 + ind_X1070 +
      ind_X1080 + ind_X1090 + ind_X1170 + ind_X1180 + ind_X1190 + ind_X1270 +
      ind_X1280 + ind_X1290 + ind_X1370 + ind_X1390 + ind_X1470 + ind_X1480 +
      ind_X1490 + ind_X1570 + ind_X1590 + ind_X1670 + ind_X1680 + ind_X1690 +
      ind_X1691 + ind_X1770 + ind_X1790 + ind_X1870 + ind_X1880 + ind_X1890 +
      ind_X1990 + ind_X2070 + ind_X2090 + ind_X2170 + ind_X2180 + ind_X2190 +
      ind_X2270 + ind_X2280 + ind_X2290 + ind_X2370 + ind_X2380 + ind_X2390 +
      ind_X2470 + ind_X2480 + ind_X2490 + ind_X2570 + ind_X2590 + ind_X2670 +
      ind_X2680 + ind_X2690 + ind_X2770 + ind_X2780 + ind_X2790 + ind_X2870 +
      ind_X2880 + ind_X2890 + ind_X2970 + ind_X2980 + ind_X2990 + ind_X3070 +
      ind_X3080 + ind_X3095 + ind_X3170 + ind_X3180 + ind_X3190 + ind_X3291 +
      ind_X3365 + ind_X3370 + ind_X3380 + ind_X3390 + ind_X3470 + ind_X3490 +
      ind_X3570 + ind_X3580 + ind_X3590 + ind_X3670 + ind_X3680 + ind_X3690 +
      ind_X3770 + ind_X3780 + ind_X3790 + ind_X3875 + ind_X3895 + ind_X3960 +
      ind_X3970 + ind_X3980 + ind_X3990 + ind_X4070 + ind_X4080 + ind_X4090 +
      ind_X4170 + ind_X4180 + ind_X4195 + ind_X4265 + ind_X4270 + ind_X4280 +
      ind_X4290 + ind_X4370 + ind_X4380 + ind_X4390 + ind_X4470 + ind_X4480 +
      ind_X4490 + ind_X4560 + ind_X4570 + ind_X4580 + ind_X4585 + ind_X4590 +
      ind_X4670 + ind_X4680 + ind_X4690 + ind_X4770 + ind_X4780 + ind_X4795 +
      ind_X4870 + ind_X4880 + ind_X4890 + ind_X4970 + ind_X4971 + ind_X4972 +
      ind_X4980 + ind_X4990 + ind_X5070 + ind_X5080 + ind_X5090 + ind_X5170 +
      ind_X5180 + ind_X5190 + ind_X5275 + ind_X5280 + ind_X5295 + ind_X5370 +
      ind_X5380 + ind_X5381 + ind_X5390 + ind_X5391 + ind_X5470 + ind_X5480 +
      ind_X5490 + ind_X5570 + ind_X5580 + ind_X5590 + ind_X5591 + ind_X5592 +
      ind_X5593 + ind_X5670 + ind_X5680 + ind_X5690 + ind_X5790 + ind_X6070 +
      ind_X6080 + ind_X6090 + ind_X6170 + ind_X6180 + ind_X6190 + ind_X6270 +
      ind_X6280 + ind_X6290 + ind_X6370 + ind_X6380 + ind_X6390 + ind_X6470 +
      ind_X6480 + ind_X6490 + ind_X6570 + ind_X6590 + ind_X6670 + ind_X6672 +
      ind_X6680 + ind_X6690 + ind_X6695 + ind_X6770 + ind_X6780 + ind_X6870 +
      ind_X6880 + ind_X6890 + ind_X6970 + ind_X6990 + ind_X6991 + ind_X6992 +
      ind_X7070 + ind_X7071 + ind_X7072 + ind_X7080 + ind_X7170 + ind_X7180 +
      ind_X7181 + ind_X7190 + ind_X7270 + ind_X7280 + ind_X7290 + ind_X7370 +
      ind_X7380 + ind_X7390 + ind_X7460 + ind_X7470 + ind_X7480 + ind_X7490 +
      ind_X7570 + ind_X7580 + ind_X7590 + ind_X7670 + ind_X7680 + ind_X7690 +
      ind_X7770 + ind_X7780 + ind_X7790 + ind_X7860 + ind_X7870 + ind_X7880 +
      ind_X7890 + ind_X7970 + ind_X7980 + ind_X7990 + ind_X8070 + ind_X8080 +
      ind_X8090 + ind_X8170 + ind_X8180 + ind_X8190 + ind_X8191 + ind_X8192 +
      ind_X8270 + ind_X8290 + ind_X8370 + ind_X8380 + ind_X8390 + ind_X8470 +
      ind_X8560 + ind_X8561 + ind_X8562 + ind_X8563 + ind_X8564 + ind_X8570 +
      ind_X8580 + ind_X8590 + ind_X8660 + ind_X8670 + ind_X8680 + ind_X8690 +
      ind_X8770 + ind_X8780 + ind_X8790 + ind_X8870 + ind_X8880 + ind_X8891 +
      ind_X8970 + ind_X8980 + ind_X8990 + ind_X9070 + ind_X9080 + ind_X9090 +
      ind_X9160 + ind_X9170 + ind_X9180 + ind_X9190 + ind_X9290 + ind_X9370 +
      ind_X9380 + ind_X9390 + ind_X9470 + ind_X9480 + ind_X9490 + ind_X9570 +
      ind_X9590 + ind_X9890 + educ_X10 + educ_X20 + educ_X30 + educ_X40 + educ_X50 +
      educ_X60 + educ_X71 + educ_X73 + educ_X81 + educ_X91 + educ_X92 + educ_X111 +
      educ_X123 + educ_X124 + educ_X125 + classwly_X13 + classwly_X14 + classwly_X22 +
      classwly_X25 +
      classwly_X27 + classwly_X28 + classwly_X29 + strechlk_X1 + strechlk_X3 + strechlk_X4 +
      spmmort_X2 + + spmmort_X1 +
      spmmort_X3 + health_X1 + health_X2 +
      health_X3 + health_X4 + health_X5 + paidgh_X10 + paidgh_X21 + paidgh_X22, design = asec_pca_2019_2020, scale. = FALSE, scores = TRUE)

# obtain summary metrics
summary(asec_pca)

# obtain loadings
asec_pca$rotation

# AARON: HOW CAN I EXTRACT PCS?
# obtain component values for each observation
pca_data <- as_tibble(asec_pca$x[,"PC1"]) %>%
  select(PC1:PC10)

# SYLVIA: NEED OT FIX THIS
# combine the pcs to the names and parties
asec_pcs <- bind_cols(
  asec2019_2020,
  asec_pcs
)

# ---------------------------------Model prep---------------------------------

# Preparing data for models
asec2019_2020 <- asec_allyears %>%
  filter(year == 2019 | year == 2020) %>%
  filter(!is.na(employed)) %>%
  mutate(employed = as.factor(employed)) %>% # Make our y variable a factor
  select(-year, -serial, -cpsid, -immigrant) %>% # deselect variables we don't want to include as predictors
  select(-region, -statefip, -metro, -metarea, -metfips, -statefip) %>% # deselect most location variables other than county
  select(-empstat, -labforce) %>% # deselect variables that are unuseful (labforce)
  mutate_at(vars(race, unitsstr, citizen, hispan,
                 occ, ind, educ, classwly,
                 strechlk, spmmort, whymove, health, paidgh), list(~ as.factor(.)))

# Save as a data frame? Try this to see if we can get split to run
asec2019_2020_df <- as_tibble(asec2019_2020)

# Set seed so that selection of training/testing data is consistent between runs
# of the code chunk
set.seed(20201020)

# Split into training and testing data
split <- initial_split(data = asec2019_2020_df, prop = 0.8, strata = employed)

asec2019_2020_train <- training(split)
asec2019_2020_test <- testing(split)

# Set up 10 v-folds
folds <- vfold_cv(data = asec2019_2020_train, v = 10)

# Convert back to survey object
asec2019_2020_train <- asec2019_2020_train %>%
  as_survey_design(weights = asecwtcvd)
asec2019_2020_test <- asec2019_2020_test %>%
  as_survey_design(weights = asecwtcvd)

# Create recipe
asec_rec <- 
  recipe(employed ~ ., data = asec2019_2020_train) %>%
  #update_role(serial, cpsid, cpsidp, new_role = "ID") %>%
  step_dummy(all_nominal_predictors()) %>% # dummy encode categorical predictors 
  step_center(all_predictors()) %>% # center predictors
  step_scale(all_predictors()) %>% # scale predictors
  step_nzv(all_predictors()) %>%   # drop near zero variance predictors
  themis::step_downsample(employed) # subsampling due to class imbalances between employment class 

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
            metrics = metric_set(roc_auc, rmse))

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

# -------------------------Model 3: KNN-----------------------------------------
# Build the model (hyperparameter tuning for no. of neighbors and weight function)
knn_mod <- nearest_neighbor(neighbors = tune(), weight_func = tune()) %>% 
    set_engine("kknn") %>% 
    set_mode("classifiation")

# Create a workflow
knn_workflow <- workflow() %>% 
    add_model(knn_mod) %>% 
    add_recipe(asec_rec)

# Define parameters to hypertune
knn_param <- 
  knn_wflow %>% 
  parameters() %>% 
  update(
    `long df` = spline_degree(c(2, 18)), 
    `lat df` = spline_degree(c(2, 18)),
    neighbors = neighbors(c(3, 50)),
    weight_func = weight_func(values = c("rectangular", "inv", "gaussian", "triangular", "optimal"))
  )

# Execute hyperparameter tuning using the set parameters and the cross_validation folds
knn_cv <- logistic_workflow %>% 
  tune_grid(resamples = folds, #maybe tune_bayesian?
            param_info = knn_param,
            metrics = metric_set(roc_auc, rmse))

# Calculate RMSE and MAE for each fold 
collect_metrics(knn_cv, summarize = FALSE) 

# Select best model based on rmse (MARLYN note: we can choose to do it based on best roc_auc?)
knn_best <- knn_cv %>%
  select_best(metric = "roc_auc")

# Finalize workflow with best model
knn_last_workflow <- knn_workflow %>%
  finalize_workflow(parameters = knn_best)

# -------------Running the best model specification on the 2021 data------------



# -------------------------Run model on immigrant data--------------------------
