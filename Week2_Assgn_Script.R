library(tidyverse)
library(haven)

# read in nfhs data
nfhs <- read_dta("IAHR52FL.dta")

# cuts data set down to 10% for testing purposes; comment out for prod
# nfhs <- nfhs %>%
#   sample_frac(0.1)

# nfhs_wk2 <- select(test_nfhs, hhid:hv208, hv270) # subset for quiz questions

nfhs_edu <- select(nfhs, hhid, hv002, hv003, hv108_01:hv108_35) %>%
  gather(hv108_01:hv108_35, key = "member", value = "years_of_education") %>%
  separate(member, into = c("init_code", "roster_number"), sep = "_") %>%
  select(hhid, roster_number, years_of_education) %>%
  rename(household_id = hhid) %>%
  drop_na(years_of_education)

# nfhs_fem <- select(nfhs, hhid, ha0_01:ha6_11) # full dataset of bio data for female household members

### Since gathering and separating each set of data would duplicate and triplicate total row counts,
### potentially crashing my computer, I created three separate data frames that gather and separate
### the variables, and then join everything together based on the roster_number
### It is possible to do this all in one data frame, but it would take extra effort to remove duplicates
### and remove rows that associated household members with data from other household members

# isolate age variable and tidy data frame
nfhs_fem_age <- select(nfhs, hhid, ha0_01:ha6_11) %>%
  gather(ha1_01:ha1_11, key = "key_age", value = "years") %>%
  separate(key_age, into = c("var code", "roster_number"), sep = "_") %>%
  select(hhid, roster_number, years) %>%
  drop_na(years)

# isolate weight variable and tidy data frame
nfhs_fem_weight <- select(nfhs, hhid, ha0_01:ha6_11) %>%
  gather(ha2_01:ha2_11, key = "key_weight", value = "weight_kg") %>%
  separate(key_weight, into = c("var code", "roster_number"), sep = "_") %>%
  select(hhid, roster_number, weight_kg) %>%
  drop_na(weight_kg) %>%
  # filter out "missing" data code after dropping NA
  ### Goal is to keep line but remove the potentially problematic
  ### numeral from the data as it would skew analysis in most cases
  filter(weight_kg != 9999)

# isolate height variable and tidy data frame
nfhs_fem_height <- select(nfhs, hhid, ha0_01:ha6_11) %>%
  gather(ha3_01:ha3_11, key = "key_height", value = "height_cm") %>%
  separate(key_height, into = c("var code", "roster_number"), sep = "_") %>%
  select(hhid, roster_number, height_cm) %>%
  drop_na(height_cm) %>%
  # filter out "missing" data code after dropping NA
  ### Goal is to keep line but remove the potentially problematic
  ### numeral from the data as it would skew analysis in most cases
  filter(height_cm != 9999)

# join data back together and add sex factor
nfhs_fem_biom <- nfhs_fem_age %>%
  ### keep naming conventions consistent to avoid calling out key columns
  full_join(nfhs_fem_weight) %>%
  full_join(nfhs_fem_height) %>%
  mutate(female = factor(TRUE))

# nfhs_male <- select(test_nfhs, hhid, hb0_01:hb6_18)
