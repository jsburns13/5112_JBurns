library(tidyverse)
library(haven)

# read in nfhs data
nfhs <- read_dta("IAHR52FL.dta")

# cuts data set down to 10% for testing purposes; comment out for prod
# nfhs <- nfhs %>%
#   sample_frac(0.1)

# subset for household data
nfhs_wk2 <- select(nfhs, hhid:hv208, hv270) %>%
  # rename roster_number for joins
  rename(roster_number = hv003)

# subset education data by household member
# 71 variables created in select statement
nfhs_edu <- select(nfhs, hhid, hvidx_01:hvidx_35, hv108_01:hv108_35)

### tidy education data for final joins
### This code technically functions, but in the process of un-pivoting and re-pivoting,
### it maxes out memory and ultimately aborts unless it's being run on a small
### test data set. Continue past this commented code for a working solution.

# nfhs_edu <- nfhs_edu %>%
# # gather education and roster_number into separate single columns
#   gather(hv108_01:hv108_35, key = "edu_key", value = "years_of_education") %>%
#   gather(hvidx_01:hvidx_35, key = "id_key", value = "roster_number") %>%
# # last 2 digits of initial column names are unique IDs that can be matched on
#   separate(edu_key, into = c("var_code", "edu_id"), sep = "_") %>%
#   separate(id_key, into = c("var_code2", "id_id"), sep = "_") %>%
# # T/F check identifies duplicated rows and filters them out
#   mutate(member_check = (edu_id == id_id)) %>%
#   filter(member_check == TRUE) %>%
#   select(hhid, roster_number, years_of_education) %>%
#   drop_na(years_of_education) %>%
# # need to convert roster_number to allow joins later on
#   transform(roster_number = as.numeric(roster_number))

### EXTRA CONTEXT FOR CODE; NOT REQUIRED TO UNDERSTAND CODE
### Since gathering and separating each set of data, as above, would multiply total row counts,
### potentially crashing my computer, I created separate data frames that gather and separate
### the variables, and then join everything together based on the roster_number.
### For similar reasons, I am not referencing the generic data frame above (nfhs_edu) in the piping below to
### limit the number of data frames loading in R and help prevent maxing out my memory.
### It is possible to do this all in one data frame, but it exhausts my memory and aborts the program when
### running on the full data set. It works with the 10% subset, though.

### SUBSET EDUCATION DATA

# isolate roster number and tidy data frame
nfhs_edu_id <- select(nfhs, hhid, hvidx_01:hvidx_35) %>%
  gather(hvidx_01:hvidx_35, key = "id_key", value = "roster_number") %>%
  separate(id_key, into = c("var_code", "edu_id"), sep = "_") %>%
  select(hhid, edu_id, roster_number) %>%
  drop_na(roster_number)

# isolate years of education and tidy data frame
nfhs_edu_edu <- select(nfhs, hhid, hv108_01:hv108_35) %>%
  gather(hv108_01:hv108_35, key = "edu_key", value = "years_of_education") %>%
  separate(edu_key, into = c("var_code", "edu_id"), sep = "_") %>%
  select(hhid, edu_id, years_of_education) %>%
  drop_na(years_of_education)

# join data back together
nfhs_edu <- nfhs_edu_id %>%
  full_join(nfhs_edu_edu) %>%
  select(-edu_id)

# remove intermediate data frames to keep environment tidy
rm(nfhs_edu_id, nfhs_edu_edu)

### SUBSET FEMALE DATA

# full dataset of bio data for female household members
# nfhs_fem <- select(nfhs, hhid, ha0_01:ha6_11)

# isolate roster number and tidy data frame
nfhs_fem_id <- select(nfhs, hhid, ha0_01:ha6_11) %>%
  gather(ha0_01:ha0_11, key = "key_id", value = "roster_number") %>%
  separate(key_id, into = c("var code", "female_id"), sep = "_") %>%
  select(hhid, female_id, roster_number) %>%
  drop_na(roster_number)

# isolate age variable and tidy data frame
nfhs_fem_age <- select(nfhs, hhid, ha0_01:ha6_11) %>%
  gather(ha1_01:ha1_11, key = "key_age", value = "age_years") %>%
  separate(key_age, into = c("var code", "female_id"), sep = "_") %>%
  select(hhid, female_id, age_years) %>%
  drop_na(age_years)

# isolate weight variable and tidy data frame
nfhs_fem_weight <- select(nfhs, hhid, ha0_01:ha6_11) %>%
  gather(ha2_01:ha2_11, key = "key_weight", value = "weight_kg") %>%
  separate(key_weight, into = c("var code", "female_id"), sep = "_") %>%
  select(hhid, female_id, weight_kg) %>%
  drop_na(weight_kg) %>%
# filter out "missing" data code after dropping NA
### Goal is to keep line but remove the potentially problematic
### numeral from the data as it would skew analysis in most cases
  filter(weight_kg != 9999)

# isolate height variable and tidy data frame
nfhs_fem_height <- select(nfhs, hhid, ha0_01:ha6_11) %>%
  gather(ha3_01:ha3_11, key = "key_height", value = "height_cm") %>%
  separate(key_height, into = c("var code", "female_id"), sep = "_") %>%
  select(hhid, female_id, height_cm) %>%
  drop_na(height_cm) %>%
# filter out "missing" data code after dropping NA
### Goal is to keep line but remove the potentially problematic
### numeral from the data as it would skew analysis in most cases
  filter(height_cm != 9999)

# join data back together and add female true/false factor
nfhs_fem_biom <- nfhs_fem_id %>%
  ### keep naming conventions consistent to avoid calling out key columns
  full_join(nfhs_fem_age) %>%
  full_join(nfhs_fem_weight) %>%
  full_join(nfhs_fem_height) %>%
  mutate(female = factor(TRUE)) %>%
# remove intermediate id column
  select(-female_id)

# remove intermediate data frames to keep environment tidy
rm(nfhs_fem_age, nfhs_fem_height, nfhs_fem_weight, nfhs_fem_id)

### SUBSET MALE DATA

### full dataset of bio date for male household members
# nfhs_male <- select(nfhs, hhid, hb0_01:hb6_18)

# isolate roster number and tidy data frame
nfhs_male_id <- select(nfhs, hhid, hb0_01:hb6_18) %>%
  gather(hb0_01:hb0_18, key = "key_id", value = "roster_number") %>%
  separate(key_id, into = c("var code", "male_id"), sep = "_") %>%
  select(hhid, male_id, roster_number) %>%
  drop_na(roster_number)

# isolate age and tidy data frame
nfhs_male_age <- select(nfhs, hhid, hb0_01:hb6_18) %>%
  gather(hb1_01:hb1_18, key = "key_age", value = "age_years") %>%
  separate(key_age, into = c("var code", "male_id"), sep = "_") %>%
  select(hhid, male_id, age_years) %>%
  drop_na(age_years)

# isolate weight and tidy data frame
nfhs_male_weight <- select(nfhs, hhid, hb0_01:hb6_18) %>%
  gather(hb2_02:hb2_18, key = "key_weight", value = "weight_kg") %>%
  separate(key_weight, into = c("var code", "male_id"), sep = "_") %>%
  select(hhid, male_id, weight_kg) %>%
  drop_na(weight_kg) %>%
# filter out "missing" data code after dropping NA
### Goal is to keep line but remove the potentially problematic
### numeral from the data as it would skew analysis in most cases
  filter(weight_kg != 9999)

# isolate height and tidy data frame
nfhs_male_height <- select(nfhs, hhid, hb0_01:hb6_18) %>%
  gather(hb3_02:hb3_18, key = "key_height", value = "height_cm") %>%
  separate(key_height, into = c("var code", "male_id"), sep = "_") %>%
  select(hhid, male_id, height_cm) %>%
  drop_na(height_cm) %>%
# filter out "missing" data code after dropping NA
### Goal is to keep line but remove the potentially problematic
### numeral from the data as it would skew analysis in most cases
  filter(height_cm != 9999)

# join data back together and add female true/false factor
nfhs_male_biom <- nfhs_male_id %>%
  ### keep naming conventions consistent to avoid calling out key columns
  full_join(nfhs_male_age) %>%
  full_join(nfhs_male_weight) %>%
  full_join(nfhs_male_height) %>%
  mutate(female = factor(FALSE)) %>%
# remove intermediate id column  
  select(-male_id)

# remove intermediate data frames to keep environment tidy
rm(nfhs_male_age, nfhs_male_height, nfhs_male_weight, nfhs_male_id)

### COMBINE DATA TO CREATE FINAL, TIDY TABLE

# merge female and male data sets first
nfhs_indiv <- nfhs_male_biom %>%
  full_join(nfhs_fem_biom) %>%
# add in subset of household data
  left_join(nfhs_wk2) %>%
# add in education data
  left_join(nfhs_edu)

# remove intermediate data frames to keep environment tidy
rm(nfhs_edu, nfhs_fem_biom, nfhs_male_biom, nfhs_wk2)

as_tibble(nfhs_indiv) %>%
  group_by(female) %>%
  summarise(med_age = median(age_years))
