library(tidyverse)
library(haven)

# read in nfhs data
nfhs <- read_dta("IAHR52FL.dta")

# cuts data set down to 10% for testing purposes; comment out for prod
nfhs <- nfhs %>%
  sample_frac(0.1)

# subset for household data
nfhs_wk2 <- select(nfhs, hhid:hv208, hv270) %>%
  # rename roster_number for joins
  rename(roster_number = hv003)

# subset education data by household member
nfhs_edu <- select(nfhs, hhid, hv003, hv108_01:hv108_35) %>%
  gather(hv108_01:hv108_35, key = "member", value = "years_of_education") %>%
  separate(member, into = c("init_code", "roster_number"), sep = "_") %>%
  select(hhid, roster_number, years_of_education) %>%
  drop_na(years_of_education) %>%
# need to convert roster_number to allow joins later on
  transform(roster_number = as.numeric(roster_number))

# full dataset of bio data for female household members
# nfhs_fem <- select(nfhs, hhid, ha0_01:ha6_11)

### EXTRA CONTEXT FOR CODE; NOT REQUIRED TO UNDERSTAND CODE
### Since gathering and separating each set of data would duplicate and triplicate total row counts,
### potentially crashing my computer, I created three separate data frames that gather and separate
### the variables, and then join everything together based on the roster_number.
### For similar reasons, I am not referencing the data frame above (nfhs_fem) in the piping below to
### limit the number of data frames loading in R and help prevent maxing out my memory.
### It is possible to do this all in one data frame, but it would take extra effort to remove duplicates
### and remove rows that associated household members with data from other household members.

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
