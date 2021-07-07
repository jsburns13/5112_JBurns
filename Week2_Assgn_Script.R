library(tidyverse)
library(haven)

#read in\ nfhs data
nfhs <- read_dta("IAHR52FL.dta")

test_nfhs <- nfhs %>%
  sample_frac(0.1)

nfhs_wk2 <- select(test_nfhs, hhid:hv208, hv270)

nfhs_edu <- select(test_nfhs, hhid, hv002, hv003, hv108_01:hv109_01, -hv109_01)

nfhs_fem <- select(test_nfhs, hhid, ha0_01:ha6_11)

nfhs_male <- select(test_nfhs, hhid, hb0_01:hb6_18)

nfhs_edu <- select(nfhs, hhid, hv002, hv003, hv108_01:hv108_35) %>%
  gather(hv108_01:hv108_35, key = "member", value = "years_of_education") %>%
  separate(member, into = c("init_code", "roster_number"), sep = "_") %>%
  select(hhid, roster_number, years_of_education) %>%
  rename(household_id = hhid) %>%
  drop_na(years_of_education)
