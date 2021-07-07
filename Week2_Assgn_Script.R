library(tidyverse)
library(haven)

#read in nfhs data
nfhs <- read_dta("IAHR52FL(2).dta")

view(select(nfhs, hv107))

nfhs_wk2 <- select(nfhs, hhid:hv208, hv270)

nfhs_edu <- select(nfhs, hhid, hv002, hv003, hv108_01:hv109_01, -hv109_01)

nfhs_fem <- select(nfhs, hhid, ha0_01:ha6_11)

nfhs_male <- select(nfhs, hhid, hb0_01:hb6_18)

as_tibble(nfhs_edu) %>%
  gather(hv108_01:hv108_35, key = "member", value = "year of education") %>%
  separate(member, into = c("init_code", "roster number"), sep = "_")
