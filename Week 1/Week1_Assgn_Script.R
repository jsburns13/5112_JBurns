library(tidyverse)
library(haven)

#read in nfhs data
nfhs <- read_dta("IAHR52FL.dta")

#reduce variables in data set, rename variables, convert urban check to a factor
nfhs_sub <- select(nfhs, hhid:shstruc) %>%
  rename(household_size = hv009) %>%
  mutate(across(hv025, as.factor)) %>%
  rename(type_of_residence = hv025) %>%
  rename(urban_residence_type = hv026)

#add in urban and rural strings instead of 1/2 for type_of_residence
levels(nfhs_sub$type_of_residence) <- c(levels(nfhs_sub$type_of_residence), "urban", "rural")
nfhs_sub$type_of_residence[nfhs_sub$type_of_residence==1] <- "urban"
nfhs_sub$type_of_residence[nfhs_sub$type_of_residence==2] <- "rural"

#histogram of household size
ggplot(data = nfhs_sub) +
  geom_histogram(mapping = aes(x = household_size), binwidth = 1)

#boxplot of household size, categorized by urban/rural
ggplot (data = nfhs_sub) +
  geom_boxplot(mapping = aes(y = household_size, x = type_of_residence))

#summarizes household size by urban area (city, town, etc)
nfhs_sub %>%
  group_by(urban_residence_type) %>%
  summarise(hh_size_mean = mean(household_size), hh_size_median = median(household_size))
