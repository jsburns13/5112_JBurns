library(tidyverse)
library(haven)

nfhs <- read_dta("IAHR52FL.dta")

nfhs_sub <- select(nfhs, hhid:shstruc)
