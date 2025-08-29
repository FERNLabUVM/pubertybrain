rm(list=ls())
sessionInfo()

#open libraries
##install.packages("sitar")
library(haven)
library(dplyr)
library(tidyr)
library(tibble)

#set working directory
#MAC:
#setwd("/Volumes/projects/iu/bl-pbs-chakulab/Projects/Data/ABCD/Syntax/Puberty/6.0/")

#PC:
setwd("Z:/Projects/Data/ABCD/Syntax/Puberty/PubertyGrowthModels")

#Open all raw data from CleaningPubertyItems
dfygirls <- read_dta("Raw data/youthpubertygirls_long250818.dta")
dfyboys <- read_dta("Raw data/youthpubertyboys_long250818.dta")
dfpgirls <- read_dta("Raw data/parentpubertygirls_long250818.dta")
dfpboys <- read_dta("Raw data/parentpubertyboys_long250818.dta")

#goals for 08/18/25 analyses: quick clean of boys and girl's puberty data. Specifically, remove adrenal and gonadal scores and 
#additional variables. Check to make sure assessments are increasing over time. If any assessments decrease, set initial assessments to missing. 
#Then check and keep participants who have at least two distinct PDS values. 

#create function for enforcing non-decreasing PDS values 
make_monotone <- function(x) {
  keep <- !is.na(x)              
  repeat {
    v <- x[keep]                 
    if (length(v) <= 1) break
    dec <- diff(v) < 0
    if (!any(dec, na.rm = TRUE)) break
    first_keep <- which(keep)[1]
    keep[first_keep] <- FALSE
  }
  out <- x
  dropped <- !is.na(x) & !keep   
  out[dropped] <- NA_real_
  out
}

#
#youth-reported girl's puberty
#

names(dfygirls)
head(dfygirls)

#Select data following 08/18/25 goals
dfygirlsclean <- dfygirls %>%
  select(ID, age, yfPDSm) %>%
  mutate(
    ID      = factor(ID),
    age     = as.numeric(age),
    yfPDSm  = as.numeric(yfPDSm)
  )    %>%
  arrange(ID, age) %>%
  group_by(ID) %>%
  mutate(yfPDSm = make_monotone(yfPDSm)) %>%
  filter(sum(!is.na(yfPDSm)) >= 2, n_distinct(na.omit(yfPDSm)) >= 2) %>%
  ungroup()

#Check whether there are any decreases within ID:
dfygirlsclean %>%
  group_by(ID) %>%
  summarize(any_decrease = any(diff(yfPDSm) < 0, na.rm = TRUE)) %>%
  filter(any_decrease)

#Count of IDs retained:
dfygirlsclean %>% summarize(n_IDs = n_distinct(ID))

#
#youth-reported boys's puberty
#

names(dfyboys)
head(dfyboys)

#Select data following 08/18/25 goals
dfyboysclean <- dfyboys %>%
  select(ID, age, ymPDSm) %>%
  mutate(
    ID      = factor(ID),
    age     = as.numeric(age),
    ymPDSm  = as.numeric(ymPDSm)
  )    %>%
  arrange(ID, age) %>%
  group_by(ID) %>%
  mutate(ymPDSm = make_monotone(ymPDSm)) %>%
  filter(sum(!is.na(ymPDSm)) >= 2, n_distinct(na.omit(ymPDSm)) >= 2) %>%
  ungroup()

#Check whether there are any decreases within ID:
dfyboysclean %>%
  group_by(ID) %>%
  summarize(any_decrease = any(diff(ymPDSm) < 0, na.rm = TRUE)) %>%
  filter(any_decrease)

#Count of IDs retained:
dfyboysclean %>% summarize(n_IDs = n_distinct(ID))

# Write to SAS format
write_xpt(dfygirlsclean, "ygirlsclean.sas7bdat")
write_xpt(dfyboysclean, "yboysclean.sas7bdat")

#Save in R as well
save(dfygirlsclean, file = "ygirlsclean.RData")
save(dfyboysclean, file = "yboysclean.RData")
