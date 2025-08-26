rm(list=ls())
sessionInfo()

#open libraries
##install.packages("sitar")
library(haven)
library(sitar)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(sitar)

#set working directory
#MAC:
#setwd("/Volumes/projects/iu/bl-pbs-chakulab/Projects/Data/ABCD/Syntax/Puberty/6.0/")

#PC:
setwd("Z:/Projects/Data/ABCD/Syntax/Puberty/PubertyGrowthModels")

#Open data
dfpub <- read_dta("allpub.dta")
dfbrain <- read.csv("~/GitHub/pubertybrain/gmv_mplus.csv")

#many-to-one join. 
dfmerged <- dfbrain %>%
  left_join(dfpub, by = "ID")

#Note that if an ID was present in dfpuberty BUT not in dfbrain, it was dropped. 
#I believe this is what we decided.
lostIDs <- setdiff(dfpub$ID, dfbrain$ID)

length(lostIDs)   #count
head(lostIDs)     #peek at first few

#dropped 5081 participants

names(dfmerged)

dfmerged <- dfmerged %>%
  mutate(across(everything(), ~ replace(., is.na(.), -999)))

write.csv(dfmerged, "~/GitHub/pubertybrain/dfmerged.csv", row.names = TRUE)

