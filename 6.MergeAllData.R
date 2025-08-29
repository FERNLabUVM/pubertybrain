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

#######merge in adversity data

dfmerged <- read.csv("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/Manuscripts/reg report DCN/pubertybrain/dfmerged.csv")

NLE <- read_tsv("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/ABCD_6.0/rawdata/phenotype/mh_y_ple.tsv") %>%
  filter(session_id == "ses-01A")

###customized NLE code

library(dplyr)

compute_negative_events <- function(df) {
  event_vars    <- paste0("mh_y_ple_", sprintf("%03d", 1:25))
  exp_vars      <- paste0("mh_y_ple__exp_", sprintf("%03d", 1:25))
  pastyear_vars <- paste0("mh_y_ple_", sprintf("%03d", 1:25), "__01")
  
  df %>%
    rowwise() %>%
    mutate(
      total_bad_events = {
        events <- suppressWarnings(as.numeric(unlist(across(all_of(event_vars)))))
        exps   <- suppressWarnings(as.numeric(unlist(across(all_of(exp_vars)))))
        # recode special missing values to NA
        events[events %in% c(444, 777, 999)] <- NA
        exps[exps %in% c(444, 777, 999)]     <- NA
        if(sum(is.na(events)) > 5) NA
        else sum(events == 1 & exps == 2, na.rm = TRUE)
      },
      total_bad_events_baseline = {
        events <- suppressWarnings(as.numeric(unlist(across(all_of(event_vars)))))
        exps   <- suppressWarnings(as.numeric(unlist(across(all_of(exp_vars)))))
        pastyr <- suppressWarnings(as.numeric(unlist(across(all_of(pastyear_vars)))))
        # recode special missing values to NA
        events[events %in% c(444, 777, 999)] <- NA
        exps[exps %in% c(444, 777, 999)]     <- NA
        pastyr[pastyr %in% c(444, 777, 999)] <- NA
        if(sum(is.na(events)) > 5) NA
        else sum(events == 1 & exps == 2 & pastyr == 0, na.rm = TRUE)
      }
    ) %>%
    ungroup()
}

NLE <- compute_negative_events(NLE)


NLE$total_NLE_baseline <- NLE$mh_y_ple_neg_count

####
#check 5.1

path <- '/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/FERN Lab/abcd-data-release-5.1/core'

NLE5 <- read_csv(paste(path, 'mental-health/mh_y_le.csv', sep='/'), col_names=TRUE) %>% 
  filter(eventname == "1_year_follow_up_y_arm_1") %>% 
  dplyr::select(src_subject_id, ple_y_ss_total_bad, ple_y_ss_affected_bad_mean)

table(NLE5$ple_y_ss_total_bad)

