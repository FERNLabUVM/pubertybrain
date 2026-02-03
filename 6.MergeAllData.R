rm(list=ls())
sessionInfo()

####setup####

#open libraries
require("haven")
require(sitar)
require(dplyr)
require(tidyr)
require(purrr)
require(tibble)
require(sitar)

#set working directory

#Natasha:
setwd("C:/Users/nchaku/Documents/GitHub/pubertybrain")
#Alexis:
#setwd("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/Manuscripts/reg report DCN/pubertybrain/")

####merging puberty, brain, ELA, and working memory data files#####

#Open data
dfpub <- read_dta("Puberty_timing_tempo_models/Final data/PDS_251230.dta")
dfbrain <- read.csv("GMV_timing_tempo_models/gmv_mplus.csv")
wmem <- read_dta("Working_memory/WRKMEM_251230.dta")

#transform wmem to wide format

wmem <- wmem %>%
  filter(year %in% c(1, 4)) %>%
  pivot_wider(
    id_cols = ID,
    names_from = year,
    values_from = WRKMEM
  ) %>%
  rename(
    wmb = `1`,
    wm6 = `4`)

sum(!is.na(wmem$wm6))
# n=4888 who have year 6 data (others missing or not included in this data release)

#######prep adversity data##########

######ELA plus composite (from 5.1 data release)###########

ELA <- read.csv("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/Manuscripts/reg report DCN/pubertybrain/ela_plus_abcd.csv") %>%
  filter(eventname == "baseline_year_1_arm_1")

hist(ELA$ela_plus)
table(ELA$ela_plus)

####standardize IDs and merge across 5.1 and 6.0

#Clean IDs in 5.1
ELA <- ELA %>%
  mutate(ID = paste0("sub-", sub("NDAR_INV", "", src_subject_id))) %>%
  select(ID, everything()) %>%
  select(-starts_with("X"))

#subset ELA data
ELA <- ELA %>%
  select(ID, ela_plus)

####merge all####

dfmerged <- dfbrain %>%
  left_join(dfpub, by = "ID") %>%
  left_join(wmem, by = "ID") %>%
  left_join(ELA, by = "ID")

#Note that if an ID was present in a df but NOT in dfbrain, it was dropped. 
#because if they are not in dfbrain then they were not included in the 6.0 release (or they are missing ALL imaging data)

#final n=5041

write.csv(dfmerged, "dfmerged.csv", row.names = TRUE)

#format for mplus

dfmerged_mplus <- dfmerged %>%
  select(-ID,-mpfPDS,-mpmPDS) %>%
  select(numeric_id, everything())

dfmerged_mplus <- dfmerged_mplus %>%
  mutate(across(everything(), ~ ifelse(is.na(.), -999, .)))

write.table(dfmerged_mplus,'/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/Manuscripts/reg report DCN/pubertybrain/dfmerged_mplus.dat', row.names = FALSE, col.names = FALSE)

colnames(dfmerged_mplus)

#########Old negative life events variable#################
##note: did not end up using this variable due to missingness in 6.0 release.
##ELA score derived from Breslin et al was used in final models

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

save.image(file = "/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/Manuscripts/reg report DCN/pubertybrain/my_R_environment.RData")

