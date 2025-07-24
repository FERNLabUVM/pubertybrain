# Here, we import all brain MRI data and run long.combat as a harmonization technique for dealing with multiple scanner sites (for three time points)

# Set working directory
setwd("/path/to/release/abcd-data-release-5.1/core/")

# Path output
path_output = ("/path/to/data/folder/")

# Load longCombat libraries
library(longCombat)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(dplyr)
library(lattice)
library(hrbrthemes)
library(ggpubr)
library(viridis)
require(reshape2)
library(nlme)
library(lme4)
library(cowplot)
library(psych)


scanner <- read.csv("imaging/mri_y_adm_info.csv") # import dataset
df<-scanner[,c(1,2,6)] # remove first column
rm(scanner)

names(df)[1] <- "ID"
names(df)[2] <- "TP"
names(df)[3] <- "batch"

df$TP[df$TP=="baseline_year_1_arm_1"] <- 1
df$TP[df$TP=="2_year_follow_up_y_arm_1"] <- 2 
df$TP[df$TP=="4_year_follow_up_y_arm_1"] <- 3

#df$TP <- as.integer(df$TP)

df$batch<-gsub("HASH96a0c182","1",as.character(df$batch)) # remove site string from scanner site batch
df$batch<-gsub("HASHe3ce02d3","2",as.character(df$batch)) 
df$batch<-gsub("HASH48f7cbc3","3",as.character(df$batch)) 
df$batch<-gsub("HASH65b39280","4",as.character(df$batch)) 
df$batch<-gsub("HASHd422be27","5",as.character(df$batch)) 
df$batch<-gsub("HASHe4f6957a","6",as.character(df$batch)) 
df$batch<-gsub("HASHdb2589d4","7",as.character(df$batch)) 
df$batch<-gsub("HASH5b0cf1bb","8",as.character(df$batch)) 
df$batch<-gsub("HASH03db707f","9",as.character(df$batch)) 
df$batch<-gsub("HASH11ad4ed5","10",as.character(df$batch)) 
df$batch<-gsub("HASHfeb7e81a","11",as.character(df$batch)) 
df$batch<-gsub("HASH5ac2b20b","12",as.character(df$batch)) 
df$batch<-gsub("HASH3935c89e","13",as.character(df$batch)) 
df$batch<-gsub("HASHd7cb4c6d","14",as.character(df$batch)) 
df$batch<-gsub("HASH4b0b8b05","15",as.character(df$batch)) 
df$batch<-gsub("HASH1314a204","16",as.character(df$batch)) 
df$batch<-gsub("HASH311170b9","17",as.character(df$batch)) 
df$batch<-gsub("HASH6b4422a7","18",as.character(df$batch)) 
df$batch<-gsub("HASHc3bf3d9c","19",as.character(df$batch)) 
df$batch<-gsub("HASHc9398971","20",as.character(df$batch)) 
df$batch<-gsub("HASH4d1ed7b1","21",as.character(df$batch)) 
df$batch<-gsub("HASH5b2fcf80","22",as.character(df$batch)) 
df$batch<-gsub("HASH7911780b","23",as.character(df$batch)) 
df$batch<-gsub("HASHa3e45734","24",as.character(df$batch)) 
df$batch<-gsub("HASH69f406fa","25",as.character(df$batch)) 
df$batch<-gsub("HASHb640a1b8","26",as.character(df$batch)) 
df$batch<-gsub("000000000UM750MR","27",as.character(df$batch)) 
df$batch<-gsub("HASH4036a433","28",as.character(df$batch)) 
df$batch<-gsub("HASHe76e6d72","29",as.character(df$batch)) 
df$batch<-gsub("HASH7f91147d","30",as.character(df$batch)) 
df$batch<-gsub("HASH31ce566d","31",as.character(df$batch)) 
df$batch<-gsub("TBC_UWM","32",as.character(df$batch)) 

df$batch <- as.factor(df$batch)

### import brain measures
cortical_thickness <- read.csv("/path/to/release/abcd-data-release-5.1/core/imaging/mri_y_smr_thk_dsk.csv") # import smri_thick_cdk_ variables

# clean cortical
names(cortical_thickness)[1] <- "ID"
names(cortical_thickness)[2] <- "TP"
cortical_thickness$TP[cortical_thickness$TP=="baseline_year_1_arm_1"] <- 1
cortical_thickness$TP[cortical_thickness$TP=="2_year_follow_up_y_arm_1"] <- 2 
cortical_thickness$TP[cortical_thickness$TP=="4_year_follow_up_y_arm_1"] <- 3
cortical_thickness <- cortical_thickness %>% 
  filter(TP %in% c(1, 2, 3))


# import MRI QC
struc_include <- read.csv("/path/to/release/abcd-data-release-5.1/core/imaging/mri_y_qc_incl.csv")
struc_include <- struc_include[,c(1:3,5)]
names(struc_include)[1] <- "ID"
names(struc_include)[2] <- "TP"
struc_include$TP[struc_include$TP=="baseline_year_1_arm_1"] <- 1
struc_include$TP[struc_include$TP=="2_year_follow_up_y_arm_1"] <- 2 
struc_include$TP[struc_include$TP=="4_year_follow_up_y_arm_1"] <- 3
struc_include <- struc_include %>% 
  filter(TP %in% c(1, 2, 3))

t1.ids.pass <- struc_include %>% 
  dplyr::select(ID,imgincl_t1w_include) %>%  
  filter(imgincl_t1w_include >0) # t1 raw scans that passed QC
t1.ids.pass <- t1.ids.pass$ID 

# clean CT
cortical_thickness <- cortical_thickness %>% 
  .[.$ID %in% t1.ids.pass,] 


# remove hyper intensity measures not needed
subcortical <- subcortical[,c(1:17,19:31,33:35,38:48)]


# import sex
sex <- read.csv("/path/to/release/abcd-data-release-5.1/core/gender-identity-sexual-health/gish_p_gi.csv")
sex <- sex[,c(1,2,44)]
names(sex)[1] <- "ID"
names(sex)[2] <- "TP"
names(sex)[3] <- "sex"
sex$TP[sex$TP=="baseline_year_1_arm_1"] <- 1
sex$TP[sex$TP=="2_year_follow_up_y_arm_1"] <- 2 
sex$TP[sex$TP=="4_year_follow_up_y_arm_1"] <- 3
sex <- sex %>% 
  filter(TP %in% c(1, 2, 3))

sex <- sex %>%
  group_by(ID) %>%                  # Group data by ID
  mutate(sex = first(na.omit(sex))) # Fill missing sex values within each group

sex <- sex[sex$sex != 3, ] # removes intersex or nonbinary

# import age
age <- read.csv("/path/to/release/abcd-data-release-5.1/core/abcd-general/abcd_y_lt.csv")
age <- age[,c(1,2,9)]
names(age)[1] <- "ID"
names(age)[2] <- "TP"
names(age)[3] <- "age"
age$TP[age$TP=="baseline_year_1_arm_1"] <- 1
age$TP[age$TP=="2_year_follow_up_y_arm_1"] <- 2 
age$TP[age$TP=="4_year_follow_up_y_arm_1"] <- 3
age <- age %>% 
  filter(TP %in% c(1, 2, 3))

age <- age %>%
  mutate(age = as.numeric(age)/12) # convert from months to years 

# merge demographics
demo <- merge(age, sex, by = c("ID","TP"))

rm(age,sex,t1.ids.pass)


## merge demographics with cleaned MRI data in preperation for combat
cortical_thickness_demo <- merge(demo,cortical_thickness)

# Now remove NAs and make numerical
cortical_thickness_demo <- na.omit(cortical_thickness_demo) # remove NAs
cortical_thickness_demo[5:75] = lapply(cortical_thickness_demo[5:75], FUN = function(y){as.numeric(y)}) # change all brain variables to numeric

# add batch to each dataframe
cortical_thickness_final <- merge(cortical_thickness_demo, df, by = c("ID","TP"))
cortical_thickness_final$batch <- as.factor(cortical_thickness_final$batch) # ensure factor before combat



#### Check and remove additional outliers before combat cortical thickness

# For cortical_thickness_final (column 75: smri_thick_cdk_mean)
column_name_ct <- colnames(cortical_thickness_final)[75]
column_data_ct <- cortical_thickness_final[[75]]

# Boxplot visualization
plot_ct <- ggplot(cortical_thickness_final, aes(x = "", y = !!sym(column_name_ct))) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.1, color = "forestgreen") +
  labs(title = paste("Boxplot with Points of", column_name_ct), y = column_name_ct, x = "") +
  theme_minimal()
print(plot_ct)

# Mean and SD
mean_ct <- mean(column_data_ct, na.rm = TRUE)
sd_ct <- sd(column_data_ct, na.rm = TRUE)

# Outlier bounds (±5 SD)
lower_bound_ct <- mean_ct - 5 * sd_ct
upper_bound_ct <- mean_ct + 5 * sd_ct

# Outlier count
outliers_ct <- sum(column_data_ct < lower_bound_ct | column_data_ct > upper_bound_ct, na.rm = TRUE)
print(paste("Column:", column_name_ct, "- Outliers beyond ±5 SD:", outliers_ct))

# Replace outliers with NA
cortical_thickness_final[[75]] <- ifelse(column_data_ct < lower_bound_ct | column_data_ct > upper_bound_ct, NA, column_data_ct)
cortical_thickness_final <- na.omit(cortical_thickness_final)


# For surface_area_final (column 75: smri_area_cdk_total)
column_name_sa <- colnames(surface_area_final)[75]
column_data_sa <- surface_area_final[[75]]

# Boxplot visualization
plot_sa <- ggplot(surface_area_final, aes(x = "", y = !!sym(column_name_sa))) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.1, color = "forestgreen") +
  labs(title = paste("Boxplot with Points of", column_name_sa), y = column_name_sa, x = "") +
  theme_minimal()
print(plot_sa)

# Mean and SD
mean_sa <- mean(column_data_sa, na.rm = TRUE)
sd_sa <- sd(column_data_sa, na.rm = TRUE)

# Outlier bounds (±5 SD)
lower_bound_sa <- mean_sa - 5 * sd_sa
upper_bound_sa <- mean_sa + 5 * sd_sa

# Outlier count
outliers_sa <- sum(column_data_sa < lower_bound_sa | column_data_sa > upper_bound_sa, na.rm = TRUE)
print(paste("Column:", column_name_sa, "- Outliers beyond ±5 SD:", outliers_sa))

# Replace outliers with NA
surface_area_final[[75]] <- ifelse(column_data_sa < lower_bound_sa | column_data_sa > upper_bound_sa, NA, column_data_sa)
surface_area_final <- na.omit(surface_area_final)



##################################################################################################################
##################################################################################################################
###########################################     COMBAT      ######################################################
##################################################################################################################
##################################################################################################################


#install.packages("devtools")
devtools::install_github("jcbeer/longCombat", force = TRUE)

library(longCombat)

#################################
# longCombat functions:
#################################
# batchTimeViz() -- visualize change in batch (read scanner) over time
# batchBoxplot() -- to visualize residuals across batches
# trajPlot() -- visualize trajectories
# addTest() -- test for additive scanner effects
# multTest() -- test for multiplicative scanner effects
# longCombat() -- apply longitudinal ComBat


brainname <- c( "smri_thick_cdk_banksstslh",  "smri_thick_cdk_cdacatelh",   "smri_thick_cdk_cdmdfrlh",    "smri_thick_cdk_cuneuslh",   
                "smri_thick_cdk_ehinallh",    "smri_thick_cdk_fusiformlh", "smri_thick_cdk_ifpllh",      "smri_thick_cdk_iftmlh",     
                "smri_thick_cdk_ihcatelh",    "smri_thick_cdk_locclh",    "smri_thick_cdk_lobfrlh",     "smri_thick_cdk_linguallh",  
                "smri_thick_cdk_mobfrlh",   "smri_thick_cdk_mdtmlh",   "smri_thick_cdk_parahpallh",  "smri_thick_cdk_paracnlh",   
                "smri_thick_cdk_parsopclh",  "smri_thick_cdk_parsobislh",  "smri_thick_cdk_parstgrislh", "smri_thick_cdk_pericclh",   
                "smri_thick_cdk_postcnlh",  "smri_thick_cdk_ptcatelh",   "smri_thick_cdk_precnlh",     "smri_thick_cdk_pclh",       
                "smri_thick_cdk_rracatelh",  "smri_thick_cdk_rrmdfrlh",   "smri_thick_cdk_sufrlh",      "smri_thick_cdk_supllh",     
                "smri_thick_cdk_sutmlh",   "smri_thick_cdk_smlh",   "smri_thick_cdk_frpolelh",    "smri_thick_cdk_tmpolelh",   
                "smri_thick_cdk_trvtmlh",   "smri_thick_cdk_insulalh" ,  "smri_thick_cdk_banksstsrh",  "smri_thick_cdk_cdacaterh",  
                "smri_thick_cdk_cdmdfrrh",   "smri_thick_cdk_cuneusrh",   "smri_thick_cdk_ehinalrh",    "smri_thick_cdk_fusiformrh", 
                "smri_thick_cdk_ifplrh",   "smri_thick_cdk_iftmrh",  "smri_thick_cdk_ihcaterh",    "smri_thick_cdk_loccrh",     
                "smri_thick_cdk_lobfrrh",   "smri_thick_cdk_lingualrh",   "smri_thick_cdk_mobfrrh",     "smri_thick_cdk_mdtmrh",     
                "smri_thick_cdk_parahpalrh",  "smri_thick_cdk_paracnrh",    "smri_thick_cdk_parsopcrh",   "smri_thick_cdk_parsobisrh", 
                "smri_thick_cdk_parstgrisrh", "smri_thick_cdk_periccrh",    "smri_thick_cdk_postcnrh",    "smri_thick_cdk_ptcaterh",   
                "smri_thick_cdk_precnrh",    "smri_thick_cdk_pcrh",        "smri_thick_cdk_rracaterh",   "smri_thick_cdk_rrmdfrrh",   
                "smri_thick_cdk_sufrrh",    "smri_thick_cdk_suplrh",      "smri_thick_cdk_sutmrh",      "smri_thick_cdk_smrh",       
                "smri_thick_cdk_frpolerh",    "smri_thick_cdk_tmpolerh",    "smri_thick_cdk_trvtmrh",     "smri_thick_cdk_insularh",   
                "smri_thick_cdk_meanlh",    "smri_thick_cdk_meanrh",      "smri_thick_cdk_mean") # Adds all brain measures you want harmonised
model ='age + sex + TP' # adds the formula that will be used below
random ='(1|ID)' # random effects since its longitudinal data


#################################
# Model: Cortical thickness
#################################

#################################
# longCombat() -- apply longitudinal ComBat
#################################
CT_combat <- longCombat(idvar='ID',
                        timevar='TP',
                        batchvar='batch', # should be a factor
                        features=brainname,
                        formula='age + sex + TP',
                        ranef=random,
                        data=cortical_thickness_final)

par(mfrow=c(1,2))
hist(CT_combat$gammahat)
hist(CT_combat$gammastarhat)

hist(CT_combat$delta2hat)
hist(CT_combat$delta2starhat)

# get the harmonized data
CT_harmonized <- CT_combat$data_combat
# save combat feature names
featurenames.combat <- names(CT_harmonized)[4:74]

# merge with original dataframe
CT_final <- merge(cortical_thickness_final, CT_harmonized[,c(1,2,4:74)], by=c('ID','TP'))

write.csv(CT_final, paste(path_output, 'CT_combat_harmonised.csv', sep=""), row.names = FALSE)


rm('CT_harmonized')
rm('CT_combat')


# check CT boxplot before combat
batchBoxplot(idvar='ID',
             batchvar='batch', 
             feature='smri_thick_cdk_meanlh',
             formula='age + sex + TP',
             ranef='(1|ID)',
             data=CT_final,
             colors=1:31,
             orderby='var',
             title='Mean CT before ComBat')

# check CT boxplot after combat
batchBoxplot(idvar='ID', 
             batchvar='batch', 
             feature='smri_thick_cdk_meanlh.combat', 
             formula='age + sex + TP',
             ranef='(1|ID)',
             data=CT_final,
             colors=1:31,
             orderby='var',
             title='Mean CT after combat')
