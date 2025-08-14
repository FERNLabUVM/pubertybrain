# Here, we import all brain MRI data and run long.combat as a harmonization technique for dealing with multiple scanner sites (for three time points)

# Set working directory
setwd("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/ABCD_6.0/rawdata/phenotype")

# Path output
path_output = ("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/Manuscripts/reg report DCN/pubertybrain")

# Load longCombat libraries

install.packages("remotes")
remotes::install_github("jcbeer/longCombat")

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


scanner <- read_tsv("mr_y_adm__info.tsv") # import dataset
df<-scanner[,c(1,2,6)] # remove first column
rm(scanner)

names(df)[1] <- "ID"
names(df)[2] <- "TP"
names(df)[3] <- "batch"

df$TP[df$TP=="ses-00A"] <- 1
df$TP[df$TP=="ses-02A"] <- 2 
df$TP[df$TP=="ses-04A"] <- 3
df$TP[df$TP=="ses-06A"] <- 4

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

##cortical

GMV <- read_tsv("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/ABCD_6.0/rawdata/phenotype/mr_y_smri__vol__dsk.tsv") # import mri__vol__dsk variables

# clean GMV
names(GMV)[1] <- "ID"
names(GMV)[2] <- "TP"
GMV$TP[GMV$TP=="ses-00A"] <- 1
GMV$TP[GMV$TP=="ses-02A"] <- 2 
GMV$TP[GMV$TP=="ses-04A"] <- 3
GMV$TP[GMV$TP=="ses-06A"] <- 4

table(GMV$TP)

# import MRI QC
struc_include <- read_tsv("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/ABCD_6.0/rawdata/phenotype/mr_y_qc__incl.tsv")
struc_include <- struc_include[,c(1:3)]
names(struc_include)[1] <- "ID"
names(struc_include)[2] <- "TP"
struc_include$TP[struc_include$TP=="ses-00A"] <- 1
struc_include$TP[struc_include$TP=="ses-02A"] <- 2 
struc_include$TP[struc_include$TP=="ses-04A"] <- 3
struc_include$TP[struc_include$TP=="ses-06A"] <- 4

t1.ids.pass <- struc_include %>% 
  dplyr::select(ID,mr_y_qc__incl__smri__t1_indicator) %>%  
  filter(mr_y_qc__incl__smri__t1_indicator >0) # t1 raw scans that passed QC
t1.ids.pass <- t1.ids.pass$ID 

# clean GMV
GMV <- GMV %>% 
  .[.$ID %in% t1.ids.pass,] 

##subcortical

sGMV <- read_tsv("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/ABCD_6.0/rawdata/phenotype/mr_y_smri__vol__aseg.tsv") # import variables

# clean sGMV
names(sGMV)[1] <- "ID"
names(sGMV)[2] <- "TP"
sGMV$TP[sGMV$TP=="ses-00A"] <- 1
sGMV$TP[sGMV$TP=="ses-02A"] <- 2 
sGMV$TP[sGMV$TP=="ses-04A"] <- 3
sGMV$TP[sGMV$TP=="ses-06A"] <- 4

table(sGMV$TP)

##merge cort and subcort
GMV_merged <- GMV %>%
  left_join(
    sGMV %>% distinct(ID, TP, .keep_all = TRUE),
    by = c("ID", "TP")
  )

# import sex
sex <- read_tsv("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/ABCD_6.0/rawdata/phenotype/ab_g_stc.tsv")
sex <- sex  %>%
  select(participant_id,ab_g_stc__cohort_sex)
names(sex)[1] <- "ID"
names(sex)[2] <- "sex"

# import age
age <- read_tsv("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/ABCD_6.0/rawdata/phenotype/ab_g_dyn.tsv")
age <- age  %>%
  select(participant_id,session_id,ab_g_dyn__visit_age)
names(age)[1] <- "ID"
names(age)[2] <- "TP"
names(age)[3] <- "age"
age$TP[age$TP=="ses-00A"] <- 1
age$TP[age$TP=="ses-02A"] <- 2 
age$TP[age$TP=="ses-04A"] <- 3
age$TP[age$TP=="ses-06A"] <- 4
age <- age %>% 
  filter(TP %in% c(1, 2, 3, 4))

# merge demographics
demo <- age %>%
  left_join(sex, by = "ID")

rm(age,sex,t1.ids.pass)

## merge demographics with cleaned MRI data in preparation for combat
GMVdemo <- merge(demo,GMV_merged)

# Now remove NAs and make numerical

GMVdemo <- na.omit(GMVdemo) # remove NAs
GMVdemo[5:117] = lapply(GMVdemo[5:117], FUN = function(y){as.numeric(y)}) # change all brain variables to numeric

# add batch to each dataframe
GMV_final <- merge(GMVdemo, df, by = c("ID","TP"))
GMV_final$batch <- as.factor(GMV_final$batch) # ensure factor before combat


#### Check and remove additional outliers before combat

# For GMV_final, cortical vol (column 75: mr_y_smri__vol__dsk_sum)
column_name_gmv <- colnames(GMV_final)[75]
column_data_gmv <- GMV_final[[75]]

# Boxplot visualization
plot_gmv <- ggplot(GMV_final, aes(x = "", y = !!sym(column_name_gmv))) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.1, color = "forestgreen") +
  labs(title = paste("Boxplot with Points of", column_name_gmv), y = column_name_gmv, x = "") +
  theme_minimal()
print(plot_gmv)

# Mean and SD
mean_gmv <- mean(column_data_gmv, na.rm = TRUE)
sd_gmv <- sd(column_data_gmv, na.rm = TRUE)

# Outlier bounds (±5 SD)
lower_bound_gmv <- mean_gmv - 5 * sd_gmv
upper_bound_gmv <- mean_gmv + 5 * sd_gmv

# Outlier count
outliers_gmv <- sum(column_data_gmv < lower_bound_gmv | column_data_gmv > upper_bound_gmv, na.rm = TRUE)
print(paste("Column:", column_name_gmv, "- Outliers beyond ±5 SD:", outliers_gmv))

# no outliers

#For GMV_final, subcortical vol (column 116: mr_y_smri__vol__aseg__whb_sum)
column_name_sgmv <- colnames(GMV_final)[116]
column_data_sgmv <- GMV_final[[116]]

# Boxplot visualization
plot_sgmv <- ggplot(GMV_final, aes(x = "", y = !!sym(column_name_sgmv))) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.1, color = "forestgreen") +
  labs(title = paste("Boxplot with Points of", column_name_sgmv), y = column_name_sgmv, x = "") +
  theme_minimal()
print(plot_sgmv)

# Mean and SD
mean_sgmv <- mean(column_data_sgmv, na.rm = TRUE)
sd_sgmv <- sd(column_data_sgmv, na.rm = TRUE)

# Outlier bounds (±5 SD)
lower_bound_sgmv <- mean_sgmv - 5 * sd_sgmv
upper_bound_sgmv <- mean_sgmv + 5 * sd_sgmv

# Outlier count
outliers_sgmv <- sum(column_data_sgmv < lower_bound_sgmv | column_data_sgmv > upper_bound_sgmv, na.rm = TRUE)
print(paste("Column:", column_name_sgmv, "- Outliers beyond ±5 SD:", outliers_sgmv))

# no outliers



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
