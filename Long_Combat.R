# Here, we import all brain MRI data and run long.combat as a harmonization technique for dealing with multiple scanner sites (for three time points)

# Set working directory
setwd("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/ABCD_6.0/rawdata/phenotype")

# Path output
path_output = ("/Users/ab3377/Library/CloudStorage/OneDrive-UniversityofVermont/OneDrive/Manuscripts/reg report DCN/pubertybrain/")

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

colnames(GMV_final)

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


brainname <- c("mr_y_smri__vol__dsk__bstmps__lh_sum", "mr_y_smri__vol__dsk__cac__lh_sum", "mr_y_smri__vol__dsk__cmfrt__lh_sum", 
               "mr_y_smri__vol__dsk__cn__lh_sum", "mr_y_smri__vol__dsk__er__lh_sum", "mr_y_smri__vol__dsk__ff__lh_sum", 
               "mr_y_smri__vol__dsk__ic__lh_sum", "mr_y_smri__vol__dsk__ins__lh_sum", "mr_y_smri__vol__dsk__iprt__lh_sum", 
               "mr_y_smri__vol__dsk__itmp__lh_sum", "mr_y_smri__vol__dsk__lg__lh_sum", "mr_y_smri__vol__dsk__lobfrt__lh_sum", 
               "mr_y_smri__vol__dsk__locc__lh_sum", "mr_y_smri__vol__dsk__mobfrt__lh_sum", "mr_y_smri__vol__dsk__mtmp__lh_sum", 
               "mr_y_smri__vol__dsk__pactr__lh_sum", "mr_y_smri__vol__dsk__pcc__lh_sum", "mr_y_smri__vol__dsk__pcg__lh_sum", 
               "mr_y_smri__vol__dsk__pfrt__lh_sum", "mr_y_smri__vol__dsk__ph__lh_sum", "mr_y_smri__vol__dsk__pob__lh_sum", 
               "mr_y_smri__vol__dsk__poctr__lh_sum", "mr_y_smri__vol__dsk__pop__lh_sum", "mr_y_smri__vol__dsk__prcn__lh_sum", 
               "mr_y_smri__vol__dsk__prctr__lh_sum", "mr_y_smri__vol__dsk__ptg__lh_sum", "mr_y_smri__vol__dsk__ptmp__lh_sum", 
               "mr_y_smri__vol__dsk__rac__lh_sum", "mr_y_smri__vol__dsk__rmfrt__lh_sum", "mr_y_smri__vol__dsk__sfrt__lh_sum", 
               "mr_y_smri__vol__dsk__sm__lh_sum", "mr_y_smri__vol__dsk__sprt__lh_sum", "mr_y_smri__vol__dsk__stmp__lh_sum", 
               "mr_y_smri__vol__dsk__ttmp__lh_sum", "mr_y_smri__vol__dsk__bstmps__rh_sum", "mr_y_smri__vol__dsk__cac__rh_sum", 
               "mr_y_smri__vol__dsk__cmfrt__rh_sum", "mr_y_smri__vol__dsk__cn__rh_sum", "mr_y_smri__vol__dsk__er__rh_sum", 
               "mr_y_smri__vol__dsk__ff__rh_sum", "mr_y_smri__vol__dsk__ic__rh_sum", "mr_y_smri__vol__dsk__ins__rh_sum", 
               "mr_y_smri__vol__dsk__iprt__rh_sum", "mr_y_smri__vol__dsk__itmp__rh_sum", "mr_y_smri__vol__dsk__lg__rh_sum", 
               "mr_y_smri__vol__dsk__lobfrt__rh_sum", "mr_y_smri__vol__dsk__locc__rh_sum", "mr_y_smri__vol__dsk__mobfrt__rh_sum", 
               "mr_y_smri__vol__dsk__mtmp__rh_sum", "mr_y_smri__vol__dsk__pactr__rh_sum", "mr_y_smri__vol__dsk__pcc__rh_sum", 
               "mr_y_smri__vol__dsk__pcg__rh_sum", "mr_y_smri__vol__dsk__pfrt__rh_sum", "mr_y_smri__vol__dsk__ph__rh_sum", 
               "mr_y_smri__vol__dsk__pob__rh_sum", "mr_y_smri__vol__dsk__poctr__rh_sum", "mr_y_smri__vol__dsk__pop__rh_sum", 
               "mr_y_smri__vol__dsk__prcn__rh_sum", "mr_y_smri__vol__dsk__prctr__rh_sum", "mr_y_smri__vol__dsk__ptg__rh_sum", 
               "mr_y_smri__vol__dsk__ptmp__rh_sum", "mr_y_smri__vol__dsk__rac__rh_sum", "mr_y_smri__vol__dsk__rmfrt__rh_sum", 
               "mr_y_smri__vol__dsk__sfrt__rh_sum", "mr_y_smri__vol__dsk__sm__rh_sum", "mr_y_smri__vol__dsk__sprt__rh_sum", 
               "mr_y_smri__vol__dsk__stmp__rh_sum", "mr_y_smri__vol__dsk__ttmp__rh_sum", "mr_y_smri__vol__dsk__lh_sum", 
               "mr_y_smri__vol__dsk__rh_sum", "mr_y_smri__vol__dsk_sum", "mr_y_smri__vol__aseg__ab__lh_sum", 
               "mr_y_smri__vol__aseg__ag__lh_sum", "mr_y_smri__vol__aseg__cbc__lh_sum", "mr_y_smri__vol__aseg__cbwm__lh_sum", 
               "mr_y_smri__vol__aseg__cd__lh_sum", "mr_y_smri__vol__aseg__cwm__lh_sum", "mr_y_smri__vol__aseg__hc__lh_sum", 
               "mr_y_smri__vol__aseg__ilv__lh_sum", "mr_y_smri__vol__aseg__lv__lh_sum", "mr_y_smri__vol__aseg__pl__lh_sum", 
               "mr_y_smri__vol__aseg__pt__lh_sum", "mr_y_smri__vol__aseg__th__lh_sum", "mr_y_smri__vol__aseg__vdc__lh_sum", 
               "mr_y_smri__vol__aseg__ab__rh_sum", "mr_y_smri__vol__aseg__ag__rh_sum", "mr_y_smri__vol__aseg__cbc__rh_sum", 
               "mr_y_smri__vol__aseg__cbwm__rh_sum", "mr_y_smri__vol__aseg__cd__rh_sum", "mr_y_smri__vol__aseg__cwm__rh_sum", 
               "mr_y_smri__vol__aseg__hc__rh_sum", "mr_y_smri__vol__aseg__ilv__rh_sum", "mr_y_smri__vol__aseg__lv__rh_sum", 
               "mr_y_smri__vol__aseg__pl__rh_sum", "mr_y_smri__vol__aseg__pt__rh_sum", "mr_y_smri__vol__aseg__th__rh_sum", 
               "mr_y_smri__vol__aseg__vdc__rh_sum", "mr_y_smri__vol__aseg__3rdv_sum", "mr_y_smri__vol__aseg__4thv_sum", 
               "mr_y_smri__vol__aseg__avs_sum", "mr_y_smri__vol__aseg__bs_sum", "mr_y_smri__vol__aseg__cca_sum", 
               "mr_y_smri__vol__aseg__ccam_sum", "mr_y_smri__vol__aseg__ccc_sum", "mr_y_smri__vol__aseg__ccp_sum", 
               "mr_y_smri__vol__aseg__ccpm_sum", "mr_y_smri__vol__aseg__csf_sum", "mr_y_smri__vol__aseg__icv_sum", 
               "mr_y_smri__vol__aseg__lvs_sum", "mr_y_smri__vol__aseg__scgv_sum", "mr_y_smri__vol__aseg__stv_sum", 
               "mr_y_smri__vol__aseg__whb_sum", "mr_y_smri__vol__aseg__wmh_sum") # Adds all brain measures you want harmonised

model ='age + sex + TP' # adds the formula that will be used below
random ='(1|ID)' # random effects since its longitudinal data


#################################
# Model: GMV
#################################

#################################
# longCombat() -- apply longitudinal ComBat
#################################

#one observation has batch "Orchestra SDK", longCombat needs at least 2 observations per batch to 
#harmonize variance across batch. Remove this row.

GMV_final <- GMV_final %>%
  filter(batch != "Orchestra SDK")

gmv_combat <- longCombat(idvar='ID',
                        timevar='TP',
                        batchvar='batch', # should be a factor
                        features=brainname,
                        formula='age + sex + TP',
                        ranef=random,
                        data=GMV_final)

par(mfrow=c(1,2))
hist(gmv_combat$gammahat)
hist(gmv_combat$gammastarhat)

hist(gmv_combat$delta2hat)
hist(gmv_combat$delta2starhat)

# get the harmonized data
gmv_harmonized <- gmv_combat$data_combat
# save combat feature names
featurenames.combat <- names(gmv_harmonized)[4:116]

# merge with original dataframe

GMV_final_harmonized <- merge(GMV_final, gmv_harmonized[,c(1,2,4:116)], by=c('ID','TP'))

write.csv(GMV_final_harmonized, paste(path_output, 'GMV_combat_harmonized.csv', sep=""), row.names = FALSE)


# check boxplot before combat
batchBoxplot(idvar='ID',
             batchvar='batch', 
             feature='mr_y_smri__vol__dsk_sum',
             formula='age + sex + TP',
             ranef='(1|ID)',
             data=GMV_final_harmonized,
             colors=1:31,
             orderby='var',
             title='cortical GMV before ComBat')

# check boxplot after combat
batchBoxplot(idvar='ID', 
             batchvar='batch', 
             feature='mr_y_smri__vol__dsk_sum.combat', 
             formula='age + sex + TP',
             ranef='(1|ID)',
             data=GMV_final_harmonized,
             colors=1:31,
             orderby='var',
             title='cortical GMV after combat')
