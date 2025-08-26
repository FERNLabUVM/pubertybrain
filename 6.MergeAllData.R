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
load("ygirlsclean.RData")
names(dfygirlsclean)
head(dfygirlsclean)

load("yboysclean.RData")
names(dfyboysclean)
head(dfyboysclean)

#Following https://osf.io/qfr36
#See: Elhakeem et al (2024). Evaluation and comparison of nine growth and development-based 
#measures of pubertal timing. Communications Medicine, 4(1), 159.

#Notes: In all models, age was modeled as log(age) and the models were fitted separately 
#for females and males. Each SITAR model included two random effects: timing and intensity. 
#The individual level timing and intensity values were used as the measures of pubertal timing 
#and pubertal tempo, respectively. 

#Within the SITAR framework, timing is expressed as the age at peak velocity (APV or the point of 
#peak pubertal growth). Timing is geometrically expressed as left to right departures of the 
#individual growth curves from the mean growth curve. Thus, negative values indicate earlier 
#pubertal timing, while positive values reflect later pubertal timing compared to the average. 

#The second random effect modeled in this study was intensity, expressed by SITAR as peak 
#velocity (PV), i.e., the intensity reflects an individual’s growth rate compared to the 
#average growth rate and is measured as a fraction or as percentage. Geometrically, intensity 
#corresponds to stretching or shrinking of the age scale (which makes each individual growth 
#curve steeper or flatter than the average). In the context of this study, negative values reflect 
#slower pubertal growth/tempo while positive values reflect faster growth.

#We performed model comparisons. Specifically, different models were fitted with 2 to 7 knots 
#(which determines the wiggliness of the curve) in the mean growth curve and included up to two 
#random effects (for timing and intensity). The best fitting model was chosen based on the 
#lowest Bayesian Information Criteria (BIC) value. 

#We then extracted the individual-level APV (timing) and PV (intensity/tempo) values. 
#We note that some models which included both random factors (timing and intensity/tempo) 
#failed to converge. In such cases, we only included timing and intensity/tempo was thus not 
#available. 

#Goals for 8/18/25: Test models with 3-5 knots using two random effects (b+c) corresponding to timing and intensity. 
#If the model doesn't converge, try just b or yank it

#Setting the iterations for everyone
ctrl <- nlme::nlmeControl(maxIter = 150, 
                          pnlsMaxIter = 40, 
                          msMaxIter = 200)

#
# youth-reported girl's models (trying 3-5 knots)
#

#3 knots
gmod3 <- sitar(x = log(age), 
               y = yfPDSm, 
               id = ID, 
               data = dfygirlsclean, 
               random = "b+c",
               df = 3, 
               na.action = na.exclude,
               control = ctrl)

gmod3$data <- subset(
  gmod3$data,
  !is.na(yfPDSm) & !is.na(age) & is.finite(age) & age > 0
)

par(mar = c(4,4,1,1) + 0.1, cex = 0.8)
mplot(x = age, y = yfPDSm, id = ID, data = dfygirlsclean, col = ID, las = 1)
plot(gmod3, opt = 'a', col = ID, las = 1, xlim = xaxsd(), ylim = yaxsd())

par(mar = c(4,4,1,1) + 0.1, cex = 0.8)
plot(gmod3, opt = 'd', las = 1, apv = TRUE)
plot(gmod3, opt = 'v', las = 1, apv = TRUE, lty = 2)

#4 knots
gmod4 <- sitar(x = log(age), 
               y = yfPDSm, 
               id = ID, 
               data = dfygirlsclean, 
               random = "b+c",
               df = 4, 
               na.action = na.exclude,
               control = ctrl)

#extra code to restrict the dataset and increase tolerance
dfygirlscleanrstrct <- dfygirlsclean %>%
  group_by(ID) %>%
  mutate(n_id = sum(!is.na(yfPDSm))) %>%
  ungroup() %>%
  filter(n_id >= 3)   

ctrlrstrct <- nlme::nlmeControl(
  maxIter = 200,
  pnlsMaxIter = 60,
  msMaxIter = 400,
  tolerance = 1e-6,
  pnlsTol = 1e-6,
  msVerbose = TRUE,
  returnObject = TRUE
)

gmod4 <- sitar(x = log(age), 
               y = yfPDSm, 
               id = ID, 
               data = dfygirlscleanrstrct, 
               random = "b+c",
               df = 4, 
               na.action = na.exclude,
               control = ctrlrstrct)

#mod4 didn't run with two random effects, even after increasing tolerance (ctrlrstrct) and restricting 
#the sample to those with three time points (dfygirlscleanrstrct)

#5 knots
gmod5 <- sitar(x = log(age), 
               y = yfPDSm, 
               id = ID, 
               data = dfygirlsclean, 
               random = "b+c",
               df = 5, 
               na.action = na.exclude,
               control = ctrl)

gmod5 <- sitar(x = log(age), 
               y = yfPDSm, 
               id = ID, 
               data = dfygirlscleanrstrct, 
               random = "b+c",
               df = 5, 
               na.action = na.exclude,
               control = ctrlrstrct)

#mod5 didn't run with two random effects, even after increasing tolerance (ctrlrstrct) and restricting 
#the sample to those with three time points (dfygirlscleanrstrct)

#Check fits of converging models
BIC(gmod3) #lowest BIC!
#BIC(gmod4) #did not run
#BIC(gmod5) #did not run

#
# youth-reported boy's models (trying 3-5 knots)
#

#3 knots
bmod3 <- sitar(x = log(age), 
               y = ymPDSm, 
               id = ID, 
               data = dfyboysclean, 
               random = "b+c",
               df = 3, 
               na.action = na.exclude,
               control = ctrl)

dfyboyscleanrstrct <- dfyboysclean %>%
  group_by(ID) %>%
  mutate(n_id = sum(!is.na(ymPDSm))) %>%
  ungroup() %>%
  filter(n_id >= 3) 

bmod3 <- sitar(x = log(age), 
               y = ymPDSm, 
               id = ID, 
               data = dfyboyscleanrstrct, 
               random = "b+c",
               df = 3, 
               na.action = na.exclude,
               control = ctrlrstrct)

#mod3 didn't run with two random effects

#4 knots
bmod4 <- sitar(x = log(age), 
               y = ymPDSm, 
               id = ID, 
               data = dfyboysclean, 
               random = "b+c",
               df = 4, 
               na.action = na.exclude,
               control = ctrl)

bmod4 <- sitar(x = log(age), 
               y = ymPDSm, 
               id = ID, 
               data = dfyboyscleanrstrct, 
               random = "b+c",
               df = 4, 
               na.action = na.exclude,
               control = ctrlrstrct)

bmod4$data <- subset(
  bmod4$data,
  !is.na(ymPDSm) & !is.na(age) & is.finite(age) & age > 0
)

par(mar = c(4,4,1,1) + 0.1, cex = 0.8)
mplot(x = age, y = ymPDSm, id = ID, data = dfyboyscleanrstrct, col = ID, las = 1)
plot(bmod4, opt = 'a', col = ID, las = 1, xlim = xaxsd(), ylim = yaxsd())

par(mar = c(4,4,1,1) + 0.1, cex = 0.8)
plot(bmod4, opt = 'd', las = 1, apv = TRUE)
plot(bmod4, opt = 'v', las = 1, apv = TRUE, lty = 2)

#5 knots
bmod5 <- sitar(x = log(age), 
               y = ymPDSm, 
               id = ID, 
               data = dfyboysclean, 
               random = "b+c",
               df = 5, 
               na.action = na.exclude,
               control = ctrl)

bmod5 <- sitar(x = log(age), 
               y = ymPDSm, 
               id = ID, 
               data = dfyboyscleanrstrct, 
               random = "b+c",
               df = 5, 
               na.action = na.exclude,
               control = ctrlrstrct)

#mod5 didn't run with two random effects

#Check fits of converging models
#BIC(bmod3) #did not run!
BIC(bmod4) #
#BIC(bmod5) #did not run!

#
#Extract individual-level girl's estimates 
#

library(dplyr)
library(tibble)

#Extract estimates from model
re_tbl <- ranef(gmod3) %>% 
  as_tibble(rownames = "ID")   # columns: ID, b, c
head(re_tbl)

#Make sure model$data matches rows used in fit
gmod3_fix <- gmod3
idx <- with(gmod3_fix$data, !is.na(age) & is.finite(age) & age > 0 & !is.na(yfPDSm))
gmod3_fix$data <- gmod3_fix$data[idx, , drop = FALSE]

#Get population anchors from the mean curve for the IDs plot() can handle
vout <- plot(gmod3_fix, "V", apv = TRUE)
apv_df <- as.data.frame(vout$apv, stringsAsFactors = FALSE)
apv_df <- data.frame(lapply(apv_df, \(z) if (is.list(z)) unlist(z) else z),
                     row.names = rownames(apv_df), check.names = FALSE)
if (!"ID" %in% names(apv_df)) apv_df <- tibble::rownames_to_column(apv_df, "ID")
names(apv_df)[tolower(names(apv_df))=="apv"] <- "APV_years"
names(apv_df)[tolower(names(apv_df))=="pv"]  <- "PV"

#Get fixed-effect means for timing/tempo
fe <- tryCatch(fixef(gmod3), error = function(e) setNames(numeric(0), character(0)))
b0 <- if ("b" %in% names(fe)) unname(fe["b"]) else 0
c0 <- if ("c" %in% names(fe)) unname(fe["c"]) else 0

# 4) Estimate u0 (aligned-scale APV) and PV_pop robustly from overlapping IDs
anchors <- re_tbl %>%
  inner_join(apv_df %>% select(ID, APV_years, PV), by = "ID") %>%
  mutate(
    x_i   = log(as.numeric(APV_years)),
    bTot  = b0 + b,
    cTot  = c0 + c,
    u_i   = (x_i - bTot) * exp(-cTot),     # should be ~constant = u0
    PVpop_i = as.numeric(PV) * exp(cTot)   # should be ~constant = PV_pop
  )

u0_hat    <- median(anchors$u_i,    na.rm = TRUE)
PV_popHat <- median(anchors$PVpop_i, na.rm = TRUE)

#Compute APV and PV for EVERYONE with BLUPs
girls <- re_tbl %>%
  mutate(
    bTot = b0 + b,
    cTot = c0 + c,
    APV_years_fromRE = exp( bTot + u0_hat * exp(cTot) ),
    pv_fromRE        = PV_popHat * exp(-cTot)
  ) %>%
  left_join(apv_df %>% transmute(ID, APV_years = as.numeric(APV_years),
                                 PV = as.numeric(PV)), by = "ID") %>%
  mutate(
    APV_years = coalesce(APV_years, APV_years_fromRE),
    pv        = coalesce(PV,        pv_fromRE)
  ) %>%
  select(ID, b, c, APV_years, pv)

#Sanity check & flag implausible ages (adjust bounds if needed)
girls <- girls %>%
  mutate(APV_flag = case_when(
    is.na(APV_years)        ~ "missing",
    APV_years < 8           ~ "low",
    APV_years > 20          ~ "high",
    TRUE                    ~ "ok"
  ))

summary(girls$APV_years)
table(girls$APV_flag) #We can describe these; it's people with really wonky data or flat slopes. I think we drop?
head(girls)

write_dta(girls, "ygirlsSITARmod3.dta")

#
#Extract individual-level boy's estimates 
#

library(dplyr)
library(tibble)

#Extract estimates from model
re_tbl <- ranef(bmod4) %>% 
  as_tibble(rownames = "ID")   # columns: ID, b, c
head(re_tbl)

#Make sure model$data matches rows used in fit
bmod4_fix <- bmod4
idx <- with(bmod4_fix$data, !is.na(age) & is.finite(age) & age > 0 & !is.na(ymPDSm))
bmod4_fix$data <- bmod4_fix$data[idx, , drop = FALSE]

#Get population anchors from the mean curve for the IDs plot() can handle
vout <- plot(bmod4_fix, "V", apv = TRUE)
apv_df <- as.data.frame(vout$apv, stringsAsFactors = FALSE)
apv_df <- data.frame(lapply(apv_df, \(z) if (is.list(z)) unlist(z) else z),
                     row.names = rownames(apv_df), check.names = FALSE)
if (!"ID" %in% names(apv_df)) apv_df <- tibble::rownames_to_column(apv_df, "ID")
names(apv_df)[tolower(names(apv_df))=="apv"] <- "APV_years"
names(apv_df)[tolower(names(apv_df))=="pv"]  <- "PV"

#Get fixed-effect means for timing/tempo
fe <- tryCatch(fixef(gmod3), error = function(e) setNames(numeric(0), character(0)))
b0 <- if ("b" %in% names(fe)) unname(fe["b"]) else 0
c0 <- if ("c" %in% names(fe)) unname(fe["c"]) else 0

# 4) Estimate u0 (aligned-scale APV) and PV_pop robustly from overlapping IDs
anchors <- re_tbl %>%
  inner_join(apv_df %>% select(ID, APV_years, PV), by = "ID") %>%
  mutate(
    x_i   = log(as.numeric(APV_years)),
    bTot  = b0 + b,
    cTot  = c0 + c,
    u_i   = (x_i - bTot) * exp(-cTot),     # should be ~constant = u0
    PVpop_i = as.numeric(PV) * exp(cTot)   # should be ~constant = PV_pop
  )

u0_hat    <- median(anchors$u_i,    na.rm = TRUE)
PV_popHat <- median(anchors$PVpop_i, na.rm = TRUE)

#Compute APV and PV for EVERYONE with BLUPs
boys <- re_tbl %>%
  mutate(
    bTot = b0 + b,
    cTot = c0 + c,
    APV_years_fromRE = exp( bTot + u0_hat * exp(cTot) ),
    pv_fromRE        = PV_popHat * exp(-cTot)
  ) %>%
  left_join(apv_df %>% transmute(ID, APV_years = as.numeric(APV_years),
                                 PV = as.numeric(PV)), by = "ID") %>%
  mutate(
    APV_years = coalesce(APV_years, APV_years_fromRE),
    pv        = coalesce(PV,        pv_fromRE)
  ) %>%
  select(ID, b, c, APV_years, pv)

#Sanity check & flag implausible ages (adjust bounds if needed)
boys <- boys %>%
  mutate(APV_flag = case_when(
    is.na(APV_years)        ~ "missing",
    APV_years < 8           ~ "low",
    APV_years > 20          ~ "high",
    TRUE                    ~ "ok"
  ))

summary(boys$APV_years)
table(boys$APV_flag) #We can describe these; it's people with really wonky data or flat slopes. I think we drop?
head(boys)

write_dta(boys, "yboysSITARmod4.dta")