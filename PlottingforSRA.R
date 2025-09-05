rm(list=ls())
sessionInfo()

#open libraries
##install.packages("sitar")
library(haven)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

#set working directory
#MAC:
setwd("~/Documents/GitHub/pubertybrain")

#read in file!
df <- read.csv("dfmerged_mplus.csv")
names(df)

df <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999)))

#Girl's plots

#Select girls!
girls <- df %>%
  filter(!is.na(gtiming), !is.na(gtempo)) %>%
  select(numeric_id, gtiming, gtempo,
         age_1, age_2, age_3, age_4,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV,
         ela_plus)

#Make long
gmv_long <- girls %>%
  pivot_longer(c(T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV),
               names_to = "wave_gmv", values_to = "GMV") %>%
  pivot_longer(c(age_1, age_2, age_3, age_4),
               names_to = "wave_age", values_to = "age") %>%
  filter(substr(wave_gmv, 2, 2) == substr(wave_age, 5, 5)) %>%
  mutate(wave = as.integer(substr(wave_gmv, 2, 2))) %>%
  select(numeric_id, gtiming, gtempo, wave, age, GMV) %>%
  arrange(numeric_id, wave) %>%
  filter(!is.na(GMV)) %>%
  mutate(
    GMV_k = GMV / 1000,
    numeric_id = as.factor(numeric_id)                      
  )

#Create timing groups and merge with gmv_long
q20 <- quantile(girls$gtiming, 0.20, na.rm = TRUE)
q80 <- quantile(girls$gtiming, 0.80, na.rm = TRUE)
muT <- mean(girls$gtiming, na.rm = TRUE)

muT

timing_groups <- girls %>%
  transmute(
    numeric_id = as.character(numeric_id),
    gtiming,
    timing_group = case_when(
      gtiming <= q20 ~ "Early",
      gtiming >= q80 ~ "Late",
      TRUE           ~ "On-time"
    )
  ) %>%
  select(numeric_id, timing_group)

gmv_long <- gmv_long %>%
  mutate(numeric_id = as.character(numeric_id)) %>%
  left_join(timing_groups, by = "numeric_id") %>%
  mutate(timing_group = factor(timing_group, levels = c("Early","On-time","Late")))

#Color-blind-safe palette and theme
cols3 <- c("Early"="#D55E00","On-time"="#7F7F7F","Late"="#0072B2")
theme_base <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

table(gmv_long$timing_group, useNA = "ifany")

#Empirical GMV trajectories with timing
summs <- gmv_long %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm = TRUE),
            se_k   = sd(GMV_k,   na.rm = TRUE)/sqrt(n),
            .groups = "drop")

pA <- ggplot() +
  geom_line(data = gmv_long, aes(wave, GMV_k, group = numeric_id),
            alpha = 0.05, linewidth = 0.2, color = "grey35") +
  geom_point(data = gmv_long, aes(wave, GMV_k),
             alpha = 0.04, size = 0.6, color = "grey35") +
  geom_line(data = summs, aes(wave, mean_k, color = timing_group), linewidth = 1.8) +
  geom_point(data = summs, aes(wave, mean_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  scale_color_manual(values = cols3) +
  coord_cartesian(ylim = c(400, 800)) +                       # <- view 0–1000
  scale_y_continuous(breaks = seq(400, 800, by = 100)) +      # tidy ticks
  labs(x = "Year", y = "Total GMV (÷1,000)", color = NULL,   # label matches GMV_k
       title = "Empirical GMV trajectories (girls) by pubertal timing") +
  theme_base

pA

#Model-estimated trajectories with timing
tload <- c(`1`=0.000, `2`=0.210, `3`=0.684, `4`=1.000)

mu_I_k <- mean(gmv_long$GMV_k[gmv_long$wave == 1], na.rm = TRUE)
mu_F_k <- mean(gmv_long$GMV_k[gmv_long$wave == 4], na.rm = TRUE)
mu_S_k <- mu_F_k - mu_I_k   # mean total change W1→W4

id_slopes_gt <- gmv_long %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(numeric_id, gtiming) %>%
  filter(n() >= 2) %>%
  do({ fit <- lm(GMV_k ~ t, data = .); tibble(slope_k = coef(fit)[["t"]]) }) %>%
  ungroup()

b_hat_gt <- unname(coef(lm(slope_k ~ gtiming, data = id_slopes_gt))["gtiming"])

# 3) Shifts for 20/60/20 timing groups relative to mean timing
shift_early <- as.numeric(q20 - muT)  # earlier than mean (negative)
shift_late  <- as.numeric(q80 - muT)  # later than mean (positive)

shifts <- c("Early" = shift_early, "On-time" = 0, "Late" = shift_late)
lvl3   <- names(shifts)

pred <- tidyr::expand_grid(timing_group = lvl3, wave = c(1,2,3,4)) %>%
  mutate(
    t        = tload[as.character(wave)],
    slope_k  = mu_S_k + b_hat_gt * shifts[timing_group],
    GMV_k    = mu_I_k + slope_k * t,
    timing_group = factor(timing_group, levels = lvl3)
  )

#Plot
pB_abs <- ggplot() +
  # raw backdrop (grey)
  geom_line(data = gmv_long, aes(wave, GMV_k, group = numeric_id),
            alpha = 0.05, linewidth = 0.2, color = "grey40") +
  geom_point(data = gmv_long, aes(wave, GMV_k),
             alpha = 0.04, size = 0.5, color = "grey40") +
  # model-implied lines
  geom_line(data = pred, aes(wave, GMV_k, color = timing_group), linewidth = 2) +
  geom_point(data = pred, aes(wave, GMV_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  scale_color_manual(values = cols3) +
  coord_cartesian(ylim = c(400, 800)) +
  scale_y_continuous(breaks = seq(400, 800, by = 100)) +
  labs(x = "Year", y = "Total GMV (÷1,000)", color = NULL,
       title = "Model-implied GMV (girls): absolute levels") +
  theme_base

pB_abs

# Model-implied ΔGMV from baseline is just slope_k * t (starts at 0)
pred_delta <- pred |>
  dplyr::mutate(Delta_k = slope_k * t)

raw_delta <- gmv_long %>%
  dplyr::group_by(numeric_id) %>%
  dplyr::mutate(GMV0_k = GMV_k[wave == 1][1]) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(GMV0_k)) %>%
  dplyr::mutate(Delta_k = GMV_k - GMV0_k)

raw_delta %>% summarise(min = min(Delta_k, na.rm=TRUE),
                        max = max(Delta_k, na.rm=TRUE),
                        n_na = sum(!is.finite(Delta_k)))
#Plot
pB_delta <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_line(data = raw_delta, aes(wave, Delta_k, group = numeric_id),
            alpha = 0.05, linewidth = 0.2, color = "grey40", na.rm = TRUE) +
  geom_point(data = raw_delta, aes(wave, Delta_k),
             alpha = 0.04, size = 0.5, color = "grey40", na.rm = TRUE) +
  geom_line(data = pred_delta, aes(wave, Delta_k, color = timing_group), linewidth = 1) +
  geom_point(data = pred_delta, aes(wave, Delta_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  coord_cartesian(ylim = c(-175, 75)) +                        # show +100 … -200
  scale_y_continuous(breaks = seq(100, -200, by = -25)) +       # ticks every 25
  scale_color_manual(values = cols3) +
  labs(x = "Year", y = "Δ GMV from Baseline (÷1,000)", color = NULL,
       title = "Model-implied GMV change (girls): Δ from baseline") +
  theme_base

pB_delta

tload <- c(`1`=0.000, `2`=0.210, `3`=0.684, `4`=1.000)   # from Mplus

# Mplus unstandardized growth-factor means (divide by 1000 to match GMV_k)
I_mean_k <- 641.903 
S_mean_k <- -68.624

# Mplus unstandardized slope-on-timing effect (also ÷1000 to match GMV_k units)
b_mplus_k <- 1.107 

# 4) Define timing shifts for Early/On-time/Late (20/60/20 cutpoints from YOUR data)
#    (These are in the units of gtiming, typically 'years' or 'z-scores' depending on your coding)
if (!exists("q20") || !exists("q80") || !exists("muT")) {
  q20 <- quantile(girls$gtiming, 0.20, na.rm = TRUE)
  q80 <- quantile(girls$gtiming, 0.80, na.rm = TRUE)
  muT <- mean(girls$gtiming, na.rm = TRUE)
}
shifts <- c("Early"   = as.numeric(q20 - muT),   # earlier than mean (usually negative)
            "On-time" = 0,
            "Late"    = as.numeric(q80 - muT))   # later than mean (usually positive)

# 5) Model-implied slope for each timing group: S̄ + (b * Δtiming)
pred <- tidyr::expand_grid(timing_group = names(shifts), wave = c(1,2,3,4)) %>%
  mutate(
    t            = tload[as.character(wave)],
    slope_k      = S_mean_k + b_mplus_k * shifts[timing_group],
    GMV_k        = I_mean_k + slope_k * t,
    timing_group = factor(timing_group, levels = c("Early","On-time","Late"))
  )

# Absolute GMV with raw backdrop
pB_abs <- ggplot() +
  geom_line(data = gmv_long, aes(wave, GMV_k, group = numeric_id),
            alpha = 0.05, linewidth = 0.2, color = "grey40", na.rm = TRUE) +
  geom_point(data = gmv_long, aes(wave, GMV_k),
             alpha = 0.04, size = 0.5, color = "grey40", na.rm = TRUE) +
  geom_line(data = pred, aes(wave, GMV_k, color = timing_group), linewidth = 1) +
  geom_point(data = pred, aes(wave, GMV_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  coord_cartesian(ylim = c(400, 800)) +
  scale_y_continuous(breaks = seq(400, 800, by = 100)) +
  scale_color_manual(values = cols3) +
  labs(x = "Year", y = "Total GMV (÷1,000)", color = NULL,
       title = "Model-implied GMV (girls): absolute levels (Mplus-driven)",
       subtitle = "Ī = 641.903, S̄ = −68.624, SLOPE ON GTIMING = b_mplus_k; loads: 0, .210, .684, 1.0") +
  theme_base

pB_abs

# Δ from baseline (Mplus-driven)
pred_delta <- pred %>% mutate(Delta_k = slope_k * t)

# Raw Δ backdrop from your data
raw_delta <- gmv_long %>%
  group_by(numeric_id) %>%
  mutate(GMV0_k = GMV_k[wave == 1][1]) %>%
  ungroup() %>%
  filter(is.finite(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

pB_delta <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_line(data = raw_delta, aes(wave, Delta_k, group = numeric_id),
            alpha = 0.05, linewidth = 0.2, color = "grey40", na.rm = TRUE) +
  geom_point(data = raw_delta, aes(wave, Delta_k),
             alpha = 0.04, size = 0.5, color = "grey40", na.rm = TRUE) +
  geom_line(data = pred_delta, aes(wave, Delta_k, color = timing_group), linewidth = 1) +
  geom_point(data = pred_delta, aes(wave, Delta_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  coord_cartesian(ylim = c(-150, 50)) +
  scale_y_continuous(breaks = seq(100, -200, by = -25)) +
  scale_color_manual(values = cols3) +
  labs(x = "Year", y = "Δ GMV from Baseline (÷1,000)", color = NULL,
       title = "Model-implied GMV change (girls): Δ from baseline (Mplus-driven)",
       subtitle = "Δ = (S̄ + b·Δtiming) × t; params from fullModel_F") +
  theme_base

pB_delta

# --- 1) Latent-basis time scores from Mplus ---
tload <- c(`1`=0.000, `2`=0.210, `3`=0.684, `4`=1.000)

# --- 2) Mplus growth-factor means (already GMV÷1,000 units) ---
I_mean_k <- 641.903     # INTERCEPT mean
S_mean_k <- -68.624     # SLOPE mean

# --- 3) Mplus unstandardized effect of timing on slope (same units) ---
b_timing_k <- 1.107     # SLOPE ON GTIMING

# --- 4) Define timing shifts for Early/On-time/Late (20/60/20), centered at Mplus timing mean ---
gtiming_mean_mplus <- 12.461  # "Intercepts: GTIMING"
q20 <- quantile(girls$gtiming, 0.20, na.rm = TRUE)
q80 <- quantile(girls$gtiming, 0.80, na.rm = TRUE)

shifts <- c("Early"   = as.numeric(q20 - gtiming_mean_mplus),
            "On-time" = 0,
            "Late"    = as.numeric(q80 - gtiming_mean_mplus))

# --- 5) Model-implied trajectories from Mplus parameters ---
pred <- tidyr::expand_grid(timing_group = names(shifts), wave = c(1,2,3,4)) %>%
  mutate(
    t            = tload[as.character(wave)],
    slope_k      = S_mean_k + b_timing_k * shifts[timing_group],   # Mplus slope mean + timing nudge
    GMV_k        = I_mean_k + slope_k * t,                          # Mplus intercept mean + change
    timing_group = factor(timing_group, levels = c("Early","On-time","Late"))
  )

# --- 6) Absolute-level plot (Mplus-driven) ---
pB_abs <- ggplot() +
  geom_line(data = gmv_long, aes(wave, GMV_k, group = numeric_id),
            alpha = 0.05, linewidth = 0.2, color = "grey40", na.rm = TRUE) +
  geom_point(data = gmv_long, aes(wave, GMV_k),
             alpha = 0.04, size = 0.5, color = "grey40", na.rm = TRUE) +
  geom_line(data = pred, aes(wave, GMV_k, color = timing_group), linewidth = 2) +
  geom_point(data = pred, aes(wave, GMV_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  coord_cartesian(ylim = c(400, 800)) +
  scale_y_continuous(breaks = seq(400, 800, by = 100)) +
  scale_color_manual(values = cols3) +
  labs(x = "Year", y = "Total GMV (÷1,000)", color = NULL,
       title = "Model-implied GMV (girls): absolute levels (Mplus-driven)",
       subtitle = "Ī=641.9, S̄=−68.6, SLOPE ON GTIMING=+1.107; loads 0,.210,.684,1.0") +
  theme_base

# --- 7) Δ-from-baseline version (Mplus-driven) ---
# In the model block, everyone starts at Ī, so Δ = slope_k * t
pred_delta <- pred %>% mutate(Delta_k = slope_k * t)

# Raw Δ background
raw_delta <- gmv_long %>%
  group_by(numeric_id) %>%
  mutate(GMV0_k = GMV_k[wave == 1][1]) %>%
  ungroup() %>%
  filter(is.finite(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

pB_delta <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_line(data = raw_delta, aes(wave, Delta_k, group = numeric_id),
            alpha = 0.05, linewidth = 0.2, color = "grey40", na.rm = TRUE) +
  geom_point(data = raw_delta, aes(wave, Delta_k),
             alpha = 0.04, size = 0.5, color = "grey40", na.rm = TRUE) +
  geom_line(data = pred_delta, aes(wave, Delta_k, color = timing_group), linewidth = 2) +
  geom_point(data = pred_delta, aes(wave, Delta_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  coord_cartesian(ylim = c(-175, 75)) +
  scale_y_continuous(breaks = seq(100, -200, by = -25)) +
  scale_color_manual(values = cols3) +
  labs(x = "Year", y = "Δ GMV from Baseline (÷1,000)", color = NULL,
       title = "Model-implied GMV change (girls): Δ from baseline (Mplus-driven)",
       subtitle = "Δ = (S̄ + b·Δtiming) × t; parameters from fullModel_F") +
  theme_base

pB_abs
pB_delta






























#Now girl's tempo models
q20_t <- quantile(girls$gtempo, 0.20, na.rm = TRUE)
q80_t <- quantile(girls$gtempo, 0.80, na.rm = TRUE)
muT_t <- mean(girls$gtempo, na.rm = TRUE)

muT 

tempo_groups <- girls %>%
  transmute(
    numeric_id = as.character(numeric_id),
    tempo_group = case_when(
      gtempo <= quantile(girls$gtempo, 0.20, na.rm = TRUE) ~ "Slow tempo",
      gtempo >= quantile(girls$gtempo, 0.80, na.rm = TRUE) ~ "Fast tempo",
      TRUE                                                 ~ "Typical tempo"
    )
  )

gmv_long <- gmv_long %>%
  mutate(numeric_id = as.character(numeric_id)) %>%
  select(-any_of("tempo_group")) %>%                 # remove if already present
  left_join(tempo_groups, by = "numeric_id") %>%
  mutate(tempo_group = factor(tempo_group,
                              levels = c("Slow tempo","Typical tempo","Fast tempo")))

table(gmv_long$tempo_group, useNA = "ifany")

cols_t <- c("Slow tempo"="#0072B2", "Typical tempo"="#7F7F7F", "Fast tempo"="#D55E00")
if (!exists("theme_base")) {
  theme_base <- theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.2))
}

gmv_long <- gmv_long %>%
  mutate(tempo_group = factor(tempo_group, levels = c("Slow tempo","Typical tempo","Fast tempo")))

#pA: Raw spaghetti + means
summs_t <- gmv_long %>%
  filter(!is.na(tempo_group)) %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm = TRUE),
            se_k   = sd(GMV_k,   na.rm = TRUE)/sqrt(n),
            .groups = "drop")

pA <- ggplot() +
  geom_line(data = gmv_long, aes(wave, GMV_k, group = numeric_id),
            alpha = 0.05, linewidth = 0.2, color = "grey35", na.rm = TRUE) +
  geom_point(data = gmv_long, aes(wave, GMV_k),
             alpha = 0.04, size = 0.6, color = "grey35", na.rm = TRUE) +
  geom_line(data = summs_t, aes(wave, mean_k, color = tempo_group), linewidth = 1) +
  geom_point(data = summs_t, aes(wave, mean_k, color = tempo_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  coord_cartesian(ylim = c(400, 800)) +
  scale_y_continuous(breaks = seq(400, 800, by = 100)) +
  scale_color_manual(values = cols_t) +
  labs(x = "Year", y = "Total GMV (÷1,000)", color = NULL,
       title = "Empirical GMV trajectories (girls) by pubertal tempo") +
  theme_base

pA

# Anchors
mu_I_k <- mean(gmv_long$GMV_k[gmv_long$wave == 1], na.rm = TRUE)
mu_F_k <- mean(gmv_long$GMV_k[gmv_long$wave == 4], na.rm = TRUE)
mu_S_k <- mu_F_k - mu_I_k

# Empirical slope ~ tempo effect in your units
id_slopes_t <- gmv_long %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(numeric_id, gtempo, tempo_group) %>%
  filter(n() >= 2) %>%
  do({ fit <- lm(GMV_k ~ t, data = .); tibble(slope_k = coef(fit)[["t"]]) }) %>%
  ungroup()

b_hat_tempo <- unname(coef(lm(slope_k ~ gtempo, data = id_slopes_t))["gtempo"])

# 20/60/20 shifts for tempo (relative to mean)
muT_t <- mean(gmv_long$gtempo, na.rm = TRUE)
q20_t <- quantile(gmv_long$gtempo, 0.20, na.rm = TRUE)
q80_t <- quantile(gmv_long$gtempo, 0.80, na.rm = TRUE)
shifts_t <- c("Slow tempo" = as.numeric(q20_t - muT_t),
              "Typical tempo" = 0,
              "Fast tempo" = as.numeric(q80_t - muT_t))

pred_t <- tidyr::expand_grid(tempo_group = names(shifts_t), wave = c(1,2,3,4)) %>%
  mutate(
    t        = tload[as.character(wave)],
    slope_k  = mu_S_k + b_hat_tempo * shifts_t[tempo_group],
    GMV_k    = mu_I_k + slope_k * t,
    tempo_group = factor(tempo_group, levels = c("Slow tempo","Typical tempo","Fast tempo"))
  )

pB <- ggplot() +
  geom_line(data = gmv_long, aes(wave, GMV_k, group = numeric_id),
            alpha = 0.05, linewidth = 0.2, color = "grey40", na.rm = TRUE) +
  geom_point(data = gmv_long, aes(wave, GMV_k),
             alpha = 0.04, size = 0.5, color = "grey40", na.rm = TRUE) +
  geom_line(data = pred_t, aes(wave, GMV_k, color = tempo_group), linewidth = 1) +
  geom_point(data = pred_t, aes(wave, GMV_k, color = tempo_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  coord_cartesian(ylim = c(400, 800)) +
  scale_y_continuous(breaks = seq(400, 800, by = 100)) +
  scale_color_manual(values = cols_t) +
  labs(x = "Year", y = "Total GMV (÷1,000)", color = NULL,
       title = "Model-implied GMV (girls) by pubertal tempo: absolute levels",
       subtitle = sprintf("Effect from data: slope_k ~ gtempo (b = %.3f)", b_hat_tempo)) +
  theme_base

pB

# pB_delta: Δ from baseline (raw bg + model-implied)
pred_delta_t <- pred_t %>% mutate(Delta_k = GMV_k - mu_I_k)

raw_delta_t <- gmv_long %>%
  group_by(numeric_id) %>%
  mutate(GMV0_k = GMV_k[wave == 1][1]) %>%
  ungroup() %>%
  filter(is.finite(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

pB_delta <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_line(data = raw_delta_t, aes(wave, Delta_k, group = numeric_id),
            alpha = 0.05, linewidth = 0.2, color = "grey40", na.rm = TRUE) +
  geom_point(data = raw_delta_t, aes(wave, Delta_k),
             alpha = 0.04, size = 0.5, color = "grey40", na.rm = TRUE) +
  geom_line(data = pred_delta_t, aes(wave, Delta_k, color = tempo_group), linewidth = .5) +
  geom_point(data = pred_delta_t, aes(wave, Delta_k, color = tempo_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  coord_cartesian(ylim = c(-150, 50)) +                        # show +100 … -200
  scale_y_continuous(breaks = seq(100, -200, by = -25)) +       # ticks every 25
  scale_color_manual(values = cols_t) +
  labs(x = "Year", y = "Δ GMV from Baseline (÷1,000)", color = NULL,
       title = "Model-implied GMV change (girls) by pubertal tempo: Δ from baseline",
       subtitle = sprintf("Dashed logic: Δ = (μΔ + b·Δtempo) × t;  b = %.3f", b_hat_tempo)) +
  theme_base

pB_delta

# 1) Model-implied lines using ±1 SD shifts of tempo
sd_tempo <- sd(gmv_long$gtempo, na.rm = TRUE)
shifts_sd <- c("Slow tempo" = -sd_tempo,
               "Typical tempo" = 0,
               "Fast tempo" = +sd_tempo)

pred_t_sd <- tidyr::expand_grid(tempo_group = names(shifts_sd), wave = c(1,2,3,4)) %>%
  dplyr::mutate(
    t        = tload[as.character(wave)],
    slope_k  = mu_S_k + b_hat_tempo * shifts_sd[tempo_group],
    GMV_k    = mu_I_k + slope_k * t,
    tempo_group = factor(tempo_group, levels = c("Slow tempo","Typical tempo","Fast tempo"))
  )

pred_delta_t_sd <- pred_t_sd %>% dplyr::mutate(Delta_k = GMV_k - mu_I_k)

# 2) Absolute GMV plot (pB) with thicker model lines + zoom
pB <- ggplot() +
  geom_line(data = gmv_long, aes(wave, GMV_k, group = numeric_id),
            alpha = 0.04, linewidth = 0.2, color = "grey40", na.rm = TRUE) +
  geom_point(data = gmv_long, aes(wave, GMV_k),
             alpha = 0.03, size = 0.5, color = "grey40", na.rm = TRUE) +
  geom_line(data = pred_t_sd, aes(wave, GMV_k, color = tempo_group),
            linewidth = 2) +
  geom_point(data = pred_t_sd, aes(wave, GMV_k, color = tempo_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  coord_cartesian(ylim = c(450, 750)) +
  scale_y_continuous(breaks = seq(450, 750, by = 50)) +
  scale_color_manual(values = cols_t) +
  labs(x = "Year", y = "Total GMV (÷1,000)", color = NULL,
       title = "Model-implied GMV (girls) by pubertal tempo: absolute levels",
       subtitle = sprintf("Model shift = ±1 SD of tempo · b = %.3f", b_hat_tempo)) +
  theme_base

# 3) Δ-from-baseline plot (pB_delta) with ±1 SD lines + zoom
raw_delta_t <- gmv_long %>%
  dplyr::group_by(numeric_id) %>%
  dplyr::mutate(GMV0_k = GMV_k[wave == 1][1]) %>%
  dplyr::ungroup() %>%
  dplyr::filter(is.finite(GMV0_k)) %>%
  dplyr::mutate(Delta_k = GMV_k - GMV0_k)

pB_delta <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_line(data = raw_delta_t, aes(wave, Delta_k, group = numeric_id),
            alpha = 0.05, linewidth = 0.2, color = "grey40", na.rm = TRUE) +
  geom_point(data = raw_delta_t, aes(wave, Delta_k),
             alpha = 0.04, size = 0.5, color = "grey40", na.rm = TRUE) +
  geom_line(data = pred_delta_t_sd, aes(wave, Delta_k, color = tempo_group),
            linewidth = .5) +
  geom_point(data = pred_delta_t_sd, aes(wave, Delta_k, color = tempo_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  coord_cartesian(ylim = c(-120, 40)) +
  scale_y_continuous(breaks = seq(40, -120, by = -20)) +
  scale_color_manual(values = cols_t) +
  labs(x = "Year", y = "Δ GMV from Baseline (÷1,000)", color = NULL,
       title = "Model-implied GMV change (girls) by tempo: Δ from baseline",
       subtitle = sprintf("Model shift = ±1 SD of tempo · b = %.3f", b_hat_tempo)) +
  theme_base

pB_delta

pB_delta <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_line(data = pred_delta_t_sd, aes(wave, Delta_k, color = tempo_group),
            linewidth = .5) +
  geom_point(data = pred_delta_t_sd, aes(wave, Delta_k, color = tempo_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  coord_cartesian(ylim = c(-60, 20)) +
  scale_y_continuous(breaks = seq(40, -120, by = -20)) +
  scale_color_manual(values = cols_t) +
  labs(x = "Year", y = "Δ GMV from Baseline (÷1,000)", color = NULL,
       title = "Model-implied GMV change (girls) by tempo: Δ from baseline",
       subtitle = sprintf("Model shift = ±1 SD of tempo · b = %.3f", b_hat_tempo)) +
  theme_base

pB_delta


names(df)
















#Boys
boys <- df %>%
  filter(!is.na(btiming), !is.na(btempo)) %>%
  select(ID, btiming, btempo,
         age_1, age_2, age_3, age_4,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_b <- boys %>%
  pivot_longer(c(T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV),
               names_to="wave_gmv", values_to="GMV") %>%
  pivot_longer(c(age_1, age_2, age_3, age_4),
               names_to="wave_age", values_to="age") %>%
  filter(substr(wave_gmv,2,2) == substr(wave_age,5,5)) %>%
  mutate(wave = as.integer(substr(wave_gmv,2,2)),
         GMV_k = GMV/1000) %>%
  select(ID, btiming, btempo, wave, age, GMV_k) %>%
  arrange(ID, wave) %>%
  filter(!is.na(GMV_k))

theme_base <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

tload <- c(`1`=0.000, `2`=0.215, `3`=0.556, `4`=1.000)  # latent-basis time scores

# ============================================================
# TIMING (20/60/20) — boys
# ============================================================

# ----- Groups by 20/60/20 on btiming -----
q20_btim <- quantile(boys$btiming, 0.20, na.rm = TRUE)
q80_btim <- quantile(boys$btiming, 0.80, na.rm = TRUE)
mu_btim  <- mean(boys$btiming, na.rm = TRUE)

timing_groups_b <- boys %>%
  transmute(ID, btiming,
            timing_group = case_when(
              btiming <= q20_btim ~ "Early (bottom 20%)",
              btiming >= q80_btim ~ "Late (top 20%)",
              TRUE                ~ "On-time (middle 60%)"
            ))

gmv_long_timing_b <- gmv_long_b %>%
  left_join(timing_groups_b, by = c("ID","btiming"))

cols_timing <- c("Early (bottom 20%)"="#D55E00",
                 "On-time (middle 60%)"="#7F7F7F",
                 "Late (top 20%)"="#0072B2")

# ----- (A) Empirical trajectories -----
summs_btim <- gmv_long_timing_b %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm=TRUE),
            se_k   = sd(GMV_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

pA_timing_b <- ggplot() +
  geom_line(data = gmv_long_timing_b, aes(wave, GMV_k, group = ID),
            alpha = 0.05, linewidth = 0.2, color = "grey35") +
  geom_point(data = gmv_long_timing_b, aes(wave, GMV_k),
             alpha = 0.04, size = 0.6, color = "grey35") +
  geom_errorbar(data = summs_btim,
                aes(wave, ymin = mean_k - 1.96*se_k, ymax = mean_k + 1.96*se_k, color = timing_group),
                width = .08, linewidth = .6) +
  geom_line(data = summs_btim, aes(wave, mean_k, color = timing_group), linewidth = 1.8) +
  geom_point(data = summs_btim, aes(wave, mean_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_color_manual(values = cols_timing) +
  labs(x = "Wave", y = "Total GMV (÷1,000)", color = NULL,
       title = "Empirical GMV trajectories (boys) by pubertal timing (20/60/20)") +
  theme_base

# ----- (B) Model-implied Δ from baseline (estimate effect from data) -----
# Per-ID slope (latent-basis proxy)
id_slopes_btim <- gmv_long_timing_b %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(ID, btiming, timing_group) %>%
  filter(n() >= 2) %>%
  do({ fit <- lm(GMV_k ~ t, data = .); tibble(slope_k = coef(fit)[["t"]]) }) %>%
  ungroup()

# Regression of slope on btiming in *your* units
fit_slope_btim <- lm(slope_k ~ btiming, data = id_slopes_btim)
b_hat_btim <- unname(coef(fit_slope_btim)["btiming"])     # effect per 1-year later onset
mu_S_k_b   <- mean(id_slopes_btim$slope_k, na.rm = TRUE)  # mean total change W1→W4
mu_I_k_b   <- mean(gmv_long_timing_b$GMV_k[gmv_long_timing_b$wave==1], na.rm = TRUE)

# Shifts for groups relative to mean timing
shift_early_b <- as.numeric(q20_btim - mu_btim)
shift_late_b  <- as.numeric(q80_btim - mu_btim)

shifts_btim <- c("Early (bottom 20%)" = shift_early_b,
                 "On-time (middle 60%)" = 0,
                 "Late (top 20%)" = shift_late_b)

pred_delta_btim <- tidyr::expand_grid(timing_group = names(shifts_btim), wave = c(1,2,3,4)) %>%
  mutate(t        = tload[as.character(wave)],
         slope_k  = mu_S_k_b + b_hat_btim * shifts_btim[timing_group],
         Delta_k  = slope_k * t,
         timing_group = factor(timing_group, levels = names(shifts_btim)))

# Raw Δ background
raw_delta_btim <- gmv_long_timing_b %>%
  group_by(ID) %>%
  mutate(GMV0_k = GMV_k[wave==1][1]) %>%
  ungroup() %>%
  filter(!is.na(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

pB_timing_b <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_line(data = raw_delta_btim, aes(wave, Delta_k, group = ID),
            alpha = 0.05, linewidth = 0.2, color = "grey40") +
  geom_point(data = raw_delta_btim, aes(wave, Delta_k),
             alpha = 0.04, size = 0.5, color = "grey40") +
  geom_line(data = pred_delta_btim, aes(wave, Delta_k, color = timing_group), linewidth = 2) +
  geom_point(data = pred_delta_btim, aes(wave, Delta_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_y_continuous(limits = c(-100, 0), breaks = seq(0, -100, by = -20)) +
  scale_color_manual(values = cols_timing) +
  labs(x = "Wave", y = "Δ GMV from baseline (÷1,000)", color = NULL,
       title = "Model-implied GMV change (boys) by timing: Δ from baseline (20/60/20)",
       subtitle = sprintf("Effect from data: slope_k ~ btiming  (b = %.3f)", b_hat_btim)) +
  theme_base

# ============================================================
# TEMPO (20/60/20) — boys
# ============================================================

# ----- Groups by 20/60/20 on btempo -----
q20_btmp <- quantile(boys$btempo, 0.20, na.rm = TRUE)
q80_btmp <- quantile(boys$btempo, 0.80, na.rm = TRUE)
mu_btmp  <- mean(boys$btempo, na.rm = TRUE)

tempo_groups_b <- boys %>%
  transmute(ID, btempo,
            tempo_group = case_when(
              btempo <= q20_btmp ~ "Slow (bottom 20%)",
              btempo >= q80_btmp ~ "Fast (top 20%)",
              TRUE               ~ "Typical (middle 60%)"
            ))

gmv_long_tempo_b <- gmv_long_b %>%
  left_join(tempo_groups_b, by = c("ID","btempo"))

cols_tempo <- c("Slow (bottom 20%)"="#0072B2",
                "Typical (middle 60%)"="#7F7F7F",
                "Fast (top 20%)"="#D55E00")

# ----- (A) Empirical trajectories -----
summs_btmp <- gmv_long_tempo_b %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm=TRUE),
            se_k   = sd(GMV_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

pA_tempo_b <- ggplot() +
  geom_line(data = gmv_long_tempo_b, aes(wave, GMV_k, group = ID),
            alpha = 0.05, linewidth = 0.2, color = "grey35") +
  geom_point(data = gmv_long_tempo_b, aes(wave, GMV_k),
             alpha = 0.04, size = 0.6, color = "grey35") +
  geom_errorbar(data = summs_btmp,
                aes(wave, ymin = mean_k - 1.96*se_k, ymax = mean_k + 1.96*se_k, color = tempo_group),
                width = .08, linewidth = .6) +
  geom_line(data = summs_btmp, aes(wave, mean_k, color = tempo_group), linewidth = 1.8) +
  geom_point(data = summs_btmp, aes(wave, mean_k, color = tempo_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_color_manual(values = cols_tempo) +
  labs(x = "Wave", y = "Total GMV (÷1,000)", color = NULL,
       title = "Empirical GMV trajectories (boys) by pubertal tempo (20/60/20)") +
  theme_base

# ----- (B) Model-implied Δ from baseline (estimate effect from data) -----
id_slopes_btmp <- gmv_long_tempo_b %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(ID, btempo, tempo_group) %>%
  filter(n() >= 2) %>%
  do({ fit <- lm(GMV_k ~ t, data = .); tibble(slope_k = coef(fit)[["t"]]) }) %>%
  ungroup()

fit_slope_btmp <- lm(slope_k ~ btempo, data = id_slopes_btmp)
b_hat_btmp <- unname(coef(fit_slope_btmp)["btempo"])      # effect per 1-unit faster tempo
mu_S_k_bt  <- mean(id_slopes_btmp$slope_k, na.rm = TRUE)

shift_slow_b <- as.numeric(q20_btmp - mu_btmp)
shift_fast_b <- as.numeric(q80_btmp - mu_btmp)

shifts_btmp <- c("Slow (bottom 20%)" = shift_slow_b,
                 "Typical (middle 60%)" = 0,
                 "Fast (top 20%)" = shift_fast_b)

pred_delta_btmp <- tidyr::expand_grid(tempo_group = names(shifts_btmp), wave = c(1,2,3,4)) %>%
  mutate(t       = tload[as.character(wave)],
         slope_k = mu_S_k_bt + b_hat_btmp * shifts_btmp[tempo_group],
         Delta_k = slope_k * t,
         tempo_group = factor(tempo_group, levels = names(shifts_btmp)))

raw_delta_btmp <- gmv_long_tempo_b %>%
  group_by(ID) %>%
  mutate(GMV0_k = GMV_k[wave==1][1]) %>%
  ungroup() %>%
  filter(!is.na(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

pB_tempo_b <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_line(data = raw_delta_btmp, aes(wave, Delta_k, group = ID),
            alpha = 0.05, linewidth = 0.2, color = "grey40") +
  geom_point(data = raw_delta_btmp, aes(wave, Delta_k),
             alpha = 0.04, size = 0.5, color = "grey40") +
  geom_line(data = pred_delta_btmp, aes(wave, Delta_k, color = tempo_group), linewidth = 2) +
  geom_point(data = pred_delta_btmp, aes(wave, Delta_k, color = tempo_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_y_continuous(limits = c(-100, 0), breaks = seq(0, -100, by = -20)) +
  scale_color_manual(values = cols_tempo) +
  labs(x = "Wave", y = "Δ GMV from baseline (÷1,000)", color = NULL,
       title = "Model-implied GMV change (boys) by tempo: Δ from baseline (20/60/20)",
       subtitle = sprintf("Effect from data: slope_k ~ btempo  (b = %.3f)", b_hat_btmp)) +
  theme_base

# ---------- Show them ----------
pA_timing_b
pB_timing_b
pA_tempo_b
pB_tempo_b
# Or panels:
# (pA_timing_b | pB_timing_b) / (pA_tempo_b | pB_tempo_b)

# --- assumes you've already run the boys prep from earlier ---
# objects used below:
#   gmv_long_b: ID, btiming, btempo, wave (1..4), GMV_k = GMV/1000
#   timing_groups_b: timing_group (Early/On-time/Late) by btiming 20/60/20
#   tempo_groups_b:  tempo_group  (Slow/Typical/Fast) by btempo 20/60/20
#   cols_timing, cols_tempo color palettes
#   theme_base, tload time scores

library(dplyr); library(ggplot2)

# Attach groups (if not already):
gmv_long_timing_b <- gmv_long_b %>% left_join(timing_groups_b, by=c("ID","btiming"))
gmv_long_tempo_b  <- gmv_long_b %>% left_join(tempo_groups_b,  by=c("ID","btempo"))

# =========================
# A) ABSOLUTE GMV (÷1,000)
# =========================

# ---- Timing: means + 95% CIs (no raw) ----
summs_btim <- gmv_long_timing_b %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm=TRUE),
            se_k   = sd(GMV_k, na.rm=TRUE)/sqrt(n),
            .groups="drop")

pA_timing_means <- ggplot(summs_btim, aes(wave, mean_k, color = timing_group, fill = timing_group)) +
  geom_ribbon(aes(ymin = mean_k - 1.96*se_k, ymax = mean_k + 1.96*se_k),
              alpha = 0.15, linewidth = 0, show.legend = FALSE) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_color_manual(values = cols_timing) +
  scale_fill_manual(values  = cols_timing) +
  labs(x = "Wave", y = "Total GMV (÷1,000)", color = NULL,
       title = "Boys · GMV (absolute) by pubertal timing (20/60/20)") +
  theme_base

# ---- Tempo: means + 95% CIs (no raw) ----
summs_btmp <- gmv_long_tempo_b %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm=TRUE),
            se_k   = sd(GMV_k, na.rm=TRUE)/sqrt(n),
            .groups="drop")

pA_tempo_means <- ggplot(summs_btmp, aes(wave, mean_k, color = tempo_group, fill = tempo_group)) +
  geom_ribbon(aes(ymin = mean_k - 1.96*se_k, ymax = mean_k + 1.96*se_k),
              alpha = 0.15, linewidth = 0, show.legend = FALSE) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_color_manual(values = cols_tempo) +
  scale_fill_manual(values  = cols_tempo) +
  labs(x = "Wave", y = "Total GMV (÷1,000)", color = NULL,
       title = "Boys · GMV (absolute) by pubertal tempo (20/60/20)") +
  theme_base

# ==========================================
# B) Δ FROM BASELINE (clear separation) 
#     + clamp y-axis to 0…−100
# ==========================================

# ---- Timing: per-ID Δ, then group means + 95% CIs ----
delta_timing <- gmv_long_timing_b %>%
  group_by(ID) %>% mutate(GMV0_k = GMV_k[wave==1][1]) %>% ungroup() %>%
  filter(!is.na(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

summs_delta_btim <- delta_timing %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_d = mean(Delta_k, na.rm=TRUE),
            se_d   = sd(Delta_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

pB_timing_delta <- ggplot(summs_delta_btim, aes(wave, mean_d, color = timing_group, fill = timing_group)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey40") +
  geom_ribbon(aes(ymin = mean_d - 1.96*se_d, ymax = mean_d + 1.96*se_d),
              alpha = 0.18, linewidth = 0, show.legend = FALSE) +
  geom_line(linewidth = 2) +
  geom_point(size = 2.4) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_y_continuous(limits = c(-100, 0), breaks = seq(0, -100, by = -20)) +
  scale_color_manual(values = cols_timing) +
  scale_fill_manual(values  = cols_timing) +
  labs(x = "Wave", y = "Δ GMV from W1 (÷1,000)", color = NULL,
       title = "Boys · Δ GMV by pubertal timing (20/60/20)") +
  theme_base

# ---- Tempo: per-ID Δ, then group means + 95% CIs ----
delta_tempo <- gmv_long_tempo_b %>%
  group_by(ID) %>% mutate(GMV0_k = GMV_k[wave==1][1]) %>% ungroup() %>%
  filter(!is.na(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

summs_delta_btmp <- delta_tempo %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_d = mean(Delta_k, na.rm=TRUE),
            se_d   = sd(Delta_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

pB_tempo_delta <- ggplot(summs_delta_btmp, aes(wave, mean_d, color = tempo_group, fill = tempo_group)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey40") +
  geom_ribbon(aes(ymin = mean_d - 1.96*se_d, ymax = mean_d + 1.96*se_d),
              alpha = 0.18, linewidth = 0, show.legend = FALSE) +
  geom_line(linewidth = 2) +
  geom_point(size = 2.4) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_y_continuous(limits = c(-100, 0), breaks = seq(0, -100, by = -20)) +
  scale_color_manual(values = cols_tempo) +
  scale_fill_manual(values  = cols_tempo) +
  labs(x = "Wave", y = "Δ GMV from W1 (÷1,000)", color = NULL,
       title = "Boys · Δ GMV by pubertal tempo (20/60/20)") +
  theme_base

# View
pA_timing_means
pA_tempo_means
pB_timing_delta
pB_tempo_delta
# Or arrange as panels:
(pA_timing_means | pA_tempo_means) / (pB_timing_delta | pB_tempo_delta)

library(dplyr); library(tidyr); library(ggplot2); library(patchwork)

# ---------------- Prep (girls) ----------------
# If needed: convert -999 sentinels to NA
df <- df %>% mutate(across(where(is.numeric), ~ na_if(., -999)))

girls <- df %>%
  filter(!is.na(gtiming), !is.na(gtempo)) %>%
  select(ID, gtiming, gtempo,
         age_1, age_2, age_3, age_4,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_g <- girls %>%
  pivot_longer(c(T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV),
               names_to="wave_gmv", values_to="GMV") %>%
  pivot_longer(c(age_1, age_2, age_3, age_4),
               names_to="wave_age", values_to="age") %>%
  filter(substr(wave_gmv,2,2) == substr(wave_age,5,5)) %>%
  mutate(wave = as.integer(substr(wave_gmv,2,2)),
         GMV_k = GMV/1000) %>%                 # scale for readability
  select(ID, gtiming, gtempo, wave, age, GMV_k) %>%
  arrange(ID, wave) %>%
  filter(!is.na(GMV_k))

tload <- c(`1`=0.000, `2`=0.215, `3`=0.556, `4`=1.000)

theme_base <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

# Color-blind-safe palettes
cols_timing <- c("Early (bottom 20%)"="#D55E00",
                 "On-time (middle 60%)"="#7F7F7F",
                 "Late (top 20%)"="#0072B2")
cols_tempo  <- c("Slow (bottom 20%)"="#0072B2",
                 "Typical (middle 60%)"="#7F7F7F",
                 "Fast (top 20%)"="#D55E00")

# ---------------- Timing groups (20/60/20) ----------------
q20_gt <- quantile(girls$gtiming, 0.20, na.rm=TRUE)
q80_gt <- quantile(girls$gtiming, 0.80, na.rm=TRUE)
mu_gt  <- mean(girls$gtiming, na.rm=TRUE)

timing_groups_g <- girls %>%
  transmute(ID, gtiming,
            timing_group = case_when(
              gtiming <= q20_gt ~ "Early (bottom 20%)",
              gtiming >= q80_gt ~ "Late (top 20%)",
              TRUE              ~ "On-time (middle 60%)"
            ))
gmv_long_timing_g <- gmv_long_g %>% left_join(timing_groups_g, by=c("ID","gtiming"))

# ---- A1) Absolute GMV means + 95% CIs (no raw) ----
summs_gt <- gmv_long_timing_g %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm=TRUE),
            se_k   = sd(GMV_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

pA_timing_means_g <- ggplot(summs_gt, aes(wave, mean_k, color=timing_group, fill=timing_group)) +
  geom_ribbon(aes(ymin=mean_k-1.96*se_k, ymax=mean_k+1.96*se_k),
              alpha=.15, linewidth=0, show.legend=FALSE) +
  geom_line(linewidth=1.8) +
  geom_point(size=2.2) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("W1","W2","W3","W4")) +
  scale_color_manual(values=cols_timing) +
  scale_fill_manual(values=cols_timing) +
  labs(x="Wave", y="Total GMV (÷1,000)", color=NULL,
       title="Girls · GMV (absolute) by pubertal timing (20/60/20)") +
  theme_base

# ---- B1) Δ from baseline means + 95% CIs with model-implied overlay ----
delta_timing_g <- gmv_long_timing_g %>%
  group_by(ID) %>% mutate(GMV0_k = GMV_k[wave==1][1]) %>% ungroup() %>%
  filter(!is.na(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

summs_delta_gt <- delta_timing_g %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_d = mean(Delta_k, na.rm=TRUE),
            se_d   = sd(Delta_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

# Empirical slope_k ~ gtiming effect (for dashed model-implied lines)
id_slopes_gt <- gmv_long_timing_g %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(ID, gtiming, timing_group) %>%
  filter(n() >= 2) %>%
  do({ fit <- lm(GMV_k ~ t, data=.); tibble(slope_k = coef(fit)[["t"]]) }) %>%
  ungroup()
b_hat_gt <- unname(coef(lm(slope_k ~ gtiming, data=id_slopes_gt))["gtiming"])
mu_S_k_g  <- mean(id_slopes_gt$slope_k, na.rm=TRUE)

shift_early_gt <- as.numeric(q20_gt - mu_gt)
shift_late_gt  <- as.numeric(q80_gt - mu_gt)
shifts_gt <- c("Early (bottom 20%)"=shift_early_gt,
               "On-time (middle 60%)"=0,
               "Late (top 20%)"=shift_late_gt)

pred_delta_gt <- tidyr::expand_grid(timing_group = names(shifts_gt), wave = c(1,2,3,4)) %>%
  mutate(t       = tload[as.character(wave)],
         slope_k = mu_S_k_g + b_hat_gt * shifts_gt[timing_group],
         Delta_k = slope_k * t,
         timing_group = factor(timing_group, levels = names(shifts_gt)))

pB_timing_delta_g <- ggplot(summs_delta_gt, aes(wave, mean_d, color=timing_group, fill=timing_group)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey40") +
  geom_ribbon(aes(ymin=mean_d-1.96*se_d, ymax=mean_d+1.96*se_d),
              alpha=.18, linewidth=0, show.legend=FALSE) +
  geom_line(linewidth=2) +
  geom_point(size=2.4) +
  # model-implied dashed overlay
  geom_line(data=pred_delta_gt, aes(wave, Delta_k, color=timing_group),
            linewidth=1.2, linetype="dashed", inherit.aes=FALSE) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("W1","W2","W3","W4")) +
  scale_y_continuous(limits=c(-100, 0), breaks=seq(0, -100, by=-20)) +
  scale_color_manual(values=cols_timing) +
  scale_fill_manual(values=cols_timing) +
  labs(x="Wave", y="Δ GMV from W1 (÷1,000)", color=NULL,
       title="Girls · Δ GMV by pubertal timing (20/60/20)",
       subtitle=sprintf("Dashed = model-implied (slope_k ~ gtiming; b = %.3f)", b_hat_gt)) +
  theme_base

# ---------------- Tempo groups (20/60/20) ----------------
q20_gte <- quantile(girls$gtempo, 0.20, na.rm=TRUE)
q80_gte <- quantile(girls$gtempo, 0.80, na.rm=TRUE)
mu_gte  <- mean(girls$gtempo, na.rm=TRUE)

tempo_groups_g <- girls %>%
  transmute(ID, gtempo,
            tempo_group = case_when(
              gtempo <= q20_gte ~ "Slow (bottom 20%)",
              gtempo >= q80_gte ~ "Fast (top 20%)",
              TRUE              ~ "Typical (middle 60%)"
            ))
gmv_long_tempo_g <- gmv_long_g %>% left_join(tempo_groups_g, by=c("ID","gtempo"))

# ---- A2) Absolute GMV means + 95% CIs (no raw) ----
summs_gte <- gmv_long_tempo_g %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm=TRUE),
            se_k   = sd(GMV_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

pA_tempo_means_g <- ggplot(summs_gte, aes(wave, mean_k, color=tempo_group, fill=tempo_group)) +
  geom_ribbon(aes(ymin=mean_k-1.96*se_k, ymax=mean_k+1.96*se_k),
              alpha=.15, linewidth=0, show.legend=FALSE) +
  geom_line(linewidth=1.8) +
  geom_point(size=2.2) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("W1","W2","W3","W4")) +
  scale_color_manual(values=cols_tempo) +
  scale_fill_manual(values=cols_tempo) +
  labs(x="Wave", y="Total GMV (÷1,000)", color=NULL,
       title="Girls · GMV (absolute) by pubertal tempo (20/60/20)") +
  theme_base

# ---- B2) Δ from baseline means + 95% CIs with model-implied overlay ----
delta_tempo_g <- gmv_long_tempo_g %>%
  group_by(ID) %>% mutate(GMV0_k = GMV_k[wave==1][1]) %>% ungroup() %>%
  filter(!is.na(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

summs_delta_gte <- delta_tempo_g %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_d = mean(Delta_k, na.rm=TRUE),
            se_d   = sd(Delta_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

# Empirical slope_k ~ gtempo effect (for dashed model-implied lines)
id_slopes_gte <- gmv_long_tempo_g %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(ID, gtempo, tempo_group) %>%
  filter(n() >= 2) %>%
  do({ fit <- lm(GMV_k ~ t, data=.); tibble(slope_k = coef(fit)[["t"]]) }) %>%
  ungroup()
b_hat_gte <- unname(coef(lm(slope_k ~ gtempo, data=id_slopes_gte))["gtempo"])
mu_S_k_gte <- mean(id_slopes_gte$slope_k, na.rm=TRUE)

shift_slow_gte <- as.numeric(q20_gte - mu_gte)
shift_fast_gte  <- as.numeric(q80_gte - mu_gte)
shifts_gte <- c("Slow (bottom 20%)"=shift_slow_gte,
                "Typical (middle 60%)"=0,
                "Fast (top 20%)"=shift_fast_gte)

pred_delta_gte <- tidyr::expand_grid(tempo_group = names(shifts_gte), wave = c(1,2,3,4)) %>%
  mutate(t       = tload[as.character(wave)],
         slope_k = mu_S_k_gte + b_hat_gte * shifts_gte[tempo_group],
         Delta_k = slope_k * t,
         tempo_group = factor(tempo_group, levels = names(shifts_gte)))

pB_tempo_delta_g <- ggplot(summs_delta_gte, aes(wave, mean_d, color=tempo_group, fill=tempo_group)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey40") +
  geom_ribbon(aes(ymin=mean_d-1.96*se_d, ymax=mean_d+1.96*se_d),
              alpha=.18, linewidth=0, show.legend=FALSE) +
  geom_line(linewidth=2) +
  geom_point(size=2.4) +
  # model-implied dashed overlay
  geom_line(data=pred_delta_gte, aes(wave, Delta_k, color=tempo_group),
            linewidth=1.2, linetype="dashed", inherit.aes=FALSE) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("W1","W2","W3","W4")) +
  scale_y_continuous(limits=c(-100, 0), breaks=seq(0, -100, by=-20)) +
  scale_color_manual(values=cols_tempo) +
  scale_fill_manual(values=cols_tempo) +
  labs(x="Wave", y="Δ GMV from W1 (÷1,000)", color=NULL,
       title="Girls · Δ GMV by pubertal tempo (20/60/20)",
       subtitle=sprintf("Dashed = model-implied (slope_k ~ gtempo; b = %.3f)", b_hat_gte)) +
  theme_base

# ----------- Show -----------
pA_timing_means_g
pB_timing_delta_g
pA_tempo_means_g
pB_tempo_delta_g
# Or as panels:
# (pA_timing_means_g | pA_tempo_means_g) / (pB_timing_delta_g | pB_tempo_delta_g)


library(dplyr); library(tidyr); library(ggplot2); library(patchwork)

# ---------------- Prep (girls) ----------------
# If needed: convert -999 sentinels to NA
df <- df %>% mutate(across(where(is.numeric), ~ na_if(., -999)))

girls <- df %>%
  filter(!is.na(gtiming), !is.na(gtempo)) %>%
  select(ID, gtiming, gtempo,
         age_1, age_2, age_3, age_4,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_g <- girls %>%
  pivot_longer(c(T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV),
               names_to="wave_gmv", values_to="GMV") %>%
  pivot_longer(c(age_1, age_2, age_3, age_4),
               names_to="wave_age", values_to="age") %>%
  filter(substr(wave_gmv,2,2) == substr(wave_age,5,5)) %>%
  mutate(wave = as.integer(substr(wave_gmv,2,2)),
         GMV_k = GMV/1000) %>%                 # scale for readability
  select(ID, gtiming, gtempo, wave, age, GMV_k) %>%
  arrange(ID, wave) %>%
  filter(!is.na(GMV_k))

tload <- c(`1`=0.000, `2`=0.215, `3`=0.556, `4`=1.000)

theme_base <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

# Color-blind-safe palettes
cols_timing <- c("Early (bottom 20%)"="#D55E00",
                 "On-time (middle 60%)"="#7F7F7F",
                 "Late (top 20%)"="#0072B2")
cols_tempo  <- c("Slow (bottom 20%)"="#0072B2",
                 "Typical (middle 60%)"="#7F7F7F",
                 "Fast (top 20%)"="#D55E00")

# ---------------- Timing groups (20/60/20) ----------------
q20_gt <- quantile(girls$gtiming, 0.20, na.rm=TRUE)
q80_gt <- quantile(girls$gtiming, 0.80, na.rm=TRUE)
mu_gt  <- mean(girls$gtiming, na.rm=TRUE)

timing_groups_g <- girls %>%
  transmute(ID, gtiming,
            timing_group = case_when(
              gtiming <= q20_gt ~ "Early (bottom 20%)",
              gtiming >= q80_gt ~ "Late (top 20%)",
              TRUE              ~ "On-time (middle 60%)"
            ))
gmv_long_timing_g <- gmv_long_g %>% left_join(timing_groups_g, by=c("ID","gtiming"))

# ---- A1) Absolute GMV means + 95% CIs (no raw) ----
summs_gt <- gmv_long_timing_g %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm=TRUE),
            se_k   = sd(GMV_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

pA_timing_means_g <- ggplot(summs_gt, aes(wave, mean_k, color=timing_group, fill=timing_group)) +
  geom_ribbon(aes(ymin=mean_k-1.96*se_k, ymax=mean_k+1.96*se_k),
              alpha=.15, linewidth=0, show.legend=FALSE) +
  geom_line(linewidth=1.8) +
  geom_point(size=2.2) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("W1","W2","W3","W4")) +
  scale_color_manual(values=cols_timing) +
  scale_fill_manual(values=cols_timing) +
  labs(x="Wave", y="Total GMV (÷1,000)", color=NULL,
       title="Girls · GMV (absolute) by pubertal timing (20/60/20)") +
  theme_base

# ---- B1) Δ from baseline means + 95% CIs with model-implied overlay ----
delta_timing_g <- gmv_long_timing_g %>%
  group_by(ID) %>% mutate(GMV0_k = GMV_k[wave==1][1]) %>% ungroup() %>%
  filter(!is.na(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

summs_delta_gt <- delta_timing_g %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_d = mean(Delta_k, na.rm=TRUE),
            se_d   = sd(Delta_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

# Empirical slope_k ~ gtiming effect (for dashed model-implied lines)
id_slopes_gt <- gmv_long_timing_g %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(ID, gtiming, timing_group) %>%
  filter(n() >= 2) %>%
  do({ fit <- lm(GMV_k ~ t, data=.); tibble(slope_k = coef(fit)[["t"]]) }) %>%
  ungroup()
b_hat_gt <- unname(coef(lm(slope_k ~ gtiming, data=id_slopes_gt))["gtiming"])
mu_S_k_g  <- mean(id_slopes_gt$slope_k, na.rm=TRUE)

shift_early_gt <- as.numeric(q20_gt - mu_gt)
shift_late_gt  <- as.numeric(q80_gt - mu_gt)
shifts_gt <- c("Early (bottom 20%)"=shift_early_gt,
               "On-time (middle 60%)"=0,
               "Late (top 20%)"=shift_late_gt)

pred_delta_gt <- tidyr::expand_grid(timing_group = names(shifts_gt), wave = c(1,2,3,4)) %>%
  mutate(t       = tload[as.character(wave)],
         slope_k = mu_S_k_g + b_hat_gt * shifts_gt[timing_group],
         Delta_k = slope_k * t,
         timing_group = factor(timing_group, levels = names(shifts_gt)))

pB_timing_delta_g <- ggplot(summs_delta_gt, aes(wave, mean_d, color=timing_group, fill=timing_group)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey40") +
  geom_ribbon(aes(ymin=mean_d-1.96*se_d, ymax=mean_d+1.96*se_d),
              alpha=.18, linewidth=0, show.legend=FALSE) +
  geom_line(linewidth=2) +
  geom_point(size=2.4) +
  # model-implied dashed overlay
  geom_line(data=pred_delta_gt, aes(wave, Delta_k, color=timing_group),
            linewidth=1.2, linetype="dashed", inherit.aes=FALSE) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("W1","W2","W3","W4")) +
  scale_y_continuous(limits=c(-100, 0), breaks=seq(0, -100, by=-20)) +
  scale_color_manual(values=cols_timing) +
  scale_fill_manual(values=cols_timing) +
  labs(x="Wave", y="Δ GMV from W1 (÷1,000)", color=NULL,
       title="Girls · Δ GMV by pubertal timing (20/60/20)",
       subtitle=sprintf("Dashed = model-implied (slope_k ~ gtiming; b = %.3f)", b_hat_gt)) +
  theme_base

# ---------------- Tempo groups (20/60/20) ----------------
q20_gte <- quantile(girls$gtempo, 0.20, na.rm=TRUE)
q80_gte <- quantile(girls$gtempo, 0.80, na.rm=TRUE)
mu_gte  <- mean(girls$gtempo, na.rm=TRUE)

tempo_groups_g <- girls %>%
  transmute(ID, gtempo,
            tempo_group = case_when(
              gtempo <= q20_gte ~ "Slow (bottom 20%)",
              gtempo >= q80_gte ~ "Fast (top 20%)",
              TRUE              ~ "Typical (middle 60%)"
            ))
gmv_long_tempo_g <- gmv_long_g %>% left_join(tempo_groups_g, by=c("ID","gtempo"))

# ---- A2) Absolute GMV means + 95% CIs (no raw) ----
summs_gte <- gmv_long_tempo_g %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm=TRUE),
            se_k   = sd(GMV_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

pA_tempo_means_g <- ggplot(summs_gte, aes(wave, mean_k, color=tempo_group, fill=tempo_group)) +
  geom_ribbon(aes(ymin=mean_k-1.96*se_k, ymax=mean_k+1.96*se_k),
              alpha=.15, linewidth=0, show.legend=FALSE) +
  geom_line(linewidth=1.8) +
  geom_point(size=2.2) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("W1","W2","W3","W4")) +
  scale_color_manual(values=cols_tempo) +
  scale_fill_manual(values=cols_tempo) +
  labs(x="Wave", y="Total GMV (÷1,000)", color=NULL,
       title="Girls · GMV (absolute) by pubertal tempo (20/60/20)") +
  theme_base

# ---- B2) Δ from baseline means + 95% CIs with model-implied overlay ----
delta_tempo_g <- gmv_long_tempo_g %>%
  group_by(ID) %>% mutate(GMV0_k = GMV_k[wave==1][1]) %>% ungroup() %>%
  filter(!is.na(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

summs_delta_gte <- delta_tempo_g %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_d = mean(Delta_k, na.rm=TRUE),
            se_d   = sd(Delta_k,   na.rm=TRUE)/sqrt(n),
            .groups="drop")

# Empirical slope_k ~ gtempo effect (for dashed model-implied lines)
id_slopes_gte <- gmv_long_tempo_g %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(ID, gtempo, tempo_group) %>%
  filter(n() >= 2) %>%
  do({ fit <- lm(GMV_k ~ t, data=.); tibble(slope_k = coef(fit)[["t"]]) }) %>%
  ungroup()
b_hat_gte <- unname(coef(lm(slope_k ~ gtempo, data=id_slopes_gte))["gtempo"])
mu_S_k_gte <- mean(id_slopes_gte$slope_k, na.rm=TRUE)

shift_slow_gte <- as.numeric(q20_gte - mu_gte)
shift_fast_gte  <- as.numeric(q80_gte - mu_gte)
shifts_gte <- c("Slow (bottom 20%)"=shift_slow_gte,
                "Typical (middle 60%)"=0,
                "Fast (top 20%)"=shift_fast_gte)

pred_delta_gte <- tidyr::expand_grid(tempo_group = names(shifts_gte), wave = c(1,2,3,4)) %>%
  mutate(t       = tload[as.character(wave)],
         slope_k = mu_S_k_gte + b_hat_gte * shifts_gte[tempo_group],
         Delta_k = slope_k * t,
         tempo_group = factor(tempo_group, levels = names(shifts_gte)))

pB_tempo_delta_g <- ggplot(summs_delta_gte, aes(wave, mean_d, color=tempo_group, fill=tempo_group)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey40") +
  geom_ribbon(aes(ymin=mean_d-1.96*se_d, ymax=mean_d+1.96*se_d),
              alpha=.18, linewidth=0, show.legend=FALSE) +
  geom_line(linewidth=2) +
  geom_point(size=2.4) +
  # model-implied dashed overlay
  geom_line(data=pred_delta_gte, aes(wave, Delta_k, color=tempo_group),
            linewidth=1.2, linetype="dashed", inherit.aes=FALSE) +
  scale_x_continuous(breaks=c(1,2,3,4), labels=c("W1","W2","W3","W4")) +
  scale_y_continuous(limits=c(-100, 0), breaks=seq(0, -100, by=-20)) +
  scale_color_manual(values=cols_tempo) +
  scale_fill_manual(values=cols_tempo) +
  labs(x="Wave", y="Δ GMV from W1 (÷1,000)", color=NULL,
       title="Girls · Δ GMV by pubertal tempo (20/60/20)",
       subtitle=sprintf("Dashed = model-implied (slope_k ~ gtempo; b = %.3f)", b_hat_gte)) +
  theme_base

# ----------- Show -----------
pA_timing_means_g
pB_timing_delta_g
pA_tempo_means_g
pB_tempo_delta_g
# Or as panels:
(pA_timing_means_g | pA_tempo_means_g) / (pB_timing_delta_g | pB_tempo_delta_g)





library(dplyr); library(tidyr); library(ggplot2)

# Sentinels -> NA
df <- df %>% mutate(across(where(is.numeric), ~ na_if(., -999)))

# Try to find your ELA column name
ela_name <- intersect(names(df), c("ELA_PLUS","ela_plus","ELA","ela"))
if (length(ela_name) == 0) stop("No ELA column found. Rename your ELA variable to ELA_PLUS (or add it to 'ela_name').")
ela_name <- ela_name[1]

# Boys subset and long GMV (scaled)
boys <- df %>%
  filter(!is.na(btiming), !is.na(btempo)) %>%
  select(ID, btiming, btempo, all_of(ela_name),
         age_1, age_2, age_3, age_4,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_b <- boys %>%
  pivot_longer(c(T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV),
               names_to="wave_gmv", values_to="GMV") %>%
  pivot_longer(c(age_1, age_2, age_3, age_4),
               names_to="wave_age", values_to="age") %>%
  filter(substr(wave_gmv,2,2) == substr(wave_age,5,5)) %>%
  mutate(wave = as.integer(substr(wave_gmv,2,2)),
         GMV_k = GMV/1000) %>%
  select(ID, btiming, btempo, ELA = all_of(ela_name), wave, age, GMV_k) %>%
  arrange(ID, wave) %>%
  filter(!is.na(GMV_k))

# Latent-basis time scores
tload <- c(`1`=0.000, `2`=0.215, `3`=0.556, `4`=1.000)

theme_base <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))
