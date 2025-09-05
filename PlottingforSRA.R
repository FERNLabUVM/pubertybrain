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
df <- read.csv("dfmerged.csv")
names(df)

df <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999)))

# --- choose girls (robust to different encodings) and require timing/tempo ---
library(dplyr)
library(tidyr)

# Keep girls with timing/tempo + GMV & age fields
girls <- df %>%
  filter(!is.na(gtiming), !is.na(gtempo)) %>%
  select(ID, gtiming, gtempo,
         age_1, age_2, age_3, age_4,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

# Long GMV by wave with the matching age
gmv_long <- girls %>%
  pivot_longer(cols = c(T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV),
               names_to = "wave_gmv", values_to = "GMV") %>%
  pivot_longer(cols = c(age_1, age_2, age_3, age_4),
               names_to = "wave_age", values_to = "age") %>%
  filter(substr(wave_gmv, 2, 2) == substr(wave_age, 5, 5)) %>%
  mutate(wave = as.integer(substr(wave_gmv, 2, 2))) %>%
  select(ID, gtiming, gtempo, wave, age, GMV) %>%
  arrange(ID, wave) %>%
  filter(!is.na(GMV)) %>%
  mutate(GMV_k = GMV / 1000)   # scale by 1,000

# ==== TIMING GROUPS: 20/60/20 =================================================
q20 <- quantile(girls$gtiming, 0.20, na.rm = TRUE)
q80 <- quantile(girls$gtiming, 0.80, na.rm = TRUE)
muT <- mean(girls$gtiming, na.rm = TRUE)

timing_groups <- girls %>%
  transmute(
    ID, gtiming,
    timing_group = case_when(
      gtiming <= q20 ~ "Early (bottom 20%)",
      gtiming >= q80 ~ "Late (top 20%)",
      TRUE           ~ "On-time (middle 60%)"
    )
  )

gmv_long <- gmv_long %>% left_join(timing_groups, by = c("ID","gtiming"))

# Color-blind-safe palette
cols3 <- c("Early (bottom 20%)"   = "#D55E00",  # orange
           "On-time (middle 60%)" = "#7F7F7F",  # grey
           "Late (top 20%)"       = "#0072B2")  # blue

theme_base <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

# ==== (A) EMPIRICAL GMV TRAJECTORIES BY TIMING GROUP ==========================
summs <- gmv_long %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm = TRUE),
            se_k   = sd(GMV_k,   na.rm = TRUE)/sqrt(n),
            .groups = "drop")

pA <- ggplot() +
  geom_line(data = gmv_long, aes(wave, GMV_k, group = ID),
            alpha = 0.05, linewidth = 0.2, color = "grey35") +
  geom_point(data = gmv_long, aes(wave, GMV_k),
             alpha = 0.04, size = 0.6, color = "grey35") +
  geom_errorbar(data = summs,
                aes(wave, ymin = mean_k - 1.96*se_k, ymax = mean_k + 1.96*se_k, color = timing_group),
                width = .08, linewidth = .6) +
  geom_line(data = summs, aes(wave, mean_k, color = timing_group), linewidth = 1.8) +
  geom_point(data = summs, aes(wave, mean_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_color_manual(values = cols3) +
  labs(x = "Wave", y = "Total GMV (÷1,000)", color = NULL,
       title = "Empirical GMV trajectories (girls) by pubertal timing (20/60/20)") +
  theme_base

pA

# --- Robust anchors (scaled) ---
mu_I_k <- mean(gmv_long$GMV_k[gmv_long$wave == 1], na.rm = TRUE)
mu_F_k <- mean(gmv_long$GMV_k[gmv_long$wave == 4], na.rm = TRUE)
mu_S_k <- mu_F_k - mu_I_k   # average total change W1->W4 (should be negative)

# --- Shifts for 20/60/20 groups relative to the mean timing ---
shift_early <- as.numeric(q20 - muT)
shift_late  <- as.numeric(q80 - muT)

shifts <- c("Early (bottom 20%)"   = shift_early,
            "On-time (middle 60%)" = 0,
            "Late (top 20%)"       = shift_late)

# Ensure consistent factor levels for colors
lvl3 <- names(shifts)

# --- Model-implied predictions (absolute GMV) ---
pred <- tidyr::expand_grid(timing_group = lvl3, wave = c(1,2,3,4)) |>
  dplyr::mutate(
    t        = tload[as.character(wave)],
    slope_k  = mu_S_k + beta_timing_k * shifts[timing_group],
    GMV_k    = mu_I_k + slope_k * t,
    timing_group = factor(timing_group, levels = lvl3)
  )

pB_abs <- ggplot() +
  geom_line(data = gmv_long, aes(wave, GMV_k, group = ID),
            alpha = 0.05, linewidth = 0.2, color = "grey40") +
  geom_point(data = gmv_long, aes(wave, GMV_k),
             alpha = 0.04, size = 0.5, color = "grey40") +
  geom_line(data = pred, aes(wave, GMV_k, color = timing_group), linewidth = 2) +
  geom_point(data = pred, aes(wave, GMV_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_color_manual(values = cols3) +
  labs(x = "Wave", y = "Total GMV (÷1,000)", color = NULL,
       title = "Model-implied GMV (girls): absolute levels",
       subtitle = sprintf("Loads: 0,.215,.556,1.0 · SLOPE ON timing b = %.3f (÷1,000); groups = 20/60/20",
                          beta_timing_raw)) +
  theme_base

# --- Optional: emphasize differences as Δ from baseline (easier to see) ---
# Model-implied ΔGMV from baseline is just slope_k * t (starts at 0)
pred_delta <- pred |>
  dplyr::mutate(Delta_k = slope_k * t)

# If you want raw Δ background, compute per-ID baseline and subtract:
raw_delta <- gmv_long |>
  dplyr::group_by(ID) |>
  dplyr::mutate(GMV0_k = GMV_k[wave == 1][1]) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(GMV0_k)) |>
  dplyr::mutate(Delta_k = GMV_k - GMV0_k)

pB_delta <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_line(data = raw_delta, aes(wave, Delta_k, group = ID),
            alpha = 0.05, linewidth = 0.2, color = "grey40") +
  geom_point(data = raw_delta, aes(wave, Delta_k),
             alpha = 0.04, size = 0.5, color = "grey40") +
  geom_line(data = pred_delta, aes(wave, Delta_k, color = timing_group), linewidth = 2) +
  geom_point(data = pred_delta, aes(wave, Delta_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_color_manual(values = cols3) +
  labs(x = "Wave", y = "Δ GMV from baseline (÷1,000)", color = NULL,
       title = "Model-implied GMV change (girls): Δ from baseline",
       subtitle = "Same baseline for all groups; lines differ only in change (slope)") +
  theme_base

# Draw
pB_abs
pB_delta

pB

library(dplyr); library(tidyr); library(ggplot2)

# --- latent-basis time scores
tload <- c(`1`=0.000, `2`=0.215, `3`=0.556, `4`=1.000)

# gmv_long: girls-only long table from earlier, with GMV_k = GMV/1000
# timing groups already defined via 20/60/20 cutoffs:
q20 <- quantile(girls$gtiming, 0.20, na.rm = TRUE)
q80 <- quantile(girls$gtiming, 0.80, na.rm = TRUE)
muT <- mean(girls$gtiming, na.rm = TRUE)

gmv_long <- gmv_long %>%
  mutate(GMV_k = GMV / 1000)

# 1) Per-ID slope (latent-basis) in *your* units
id_slopes <- gmv_long %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(ID, gtiming) %>%
  filter(n() >= 2) %>%
  do({
    fit <- lm(GMV_k ~ t, data = .)
    tibble(slope_k = coef(fit)[["t"]])   # total change W1→W4 (scaled)
  }) %>%
  ungroup()

# 2) Regress slope on gtiming to get the effect in your units
fit_slope_timing <- lm(slope_k ~ gtiming, data = id_slopes)
b_hat  <- unname(coef(fit_slope_timing)["gtiming"])         # unstd effect in GMV_k units / year
mu_S_k <- mean(id_slopes$slope_k, na.rm = TRUE)             # mean total change W1→W4 (scaled)
mu_I_k <- mean(gmv_long$GMV_k[gmv_long$wave==1], na.rm=TRUE) # baseline mean (scaled)

# 3) Build model-implied trajectories for timing groups using 20/60/20 shifts
shift_early <- as.numeric(q20 - muT)
shift_late  <- as.numeric(q80 - muT)

shifts <- c("Early (bottom 20%)"   = shift_early,
            "On-time (middle 60%)" = 0,
            "Late (top 20%)"       = shift_late)

pred_delta <- tidyr::expand_grid(timing_group = names(shifts), wave = c(1,2,3,4)) %>%
  mutate(t        = tload[as.character(wave)],
         slope_k  = mu_S_k + b_hat * shifts[timing_group],
         Delta_k  = slope_k * t,
         timing_group = factor(timing_group, levels = names(shifts)))

# (optional) raw Δ background for context
raw_delta <- gmv_long %>%
  group_by(ID) %>%
  mutate(GMV0_k = GMV_k[wave==1][1]) %>%
  ungroup() %>%
  filter(!is.na(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

# Color-blind-safe palette
cols3 <- c("Early (bottom 20%)"="#D55E00", "On-time (middle 60%)"="#7F7F7F", "Late (top 20%)"="#0072B2")

# 4) Plot Δ from baseline (differences pop out)
pB_delta <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_line(data = raw_delta, aes(wave, Delta_k, group = ID),
            alpha = 0.05, linewidth = 0.2, color = "grey40") +
  geom_point(data = raw_delta, aes(wave, Delta_k),
             alpha = 0.04, size = 0.5, color = "grey40") +
  geom_line(data = pred_delta, aes(wave, Delta_k, color = timing_group), linewidth = 2) +
  geom_point(data = pred_delta, aes(wave, Delta_k, color = timing_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_color_manual(values = cols3) +
  labs(x = "Wave", y = "Δ GMV from baseline (÷1,000)", color = NULL,
       title = "Model-implied GMV change (girls): Δ from baseline",
       subtitle = sprintf("Effect estimated from data: slope_k ~ gtiming (b = %.3f)", b_hat)) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

pB_delta

pB_delta +
  scale_y_continuous(limits = c(-100, 0),
                     breaks = seq(0, -100, by = -20))


# ---- 20/60/20 groups on tempo ----
q20_t <- quantile(girls$gtempo, 0.20, na.rm = TRUE)
q80_t <- quantile(girls$gtempo, 0.80, na.rm = TRUE)
muT_t <- mean(girls$gtempo, na.rm = TRUE)

tempo_groups_206020 <- girls %>%
  transmute(
    ID, gtempo,
    tempo_group = case_when(
      gtempo <= q20_t ~ "Slow (bottom 20%)",
      gtempo >= q80_t ~ "Fast (top 20%)",
      TRUE            ~ "Typical (middle 60%)"
    )
  )

gmv_long_t <- gmv_long %>% left_join(tempo_groups_206020, by = c("ID","gtempo"))

# ---- Per-ID GMV slope (latent-basis proxy) & slope~tempo effect ----
tload <- c(`1`=0.000, `2`=0.215, `3`=0.556, `4`=1.000)

id_slopes_t <- gmv_long_t %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(ID, gtempo, tempo_group) %>%
  filter(n() >= 2) %>%
  do({
    fit <- lm(GMV_k ~ t, data = .)
    tibble(slope_k = coef(fit)[["t"]])
  }) %>% ungroup()

fit_slope_tempo <- lm(slope_k ~ gtempo, data = id_slopes_t)
b_hat_tempo <- unname(coef(fit_slope_tempo)["gtempo"])   # effect in GMV_k units / (tempo unit)

# ---- Model-implied Δ from baseline for 20/60/20 tempo shifts ----
mu_S_k <- mean(id_slopes_t$slope_k, na.rm = TRUE)                     # mean W4→W1 change
shift_slow <- as.numeric(q20_t - muT_t)
shift_fast <- as.numeric(q80_t - muT_t)

shifts_t <- c("Slow (bottom 20%)"   = shift_slow,
              "Typical (middle 60%)"= 0,
              "Fast (top 20%)"      = shift_fast)

pred_delta_t <- tidyr::expand_grid(tempo_group = names(shifts_t), wave = c(1,2,3,4)) %>%
  mutate(t       = tload[as.character(wave)],
         slope_k = mu_S_k + b_hat_tempo * shifts_t[tempo_group],
         Delta_k = slope_k * t,
         tempo_group = factor(tempo_group, levels = names(shifts_t)))

# ---- Raw Δ background for context ----
raw_delta_t <- gmv_long_t %>%
  group_by(ID) %>%
  mutate(GMV0_k = GMV_k[wave==1][1]) %>%
  ungroup() %>%
  filter(!is.na(GMV0_k)) %>%
  mutate(Delta_k = GMV_k - GMV0_k)

# ---- Plot (Δ from baseline) with y 0…−100 ----
cols_t <- c("Slow (bottom 20%)"="#0072B2",   # blue
            "Typical (middle 60%)"="#7F7F7F",# grey
            "Fast (top 20%)"="#D55E00")      # orange

pTempo_delta <- ggplot() +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_line(data = raw_delta_t, aes(wave, Delta_k, group = ID),
            alpha = 0.05, linewidth = 0.2, color = "grey40") +
  geom_point(data = raw_delta_t, aes(wave, Delta_k),
             alpha = 0.04, size = 0.5, color = "grey40") +
  geom_line(data = pred_delta_t, aes(wave, Delta_k, color = tempo_group), linewidth = 2) +
  geom_point(data = pred_delta_t, aes(wave, Delta_k, color = tempo_group), size = 2.2) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = c("W1","W2","W3","W4")) +
  scale_y_continuous(limits = c(-100, 0), breaks = seq(0, -100, by = -20)) +
  scale_color_manual(values = cols_t) +
  labs(x = "Wave", y = "Δ GMV from baseline (÷1,000)", color = NULL,
       title = "Model-implied GMV change (girls) by tempo: Δ from baseline (20/60/20)",
       subtitle = sprintf("Effect from data: slope_k ~ gtempo  (b = %.3f)", b_hat_tempo)) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

pTempo_delta

library(dplyr); library(tidyr); library(ggplot2); library(patchwork)

# ---------- Clean & prep (boys) ----------
df <- df %>% mutate(across(where(is.numeric), ~ na_if(., -999)))

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
