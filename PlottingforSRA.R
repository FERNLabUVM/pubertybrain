rm(list=ls())
sessionInfo()

#open libraries
##install.packages("sitar")
library(haven)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(patchwork)

#set working directory
#MAC:
setwd("~/Documents/GitHub/pubertybrain")

#read in file!
df <- read.csv("dfmerged_mplus.csv")
names(df)

# Clean + label
df_ela <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999))) %>%
  filter(sex %in% c(1, 2), !is.na(ela_plus), ela_plus >= 0, ela_plus <= 10) %>%
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("Boys", "Girls")))

# assumes df_ela already made as in your last step
stats <- df_ela %>%
  group_by(sex) %>%
  summarise(N = dplyr::n(),
            mean = mean(ela_plus),
            sd   = sd(ela_plus),
            .groups = "drop")

# find a nice y position inside each facet (max bin count by sex)
counts_fac <- df_ela %>%
  count(sex, bin = cut(ela_plus, breaks = 0:10, right = FALSE, include.lowest = TRUE)) %>%
  group_by(sex) %>% summarise(ymax = max(n), .groups = "drop")

ann_fac <- stats %>%
  left_join(counts_fac, by = "sex") %>%
  mutate(
    x = 8.8,
    y = ymax * 0.92,
    label = sprintf("Mean = %.2f\nSD = %.2f", mean, sd)
  )

p_hist_faceted +
  geom_label(
    data = ann_fac,
    aes(x = x, y = y, label = label, fill = sex),
    color = "white", label.size = 0, alpha = 0.90, show.legend = FALSE
  )

# global y max to place labels nicely
counts_ov <- df_ela %>%
  count(sex, bin = cut(ela_plus, breaks = 0:10, right = FALSE, include.lowest = TRUE))
ymax_global <- max(counts_ov$n)

ann_ov <- stats %>%
  mutate(
    x = 8.8,
    y = ymax_global * c(0.92, 0.80)[as.integer(sex)],  # stagger Boys/Girls
    label = sprintf("%s: Mean = %.2f, SD = %.2f", sex, mean, sd)
  )

p_hist_overlay +
  geom_label(
    data = ann_ov,
    aes(x = x, y = y, label = label, fill = sex),
    color = "white", label.size = 0, alpha = 0.90
  )

# -----------------------------
# 0) Data prep (boys only)
# -----------------------------
boys <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999))) %>%
  filter(sex %in% c(1, "1")) %>%
  select(numeric_id, btiming, btempo,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_b <- boys %>%
  pivot_longer(cols = starts_with("T"), names_to = "wave_gmv", values_to = "GMV") %>%
  mutate(
    wave = as.integer(substr(wave_gmv, 2, 2)),
    GMV_k = GMV / 1000
  ) %>%
  filter(!is.na(GMV_k)) %>%
  select(numeric_id, btiming, btempo, wave, GMV_k)

names(gmv_long_b)

tload <- c(`1` = 0.000, `2` = 0.215, `3` = 0.556, `4` = 1.000)

# -----------------------------
# 1) 20/60/20 groups
# -----------------------------
# Timing
q20_timing_b <- quantile(gmv_long_b$btiming, 0.20, na.rm = TRUE)
q80_timing_b <- quantile(gmv_long_b$btiming, 0.80, na.rm = TRUE)

gmv_long_b <- gmv_long_b %>%
  mutate(timing_group = case_when(
    btiming <= q20_timing_b ~ "Earlier timing",
    btiming >= q80_timing_b ~ "Later timing",
    TRUE                    ~ "On-time"
  ))

# Tempo
q20_tempo_b <- quantile(gmv_long_b$btempo, 0.20, na.rm = TRUE)
q80_tempo_b <- quantile(gmv_long_b$btempo, 0.80, na.rm = TRUE)

gmv_long_b <- gmv_long_b %>%
  mutate(tempo_group = case_when(
    btempo <= q20_tempo_b ~ "Slower tempo",
    btempo >= q80_tempo_b ~ "Faster tempo",
    TRUE                  ~ "Typical tempo"
  ))

# Factor order (Typical in the middle for tempo)
gmv_long_b <- gmv_long_b %>%
  mutate(
    timing_group = factor(timing_group,
                          levels = c("Earlier timing","On-time","Later timing")),
    tempo_group  = factor(tempo_group,
                          levels = c("Faster tempo","Typical tempo","Slower tempo"))
  )

# -----------------------------
# 2) Summaries (means + 95% CI)
# -----------------------------
# Absolute GMV (A panels): simple means/SEs
sum_timing_abs <- gmv_long_b %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm = TRUE),
            se_k   = sd(GMV_k,   na.rm = TRUE)/sqrt(n),
            .groups = "drop") %>%
  mutate(lo = mean_k - 1.96*se_k,
         hi = mean_k + 1.96*se_k)

sum_tempo_abs <- gmv_long_b %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm = TRUE),
            se_k   = sd(GMV_k,   na.rm = TRUE)/sqrt(n),
            .groups = "drop") %>%
  mutate(lo = mean_k - 1.96*se_k,
         hi = mean_k + 1.96*se_k)

# ---- LGM-based Δ from baseline (B panels) ---------------------------------
# 1) Per-ID latent-basis slope using Mplus time scores
lgm_slopes_b <- gmv_long_b %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(numeric_id, btiming, btempo, timing_group, tempo_group) %>%
  filter(n() >= 2) %>%  # need ≥ 2 waves to estimate a slope
  summarise(slope_k = coef(lm(GMV_k ~ t))[["t"]], .groups = "drop")

# 2) Predicted Δ at each wave: Δ = slope × t
ids_b <- lgm_slopes_b %>% distinct(numeric_id, timing_group, tempo_group)
lgm_delta_b <- tidyr::expand_grid(ids_b, wave = 1:4) %>%
  mutate(t = tload[as.character(wave)]) %>%
  left_join(lgm_slopes_b, by = c("numeric_id","timing_group","tempo_group")) %>%
  mutate(Delta_k = slope_k * t)

# 3) Summaries (means + 95% CI) for Δ by group
sum_timing_delta <- lgm_delta_b %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_d = mean(Delta_k, na.rm = TRUE),
            se_d   = sd(Delta_k,   na.rm = TRUE)/sqrt(n),
            .groups = "drop") %>%
  mutate(lo = mean_d - 1.96*se_d,
         hi = mean_d + 1.96*se_d)

sum_tempo_delta <- lgm_delta_b %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_d = mean(Delta_k, na.rm = TRUE),
            se_d   = sd(Delta_k,   na.rm = TRUE)/sqrt(n),
            .groups = "drop") %>%
  mutate(lo = mean_d - 1.96*se_d,
         hi = mean_d + 1.96*se_d)

# -----------------------------
# 3) Styling
# -----------------------------
cols_timing <- c("Earlier timing" = "#D55E00",
                 "On-time"        = "#7F7F7F",
                 "Later timing"   = "#0072B2")

cols_tempo  <- c("Faster tempo"   = "#D55E00",
                 "Typical tempo"  = "#7F7F7F",
                 "Slower tempo"   = "#0072B2")

theme_base <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

x_sc <- scale_x_continuous(breaks = 1:4, labels = c("Baseline","Year 2","Year 4","Year 6"))

# -----------------------------
# 4) Plots (4 panels)
# -----------------------------
# A1: Absolute GMV by timing
pA1 <- ggplot(sum_timing_abs, aes(wave, mean_k, color = timing_group, fill = timing_group)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  x_sc +
  scale_color_manual(name = "Timing", values = cols_timing) +
  scale_fill_manual( name = "Timing", values = cols_timing) +
  labs(title = NULL, x = "Year", y = "Total GMV (÷1,000)") +
  theme_base

# A2: Absolute GMV by tempo
pA2 <- ggplot(sum_tempo_abs, aes(wave, mean_k, color = tempo_group, fill = tempo_group)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  x_sc +
  scale_color_manual(name = "Tempo", values = cols_tempo) +
  scale_fill_manual( name = "Tempo", values = cols_tempo) +
  labs(title = NULL, x = "Year", y = "Total GMV (÷1,000)") +
  theme_base

# B1: Δ GMV (LGM-based) by timing
pB1 <- ggplot(sum_timing_delta, aes(wave, mean_d, color = timing_group, fill = timing_group)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  x_sc +
  scale_color_manual(name = "Boys' Timing", values = cols_timing) +
  scale_fill_manual( name = "Boys' Timing", values = cols_timing) +
  scale_y_continuous(limits = c(-70, 10), breaks = seq(10, -70, by = -10)) +
  labs(title = NULL, x = "Year", y = "Δ GMV from Baseline") +
  theme_base

# B2: Δ GMV (LGM-based) by tempo
pB2 <- ggplot(sum_tempo_delta, aes(wave, mean_d, color = tempo_group, fill = tempo_group)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  x_sc +
  scale_color_manual(name = "Boys' Tempo", values = cols_tempo) +
  scale_fill_manual( name = "Boys'Tempo", values = cols_tempo) +
  scale_y_continuous(limits = c(-100, 10), breaks = seq(10, -70, by = -10)) +
  labs(title = NULL, x = "Year", y = "Δ GMV from Baseline") +
  theme_base

# Arrange 2x2
#(pA1 | pA2) / (pB1 | pB2)

pB1
pB2

library(grid)  # for unit()

# 1) A bigger, cleaner theme for slides
theme_pres <- theme_minimal(base_size = 18) +
  theme(
    axis.title       = element_text(size = 18, face = "bold"),
    axis.text        = element_text(size = 16),
    legend.title     = element_text(size = 18, face = "bold"),
    legend.text      = element_text(size = 15),
    legend.key.size  = unit(12, "pt"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.4)
  )

# 2) Thicker default geoms (applies to plots created AFTER these calls)
update_geom_defaults("line",   list(linewidth = 20))
update_geom_defaults("point",  list(size = 8))
update_geom_defaults("ribbon", list(alpha = 0.25))
update_geom_defaults("hline",  list(linewidth = 5, linetype = 2))

# 3) Helper to beef up legend keys (lines/points) on any plot
thick_legend <- guides(
  color = guide_legend(override.aes = list(linewidth = 3, size = 2)),
  fill  = guide_legend(override.aes = list(alpha = 0.25))
)

pA1 <- pA1 + theme_pres + thick_legend
pA2 <- pA2 + theme_pres + thick_legend
pB1 <- pB1 + theme_pres + thick_legend
pB2 <- pB2 + theme_pres + thick_legend

pB1
pB2

# -----------------------------
# 0) Data prep (girls only)
# -----------------------------
girls <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999))) %>%
  filter(sex %in% c(2, "2")) %>%
  select(numeric_id, gtiming, gtempo,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_g <- girls %>%
  pivot_longer(cols = starts_with("T"), names_to = "wave_gmv", values_to = "GMV") %>%
  mutate(
    wave  = as.integer(substr(wave_gmv, 2, 2)),
    GMV_k = GMV / 1000
  ) %>%
  filter(!is.na(GMV_k)) %>%
  select(numeric_id, gtiming, gtempo, wave, GMV_k)

# GIRLS: Mplus latent-basis time scores for SLOPE
tload <- c(`1` = 0.000, `2` = 0.210, `3` = 0.684, `4` = 1.000)

# -----------------------------
# 1) 20/60/20 groups
# -----------------------------
# Timing
q20_timing_g <- quantile(gmv_long_g$gtiming, 0.20, na.rm = TRUE)
q80_timing_g <- quantile(gmv_long_g$gtiming, 0.80, na.rm = TRUE)

gmv_long_g <- gmv_long_g %>%
  mutate(timing_group = case_when(
    gtiming <= q20_timing_g ~ "Earlier timing",
    gtiming >= q80_timing_g ~ "Later timing",
    TRUE                    ~ "On-time"
  ))

# Tempo
q20_tempo_g <- quantile(gmv_long_g$gtempo, 0.20, na.rm = TRUE)
q80_tempo_g <- quantile(gmv_long_g$gtempo, 0.80, na.rm = TRUE)

gmv_long_g <- gmv_long_g %>%
  mutate(tempo_group = case_when(
    gtempo <= q20_tempo_g ~ "Slower tempo",
    gtempo >= q80_tempo_g ~ "Faster tempo",
    TRUE                  ~ "Typical tempo"
  ))

# Factor order (Typical in the middle for tempo)
gmv_long_g <- gmv_long_g %>%
  mutate(
    timing_group = factor(timing_group,
                          levels = c("Earlier timing","On-time","Later timing")),
    tempo_group  = factor(tempo_group,
                          levels = c("Faster tempo","Typical tempo","Slower tempo"))
  )

# -----------------------------
# 2) Summaries (means + 95% CI)
# -----------------------------
# Absolute GMV (A panels): simple means/SEs
sum_timing_abs <- gmv_long_g %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm = TRUE),
            se_k   = sd(GMV_k,   na.rm = TRUE)/sqrt(n),
            .groups = "drop") %>%
  mutate(lo = mean_k - 1.96*se_k,
         hi = mean_k + 1.96*se_k)

sum_tempo_abs <- gmv_long_g %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_k = mean(GMV_k, na.rm = TRUE),
            se_k   = sd(GMV_k,   na.rm = TRUE)/sqrt(n),
            .groups = "drop") %>%
  mutate(lo = mean_k - 1.96*se_k,
         hi = mean_k + 1.96*se_k)

# ---- LGM-based Δ from baseline (B panels) ---------------------------------
# 1) Per-ID latent-basis slope using GIRLS Mplus time scores
lgm_slopes_g <- gmv_long_g %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(numeric_id, gtiming, gtempo, timing_group, tempo_group) %>%
  filter(n() >= 2) %>%  # need ≥ 2 waves to estimate a slope
  summarise(slope_k = coef(lm(GMV_k ~ t))[["t"]], .groups = "drop")

# 2) Predicted Δ at each wave: Δ = slope × t
ids_g <- lgm_slopes_g %>% distinct(numeric_id, timing_group, tempo_group)
lgm_delta_g <- tidyr::expand_grid(ids_g, wave = 1:4) %>%
  mutate(t = tload[as.character(wave)]) %>%
  left_join(lgm_slopes_g, by = c("numeric_id","timing_group","tempo_group")) %>%
  mutate(Delta_k = slope_k * t)

# 3) Summaries (means + 95% CI) for Δ by group
sum_timing_delta <- lgm_delta_g %>%
  group_by(timing_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_d = mean(Delta_k, na.rm = TRUE),
            se_d   = sd(Delta_k,   na.rm = TRUE)/sqrt(n),
            .groups = "drop") %>%
  mutate(lo = mean_d - 1.96*se_d,
         hi = mean_d + 1.96*se_d)

sum_tempo_delta <- lgm_delta_g %>%
  group_by(tempo_group, wave) %>%
  summarise(n = dplyr::n(),
            mean_d = mean(Delta_k, na.rm = TRUE),
            se_d   = sd(Delta_k,   na.rm = TRUE)/sqrt(n),
            .groups = "drop") %>%
  mutate(lo = mean_d - 1.96*se_d,
         hi = mean_d + 1.96*se_d)

# -----------------------------
# 3) Styling (presentation)
# -----------------------------
cols_timing <- c("Earlier timing" = "#D55E00",
                 "On-time"        = "#7F7F7F",
                 "Later timing"   = "#0072B2")

cols_tempo  <- c("Faster tempo"   = "#D55E00",
                 "Typical tempo"  = "#7F7F7F",
                 "Slower tempo"   = "#0072B2")

x_sc <- scale_x_continuous(breaks = 1:4, labels = c("Baseline","Year 2","Year 4","Year 6"))

# -----------------------------
# 4) Plots (4 panels)
# -----------------------------
# A1: Absolute GMV by timing
pA1_g <- ggplot(sum_timing_abs, aes(wave, mean_k, color = timing_group, fill = timing_group)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), color = NA) +
  geom_line() +
  geom_point() +
  x_sc +
  scale_color_manual(name = "Timing", values = cols_timing) +
  scale_fill_manual( name = "Timing", values = cols_timing) +
  labs(title = NULL, x = "Year", y = "Total GMV (÷1,000)") +
  theme_pres + thick_legend

# A2: Absolute GMV by tempo
pA2_g <- ggplot(sum_tempo_abs, aes(wave, mean_k, color = tempo_group, fill = tempo_group)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), color = NA) +
  geom_line() +
  geom_point() +
  x_sc +
  scale_color_manual(name = "Tempo", values = cols_tempo) +
  scale_fill_manual( name = "Tempo", values = cols_tempo) +
  labs(title = NULL, x = "Year", y = "Total GMV (÷1,000)") +
  theme_pres + thick_legend

# B1: Δ GMV (LGM-based) by timing
pB1_g <- ggplot(sum_timing_delta, aes(wave, mean_d, color = timing_group, fill = timing_group)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  x_sc +
  scale_color_manual(name = "Girls' Timing", values = cols_timing) +
  scale_fill_manual( name = "Girls' Timing", values = cols_timing) +
  scale_y_continuous(limits = c(-70, 10), breaks = seq(10, -70, by = -10)) +
  labs(title = NULL, x = "Year", y = "Δ GMV from Baseline") +
  theme_base

pB1_g

# B2: Δ GMV (LGM-based) by tempo
pB2_g <- ggplot(sum_tempo_delta, aes(wave, mean_d, color = tempo_group, fill = tempo_group)) +
  geom_hline(yintercept = 0, , linetype = 2, linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), color = NA) +
  geom_line() +
  geom_point() +
  x_sc +
  scale_color_manual(name = "Tempo", values = cols_tempo) +
  scale_fill_manual( name = "Tempo", values = cols_tempo) +
  scale_y_continuous(limits = c(-100, 10), breaks = seq(10, -100, by = -10)) +
  labs(title = NULL, x = "Year", y = "Δ GMV from Baseline") +
  theme_pres + thick_legend

# Arrange 2x2 (or print individually)
# (pA1_g | pA2_g) / (pB1_g | pB2_g)

pA1_g; 
pA2_g; 
pB1_g <- pB1_g + theme_pres + thick_legend 
pB1_g
pB1

(pB1 / pB1_g) 



library(dplyr)
library(tidyr)
library(ggplot2)

# 1) Girls + clean + long GMV (÷1,000)
girls <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999))) %>%
  filter(sex %in% c(2, "2")) %>%
  select(numeric_id, gtempo,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_g <- girls %>%
  pivot_longer(starts_with("T"), names_to = "wave_gmv", values_to = "GMV") %>%
  mutate(
    wave  = as.integer(substr(wave_gmv, 2, 2)),
    GMV_k = GMV / 1000
  ) %>%
  filter(!is.na(GMV_k)) %>%
  select(numeric_id, gtempo, wave, GMV_k)

# 2) Mplus latent-basis time scores (girls)
tload <- c(`1` = 0.000, `2` = 0.210, `3` = 0.684, `4` = 1.000)

# 3) Per-ID latent-basis slopes (need ≥ 2 waves)
slopes_g <- gmv_long_g %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(numeric_id) %>%
  filter(n() >= 2) %>%
  summarise(slope_k = coef(lm(GMV_k ~ t))[["t"]], .groups = "drop") %>%
  left_join(girls %>% distinct(numeric_id, gtempo), by = "numeric_id") %>%
  filter(!is.na(gtempo))

range(gmv_long_g$gtempo, na.rm = TRUE)   # returns c(min, max)

# 4) Scatter: tempo vs. individual slope
ggplot(slopes_g, aes(gtempo, slope_k)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey50") +
  geom_point(alpha = 0.35, size = 2, color = "grey20") +
  scale_y_continuous(limits = c(-120, 50), breaks = seq(-120, 50, by = 20)) +
  scale_x_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, by = .2)) +
  
  labs(
    x = "Pubertal tempo",
    y = "Model estimated GMV slope"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

# --- Mplus-implied line from covariance -------------------------
S_mean    <- -68.624
mu_gtempo <-  0.687
cov_S_gT  <-  0.252
var_gT    <-  0.020

b_mplus        <- cov_S_gT / var_gT         # 12.6
intercept_mplus<- S_mean

# Option 1: simple abline (no legend)
p_scatter <- ggplot(slopes_g, aes(gtempo, slope_k)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey50") +
  geom_point(alpha = 0.35, size = 2, color = "darksalmon") +
  geom_abline(intercept = intercept_mplus, slope = b_mplus,
              linewidth = 1.4, color = "grey0") +
  scale_y_continuous(limits = c(-120, 50), breaks = seq(-120, 50, 20)) +
  scale_x_continuous(limits = c(-.5, 1.5),  breaks = seq(-.5, 1.5,  .2)) +
  labs(x = "Pubertal tempo", y = "Model estimated GMV slope") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

p_scatter

library(dplyr)
library(tidyr)
library(ggplot2)

# 1) Boys + clean + long GMV (÷1,000)
boys <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999))) %>%
  filter(sex %in% c(1, "1")) %>%
  select(numeric_id, btempo,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_b <- boys %>%
  pivot_longer(starts_with("T"), names_to = "wave_gmv", values_to = "GMV") %>%
  mutate(
    wave  = as.integer(substr(wave_gmv, 2, 2)),
    GMV_k = GMV / 1000
  ) %>%
  filter(!is.na(GMV_k)) %>%
  select(numeric_id, btempo, wave, GMV_k)

# 2) Mplus latent-basis time scores (girls)
tload <- c(`1` = 0.000, `2` = 0.215, `3` = 0.556, `4` = 1.000)

# 3) Per-ID latent-basis slopes (need ≥ 2 waves)
slopes_b <- gmv_long_b %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(numeric_id) %>%
  filter(n() >= 2) %>%
  summarise(slope_k = coef(lm(GMV_k ~ t))[["t"]], .groups = "drop") %>%
  left_join(boys %>% distinct(numeric_id, btempo), by = "numeric_id") %>%
  filter(!is.na(btempo))

range(gmv_long_b$btempo, na.rm = TRUE)   # returns c(min, max)
range(slopes_b$slope_k, na.rm = TRUE)   # returns c(min, max)

# 4) Scatter: tempo vs. individual slope
ggplot(slopes_b, aes(btempo, slope_k)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey50") +
  geom_point(alpha = 0.35, size = 2, color = "grey20") +
  scale_y_continuous(limits = c(-120, 50), breaks = seq(-120, 50, by = 20)) +
  scale_x_continuous(limits = c(-.5, 1.0), breaks = seq(-.5, 1.0, by = .2)) +
  labs(
    x = "Pubertal tempo",
    y = "Model estimated GMV slope"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

# --- Mplus-implied line from covariance -------------------------
S_mean    <- -99.310   # from Mplus [SLOPE] mean (unstd; ÷1,000)
mu_btempo <-  0.331    # from Mplus [BTEMPO] intercept/mean
cov_S_bT  <-  0.198    # SLOPE WITH BTEMPO (unstd)
var_bT    <-  0.020    # Var(BTEMPO) (unstd)

b_mplus       <- cov_S_bT / var_bT             # ~ 9.90
int_mplus     <- S_mean - b_mplus * mu_btempo  # center at (mu_btempo, S_mean)

# (optional) empirical fit to your per-ID OLS slopes
fit_emp <- lm(slope_k ~ btempo, data = slopes_b)

scatter_b <- ggplot(slopes_b, aes(btempo, slope_k)) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4, color = "grey50") +
  geom_point(alpha = 0.35, size = 2, color = "aquamarine3") +
  geom_abline(intercept = coef(fit_emp)[1], slope = coef(fit_emp)[2],
              linewidth = 1.6, color = "grey0") +
  scale_y_continuous(limits = c(-120, 50), breaks = seq(-120, 50, 20)) +
  scale_x_continuous(limits = c(-.5, 1.5),  breaks = seq(-.5, 1.5,  .2)) +
  labs(x = "Pubertal tempo", y = "Model estimated GMV slope") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

scatter_b
(scatter_b / p_scatter) 











library(dplyr)
library(tidyr)
library(ggplot2)

boys <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999))) %>%
  filter(sex %in% c(1, "1")) %>%
  select(numeric_id, btiming, btempo,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_b <- boys %>%
  pivot_longer(cols = starts_with("T"), names_to = "wave_gmv", values_to = "GMV") %>%
  mutate(
    wave = as.integer(substr(wave_gmv, 2, 2)),
    GMV_k = GMV / 1000
  ) %>%
  filter(!is.na(GMV_k)) %>%
  select(numeric_id, btiming, btempo, wave, GMV_k)

# your long boys data: gmv_long_b (numeric_id, wave, GMV_k)
tload <- c(`1` = 0.000, `2` = 0.215, `3` = 0.556, `4` = 1.000)

# 1) add t and fit per-ID latent-basis line (need ≥2 waves)
fits_b <- gmv_long_b %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(numeric_id) %>%
  filter(n() >= 2) %>%
  do({
    m <- lm(GMV_k ~ t, data = .)
    tibble(int_k = unname(coef(m)[1]),  # predicted GMV at t=0 (baseline)
           slp_k = unname(coef(m)[2]))
  }) %>%
  ungroup()

# 2) predict each ID's GMV at the latent-basis times, then average
lb_mean <- fits_b %>%
  tidyr::expand_grid(wave = 1:4) %>%
  mutate(t = tload[as.character(wave)],
         pred = int_k + slp_k * t) %>%
  group_by(wave) %>%
  summarise(
    mean_lb = mean(pred, na.rm = TRUE),
    se_lb   = sd(pred,   na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(lo = mean_lb - 1.96 * se_lb,
         hi = mean_lb + 1.96 * se_lb)

# 3) base spaghetti (one line per ID)
p_lines_b <- gmv_long_b %>%
  arrange(numeric_id, wave) %>%
  ggplot(aes(wave, GMV_k, group = numeric_id)) +
  geom_line(alpha = 0.12, linewidth = 0.25, color = "aquamarine3", na.rm = TRUE) +
  geom_point(alpha = 0.10, size = 0.6, color = "aquamarine3", na.rm = TRUE) +
  scale_x_continuous(breaks = 1:4,
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  labs(x = "Wave", y = "Total GMV (÷1,000)") +
  scale_y_continuous(limits = c(400, 1000), breaks = seq(400, 1000, by = 100)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank()) +
  geom_ribbon(data = lb_mean,
              aes(wave, ymin = lo, ymax = hi),
              inherit.aes = FALSE, alpha = 0.18, fill = "grey10", color = NA) +
  geom_line(data = lb_mean,
            aes(wave, mean_lb),
            inherit.aes = FALSE, linewidth = 1.6, color = "grey10") +
  geom_point(data = lb_mean,
             aes(wave, mean_lb),
             inherit.aes = FALSE, size = 2.2, color = "grey10")

p_lines_b

# If you don't already have gmv_long_g, uncomment this block:
girls <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999))) %>%
  filter(sex %in% c(2, "2")) %>%
  select(numeric_id, gtiming, gtempo,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)
gmv_long_g <- girls %>%
  pivot_longer(starts_with("T"), names_to = "wave_gmv", values_to = "GMV") %>%
  mutate(wave = as.integer(substr(wave_gmv, 2, 2)),
         GMV_k = GMV / 1000) %>%
  filter(!is.na(GMV_k)) %>%
  select(numeric_id, wave, GMV_k)

# ---- GIRLS latent-basis loads ----
tload <- c(`1` = 0.000, `2` = 0.210, `3` = 0.684, `4` = 1.000)

# 1) per-ID latent-basis line (need ≥2 waves)
fits_g <- gmv_long_g %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(numeric_id) %>%
  filter(n() >= 2) %>%
  do({
    m <- lm(GMV_k ~ t, data = .)
    tibble(int_k = unname(coef(m)[1]),   # predicted GMV at t = 0 (baseline)
           slp_k = unname(coef(m)[2]))   # change over t = 0 → 1 (Baseline → Y6)
  }) %>%
  ungroup()

# 2) predict each ID's GMV at the latent-basis times, then average
lb_mean_g <- fits_g %>%
  tidyr::expand_grid(wave = 1:4) %>%
  mutate(t    = tload[as.character(wave)],
         pred = int_k + slp_k * t) %>%
  group_by(wave) %>%
  summarise(
    mean_lb = mean(pred, na.rm = TRUE),
    se_lb   = sd(pred,   na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(lo = mean_lb - 1.96 * se_lb,
         hi = mean_lb + 1.96 * se_lb)

# 3) spaghetti + latent-basis mean line + CI
p_lines_g <- gmv_long_g %>%
  arrange(numeric_id, wave) %>%
  ggplot(aes(wave, GMV_k, group = numeric_id)) +
  geom_line(alpha = 0.12, linewidth = 0.25, color = "darksalmon", na.rm = TRUE) +
  geom_point(alpha = 0.10, size = 0.6, color = "darksalmon", na.rm = TRUE) +
  scale_x_continuous(breaks = 1:4,
                     labels = c("Baseline","Year 2","Year 4","Year 6")) +
  labs(x = "Wave", y = "Total GMV (÷1,000)") +
  scale_y_continuous(limits = c(400, 1000), breaks = seq(400, 1000, by = 100)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank()) +
  geom_ribbon(data = lb_mean_g,
              aes(wave, ymin = lo, ymax = hi),
              inherit.aes = FALSE, alpha = 0.18, fill = "grey10", color = NA) +
  geom_line(data = lb_mean_g,
            aes(wave, mean_lb),
            inherit.aes = FALSE, linewidth = 1.6, color = "grey10") +
  geom_point(data = lb_mean_g,
             aes(wave, mean_lb),
             inherit.aes = FALSE, size = 2.2, color = "grey10")

p_lines_g

(p_lines_b | p_lines_g) 














library(dplyr)
library(tidyr)
library(ggplot2)

# --- Boys long GMV (÷1,000) -----------------------------------------------
boys <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999))) %>%
  filter(sex %in% c(1, "1")) %>%
  select(numeric_id, btiming,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_b <- boys %>%
  pivot_longer(starts_with("T"), names_to = "wave_gmv", values_to = "GMV") %>%
  mutate(
    wave  = as.integer(substr(wave_gmv, 2, 2)),
    GMV_k = GMV / 1000
  ) %>%
  filter(!is.na(GMV_k)) %>%
  select(numeric_id, btiming, wave, GMV_k)

# --- Latent-basis time scores (boys Mplus) ---------------------------------
tload <- c(`1` = 0.000, `2` = 0.215, `3` = 0.556, `4` = 1.000)

# --- Per-ID latent-basis intercepts (need ≥2 waves) ------------------------
ints_b <- gmv_long_b %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(numeric_id) %>%
  filter(n() >= 2) %>%
  summarise(int_k = unname(coef(lm(GMV_k ~ t))[1]), .groups = "drop") %>%  # intercept at t=0
  left_join(boys %>% distinct(numeric_id, btiming), by = "numeric_id") %>%
  filter(!is.na(btiming))

# --- Scatter: pubertal timing vs latent-basis intercept --------------------
p_intercept <- ggplot(ints_b, aes(btiming, int_k)) +
  geom_point(alpha = 0.35, size = 2, color = "aquamarine3") +
  geom_smooth(method = "lm", se = TRUE, color = "grey25") +  # empirical OLS (optional)
  scale_x_continuous(name = "Pubertal timing (age at onset)",
                     breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(name = "GMV intercept (latent-basis; ÷1,000)",
                     breaks = scales::pretty_breaks(8)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

p_intercept






library(dplyr)
library(tidyr)
library(ggplot2)

# --- Boys + clean -----------------------------------------------------------
boys <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999))) %>%
  filter(sex %in% c(1, "1")) %>%
  select(numeric_id, ela_plus,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_b <- boys %>%
  pivot_longer(starts_with("T"), names_to = "wave_gmv", values_to = "GMV") %>%
  mutate(
    wave  = as.integer(substr(wave_gmv, 2, 2)),
    GMV_k = GMV / 1000
  ) %>%
  filter(!is.na(GMV_k)) %>%
  select(numeric_id, ela_plus, wave, GMV_k)

# --- Boys’ latent-basis time scores (from Mplus) ----------------------------
tload <- c(`1` = 0.000, `2` = 0.215, `3` = 0.556, `4` = 1.000)

# --- Per-ID latent-basis intercepts (need ≥ 2 waves) ------------------------
ints_b <- gmv_long_b %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(numeric_id) %>%
  filter(n() >= 2) %>%
  summarise(int_k = unname(coef(lm(GMV_k ~ t))[1]), .groups = "drop") %>%  # intercept at t=0
  left_join(boys %>% distinct(numeric_id, ela_plus), by = "numeric_id") %>%
  filter(!is.na(ela_plus))

N_b <- nrow(ints_b)
r_b <- cor(ints_b$ela_plus, ints_b$int_k, use = "pairwise.complete.obs")

# --- Scatter + OLS fit ------------------------------------------------------
p_ela_intercept_b <- ggplot(ints_b, aes(ela_plus, int_k)) +
  geom_point(alpha = 0.35, size = 2, color = "aquamarine3") +
  geom_smooth(method = "lm", se = TRUE, color = "grey20", linewidth = 1.2) +
  labs(
    title=NULL, x = "Early-Life Adversity (ELA)",
    y = "GMV intercept"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

p_ela_intercept_b


library(dplyr)
library(tidyr)
library(ggplot2)

# --- Girls + clean ----------------------------------------------------------
girls <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999))) %>%
  filter(sex %in% c(2, "2")) %>%
  select(numeric_id, ela_plus,
         T1_totalGMV, T2_totalGMV, T3_totalGMV, T4_totalGMV)

gmv_long_g <- girls %>%
  pivot_longer(starts_with("T"), names_to = "wave_gmv", values_to = "GMV") %>%
  mutate(
    wave  = as.integer(substr(wave_gmv, 2, 2)),
    GMV_k = GMV / 1000
  ) %>%
  filter(!is.na(GMV_k)) %>%
  select(numeric_id, ela_plus, wave, GMV_k)

# --- Girls’ latent-basis time scores (Mplus) --------------------------------
tload <- c(`1` = 0.000, `2` = 0.210, `3` = 0.684, `4` = 1.000)

# --- Per-ID latent-basis intercepts (need ≥ 2 waves) ------------------------
ints_g <- gmv_long_g %>%
  mutate(t = tload[as.character(wave)]) %>%
  group_by(numeric_id) %>%
  filter(n() >= 2) %>%
  summarise(int_k = unname(coef(lm(GMV_k ~ t))[1]), .groups = "drop") %>%  # intercept at t=0
  left_join(girls %>% distinct(numeric_id, ela_plus), by = "numeric_id") %>%
  filter(!is.na(ela_plus))

N_g <- nrow(ints_g)
r_g <- cor(ints_g$ela_plus, ints_g$int_k, use = "pairwise.complete.obs")

# --- Mplus-implied regression line: INTERCEPT ON ELA_PLUS -------------------
# from your girls unstandardized output (GMV already ÷1,000):
I_mean_g      <- 641.903
mu_ELA_g      <- 1.825
b_Mplus_IonEg <- -4.035

ab_intercept_g <- I_mean_g - b_Mplus_IonEg * mu_ELA_g  # centers at (mu_ELA, I_mean)
ab_slope_g     <- b_Mplus_IonEg

# --- Plot -------------------------------------------------------------------
p_ela_intercept_g <- ggplot(ints_g, aes(ela_plus, int_k)) +
  geom_point(alpha = 0.35, size = 2, color = "darksalmon") +
  geom_smooth(method = "lm", se = TRUE, color = "grey20", linewidth = 1.2) + 
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  scale_y_continuous(limits = c(400, 900), breaks = seq(400, 900, 100)) +
  labs(
    title = NULL, x = "Early-Life Adversity (ELA)",
    y = "GMV intercept"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

p_ela_intercept_b <- ggplot(ints_b, aes(ela_plus, int_k)) +
  geom_point(alpha = 0.35, size = 2, color = "aquamarine3") +
  geom_smooth(method = "lm", se = TRUE, color = "grey20", linewidth = 1.2) +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  scale_y_continuous(limits = c(400, 900), breaks = seq(400, 900, 100)) +
  labs(
    title = NULL, x = "Early-Life Adversity (ELA)",
    y = "GMV intercept"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

p_ela_intercept_g

(p_ela_intercept_b / p_ela_intercept_g)





library(dplyr)
library(tidyr)
library(ggplot2)

# ---------------------------
# Common cleaning
# ---------------------------
df_clean <- df %>%
  mutate(across(where(is.numeric), ~ na_if(., -999)))

# Axis helpers
x_sc <- scale_x_continuous(limits = c(0, 10), breaks = 0:10)      # ELA 0–10
y_sc <- scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.1))  # tempo 0–1.5

# ===========================
# BOYS: ELA vs BTEMPO
# ===========================
boys_t <- df_clean %>%
  filter(sex %in% c(1, "1")) %>%
  select(numeric_id, ela_plus, btempo) %>%
  filter(!is.na(ela_plus), !is.na(btempo))

N_b <- nrow(boys_t)
r_b <- cor(boys_t$ela_plus, boys_t$btempo, use = "pairwise.complete.obs")

p_ela_tempo_boys <- ggplot(boys_t, aes(ela_plus, btempo)) +
  geom_point(alpha = 0.4, size = 2, color = "aquamarine3") +
  geom_smooth(method = "lm", se = TRUE, color = "grey20", linewidth = 1.2) +  # empirical
  x_sc + y_sc +
  labs(title = NULL,
       x = "Early-Life Adversity (ELA)",
       y = "Pubertal tempo") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

p_ela_tempo_boys

# (Optional) If you have the BOYS Mplus coefficient (BTEMPO ON ELA = b_boys),
#   and Mplus means for btempo (mu_btempo) & ELA (mu_ela_b), add this:
# b_boys   <- NA_real_   # fill from Mplus if available
# mu_btempo<- NA_real_
# mu_ela_b <- NA_real_
# if (!is.na(b_boys) && !is.na(mu_btempo) && !is.na(mu_ela_b)) {
#   ab_int_b <- mu_btempo - b_boys * mu_ela_b
#   p_ela_tempo_boys <- p_ela_tempo_boys +
#     geom_abline(intercept = ab_int_b, slope = b_boys,
#                 color = "#0072B2", linewidth = 1.4)
# }

# ===========================
# GIRLS: ELA vs GTEMPO (+ Mplus line)
# ===========================
girls_t <- df_clean %>%
  filter(sex %in% c(2, "2")) %>%
  select(numeric_id, ela_plus, gtempo) %>%
  filter(!is.na(ela_plus), !is.na(gtempo))

N_g <- nrow(girls_t)
r_g <- cor(girls_t$ela_plus, girls_t$gtempo, use = "pairwise.complete.obs")

# Mplus (unstandardized) from your output:
b_girls   <- -0.005   # GTEMPO ON ELA_PLUS
mu_gtempo <-  0.687   # mean GTEMPO
mu_ela_g  <-  1.825   # mean ELA
ab_int_g  <- mu_gtempo - b_girls * mu_ela_g  # centers at (μ_ELA, μ_tempo)

p_ela_tempo_girls <- ggplot(girls_t, aes(ela_plus, gtempo)) +
  geom_point(alpha = 0.4, size = 2, color = "darksalmon") +
  geom_smooth(method = "lm", se = TRUE, color = "grey20", linewidth = 1.2) +  # empirical
  x_sc + y_sc +
  labs(title = NULL,
       x = "Early-Life Adversity (ELA)",
       y = "Pubertal tempo") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

p_ela_tempo_girls

(p_ela_tempo_boys / p_ela_tempo_girls)
