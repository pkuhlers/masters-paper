## Plot tumor volume over time by treatment
## Replicate plots from pdxnet portal
library(tidyverse)
vol <- read.csv("derived_data/pdx_colon_clean.csv", row.names = 1)
theme_set(theme_classic())

## Profile Plots facet by drug
pdf("figures/growth_plots.pdf")
ggplot(vol, aes(x = OBS_DAY, y = log(TUMOR_WT))) +
  geom_line(alpha = 0.5, aes(group = ID)) +
  geom_smooth(method = 'lm', formula = y~x, se = F) +
  facet_wrap(~ AgentName) +
  guides(color = "none") +
  labs(x = "Observation Day", y = expression(paste("Log Tumor Volume (m", m^3, ")")))
dev.off()

## ExpName ?
ggplot(vol, aes(x = days_since_first, y = log(TUMOR_WT), color = ExpNameCSV)) +
  geom_line(alpha = 0.5, aes(group = ID)) +
  geom_smooth(method = 'lm', formula = y~x, se = F) +
  facet_wrap(~ AgentName) +
  guides(color = "none") +
  labs(x = "Observation Day", y = expression(paste("Log Tumor Volume (m", m^3, ")")))

## Readjusted time origin facet by drug
pdf("figures/growth_rescaled_days_drug_facet.pdf")
ggplot(vol, aes(x = (days_since_first), y = log(TUMOR_WT))) +
  geom_line(alpha = 0.5, aes(group = ID)) +
  # geom_smooth(method = 'loess', formula = y~x, se = F) +
  geom_smooth(method = 'lm', formula = y~x, se = F) +
  facet_wrap(~ AgentName) +
  guides(color = "none") +
  labs(x = "Days Since Staging", y = expression(paste("Log Tumor Volume (m", m^3, ")")))
dev.off()

## Readjust time origin and slopes for each drug
pdf("figures/growth_rescaled_days.pdf")
ggplot(vol, aes(x = (days_since_first), y = log(TUMOR_WT))) +
  geom_line(alpha = 0.2, aes(group = ID)) +
  geom_smooth(method = 'lm', formula = y~x, se = F, aes(color = AgentName)) +
  # geom_smooth(method = 'loess', formula = y~x, se = F) +
  labs(x = "Days Since Staging", y = expression(paste("Log Tumor Volume (m", m^3, ")")))
dev.off()

## Readjust time origin + mean profile rather than indiv. profile
v <- vol %>% group_by(days_since_first, drug) %>% summarize(avgwt = mean(log(TUMOR_WT)))
ggplot(v, aes(x = days_since_first, y = avgwt)) +
  geom_line() +
  geom_smooth(method = "lm", se = F) +
  ggpubr::stat_regline_equation() +
  facet_wrap(~ drug)

## Maximum volume distribution
pdf("figures/max_vol_hist.pdf")
ggplot(vol, aes(x = maxVol)) +
  geom_histogram(bins = 20, color = "black", fill = "lightblue") +
  geom_vline(xintercept = quantile(vol$maxVol, 0.25), linetype = "dashed") +
  labs(x = expression(paste("Tumor Volume (m", m^3, ")")),
       y = "Frequency")
       # title = "Maximum Achieved Tumor Volume")
dev.off()

## Distribution of time to 1500
pdf("figures/time_to_1500_hist.pdf")
ggplot(vol, aes(x = days_to_1500)) +
  geom_histogram(bins = 20, color = "black", fill = "lightblue") +
  labs(x = expression(paste("Days to 1500m", m^3)),
       y = "Frequency")
dev.off()

## Distribution of number of follow ups
pdf("figures/total_obs_hist.pdf")
ggplot(vol, aes(x = total_obs)) +
  geom_histogram(bins = 15, color = "black", fill = "lightblue") +
  labs(x = "Number of Observations",
       y = "Frequency")
       # title = "Observations per Mouse")
dev.off()