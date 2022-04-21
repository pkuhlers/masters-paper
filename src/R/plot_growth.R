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
ggplot(vol, aes(x = days_since_first, y = log(TUMOR_WT))) +
  geom_line(alpha = 0.5, aes(group = ID)) +
  geom_smooth(method = 'lm', formula = y~x, se = F) +
  facet_wrap(~ AgentName) +
  guides(color = "none") +
  labs(x = "Observation Day", y = expression(paste("Log Tumor Volume (m", m^3, ")")))
dev.off()

## Readjust time origin and slopes for each drug
pdf("figures/growth_rescaled_days.pdf")
ggplot(vol, aes(x = days_since_first, y = log(TUMOR_WT))) +
  geom_line(alpha = 0.2, aes(group = ID)) +
  geom_smooth(method = 'lm', formula = y~x, se = F, aes(color = AgentName)) +
  labs(x = "Days Since Staging", y = expression(paste("Log Tumor Volume (m", m^3, ")")))
dev.off()

## Maximum volume distribution
pdf("figures/max_vol_hist.pdf")
ggplot(vol, aes(x = maxVol)) +
  geom_histogram(bins = 20, color = "black", fill = "lightblue") +
  geom_vline(xintercept = quantile(vol$maxVol, 0.25), linetype = "dashed") +
  labs(x = expression(paste("Tumor Volume (m", m^3, ")")),
       y = "Frequency")
       # title = "Maximum Achieved Tumor Volume")
dev.off()

## Distribution of number of follow ups
pdf("figures/total_obs_hist.pdf")
ggplot(vol, aes(x = total_obs)) +
  geom_histogram(bins = 15, color = "black", fill = "lightblue") +
  labs(x = "Number of Observations",
       y = "Frequency")
       # title = "Observations per Mouse")
dev.off()
