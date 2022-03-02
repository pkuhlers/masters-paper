## Plot tumor volume over time by treatment
## Replicate plots from pdxnet portal
library(ggplot2)
vol <- read.csv("source_data/pdx_colon.csv")
theme_set(theme_classic())

pdf("figures/growth_plots.pdf")
ggplot(vol, aes(x = OBS_DAY, y = log(TUMOR_WT))) +
  geom_line(alpha = 0.5, aes(group = ID)) +
  geom_smooth(method = 'lm', formula = y~x, se = F) +
  facet_wrap(~ AgentName) +
  guides(color = "none") +
  labs(x = "Observation Day", y = expression(paste("Log Tumor Volume (m", m^3, ")")))
dev.off()
