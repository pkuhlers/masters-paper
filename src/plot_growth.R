## Plot tumor volume over time by treatment
## Replicate plots from pdxnet portal
library(ggplot2)
vol <- read.csv("source_data/pdx_colon.csv")
theme_set(theme_classic())

pdf("figures/growth_plots.pdf")
ggplot(vol, aes(x = OBS_DAY, y = TUMOR_WT, color = ID)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ AgentName) +
  guides(color = "none") +
  labs(x = "Observation Day", y = expression(paste("Tumor Volume (m", m^3, ")")))
dev.off()