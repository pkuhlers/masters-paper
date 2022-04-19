## Analysis of colon PDX growth using mixed models
library(tidyverse)
library(survival)
growth <- read.csv("derived_data/pdx_colon_clean.csv") %>%
  mutate(drug = relevel(factor(AgentName), ref = "Control")) %>%
  filter(total_obs > 1)

## AFT model for days to 1500 (or next closest day)
aft <- survreg(Surv(days_to_1500, reached_1500) ~ drug, data = growth)
summary(aft)
saveRDS(aft, "derived_data/aft_model.rds")

ggsurvplot(
  survfit(Surv(days_to_1500, reached_1500) ~ drug, data = growth),
  linetype = c("dashed", rep("solid", 7)),
  censor.shape = "|"
)

