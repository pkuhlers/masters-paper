## Analysis of colon PDX growth using mixed models
library(tidyverse)
library(survival)
growth <- read.csv("source_data/pdx_colon.csv") %>%
  mutate(day = OBS_DAY - 7,
         drug = relevel(factor(AgentName), ref = "Control"),
         censor = 1)
gg <- growth %>%
  group_by(ID, drug) %>%
  summarize(maxDay = max(OBS_DAY)) %>%
  mutate(censor = 1)
aft <- survreg(Surv(maxDay, censor) ~ drug, data = gg)

growth <- read.csv("source_data/pdx_pancreas.csv") %>%
  mutate(day = OBS_DAY - 7,
         drug = relevel(factor(AgentName), ref = "Control"),
         censor = 1)
