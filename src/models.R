## Analysis of colon PDX growth using mixed models
library(tidyverse)
library(lme4)
growth <- read.csv("source_data/pdx_colon.csv") %>%
  mutate(day = OBS_DAY - 7)

## Random Intercept by mouse
fit1 <- glmer(
  TUMOR_WT ~ OBS_DAY + OBS_DAY:AgentName + (1|ID),
  data = growth,
  family = gaussian(link = "log")
)
summary(fit1)

## Random slope and intercept by mouse
fit2 <- glmer(
  TUMOR_WT ~ OBS_DAY + OBS_DAY:AgentName + (1 + OBS_DAY|ID),
  data = growth,
  family = gaussian(link = "log")
)
summary(fit2)
