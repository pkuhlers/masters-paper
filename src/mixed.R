## Analysis of colon PDX growth using mixed models
library(tidyverse)
library(lme4)
growth <- read.csv("source_data/pdx_colon.csv") %>%
  mutate(day = OBS_DAY - 7,
         drug = relevel(factor(AgentName), ref = "Control"))

## OLS estimates
fit0 <- glm(
  TUMOR_WT ~ OBS_DAY + OBS_DAY:drug,
  data = growth,
  family = gaussian(link = "log"))
summary(fit0)

## Random Intercept by mouse
fit1 <- glmer(
  TUMOR_WT ~ OBS_DAY + OBS_DAY:drug + (1|ID),
  data = growth,
  family = gaussian(link = "log")
)
summary(fit1)

## Random slope and intercept by mouse
fit2 <- glmer(
  TUMOR_WT ~ OBS_DAY + OBS_DAY:drug + (1 + OBS_DAY|ID),
  data = growth,
  family = gaussian(link = "log"), 
)
summary(fit2)

## Exploring some different model fitting methods
library(nlme)
library(MASS)
fit <- lme(TUMOR_WT ~ OBS_DAY + OBS_DAY:drug, random = ~ OBS_DAY | ID, data = growth2)
summary(fit)
fit <-
  MASS::glmmPQL(
    TUMOR_WT ~ OBS_DAY + OBS_DAY:drug,
    random = ~ OBS_DAY |
      ID,
    family = gaussian(link = "log"),
    data = growth
  )
summary(fit)
