## Analysis of colon PDX growth using mixed models
library(tidyverse)
library(lme4)
growth <- read.csv("source_data/pdx_colon.csv") %>%
  mutate(day = OBS_DAY - 7,
         month = OBS_DAY / 30,
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
  family = gaussian(link = "log")
)
summary(fit2)

## Random slope and intercept by mouse -- by month
fitM <- glmer(
  TUMOR_WT ~ month + month:drug + (1 + month|ID),
  data = growth,
  family = gaussian(link = "log")
)
saveRDS(fitM, "derived_data/colon_month_glmer.rds")

## Exploring some different model fitting methods
library(nlme)
library(MASS)
## log-linear model fit with PQL
fit_day_pql <-
  MASS::glmmPQL(
    TUMOR_WT ~ OBS_DAY + OBS_DAY:drug,
    random = ~ OBS_DAY |
      ID,
    family = gaussian(link = "log"),
    data = growth
  )
summary(fit_day_pql)

fit_month_pql <-
  MASS::glmmPQL(
    TUMOR_WT ~ month + month:drug,
    random = ~ month |
      ID,
    family = gaussian(link = "log"),
    data = growth
  )
summary(fit_month_pql)
saveRDS(fit, "derived_data/colon_kk")