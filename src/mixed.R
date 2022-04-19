## Analysis of colon PDX growth using mixed models
library(tidyverse)
library(lme4)
growth <- read.csv("derived_data/pdx_colon_clean.csv") %>%
  mutate(drug = relevel(factor(AgentName), ref = "Control"),
         months_since_first = days_since_first/30)

## OLS estimates
fit0 <- glm(
  TUMOR_WT ~ month + month:drug,
  data = growth,
  family = gaussian(link = "log"))
summary(fit0)

# ## Random Intercept by mouse
# fit1 <- glmer(
#   TUMOR_WT ~ OBS_DAY + OBS_DAY:drug + (1|ID),
#   data = growth,
#   family = gaussian(link = "log")
# )
# summary(fit1)
#
# ## Random slope and intercept by mouse
# fit2 <- glmer(
#   TUMOR_WT ~ OBS_DAY + OBS_DAY:drug + (1 + OBS_DAY|ID),
#   data = growth,
#   family = gaussian(link = "log"),
#   glmerControl(optimizer = "bobyqa")
# )
# summary(fit2)

## Random slope and intercept by mouse -- by month
fitM <- glmer(
  TUMOR_WT ~ month + month:drug + (1 + month|ID),
  data = growth,
  family = gaussian(link = "log")
)
summary(fitM)
saveRDS(fitM, "derived_data/colon_month_glmer.rds")

fit_first <- glmer(
  TUMOR_WT ~ months_since_first + months_since_first:drug + (1 + months_since_first|ID),
  data = growth,
  family = gaussian(link = "log")
)
summary(fit_first)

## Random slope and intercept by mouse nested in experiment -- by month
fitNest <- glmer(
  TUMOR_WT ~ month + month:drug + (1 + month|ExpNameCSV/ID),
  data = growth,
  family = gaussian(link = "log")
)
summary(fitNest)

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