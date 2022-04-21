## Analysis of colon PDX growth using mixed models
library(tidyverse)
library(lme4)
library(glmmTMB)
growth <- read.csv("derived_data/pdx_colon_clean.csv") %>%
  mutate(drug = relevel(factor(AgentName), ref = "Control"),
         months_since_first = days_since_first/30)

## OLS estimates
fit0 <- glm(
  TUMOR_WT ~ month + month:drug,
  data = growth,
  family = gaussian(link = "log"))
summary(fit0)

## Random slope and intercept by mouse -- by month
fitM <- glmer(
  TUMOR_WT ~ month + month:drug + (1 + month|ID),
  data = growth,
  family = gaussian(link = "log")
)
summary(fitM)
saveRDS(fitM, "derived_data/colon_month_glmer.rds")

## Random slope and intercept by mouse -- months since first observation
fit_first <- glmer(
  TUMOR_WT ~ months_since_first + months_since_first:drug + (1 + months_since_first|ID),
  data = growth,
  family = gaussian(link = "log")
)
summary(fit_first)
saveRDS(fit_first, "derived_data/colon_month_first_glmer.rds")


pdf("figures/mixed_forest.pdf")
plot_model(
  fit_first,
  transform = "exp",
  rm.terms = "months_since_first",
  title = "",
  axis.title = c("Relative Growth Rate", "")
)
dev.off()
