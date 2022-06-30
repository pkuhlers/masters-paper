## Analysis of colon PDX growth using mixed models
library(tidyverse)
library(lme4)
library(sjPlot)
theme_set(theme_classic())
growth <- read.csv("derived_data/pdx_colon_clean.csv") %>%
  mutate(drug = relevel(factor(AgentName), ref = "Control"),
         months_since_first = days_since_first/30)

## OLS estimates
fit0 <- glm(
  TUMOR_WT ~ months_since_first + months_since_first:drug,
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
  title = "", axis.lim = c(0.3, 2),
  axis.title = c("Relative Growth Rate", ""),
  axis.labels = rev(levels(growth$drug)[-1]),
  vline.color = "black"
)
dev.off()

####################################################################
## Some exploration
####################################################################

# growth$months_square <- growth$months_since_first^2
# fit_square <- glmer(
#   TUMOR_WT ~ poly(months_since_first, 2) + poly(months_since_first,2):drug + (1 + poly(months_since_first, 1)||ID),
#   data = growth,
#   family = gaussian(link = "log")
# )
# summary(fit_square)
# 
# growth$log_month <- log(growth$months_since_first + 1)
# fit_log <- glmer(
#   TUMOR_WT ~ log_month + log_month:drug + (1 + log_month|ID),
#   data = growth,
#   family = gaussian(link = "log")
# )
# growth$idk <- growth$months_since_first + 1
# 
# fit_idk <- glmer(
#   TUMOR_WT ~ idk + idk:drug + (1 + idk|ID),
#   data = growth,mustart = pmax(growth$TUMOR_WT, 1e-3),
#   family = gaussian(link = "log")
# )
# ## More explore
# test <- growth %>%
#   filter(TUMOR_WT < 1600)
# 
# growth$tumor_g <- growth$TUMOR_WT/1000
# uh <- glmer(
#   tumor_g ~ months_since_first + months_since_first:drug + (1 + months_since_first|ID),
#   data = growth,
#   family = gaussian(link = "log")
# )
# ## lmer?
# fit_first_lmer <- lmer(
#   TUMOR_WT ~ months_since_first + months_since_first:drug + (1 + months_since_first|ID),
#   data = growth
# )
# fit_first_full <- glmer(
#   TUMOR_WT ~ months_since_first + drug + months_since_first:drug + (1 + months_since_first|ID),
#   data = growth,
#   family = gaussian(link = "log")
# )
# summary(fit_first_full)
# 
# ## TMB fit
# 
# fit_tmb <- glmmTMB(TUMOR_WT ~ months_since_first + months_since_first:drug + (1 + months_since_first|ID),
#                    data = growth,
#                    family = gaussian(link = "log"))
# summary(fit_tmb)
# 
# fit_tmb_noint <- glmmTMB(TUMOR_WT ~ months_since_first + months_since_first:drug + (0 + months_since_first|ID),
#                    data = growth,
#                    family = gaussian(link = "log"))
# summary(fit_tmb)
# library(gee)
# ge <- gee(TUMOR_WT ~ months_since_first + months_since_first:drug, data = growth,
#           family = gaussian(link = "log"), id = ID, corstr = "independence")
# 

