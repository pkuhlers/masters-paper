## Analysis of colon PDX growth using mixed models
library(tidyverse)
library(survival)
library(survminer)
library(sjPlot)
theme_set(theme_classic())
growth <- read.csv("derived_data/pdx_colon_clean.csv", row.names = 1) %>%
  mutate(drug = relevel(factor(AgentName), ref = "Control"))

########################################################################
## AFT model for days to 1500 (or next closest day) -- 'Naive' Model
########################################################################


aft <- survreg(Surv(days_to_1500, reached_1500) ~ drug, data = growth %>% filter(total_obs > 1))
summary(aft)

pdf("figures/naive_aft_forest.pdf")
plot_model(
  aft,
  rm.terms = "Log(scale)",
  title = "",
  axis.labels = rev(levels(growth$drug)[-1]),
  vline.color = "black",
  sort.est = T,
  axis.title = c("Acceleration Factors", "")
)
dev.off()

## Kaplan Meier plot to complement the AFT
pdf("figures/naive_km.pdf")
ggsurvplot(
  survfit(Surv(days_to_1500, reached_1500) ~ drug, data = growth),
  linetype = c("dashed", rep("solid", 7)),
  censor.shape = "|"
)
dev.off()

###########################################
## Mixed model imputed AFT
###########################################

## Extract the coefficients of the mixed model
## Create a model matrix for the drugs each *SUBJECT* was assigned too
## Multiply the coefficent matrix with model matrix to get the subject specific equation

fit_mm <- readRDS("derived_data/colon_month_first_glmer.rds")
mm_coef <- coef(fit_mm)$ID
trt_by_subj <- growth %>%
  select(ID, drug) %>%
  arrange(ID) %>%
  distinct()
mod_mat <- cbind(1, model.matrix(~ drug, trt_by_subj))
subj_effects <- mm_coef * mod_mat

## Model imputed time to 1500 -- mixed model was on the log scale so log(1500)

mm_impute <-
  data.frame(imputed_time_to_1500 = (log(1500) - subj_effects[, 1]) / rowSums(subj_effects[, c(2:9)])) %>%
  rownames_to_column("ID") %>%
  left_join(x = growth,
            y = .,
            by = "ID")

imputed_aft <- survreg(Surv(imputed_time_to_1500) ~ drug, data = mm_impute)
summary(imputed_aft)

pdf("figures/imputed_aft_forest.pdf")
plot_model(
  imputed_aft,
  rm.terms = "Log(scale)",
  title = "",
  axis.labels = rev(levels(growth$drug)[-1]),
  vline.color = "black",
  sort.est = T,
  axis.title = c("Acceleration Factors", "")
)
dev.off()

ggsurvplot(
  survfit(Surv(imputed_time_to_1500 * 30) ~ drug, data = mm_impute),
  linetype = c("dashed", rep("solid", 7)),
  censor.shape = "|"
)

## Compare the two models on the same plot
pdf("figures/aft_models_overlaid_forest.pdf")
plot_models(
  aft,
  imputed_aft,
  rm.terms = "Log(scale)",
  title = "",
  axis.title = c("Acceleration Factors", ""),
  axis.labels = rev(levels(growth$drug)[-1]),
  vline.color = "black",
  m.labels = c("Naive", "Imputed"),
  legend.title = "Models",
  axis.lim = c(0.5, 3)
)
dev.off()