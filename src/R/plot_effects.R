## Forest plots of effects
library(tidyverse)
library(sjPlot)


aft <- readRDS("derived_data/aft_model.rds")
plot_model(aft, title = "AFT Model Acceleration Factors",)

mm <- readRDS("derived_data/colon_month_first_glmer.rds")
plot_model(mm, transform = "exp")

aft_impute <- readRDS("derived_data/colon_imputed_aft.rds")
plot_model(aft_impute)
