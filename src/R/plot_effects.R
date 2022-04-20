library(sjPlot)
aft <- readRDS("derived_data/aft_model.rds")

plot_model(aft, title = "AFT Model Acceleration Factors")