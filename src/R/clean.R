library(tidyverse)
growth <- read.csv("source_data/pdx_colon.csv", row.names = 1) %>%
  mutate(
    day = OBS_DAY - 7,
    month = OBS_DAY / 30,
    drug = relevel(factor(AgentName), ref = "Control")
  ) %>%
  group_by(ID) %>%
  mutate(
    first = min(OBS_DAY),
    maxVol = max(TUMOR_WT),
    days_since_first = OBS_DAY - first,
    reached_1500 = ifelse(maxVol >= 1500, 1, 0),
    total_obs = length(OBS_DAY),
    days_to_1500 = days_since_first[which.min(1500 - TUMOR_WT)]
  ) %>%
  ungroup() %>%
  distinct()

write.csv(growth, "derived_data/pdx_colon_clean.csv")
