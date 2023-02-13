##This code reads in the raw data and outputs it into a friendlier format for the final

library(tidyverse)

Cond <- read_csv("rawdata/HydroVu_all_29Apr21.csv", skip = 8) %>%
  rename(DateTime = `Date Time`, SpCond_mS_cm = `Specific Conductivity (µS/cm)`) %>%
  select(DateTime, SpCond_mS_cm)

write_csv(Cond, "McDonald_Conductivity.csv")

Stage <- read_csv("rawdata/HydroVu_PT_29Apr21.csv", skip = 8) %>%
  rename(DateTime = `Date Time`, Stage_m = `Depth (m)`) %>%
  select(DateTime, Stage_m)

write_csv(Stage, "McDonald_Stage.csv")
