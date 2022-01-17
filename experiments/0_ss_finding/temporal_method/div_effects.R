rm(list = ls())

## Prelims ----
library(microxanox)
library(tidyverse)
library(patchwork)
library(here)

colfunc_CB <- colorRampPalette(c("#024F17", "#B5FFC9"))
colfunc_SB <- colorRampPalette(c("#7D1402", "#FCBEB3"))
colfunc_PB <- colorRampPalette(c("#6E0172", "#F9AEFC"))

source(here("R/various_useful_functions.r"))



all_stab_results_small <- readRDS(here("experiments/0_ss_finding/temporal_method/data/all_stab_results_small.RDS"))
sort(unique(all_stab_results_small$stand_var))


reference_var <- 0.47368421

temp <- all_stab_results_small %>%
  filter(round(stand_var,4) == round(reference_var,4) | stand_var == 0,
         Species == "CB_tot",
         num_strains == 9) %>%
  arrange(var_treat, stand_var) %>%
  group_by(var_treat) %>%
  summarise(an2ox_div_effect = hyst_max_log[2]-hyst_max_log[1],
            ox2an_div_effect = hyst_min_log[2]-hyst_min_log[1])
  
temp

