## SS finding experiment
## 
## 


rm(list = ls())
library(here)
source(here("R/various_useful_functions.r"))

num_strains <- 9
num_CB_strains <- num_strains
num_SB_strains <- num_strains
num_PB_strains <- num_strains

source(here("experiments/0_ss_finding/setup_experiment.R"))


## Get SS
var_expt <- var_expt[1:2,]
expt_res <- run_temporal_ssfind_experiment(parameter,
                                           var_expt,
                                           total_initial_abundances = total_initial_abundances,
                                           cores = 1)
saveRDS(expt_res, here("experiments/0_ss_finding/data/", paste0(num_strains, "_strain_SS_data.RDS")))


## Get stability
expt_res <- readRDS(here("experiments/0_ss_finding/data/", paste0(num_strains, "_strain_SS_data.RDS")))
cluster1 <- multidplyr::new_cluster(2)
multidplyr::cluster_library(cluster1, c("microxanox", "dplyr"))
stab_data <-  expt_res_single %>%
  multidplyr::partition(cluster1) %>%
  mutate(stability_measures = list(get_stability_measures_new(ssfind_result))) %>%
  collect() %>%
  unnest(cols = c(stability_measures)) 
saveRDS(stab_data, here("experiments/0_ss_finding/data/", paste0(num_strains, "_strain_stab_data.RDS")))
















