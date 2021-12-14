## SS finding experiment
## 
## 


rm(list = ls())
library(here)
source(here("R/various_useful_functions.r"))

num_strains <- 3
num_CB_strains <- num_strains
num_SB_strains <- num_strains
num_PB_strains <- num_strains

source(here("experiments/0_ss_finding/setup_experiment.R"))

#var_expt <- var_expt[1,]

## Estimate time require
nrow(var_expt)
wait_time
length(parameter$log10a_series)
num_cores = 12
## constants in following formula come from the excel sheet
time_estimate <- ( nrow(var_expt) * wait_time * length(parameter$log10a_series) /
  num_cores * 0.000123965 + 126 )
time_estimate
time_estimate / 60 ## minutes
time_estimate / 60 / 60 ## hours
time_estimate / 60 / 60 / 24 ## days


system.time({
expt_res <- run_temporal_ssfind_experiment(parameter,
                                           var_expt,
                                           total_initial_abundances = total_initial_abundances,
                                           cores = num_cores)
})
saveRDS(expt_res, here("experiments/0_ss_finding/data/", paste0(num_strains, "_strain_SS_data_1e3.RDS")))


## Get stability
num_cores = 12
expt_res <- readRDS(here("experiments/0_ss_finding/data/", paste0(num_strains, "_strain_SS_data_1e3.RDS")))
cluster1 <- multidplyr::new_cluster(num_cores)
multidplyr::cluster_library(cluster1, c("microxanox", "dplyr"))
system.time({
stab_data <-  expt_res %>%
  multidplyr::partition(cluster1) %>%
  mutate(stability_measures = list(get_stability_measures_new(ssfind_result))) %>%
  collect() %>%
  unnest(cols = c(stability_measures)) 
})
saveRDS(stab_data, here("experiments/0_ss_finding/data/", paste0(num_strains, "_strain_stab_data_1e3.RDS")))
















