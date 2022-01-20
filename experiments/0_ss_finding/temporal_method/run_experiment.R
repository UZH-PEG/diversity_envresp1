## SS finding experiment by temporal method
## Be careful to not overwrite data files

rm(list = ls())
library(here)
source(here("R/various_useful_functions.r"))
max_cores <- benchmarkme::get_cpu()$no_of_cores


## Run for 2, 3, 6, and 9 strains
for(num_strains in c(2, 3, 6, 9)) {

  num_strains <- 2 ## for testing
    
  ## Run the code that sets up the experiment
  source(here("experiments/0_ss_finding/temporal_method/setup_experiment.R"))
  
  ## Estimate time require ----
  nrow(var_expt)
  wait_time
  length(parameter$log10a_series)
  num_cores <- min(c(max_cores, nrow(var_expt)))
  ## constants in following formula come from the excel sheet
  time_estimate <- ( nrow(var_expt) * wait_time * length(parameter$log10a_series) /
                       num_cores * 0.000123965 + 126 )
  time_estimate
  time_estimate / 60 ## minutes
  time_estimate / 60 / 60 ## hours
  time_estimate / 60 / 60 / 24 ## days
  
  ## Run experiment ----
  system.time({
    expt_res <- run_temporal_ssfind_experiment(parameter,
                                               var_expt,
                                               total_initial_abundances = total_initial_abundances,
                                               cores = num_cores)
  })
  saveRDS(expt_res,
          here("experiments/0_ss_finding/temporal_method/data/",
               paste0(num_strains, "_strain_SS_data_XXX.RDS")))
  ## End of run experiment
  
  
  
  ## Get stability measures ----
  num_cores <- min(c(max_cores, nrow(var_expt)))
  expt_res <- readRDS(here("experiments/0_ss_finding/temporal_method/data/",
                           paste0(num_strains,
                                  "_strain_SS_data_1e3.RDS")))
  ## Get total biomass of CB, of SB, and of PB, to allow calculation of stability of these
  expt_res <- expt_res %>%
    mutate(ssfind_result = list(get_total_bio(ssfind_result)))
  cluster1 <- multidplyr::new_cluster(num_cores)
  multidplyr::cluster_library(cluster1, c("microxanox", "dplyr"))
  system.time({
    stab_data <-  expt_res %>%
      multidplyr::partition(cluster1) %>%
      mutate(stability_measures = list(get_stability_measures_new(ssfind_result))) %>%
      collect() %>%
      unnest(cols = c(stability_measures)) 
  })
  saveRDS(stab_data,
          here("experiments/0_ss_finding/temporal_method/data/",
               paste0(num_strains, "_strain_stab_data_XXX.RDS")))
  ## End of getting stability measures

  
}
## End of number of strains loop












