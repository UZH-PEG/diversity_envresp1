## SS finding experiment by temporal method
## Be careful to not overwrite data files

max_cores <- benchmarkme::get_cpu()$no_of_cores-2

## setup experiment with two strains
num_strains <- 2
source(here::here("experiments/0_ss_finding/temporal_method/setup_experiment.R"))

## keep only the most tolerant strain
#var_expt <- var_expt[1:2,]
for(i in 1:nrow(var_expt)) {
  temp_pars <- var_expt$pars[i][[1]]
  temp_pars$CB <- temp_pars$CB[which(temp_pars$CB$h_SR_CB==max(temp_pars$CB$h_SR_CB))[1],]
  temp_pars$PB <- temp_pars$PB[which(temp_pars$PB$h_O_PB==max(temp_pars$PB$h_O_PB))[1],]
  temp_pars$SB <- temp_pars$SB[which(temp_pars$SB$h_O_SB==max(temp_pars$SB$h_O_SB))[1],]
  initial_subtrates <- temp_pars$initial_state[!str_detect(names(temp_pars$initial_state), "B_")]
  temp_pars$initial_state <- c(NA, NA, NA, initial_subtrates)
  names(temp_pars$initial_state) <- c("CB_1", "PB_1", "SB_1", names(initial_subtrates))
  var_expt$pars[i][[1]] <- temp_pars
}
num_strains <- -1 ## use this to label that only the most tolerant strain is kept


datadir <- here::here("data",
                      microxanox_path,
                      "temporal_method")
#dir.create(datadir, recursive = TRUE, showWarnings = FALSE)

ss_data_filename <- file.path(datadir,
                              paste0("ss_data_",
                                     num_strains, "strains_waittime",
                                     formatC(wait_time, format = "e", digits = 0),
                                     "_", event_def,".RDS"))
stab_data_filename <- file.path(datadir,
                                paste0("stab_data_",
                                       num_strains, "strains_waittime",
                                       formatC(wait_time, format = "e", digits = 0),
                                       "_", event_def,".RDS"))


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
saveRDS(expt_res, ss_data_filename)
## End of run experiment



## Get stability measures ----
num_cores <- min(c(max_cores, nrow(var_expt)))
expt_res <- readRDS(ss_data_filename)
## Get total biomass of CB, of SB, and of PB, to allow calculation of stability of these
expt_res <- expt_res %>%
  mutate(ssfind_result = list(get_total_bio(ssfind_result)))
cluster1 <- multidplyr::new_cluster(num_cores)
multidplyr::cluster_library(cluster1, c("microxanox", "dplyr"))
system.time({
  stab_data <-  expt_res %>%
    multidplyr::partition(cluster1) %>%
    mutate(stability_measures = list(get_stability_measures(ssfind_result))) %>%
    collect() %>%
    unnest(cols = c(stability_measures)) %>%
    mutate(num_strains = num_strains)
})
saveRDS(stab_data, stab_data_filename)
## End of getting stability measures



## End of number of strains loop












