## SS finding experiment by replication method
## Be careful to not overwrite data files


max_cores <- benchmarkme::get_cpu()$no_of_cores-2
options(mc.cores = max_cores)

## Run for 2, 3, 6, and 9 strains
#for(num_strains in c(2, 3, 6, 9)) {

  num_strains <- 9 ## for testing

  ## Run the code that sets up the experiment
  source(here("experiments/replication_method/setup_experiment.R"))

  datadir <- here::here("data",
                        microxanox_path,
                        "replication_method",
                        event_definition)
  
  ss_data_filename <- file.path(datadir,
                                paste0("ss_data_",
                                       num_strains, "strains_sim_length",
                                       formatC(sim_length, format = "e", digits = 0),
                                       "_", event_def,".RDS"))
  stab_data_filename <- file.path(datadir,
                                  paste0("stab_data_",
                                         num_strains, "strains_sim_length",
                                         formatC(sim_length, format = "e", digits = 0),
                                         "_", event_def,".RDS"))

  #var_expt <- var_expt[1,]

  system.time(
    run_replication_ssfind_experiment(parameter, var_expt) %>%
      saveRDS(ss_data_filename)
  )



  ## Get stability measures ----
  num_cores <- min(c(max_cores, nrow(var_expt)))
  expt_res <- readRDS(ss_data_filename)
  ## Get total biomass of CB, of SB, and of PB, to allow calculation of stability of these
  expt_res <- expt_res %>%
    mutate(ss_res = list(get_total_bio(ss_res)))
  cluster1 <- multidplyr::new_cluster(num_cores)
  multidplyr::cluster_library(cluster1, c("microxanox", "dplyr"))
  system.time({
    stab_data <-  expt_res %>%
      multidplyr::partition(cluster1) %>%
      mutate(stability_measures = list(get_stability_measures_replication_ssfind_result(ss_res))) %>%
      collect() %>%
      unnest(cols = c(stability_measures)) %>%
      mutate(num_strains = num_strains)
  })
  saveRDS(stab_data, stab_data_filename)
  ## End of getting stability measures


#}



#x <- parameter$ss_expt[i, ]
