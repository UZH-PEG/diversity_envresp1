## Code for recalculating stability measures
## See file R/how_to_install_microxanox for (surprise) how to install the microxanox package

rm(list = ls())
library(here)
microxanox_release <- "0.4.9"
library(microxanox)
source(here::here("experiments/0_ss_finding/temporal_method/check_microxanox_version.R"))
library(tidyverse)
source(here("R/various_useful_functions.r"))
max_cores <- benchmarkme::get_cpu()$no_of_cores

wait_time <- 1e6
event_def <- "event_definition_2"
datadir <- here::here("data/0_ss_finding/temporal_method/")

## Run for 2, 3, 6, and 9 strains
for(num_strains in c(2, 3, 6, 9)) {

  num_strains <- 9 ## for testing
  
  
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
  
  
  ## Get stability measures ----
  expt_res <- readRDS(ss_data_filename)
  num_cores <- min(c(max_cores, nrow(expt_res)))
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
      unnest(cols = c(stability_measures)) 
  })
  saveRDS(stab_data, stab_data_filename)
  ## End of getting stability measures

  
}
## End of number of strains loop












