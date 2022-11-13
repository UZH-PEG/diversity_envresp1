## Note that the microxanox function "get_ssbyaN_parameter" may be useful here, though the code below
## would need to be changed in order to use it.
## See file R/how_to_install_microxanox for (surprise) how to install the microxanox package

# 
# 
# 
# microxanox_version_here <- "0.9.1"
# microxanox_version_path <- paste0("microxanox_v", microxanox_version_here)
# 
# 
# library(microxanox)
# source(here::here("experiments/0_ss_finding/temporal_method/check_microxanox_version.R"))
# library(tidyverse)
# library(patchwork)
# library(here)
# source(here("R/various_useful_functions.r"))
# zero <- 0 ## don't change
# unity <- 1 ## don't change!!!

## Re-run sim selected in figure 3 for longer
ss_9s <- readRDS(here::here("data",
                            microxanox_path,
                            "temporal_method",
                            "event_definition_2",
                            "stab_data_9strains_waittime1e+06_event_definition_2.RDS"))
sort(unique(ss_9s$CB_var_gmax_s))
sort(unique(ss_9s$SB_var_gmax_s))
if(!test_mode) {
  ss_result <- ss_9s %>%
    filter(abs(CB_var_gmax_s - 0.028254848) < 0.0001,
           abs(SB_var_gmax_s - 0.084764545) < 0.0001,
           abs(PB_var_gmax_s - 0.084764545) < 0.0001)
}
if(test_mode) {
  ss_result <- ss_9s %>%
    filter(abs(CB_var_gmax_s - 0.0) < 0.0001,
           abs(SB_var_gmax_s - 0.0) < 0.0001,
           abs(PB_var_gmax_s - 0.0) < 0.0001)
  }
parameter <- ss_result$parameter[[1]]
parameter$strain_parameter <- ss_result$pars[[1]]

total_initial_abundances <- 10^5
parameter <- set_temporal_ssfind_initial_state(parameter,
                                               total_initial_abundances,
                                               total_initial_abundances,
                                               total_initial_abundances)

wait_time <-  1e6
if(test_mode) wait_time <- 1e5
parameter$sim_duration <- wait_time * length(parameter$log10a_series)
parameter$sim_sample_interval <- wait_time
parameter$sim_duration
system.time(
  result_1e6 <- run_temporal_ssfind(parameter)
)

saveRDS(result_1e6, here::here("data",
                               microxanox_path,
                               "temporal_method",
                               "single_sims",
                               "single_1e6_result.RDS"))


wait_time <-  2e6
if(test_mode) wait_time <- 2e5
parameter$sim_duration <- wait_time * length(parameter$log10a_series)
parameter$sim_sample_interval <- wait_time
parameter$sim_duration
result_2e6 <- run_temporal_ssfind(parameter)

saveRDS(result_2e6,  here::here("data",
                                microxanox_path,
                                "temporal_method",
                                "single_sims",
                                "single_2e6_result.RDS"))
