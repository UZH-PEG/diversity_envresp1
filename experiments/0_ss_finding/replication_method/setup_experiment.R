## see file R/how_to_install_microxanox for (surprise) how to install the microxanox package

library(here)
microxanox_release <- "0.4.9"
library(microxanox)
source(here::here("experiments/0_ss_finding/temporal_method/check_microxanox_version.R"))
library(tidyverse)
library(patchwork)
source(here("R/various_useful_functions.r"))
zero <- 0 ## don't change
unity <- 1 ## don't change!!!


num_CB_strains <- num_strains
num_SB_strains <- num_strains
num_PB_strains <- num_strains

sp <- new_strain_parameter(
  n_CB = num_CB_strains,
  n_PB = num_SB_strains,
  n_SB = num_PB_strains,
  values_initial_state = "bush_ssfig3"
)

parameter <- new_runsim_parameter(
  dynamic_model = bushplus_dynamic_model,
  event_definition = event_definition_2,
  event_interval = 100,
  noise_sigma = 0,
  minimum_abundances = rep(1, 3),
  sim_duration = 2000,
  sim_sample_interval = 100,
  strain_parameter = sp,
  log10a_series = c(
    log10(sp$a_O),
    log10(sp$a_O)
  )
)
names(parameter$minimum_abundances) <- c("CB", "PB", "SB")
rm(sp)




## Define diversity
## multiplier of SBPB variation
CB_var_multiplier <- 2
SBPB_var_multiplier <- 6

CB_gmax_div <- 0.015789474 * CB_var_multiplier
CB_h_div <- -0.08 * CB_var_multiplier
SB_gmax_div <- 0.015789474 * SBPB_var_multiplier
SB_h_div <- -0.323  * SBPB_var_multiplier
PB_gmax_div <- 0.015789474  * SBPB_var_multiplier
PB_h_div <- -0.323  * SBPB_var_multiplier

num_div_treatment_levels <- 20

## resolution for sub1
#num_div_treatment_levels <- 50

## Create diversity
var_expt <- create_diversity_factorial2(
  zero = zero, unity = unity,
  num_div_treatment_levels = num_div_treatment_levels,
  CB_gmax_div = CB_gmax_div, CB_h_div = CB_h_div,
  SB_gmax_div = SB_gmax_div, SB_h_div = SB_h_div,
  PB_gmax_div = PB_gmax_div, PB_h_div = PB_h_div,
  default_9strain = new_strain_parameter(
    n_CB = num_CB_strains,
    n_SB = num_SB_strains,
    n_PB = num_PB_strains,
    values_initial_state =  "bush_ssfig3"
  )
)

minimum_abundances <- rep(1, 3)
names(minimum_abundances) <- c("CB", "PB", "SB")

grid_num_a <- 300 #usually 1000 ## number of a_0 values
grid_num_a <- 10 ## FOR TEST
a_Os <- 10^seq(-8, -0, length=grid_num_a) ## sequence of a_0 values
grid_num_N <- 2 ## number of N values
initial_CBs <- 10^seq(0, 10, length=grid_num_N) ## sequence of N values
initial_PBs <- 1e8 ## not varied
initial_SBs <- 1e8 ## not varied
# next line creates all possible combinations
ss_expt <- expand.grid(N_CB = initial_CBs,
                       N_PB = initial_PBs,
                       N_SB = initial_SBs,
                       a_O = a_Os)

parameter <- new_replication_ssfind_parameter(
  dynamic_model = parameter$dynamic_model,
  event_definition = eval(parse(text = event_def)),
  event_interval = 1000,
  noise_sigma = parameter$noise_sigma,
  minimum_abundances = minimum_abundances,
  sim_duration = sim_length,
  sim_sample_interval = sim_length,
  log10a_series = parameter$log10a_series,
  solver_method = parameter$solver_method,
  ss_expt = ss_expt
)
rm(minimum_abundances, grid_num_a, a_Os, grid_num_N, initial_CBs, initial_PBs, initial_SBs, ss_expt)

