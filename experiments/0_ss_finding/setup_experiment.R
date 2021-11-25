
# rm(list = ls())

knitr::opts_knit$set(
  progress = TRUE, 
  verbose = FALSE, 
  cache = TRUE
)

microxanox_release <- "0.3.1"

#tmplib <- tempfile()
#dir.create(tmplib)


### From '?remotes::install_github`:
# auth_token
#   To install from a private repo, generate a personal access token (PAT) in
#   "https://github.com/settings/tokens" and supply to this argument. This is
#   safer than using a password because you can easily delete a PAT without
#   affecting any others. Defaults to the GITHUB_PAT environment variable.

# remotes::install_github(
#   "opetchey/microxanox",
#   ref = microxanox_release,
#   # auth_token = "ENTER YOUR TOKEN or PROVED AS ENVIRONMENT VARIABLE",
#   build_vignettes = FALSE,
#   force = TRUE,
#   upgrade = FALSE,
#   lib = tmplib
# )

#library(microxanox, lib.loc = tmplib)

library(microxanox)
if (packageVersion("microxanox") < package_version("0.3.0")) {
  stop("microxanox version needs to be at least 0.3.0!")
}
library(tidyverse)
library(patchwork)
library(here)
source(here("R/various_useful_functions.r"))
zero <- 0 ## don't change
unity <- 1 ## don't change!!!
options(mc.cores = 7)
eval_ss_flag <- FALSE
plot_ss_results <- TRUE



sp <- new_strain_parameter(
  n_CB = num_CB_strains,
  n_PB = num_SB_strains,
  n_SB = num_PB_strains,
  values_initial_state = "bush_ssfig3"
)

parameter <- new_runsim_parameter(
  dynamic_model = bushplus_dynamic_model,
  event_definition = event_definition_1,
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

num_div_treatment_levels <- 5



## Create diversity


var_expt <- create_diversity_factorial(
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





wait_time <- 1000
#p$log10a_series <- seq(-2.5, -2, 0.025)
parameter$log10a_series <- seq(-8, 0, length = 10)
parameter$minimum_abundances["CB"] <- 1
parameter$minimum_abundances["SB"] <- 1
parameter$minimum_abundances["PB"] <- 1
parameter$event_interval <- 1000
parameter$sim_duration <- wait_time * length(parameter$log10a_series)
parameter$sim_sample_interval <- wait_time
total_initial_abundances <- 10^5
