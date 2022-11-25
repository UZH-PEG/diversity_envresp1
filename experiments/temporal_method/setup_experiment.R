## see file R/how_to_install_microxanox for (surprise) how to install the microxanox package

zero <- 0 ## don't change
unity <- 1 ## don't change!!!
options(mc.cores = 7)
eval_ss_flag <- FALSE
plot_ss_results <- TRUE

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

## for testing only, comment out to make a complete run
if(test_mode)
  num_div_treatment_levels <- 5


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

## filter for sub1
#var_expt <- var_expt_new %>%
#  filter(SB_var_gmax_s > 0.06)




# var_expt <- create_diversity_factorial(
#   zero = zero, unity = unity,
#   num_div_treatment_levels = num_div_treatment_levels,
#   CB_gmax_div = CB_gmax_div, CB_h_div = CB_h_div,
#   SB_gmax_div = SB_gmax_div, SB_h_div = SB_h_div,
#   PB_gmax_div = PB_gmax_div, PB_h_div = PB_h_div,
#   default_9strain = new_strain_parameter(
#     n_CB = num_CB_strains,
#     n_SB = num_SB_strains,
#     n_PB = num_PB_strains,
#     values_initial_state =  "bush_ssfig3"
#   )
# )
# ## some selection of treatments
# CB_vars <- unique(var_expt$CB_var_gmax_s)
# SB_vars <- unique(var_expt$SB_var_gmax_s)
# CB_var_expt <- var_expt %>%
#   filter(SB_var_gmax_s == 0)
# SBPB_var_expt <- var_expt %>%
#   filter(CB_var_gmax_s == 0)
# for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
#                    SB_var_gmax_s = sort(SB_vars))
# CBSBPB_var_expt <- var_expt %>%
#   right_join(for_join)
# var_expt_new <- CB_var_expt %>%
#   bind_rows(SBPB_var_expt) %>%
#   bind_rows(CBSBPB_var_expt) %>%
#   unique()
# var_expt <- var_expt_new





#wait_time <- 1e3 ## set in run_all.r
parameter$log10a_series <- seq(-8, 0, length = 300)

##### next two lines for testing purposes, comment out to make a complete run
#wait_time <- 1e2 ## for testing
if(test_mode)
  parameter$log10a_series <- seq(-8, 0, length = 30) ## for testing

##### end of lines for testing
parameter$minimum_abundances["CB"] <- 1
parameter$minimum_abundances["SB"] <- 1
parameter$minimum_abundances["PB"] <- 1
parameter$event_interval <- 1000
parameter$sim_duration <- wait_time * length(parameter$log10a_series)
parameter$sim_sample_interval <- wait_time
total_initial_abundances <- 10^5
#event_def <- event_definition
parameter$event_definition <- eval(parse(text = event_definition))

