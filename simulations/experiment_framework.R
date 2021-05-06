## Setup R ----

rm(list = ls())

# devtools::install_github("opetchey/microxanox",
#                          ref="main",
#                          auth_token = "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",
#                          build_vignettes = TRUE,
#                          force = TRUE)

library(tidyverse)
library(microxanox)
library(patchwork)
library(here)
source(here("simulations/r functions/various_useful_functions.r"))


## Initialise general simulation conditions ----
default_dynamic_model <- bushplus_dynamic_model
default_event_definition <- event_definition_1
## no default parameter values
default_event_interval <- 100
default_noise_sigma <- 0
default_minimum_abundances <- rep(100, 3)
names(default_minimum_abundances) <- c("CB", "PB", "SB") 
default_sim_duration <- 50000
default_sim_sample_interval <- 10
## note that next line (log10a_series is over-ridden with getting stable states)
#default_log10a_series <- c(-2, -2, -2, -2, -10, -10, -10, -10, -10)
num_CB_strains <- 9
num_SB_strains <- 9
num_PB_strains <- 9
default_9strain <- new_starter(n_CB = num_CB_strains,
                             n_SB = num_SB_strains,
                             n_PB = num_PB_strains,
                             values_initial_state = "bush_anoxic")


## Initialise general steady state finding setting ----
ssfind_minimum_abundances <- rep(0, 3)
names(ssfind_minimum_abundances) <- c("CB", "PB", "SB") 
ssfind_simulation_duration <- 100000
ssfind_simulation_sampling_interval <- ssfind_simulation_duration
ssfind_event_interval <- ssfind_simulation_duration
grid_num_a <- 100 ## number of a_0 values
a_Os <- 10^seq(-4.5, -1.5, length=grid_num_a) ## sequence of a_0 values
grid_num_N <- 2 ## number of N values
initial_CBs <- 10^seq(0, 8, length=grid_num_N) ## sequence of N values
initial_PBs <- 1e5 ## not varied
initial_SBs <- 1e5 ## not varied
## next line creates all possible combinations
ss_expt <- expand.grid(N_CB = initial_CBs,
                       N_PB = initial_PBs,
                       N_SB = initial_SBs,
                       a_O = a_Os)




## Factorial CB, SB, PB variation experiment ----
CB_var_gmax_s <- seq(0, 0.1, length=2)
CB_var_h_s = seq(0, -0.5, length=2)
SB_var_gmax_s <- seq(0, 0.1, length=2)
SB_var_h_s = seq(0, -0.5, length=2)
PB_var_gmax_s <- seq(0, 0.1, length=2)
PB_var_h_s = seq(0, -0.5, length=2)

var_expt <- crossing(CB_var_gmax_s,
                     CB_var_h_s,
                     SB_var_gmax_s,
                     SB_var_h_s,
                     PB_var_gmax_s,
                     PB_var_h_s)

var_expt <- var_expt %>%
  group_by(CB_var_gmax_s, CB_var_h_s,
           SB_var_gmax_s, SB_var_h_s,
           PB_var_gmax_s, PB_var_h_s,
  ) %>%
  do(pars = add_strain_var(default_9strain,
                           CB_var_gmax = .$CB_var_gmax_s,
                           CB_var_h = .$CB_var_h_s,
                           SB_var_gmax = .$SB_var_gmax_s,
                           SB_var_h = .$SB_var_h_s,
                           PB_var_gmax = .$PB_var_gmax_s,
                           PB_var_h = .$PB_var_h_s))
var_expt$pars[[1]]$CB
var_expt$pars[[1]]$SB
var_expt$pars[[1]]$PB

#var_expt <- var_expt[1,]

## Next chunck of code:
## For each line of var_expt, add strain variation, and get stable states.
var_expt <- var_expt %>%
  group_by(CB_var_gmax_s, CB_var_h_s,
           SB_var_gmax_s, SB_var_h_s,
           PB_var_gmax_s, PB_var_h_s) %>%
  do(pars = add_strain_var(default_9strain,
                           CB_var_gmax = .$CB_var_gmax_s,
                           CB_var_h = .$CB_var_h_s,
                           SB_var_gmax = .$SB_var_gmax_s,
                           SB_var_h = .$SB_var_h_s,
                           PB_var_gmax = .$PB_var_gmax_s,
                           PB_var_h = .$PB_var_h_s),
     ss_res = ss_by_a_N(ss_expt, .$pars[[1]]),
  )
## save results to file
#saveRDS(var_expt, here("simulations/sim data/ss_res_2.RDS"))

var_expt <- readRDS(here("simulations/sim data/ss_res_2.RDS"))

number <- 1
plot_ss_result1(var_expt$ss_res[[number]],
                var_expt$CB_var_gmax_s[number],
                var_expt$CB_var_h_s[number],
                var_expt$SB_var_gmax_s[number],
                var_expt$SB_var_h_s[number],
                var_expt$PB_var_gmax_s[number],
                var_expt$PB_var_h_s[number],
                filename_prefix = here("simulations/figures/ssres_CBSBPB_var"))

var_expt %>%
  group_by(CB_var_gmax_s, CB_var_h_s,
           SB_var_gmax_s, SB_var_h_s,
           PB_var_gmax_s, PB_var_h_s) %>%
  do(null = plot_ss_result1(.$ss_res[[1]],
                            .$CB_var_gmax_s,
                            .$CB_var_h_s,
                            .$SB_var_gmax_s,
                            .$SB_var_h_s,
                            .$PB_var_gmax_s,
                            .$PB_var_h_s,
                            filename_prefix = "ssres_CBSBPB_var"))

# ss_res_oi <- var_expt$ss_res[[1]]
# xxx <- ss_res_oi %>%
#   select(-initial_N_CB, -a_O) %>%
#   mutate(a = 10^a,
#          direction = c(rep("up", grid_num_a),
#                        rep("down", grid_num_a))) %>%
#   gather(species, quantity, 2:(ncol(.)-1)) %>% 
#   mutate(var_type=ifelse(grepl("B_", species), "Organism", "Substrate"),
#          functional_group = case_when(str_detect(species, "CB_") ~ "CB",
#                                       str_detect(species, "SB_") ~ "SB",
#                                       str_detect(species, "PB_") ~ "PB")) %>%
#   group_by(direction, a, functional_group) %>%
#   summarise(total_quantity = sum(quantity)) %>%
#   mutate(log10_total_quantity = log10(total_quantity+1))
# xxx %>%
#   dplyr::filter(functional_group == "CB") %>%
#   ggplot(aes(x = log10(a), y = log10_total_quantity)) +
#     geom_path()






## Gradient CB, SB, PB variation  experiment ----
CB_var_gmax_s <- seq(0, 0.1, length=20)
CB_var_h_s = seq(0, -0.5, length=20)
SB_var_gmax_s <- seq(0, 0.1, length=20)
SB_var_h_s = seq(0, -0.5, length=20)
PB_var_gmax_s <- seq(0, 0.1, length=20)
PB_var_h_s = seq(0, -0.5, length=20)

var_expt <- tibble(CB_var_gmax_s,
                     CB_var_h_s,
                     SB_var_gmax_s,
                     SB_var_h_s,
                     PB_var_gmax_s,
                     PB_var_h_s)

var_expt <- var_expt %>%
  group_by(CB_var_gmax_s, CB_var_h_s,
           SB_var_gmax_s, SB_var_h_s,
           PB_var_gmax_s, PB_var_h_s,
  ) %>%
  do(pars = add_strain_var(default_9strain,
                           CB_var_gmax = .$CB_var_gmax_s,
                           CB_var_h = .$CB_var_h_s,
                           SB_var_gmax = .$SB_var_gmax_s,
                           SB_var_h = .$SB_var_h_s,
                           PB_var_gmax = .$PB_var_gmax_s,
                           PB_var_h = .$PB_var_h_s))
var_expt$pars[[10]]$CB
var_expt$pars[[10]]$SB
var_expt$pars[[10]]$PB

## Next chunck of code:
## For each line of var_expt, add strain variation, and get stable states.
var_expt <- var_expt %>%
  group_by(CB_var_gmax_s, CB_var_h_s,
           SB_var_gmax_s, SB_var_h_s,
           PB_var_gmax_s, PB_var_h_s) %>%
  do(pars = add_strain_var(default_9strain,
                           CB_var_gmax = .$CB_var_gmax_s,
                           CB_var_h = .$CB_var_h_s,
                           SB_var_gmax = .$SB_var_gmax_s,
                           SB_var_h = .$SB_var_h_s,
                           PB_var_gmax = .$PB_var_gmax_s,
                           PB_var_h = .$PB_var_h_s),
     ss_res = ss_by_a_N(ss_expt, .$pars[[1]]),
  )
## save results to file
#saveRDS(var_expt, here("simulations/sim data/ss_res_3.RDS"))

var_expt <- readRDS(here("simulations/sim data/ss_res_3.RDS"))

number <- 1
plot_ss_result1(var_expt$ss_res[[number]],
                var_expt$CB_var_gmax_s[number],
                var_expt$CB_var_h_s[number],
                var_expt$SB_var_gmax_s[number],
                var_expt$SB_var_h_s[number],
                var_expt$PB_var_gmax_s[number],
                var_expt$PB_var_h_s[number],
                filename_prefix = here("simulations/figures/ssres_CBSBPB_var"))

getwd()

var_expt %>%
  group_by(CB_var_gmax_s, CB_var_h_s,
           SB_var_gmax_s, SB_var_h_s,
           PB_var_gmax_s, PB_var_h_s) %>%
  do(null = plot_ss_result1(.$ss_res[[1]],
                            .$CB_var_gmax_s,
                            .$CB_var_h_s,
                            .$SB_var_gmax_s,
                            .$SB_var_h_s,
                            .$PB_var_gmax_s,
                            .$PB_var_h_s,
                            filename_prefix = here("simulations/figures/ssres_CBSBPB_var")))

# ss_res_oi <- var_expt$ss_res[[1]]
# xxx <- ss_res_oi %>%
#   select(-initial_N_CB, -a_O) %>%
#   mutate(a = 10^a,
#          direction = c(rep("up", grid_num_a),
#                        rep("down", grid_num_a))) %>%
#   gather(species, quantity, 2:(ncol(.)-1)) %>% 
#   mutate(var_type=ifelse(grepl("B_", species), "Organism", "Substrate"),
#          functional_group = case_when(str_detect(species, "CB_") ~ "CB",
#                                       str_detect(species, "SB_") ~ "SB",
#                                       str_detect(species, "PB_") ~ "PB")) %>%
#   group_by(direction, a, functional_group) %>%
#   summarise(total_quantity = sum(quantity)) %>%
#   mutate(log10_total_quantity = log10(total_quantity+1))
# xxx %>%
#   dplyr::filter(functional_group == "CB") %>%
#   ggplot(aes(x = log10(a), y = log10_total_quantity)) +
#     geom_path()






## Some dynamics ----
default_log10a_series <- c(-2, -2, -2, -8, -8, -8, -6, -4, -2, -2)
var_expt <- readRDS(here("simulations/sim data/ss_res_3.RDS"))
sim_number <- 1
sim_res <- run_simulation(parameter_values = var_expt$pars[[sim_number]],
                          initial_state = var_expt$pars[[sim_number]]$initial_state)
plot_dynamics(sim_res)
ggsave(here("simulations/figures/switching_lowvar.pdf"), width = 10)

sim_number <- 20
sim_res <- run_simulation(parameter_values = var_expt$pars[[sim_number]],
                          initial_state = var_expt$pars[[sim_number]]$initial_state)
plot_dynamics(sim_res)
ggsave(here("simulations/figures/switching_highvar.pdf"), width = 10)




## Tolerance change experiment ----
## 9 strains defaults 

#inhib_const_mults <- 2^seq(-1, 0.4, length=10)

h_factor <- 1.5

h_factor_starter <- default_9strain
h_factor_starter$CB$h_SR_CB <- default_9strain$CB$h_SR_CB * h_factor 
h_factor_starter$SB$h_O_SB <- default_9strain$SB$h_O_SB * h_factor 
h_factor_starter$PB$h_O_PB <- default_9strain$PB$h_O_PB * h_factor 

CB_var_gmax_s <- 0 
CB_var_h_s <- 0

var_expt <- crossing(CB_var_gmax_s,
                     CB_var_h_s)

# var_expt <- var_expt %>%
#   group_by(CB_var_gmax_s, CB_var_h_s) %>%
#   do(pars = add_strain_var(default_9strain,
#                            CB_var_gmax = .$CB_var_gmax_s,
#                            CB_var_h = .$CB_var_h_s),
#      sim_res = run_simulation(
#        parameter_values = .$pars[[1]],
#        initial_state = .$pars[[1]]$initial_state
#      ),
#      null = plot_and_save(CB_var_gmax = .$CB_var_gmax_s,
#                    CB_var_h = .$CB_var_h_s,
#                    sim_res = .$sim_res[[1]])
#      )
# 
# var_expt <- var_expt %>%
#   group_by(CB_var_gmax_s, CB_var_h_s) %>%
#   do(pars = add_strain_var(h_factor_starter,
#                            CB_var_gmax = .$CB_var_gmax_s,
#                            CB_var_h = .$CB_var_h_s))


# next line runs the "experiment", calling various functions to do this.
#ss_9strain <- ss_by_a_N(ss_expt, default_9strain)
#ss_9strain <- ss_by_a_N(ss_expt, var_expt$pars[[1]])

var_expt <- var_expt %>%
  group_by(CB_var_gmax_s, CB_var_h_s) %>%
  do(pars = add_strain_var(h_factor_starter,
                           CB_var_gmax = .$CB_var_gmax_s,
                           CB_var_h = .$CB_var_h_s),
     ss_res = ss_by_a_N(ss_expt, .$pars[[1]]),
  )

number <- 1
plot_ss_result(var_expt$ss_res[[number]],
                var_expt$CB_var_gmax_s[number],
                var_expt$CB_var_h_s[number],
                var_expt$SB_var_gmax_s[number],
                var_expt$SB_var_h_s[number],
                var_expt$PB_var_gmax_s[number],
                var_expt$PB_var_h_s[number],
                filename_prefix = here("simulations/figures", paste0("h-factor", h_factor)))

# var_expt %>%
#   group_by(CB_var_gmax_s, CB_var_h_s) %>%
#   do(null = plot_ss_result1(.$ss_res[[1]],
#                             .$CB_var_gmax_s,
#                             .$CB_var_h_s,
#                             filename_prefix = "ssres_CB_var"))






## OLD, probably not working. CB var only experiment ----
## 9 strains defaults 
num_CB_strains <- 9
num_SB_strains <- 9
num_PB_strains <- 9
default_9strain <- new_starter(n_CB = num_CB_strains,
                               n_SB = num_SB_strains,
                               n_PB = num_PB_strains,
                               values_initial_state = "bush_anoxic")

CB_var_gmax_s <- seq(0, 0.006, 0.006)
CB_var_h_s <- seq(0, -0.04, -0.04)
var_expt <- crossing(CB_var_gmax_s,
                     CB_var_h_s)

var_expt <- tibble(CB_var_gmax_s = seq(0, 0.1, length=5),
                   CB_var_h_s = seq(0, -0.5, length=5))

CB_var_gmax_s <- seq(0, 0.1, length=10)
CB_var_h_s = seq(0, -0.5, length=10)
var_expt <- crossing(CB_var_gmax_s,
                     CB_var_h_s)

# var_expt <- var_expt %>%
#   group_by(CB_var_gmax_s, CB_var_h_s) %>%
#   do(pars = add_strain_var(default_9strain,
#                            CB_var_gmax = .$CB_var_gmax_s,
#                            CB_var_h = .$CB_var_h_s),
#      sim_res = run_simulation(
#        parameter_values = .$pars[[1]],
#        initial_state = .$pars[[1]]$initial_state
#      ),
#      null = plot_and_save(CB_var_gmax = .$CB_var_gmax_s,
#                    CB_var_h = .$CB_var_h_s,
#                    sim_res = .$sim_res[[1]])
#      )
# 


ssfind_minimum_abundances <- rep(0, 3)
names(ssfind_minimum_abundances) <- c("CB", "PB", "SB") 
ssfind_simulation_duration <- 100000
ssfind_simulation_sampling_interval <- ssfind_simulation_duration
ssfind_event_interval <- ssfind_simulation_duration
grid_num_a <- 100 ## number of a_0 values
a_Os <- 10^seq(-4.5, -1.5, length=grid_num_a) ## sequence of a_0 values
grid_num_N <- 2 ## number of N values
initial_CBs <- 10^seq(0, 8, length=grid_num_N) ## sequence of N values
initial_PBs <- 1e5 ## not varied
initial_SBs <- 1e5 ## not varied
## next line creates all possible combinations
ss_expt <- expand.grid(N_CB = initial_CBs,
                       N_PB = initial_PBs,
                       N_SB = initial_SBs,
                       a_O = a_Os)
var_expt <- var_expt %>%
  group_by(CB_var_gmax_s, CB_var_h_s) %>%
  do(pars = add_strain_var(default_9strain,
                           CB_var_gmax = .$CB_var_gmax_s,
                           CB_var_h = .$CB_var_h_s))
var_expt$pars[[5]]$CB

var_expt <- var_expt %>%
  group_by(CB_var_gmax_s, CB_var_h_s) %>%
  do(pars = add_strain_var(default_9strain,
                           CB_var_gmax = .$CB_var_gmax_s,
                           CB_var_h = .$CB_var_h_s),
     ss_res = ss_by_a_N(ss_expt, .$pars[[1]]),
  )

#saveRDS(var_expt, here("simulations/sim data/ss_res_1.RDS"))

number <- 1
plot_ss_result1(var_expt$ss_res[[number]],
                var_expt$CB_var_gmax_s[number],
                var_expt$CB_var_h_s[number])

var_expt %>%
  group_by(CB_var_gmax_s, CB_var_h_s) %>%
  do(null = plot_ss_result1(.$ss_res[[1]],
                            .$CB_var_gmax_s,
                            .$CB_var_h_s,
                            filename_prefix = "ssres_CB_var"))

# ss_res_oi <- var_expt$ss_res[[1]]
# xxx <- ss_res_oi %>%
#   select(-initial_N_CB, -a_O) %>%
#   mutate(a = 10^a,
#          direction = c(rep("up", grid_num_a),
#                        rep("down", grid_num_a))) %>%
#   gather(species, quantity, 2:(ncol(.)-1)) %>% 
#   mutate(var_type=ifelse(grepl("B_", species), "Organism", "Substrate"),
#          functional_group = case_when(str_detect(species, "CB_") ~ "CB",
#                                       str_detect(species, "SB_") ~ "SB",
#                                       str_detect(species, "PB_") ~ "PB")) %>%
#   group_by(direction, a, functional_group) %>%
#   summarise(total_quantity = sum(quantity)) %>%
#   mutate(log10_total_quantity = log10(total_quantity+1))
# xxx %>%
#   dplyr::filter(functional_group == "CB") %>%
#   ggplot(aes(x = log10(a), y = log10_total_quantity)) +
#     geom_path()



## OLD probably not working. 9 strains with no variation among strains ----
CB_var_gmax <- 0
CB_var_h <- 0
novar_9strain <- add_strain_var(default_9strain,
                                CB_var_gmax = CB_var_gmax,
                                CB_var_h = CB_var_h)
novar_9strain$CB
ggplot(novar_9strain$CB) +
  geom_point(aes(x = h_SR_CB, y = g_max_CB))

## run simulation
sim_res <- run_simulation(parameter_values = novar_9strain,
                          initial_state = novar_9strain$initial_state)
plot_dynamics_new(sim_res)
ggsave(paste0("various_stuff/working/figures/9strain_CBvar-",
              CB_var_gmax, "-", CB_var_h, ".pdf"),
       width = 10)



