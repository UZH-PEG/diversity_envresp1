---
title: "experiment 1"
author: "Owen Petchey"
date: "6/25/2021"
output:
  html_document:
    keep_md: yes
---

This experiment supersedes all previous ones.
It is a factorial manipulation of diversity of the three groups.
It takes about 50 hours to run while using 12 cores.


# Setup

## R


```r
# rm(list = ls())

knitr::opts_knit$set(
  progress = TRUE, 
  verbose = FALSE, 
  cache = TRUE
)

microxanox_release <- "0.2.3_beta"

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

library(microxanoxBeta)
if (packageVersion("microxanoxBeta") < package_version("0.2.3")) {
  stop("microxanox version needs to be at least 0.2.3!")
}
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
## ✓ readr   2.0.2     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(patchwork)
library(here)
```

```
## here() starts at /Users/rainer/git/diversity_envresp1
```

```r
source(here("R/various_useful_functions.r"))
zero <- 0 ## don't change
unity <- 1 ## don't change!!!
options(mc.cores = 7)
eval_dynamics_flag <- TRUE
```

## Version of `microxanox` package used: 0.2.2

## General simulation conditions


```r
# default_dynamic_model <- bushplus_dynamic_model
# default_event_definition <- event_definition_1
# default_event_interval <- 100
# default_noise_sigma <- 0
# default_minimum_abundances <- rep(1, 3)
# names(default_minimum_abundances) <- c("CB", "PB", "SB")
# default_sim_duration <- 80000
# default_sim_sample_interval <- 100
# initial_pars_from <- "bush_ssfig3"
## note that next line (log10a_series is over-ridden with getting stable states)
#default_log10a_series <- c(-2, -2, -2, -2, -10, -10, -10, -10, -10)

num_CB_strains <- 9
num_SB_strains <- 9
num_PB_strains <- 9

sp <- new_strain_parameter(
  n_CB = 9,
  n_PB = 9,
  n_SB = 9,

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
```


## Define diversity


```r
## multiplier of SBPB variation
CB_var_multiplier <- 2
SBPB_var_multiplier <- 6

CB_gmax_div <- 0.015789474 * CB_var_multiplier
CB_h_div <- -0.08 * CB_var_multiplier
SB_gmax_div <- 0.015789474 * SBPB_var_multiplier
SB_h_div <- -0.323  * SBPB_var_multiplier
PB_gmax_div <- 0.015789474  * SBPB_var_multiplier
PB_h_div <- -0.323  * SBPB_var_multiplier

## num_div_treatment_levels <- 20

num_div_treatment_levels <- 3 ## FOR TEST
```

## Create diversity


```r
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
```

## Display diversity


```r
display_diversity(
  9, ## FOR TEST was 400
  var_expt = var_expt,
  num_CB_strains = num_CB_strains,
  num_SB_strains = num_SB_strains,
  num_PB_strains = num_PB_strains
)
```

![](experiment-1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


# Temporal switching



```r
var_expt_levels <- var_expt[, 1:6]

no_diversity <- which(rowSums(abs(var_expt_levels)) == 0)
max_diversty_all <- which(
  max(rowSums(abs(var_expt_levels))) == rowSums(abs(var_expt_levels))
)
max_only_CB_diversity <- which(
  max(rowSums(abs(var_expt_levels[, 1:2]))) == rowSums(abs(var_expt_levels[, 1:2])) &
  rowSums(abs(var_expt_levels[,3:6])) == 0
)
# var_expt_levels[381,]

max_only_SBPB_diversity <- which(
  max(rowSums(abs(var_expt_levels[, 3:6]))) == rowSums(abs(var_expt_levels[, 3:6])) &
  rowSums(abs(var_expt_levels[, 1:2])) == 0
)
#var_expt_levels[20,]
```




## Oxic to anoxic

### No diversity


```r
# default_sim_duration <- 80000
parameter$sim_duration <- 80000
```


```r
# default_log10a_series <- c(-1, -7, -7)
parameter$log10a_series <- c(-1, -7, -7)
parameter$strain_parameter <- var_expt$pars[[no_diversity]]
parameter$strain_parameter$initial_state <- new_initial_state(
  num_CB_strains,
  num_PB_strains,
  num_SB_strains,
  values = "bush_ssfig3"
)
parameter$strain_parameter$initial_state[grep("CB_", names(parameter$strain_parameter$initial_state))] <- 10^10/num_CB_strains
sim_res_novar1 <- run_simulation(parameter)
saveRDS(sim_res_novar1, here("experiments/experiment 1/data/sim_res_novar1.RDS"))
```



```r
sim_res_novar1 <- readRDS(here("experiments/experiment 1/data/sim_res_novar1.RDS"))
plot_dynamics(sim_res_novar1)
```

![](experiment-1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
#ggsave(here("simulations/expt2/figures/switching_novar.pdf"), width = 10)
```

### Maximum diversity


```r
# Is this actually needed?
# sim_number <- num_div_treatment_levels

parameter$strain_parameter <- var_expt$pars[[max_diversty_all]]
parameter$strain_parameter$initial_state <- sim_res_novar1$strain_parameter$initial_state
sim_res_highvar1 <- run_simulation(parameter)
saveRDS(sim_res_highvar1, here("experiments/experiment 1/data/sim_res_highvar1.RDS"))
```



```r
sim_res_highvar1 <- readRDS(here("experiments/experiment 1/data/sim_res_highvar1.RDS"))
plot_dynamics(sim_res_highvar1)
```

![](experiment-1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
#ggsave(here("simulationsexpt2/figures/switching_highvar.pdf"), width = 10)
```

## Anoxic to oxic

### No diversity


```r
parameter$sim_duration <- 60000
```



```r
# sim_number <- 1
parameter$log10a_series <- c(-5, -3, -1, -1)
parameter$strain_parameter <- var_expt$pars[[no_diversity]]
parameter$strain_parameter$initial_state <- new_initial_state(
  num_CB_strains,
  num_PB_strains,
  num_SB_strains,
  values = "bush_ssfig3"
)
parameter$strain_parameter$initial_state[grep("CB_", names(parameter$strain_parameter$initial_state))] <- 10/num_CB_strains
sim_res_novar2 <- run_simulation(parameter)
saveRDS(sim_res_novar2, here("experiments/experiment 1/data/sim_res_novar2.RDS"))
```



```r
sim_res_novar2 <- readRDS(here("experiments/experiment 1/data/sim_res_novar2.RDS"))
plot_dynamics(sim_res_novar2)
```

![](experiment-1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
#ggsave(here("simulations/expt2/figures/switching_novar.pdf"), width = 10)
```

### Maximum diversity


```r
sim_number <- num_div_treatment_levels
parameter$strain_parameter <- var_expt$pars[[max_diversty_all]]
parameter$strain_parameter$initial_state <- sim_res_novar2$strain_parameter$initial_state
sim_res_highvar2 <- run_simulation(parameter)
saveRDS(sim_res_highvar2, here("experiments/experiment 1/data/sim_res_highvar2.RDS"))
```



```r
sim_res_highvar2 <- readRDS(here("experiments/experiment 1/data/sim_res_highvar2.RDS"))
plot_dynamics(sim_res_highvar2)
```

![](experiment-1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
#ggsave(here("simulationsexpt2/figures/switching_highvar.pdf"), width = 10)
```


## Anoxic to oxic to anoxic

### No diversity


```r
parameter$sim_duration <- 1000000
```


```r
parameter$minimum_abundances <- rep(100, 3)
names(parameter$minimum_abundances) <- c("CB", "PB", "SB")
```


```r
# sim_number1 <- 1
parameter$log10a_series <- c(-1, -8, -1)
parameter$strain_parameter <- var_expt$pars[[no_diversity]]
parameter$strain_parameter$initial_state <- new_initial_state(
  num_CB_strains,
  num_PB_strains,
  num_SB_strains,
  values = "bush_ssfig3"
)
parameter$strain_parameter$initial_state[grep("CB_", names(parameter$strain_parameter$initial_state))] <- 10/num_CB_strains
sim_res_novar3 <- run_simulation(parameter)
saveRDS(sim_res_novar3, here("experiments/experiment 1/data/sim_res_novar3.RDS"))
```



```r
sim_res_novar3 <- readRDS(here("experiments/experiment 1/data/sim_res_novar3.RDS"))
plot_dynamics(sim_res_novar3)
```

![](experiment-1_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
#ggsave(here("simulations/expt2/figures/switching_novar.pdf"), width = 10)
```

### Maximum diversity


```r
sim_number2 <- num_div_treatment_levels
parameter$strain_parameter <- var_expt$pars[[max_diversty_all]]
parameter$strain_parameter$initial_state <- sim_res_novar3$strain_parameter$initial_state
sim_res_highvar3 <- run_simulation(parameter)
saveRDS(sim_res_highvar3, here("experiments/experiment 1/data/sim_res_highvar3.RDS"))
```



```r
sim_res_highvar3 <- readRDS(here("experiments/experiment 1/data/sim_res_highvar3.RDS"))
plot_dynamics(sim_res_highvar3)
```

![](experiment-1_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
#ggsave(here("simulationsexpt2/figures/switching_highvar.pdf"), width = 10)
```

### Visualise


```r
visualise_temporal_env_eco(sim_res_novar3, sim_res_highvar3)
```



# Stable state finding

## Finding

### Setup parameter


```r
options(mc.cores = 7)
```



```r
# default_sim_duration <- 1000000
# ssfind_minimum_abundances <- rep(0, 3)
# names(ssfind_minimum_abundances) <- c("CB", "PB", "SB")
# sfind_simulation_duration <- default_sim_duration
# ssfind_simulation_sampling_interval <- ssfind_simulation_duration
# ssfind_event_interval <- ssfind_simulation_duration

minimum_abundances <- rep(0, 3)
names(minimum_abundances) <- c("CB", "PB", "SB")

## grid_num_a <- 1000 #usually 1000 ## number of a_0 values
grid_num_a <- 100 ## FOR TEST

a_Os <- 10^seq(-7, -0.5, length=grid_num_a) ## sequence of a_0 values
grid_num_N <- 2 ## number of N values
initial_CBs <- 10^seq(0, 10, length=grid_num_N) ## sequence of N values
initial_PBs <- 1e8 ## not varied
initial_SBs <- 1e8 ## not varied
# next line creates all possible combinations
ss_expt <- expand.grid(N_CB = initial_CBs,
                      N_PB = initial_PBs,
                      N_SB = initial_SBs,
                      a_O = a_Os)

parameter <- new_ss_by_a_N_parameter(
  dynamic_model = parameter$dynamic_model,
  event_definition = parameter$event_definition,
  event_interval = 1000000,
  noise_sigma = parameter$noise_sigma,
  minimum_abundances = minimum_abundances,
  sim_duration = 1000000,
  sim_sample_interval = 1000000,
  log10a_series = parameter$log10a_series,
  solver_method = parameter$solver_method,
  ss_expt = ss_expt
)
rm(minimum_abundances, grid_num_a, a_Os, grid_num_N, initial_CBs, initial_PBs, initial_SBs, ss_expt)
saveRDS(parameter, here("experiments/experiment 1/data/parameter_1e6_x2x6_factorial.RDS"))
saveRDS(var_expt, here("experiments/experiment 1/data/var_expt_1e6_x2x6_factorial.RDS"))
```

### Run stable state finding

*Careful, this simulation takes about 600 hours on a single core


```r
run_ss_var_experiment(
  parameter = readRDS(here("experiments/experiment 1/data/parameter_1e6_x2x6_factorial.RDS")), 
  var_expt = readRDS(here("experiments/experiment 1/data/var_expt_1e6_x2x6_factorial.RDS"))) %>%
saveRDS(here("experiments/experiment 1/data/ss_data_1e6_x2x6_factorial.RDS"))
```

### Process the stable state data

Bring in various stable state datasets

```r
cluster <- multidplyr::new_cluster(7)
multidplyr::cluster_library(cluster, c("microxanoxBeta", "dplyr"))

## sim length 80'000, 20 x 20 factorial, reference maximum diversity
# readRDS(here("experiments/experiment 1/data/ss_data_80000.RDS")) %>%
#   mutate(sim_length = 80000) %>% 
#   multidplyr::partition(cluster) %>%
#   mutate(stability_measures = list(get_stability_measures(ss_res))) %>%
#   collect() %>%
#   unnest(cols = c(stability_measures)) %>%
#   saveRDS(here("experiments/experiment 1/data/stab_data_80000.RDS"))



## sim length 1'000'000, 20 x 20 factorial, reference maximum diversity
# readRDS(here("experiments/experiment 1/data/ss_data_1000000_20factorial.RDS")) %>%
#   mutate(sim_length = 1000000) %>% 
#   multidplyr::partition(cluster) %>%
#   mutate(stability_measures = list(get_stability_measures(ss_res))) %>%
#   collect() %>%
#   unnest(cols = c(stability_measures)) %>%
#   saveRDS(here("experiments/experiment 1/data/stab_data_1000000_20factorial.RDS"))


## sim length 1'000'000, 20 SBPBgrad, 5x maximum diversity
# readRDS(here("experiments/experiment 1/data/ss_data_1e6_noCB_5xSBPB_.RDS")) %>%
#   mutate(sim_length = 1000000) %>% 
#   multidplyr::partition(cluster) %>%
#   mutate(stability_measures = list(get_stability_measures(ss_res))) %>%
#   collect() %>%
#   unnest(cols = c(stability_measures)) %>%
#   saveRDS(here("experiments/experiment 1/data/stab_data_1e6_noCB_5xSBPB_.RDS"))


## sim length 300'000, 20 SBPBgrad, reference maximum diversity
# readRDS(here("experiments/experiment 1/data/ss_data_300000_small.RDS")) %>%
#   mutate(sim_length = 300000) %>% 
#   multidplyr::partition(cluster) %>%
#   mutate(stability_measures = list(get_stability_measures(ss_res))) %>%
#   collect() %>%
#   unnest(cols = c(stability_measures)) %>%
#   saveRDS(here("experiments/experiment 1/data/stab_data_300000.RDS"))

## sim length 1'000'000, 20 SBPBgrad, 2xCB variation, 6xSBPB variation
readRDS(here("experiments/experiment 1/data/ss_data_1e6_x2x6_factorial.RDS")) %>%
  mutate(sim_length = 1000000) %>% 
  multidplyr::partition(cluster) %>%
  mutate(stability_measures = list(get_stability_measures(ss_res))) %>%
  collect() %>%
  unnest(cols = c(stability_measures)) %>%
  saveRDS(here("experiments/experiment 1/data/stab_data_1e6_x2x6_factorial.RDS"))
```


## SS, no diversity, all diversity, CB only, and SBPB only
No Experimental data


```r
## find various combinations of diversity
var_expt <- readRDS(here("experiments/experiment 1/data/ss_data_1000000_20factorial.RDS"))

var_expt_levels <- var_expt[,1:6]

no_diversity <- which(rowSums(abs(var_expt_levels))==0)
max_diversty_all <- which(max(rowSums(abs(var_expt_levels))) ==
                            rowSums(abs(var_expt_levels)))
max_only_CB_diversity <- which(max(rowSums(abs(var_expt_levels[,1:2]))) ==
                            rowSums(abs(var_expt_levels[,1:2])) &
                              rowSums(abs(var_expt_levels[,3:6]))==0)
#var_expt_levels[381,]

max_only_SBPB_diversity <- which(max(rowSums(abs(var_expt_levels[,3:6]))) ==
                            rowSums(abs(var_expt_levels[,3:6])) &
                              rowSums(abs(var_expt_levels[,1:2]))==0)
#var_expt_levels[20,]
```



```r
p1  <- plot_ss_result1(var_expt,
                result_index = no_diversity,
                filename_prefix = NULL,
                save_image_file = FALSE)
p1


#junk1 <- var_expt[no_diversity,]$ss_res[[1]]
```


```r
p2  <- plot_ss_result1(var_expt,
                result_index = max_diversty_all,
                filename_prefix = NULL,
                save_image_file = FALSE)
p2
```


```r
p3  <- plot_ss_result1(var_expt,
                result_index = max_only_CB_diversity,
                filename_prefix = NULL,
                save_image_file = FALSE)
p3
```


```r
p4  <- plot_ss_result1(var_expt,
                result_index = max_only_SBPB_diversity,
                filename_prefix = NULL,
                save_image_file = FALSE)
p4
```



```r
p_overlay1 <- plot_ss_result2(var_expt[no_diversity,]$ss_res[[1]],
                             var_expt[max_diversty_all,]$ss_res[[1]],
                             xlims = c(-7, -1))
p_overlay1
```


```r
p_overlay2 <- plot_ss_result2(var_expt[no_diversity,]$ss_res[[1]],
                             var_expt[max_only_CB_diversity,]$ss_res[[1]],
                             xlims = c(-7, -1))
p_overlay2

#ss_result1 <- var_expt[no_diversity,]$ss_res[[1]]
#ss_result2 <- var_expt[max_only_CB_diversity,]$ss_res[[1]]
```


```r
p_overlay3 <- plot_ss_result2(var_expt[no_diversity,]$ss_res[[1]],
                             var_expt[max_only_SBPB_diversity,]$ss_res[[1]],
                             xlims = c(-7, -1))
p_overlay3


#ss_result3 <- var_expt[max_only_SBPB_diversity,]$ss_res[[1]]
```

## Look at stability measures
No experimental data.

### Calculations

```r
stab_data <- bind_rows(
  readRDS(here("experiments/experiment 1/data/stab_data_1000000_20factorial.RDS")), 
  readRDS(here("experiments/experiment 1/data/stab_data_80000.RDS"))
)

CB_vars <- unique(stab_data$CB_var_gmax_s)
SB_vars <- unique(stab_data$CB_var_gmax_s)

CB_stab_data <- stab_data %>%
  filter(SB_var_gmax_s == 0) %>%
  mutate(var_treat = "CB",
         var_gmax = CB_var_gmax_s)

SBPB_stab_data <- stab_data %>%
  filter(CB_var_gmax_s == 0) %>%
  mutate(var_treat = "SB-PB",
         var_gmax = SB_var_gmax_s)

for_join <- tibble(CB_var_gmax_s = CB_vars,
                     SB_var_gmax_s = SB_vars)
CBSBPB_stab_data <- stab_data %>%
  right_join(for_join) %>%
  mutate(var_treat = "CB-SB-PB",
         var_gmax = CB_var_gmax_s)

all_stab_results <- CB_stab_data %>%
  bind_rows(SBPB_stab_data) %>%
#  bind_rows(results3) %>%
 # bind_rows(results4) %>%
  bind_rows(CBSBPB_stab_data)

all_stab_results <- all_stab_results %>%
    mutate(var_treat = forcats::fct_relevel(var_treat, levels = c("CB", "SB-PB", "CB-SB-PB")))

saveRDS(all_stab_results, here("experiments/experiment 1/data/all_stab.RDS"))
```

### Plot raw

```r
all_stab_results <- readRDS(here("experiments/experiment summary/all_stab.RDS"))

all_stab_results %>%
  filter(Species == "O") %>%
  ggplot(aes(x = var_gmax, y = hyst_range_raw, col=var_treat, shape = as_factor(sim_length))) +
  geom_point() +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Extent of bistability region\n[log10 oxygen diffusivity]") +
  labs(col = "Variation in\nonly these\nfunctional groups")
##ggsave("manuscript/figures/extent_of_bistab1.pdf", height = 4)

all_stab_results %>%
  #filter(var_treat == "CB") %>%
  filter(Species == "O",
         sim_length == 1e6) %>%
  ggplot(aes(x = var_gmax,
             ymin = hyst_min_raw,
             ymax = hyst_max_raw,
             fill=var_treat)) +
  geom_ribbon(alpha = 0.5) +
  facet_wrap( ~ var_treat, nrow = 3) +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Oxygen diffusivity\n[log10 uM per hour]") +
  labs(fill = "Variation in\nonly these\nfunctional groups") +
  coord_flip() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

all_stab_results %>%
  #filter(var_treat == "CB") %>%
  filter(Species == "O",
         sim_length == 8e4) %>%
  ggplot(aes(x = var_gmax,
             ymin = hyst_min_raw,
             ymax = hyst_max_raw,
             fill=var_treat)) +
  geom_ribbon(alpha = 0.5) +
  facet_wrap( ~ var_treat, nrow = 3) +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Oxygen diffusivity\n[log10 uM per hour]") +
  labs(fill = "Variation in\nonly these\nfunctional groups") +
  coord_flip() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )



##ggsave("manuscript/figures/extent_of_bistab2.pdf", height = 4)

bind_rows(
  readRDS(here("experiments/experiment 1/data/stab_data_1000000_20factorial.RDS")), 
  readRDS(here("experiments/experiment 1/data/stab_data_1e6_noCB_5xSBPB_.RDS"))
) %>%
  filter(
    Species == "O",
    sim_length == 1e6
  ) %>%
  ggplot(aes(x = CB_var_gmax_s, y = SB_var_gmax_s, fill = hyst_range_raw, col = hyst_range_raw)) +
  geom_point(size = 3)
```

### Plot log

```r
all_stab_results <- readRDS(here("experiments/experiment summary/all_stab.RDS"))

all_stab_results %>%
  filter(Species == "O") %>%
  ggplot(aes(x = var_gmax, y = hyst_range_log, col=var_treat, shape = as_factor(sim_length))) +
  geom_point() +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Extent of bistability region\n[log10 oxygen diffusivity]") +
  labs(col = "Variation in\nonly these\nfunctional groups")
##ggsave("manuscript/figures/extent_of_bistab1.pdf", height = 4)

all_stab_results %>%
  #filter(var_treat == "CB") %>%
  filter(Species == "O",
         sim_length == 1e6) %>%
  ggplot(aes(x = var_gmax,
             ymin = hyst_min_log,
             ymax = hyst_max_log,
             fill=var_treat)) +
  geom_ribbon(alpha = 0.5) +
  facet_wrap( ~ var_treat, nrow = 3) +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Oxygen diffusivity\n[log10 uM per hour]") +
  labs(fill = "Variation in\nonly these\nfunctional groups") +
  coord_flip() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

all_stab_results %>%
  #filter(var_treat == "CB") %>%
  filter(Species == "O",
         sim_length == 8e4) %>%
  ggplot(aes(x = var_gmax,
             ymin = hyst_min_log,
             ymax = hyst_max_log,
             fill=var_treat)) +
  geom_ribbon(alpha = 0.5) +
  facet_wrap( ~ var_treat, nrow = 3) +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Oxygen diffusivity\n[log10 uM per hour]") +
  labs(fill = "Variation in\nonly these\nfunctional groups") +
  coord_flip() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )



##ggsave("manuscript/figures/extent_of_bistab2.pdf", height = 4)

bind_rows(
  readRDS(here("experiments/experiment 1/data/stab_data_1000000_20factorial.RDS")), 
  readRDS(here("experiments/experiment 1/data/stab_data_1e6_noCB_5xSBPB_.RDS"))
) %>%
  filter(
    Species == "O",
    sim_length == 1e6
  ) %>%
  ggplot(aes(x = CB_var_gmax_s, y = SB_var_gmax_s, fill = hyst_range_log, col = hyst_range_log)) +
  geom_point(size = 3)
```

## Extra SBPB diversity
No experimental data.


```r
var_expt_x <- readRDS(here("experiments/experiment 1/data/ss_data_1e6_noCB_5xSBPB_.RDS"))
stab_data_x <- readRDS(here("experiments/experiment 1/data/stab_data_1e6_noCB_5xSBPB_.RDS"))

p1  <- plot_ss_result1(var_expt_x,
                result_index = 1,
                filename_prefix = NULL,
                save_image_file = FALSE)
p1

p1  <- plot_ss_result1(var_expt_x,
                result_index = 19,
                filename_prefix = NULL,
                save_image_file = FALSE)
p1

p_overlay1 <- plot_ss_result2(var_expt_x[1,]$ss_res[[1]],
                             var_expt_x[19,]$ss_res[[1]],
                             xlims = c(-7, -1))
p_overlay1
```

### raw

```r
stab_data_x %>%
  filter(Species == "O") %>%
  ggplot(aes(x = SB_var_gmax_s, y = hyst_range_raw)) +
  geom_point() +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Extent of bistability region\n[log10 oxygen diffusivity]") +
  labs(col = "Variation in\nonly these\nfunctional groups")
##ggsave("manuscript/figures/extent_of_bistab1.pdf", height = 4)

stab_data_x %>%
  #filter(var_treat == "CB") %>%
  filter(Species == "O") %>%
  ggplot(aes(x = SB_var_gmax_s,
             ymin = hyst_min_raw,
             ymax = hyst_max_raw)) +
  geom_ribbon(alpha = 0.5) +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Oxygen diffusivity\n[log10 uM per hour]") +
  coord_flip() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
```

### log

```r
stab_data_x %>%
  filter(Species == "O") %>%
  ggplot(aes(x = SB_var_gmax_s, y = hyst_range_log)) +
  geom_point() +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Extent of bistability region\n[log10 oxygen diffusivity]") +
  labs(col = "Variation in\nonly these\nfunctional groups")
##ggsave("manuscript/figures/extent_of_bistab1.pdf", height = 4)

stab_data_x %>%
  #filter(var_treat == "CB") %>%
  filter(Species == "O") %>%
  ggplot(aes(x = SB_var_gmax_s,
             ymin = hyst_min_log,
             ymax = hyst_max_log)) +
  geom_ribbon(alpha = 0.5) +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Oxygen diffusivity\n[log10 uM per hour]") +
  coord_flip() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
```



## 2x CB, 6xSBPB diversity


```r
## find various combinations of diversity
var_expt <- readRDS(here("experiments/experiment 1/data/ss_data_1e6_x2x6_factorial.RDS"))
stab_data <- readRDS(here("experiments/experiment 1/data/stab_data_1e6_x2x6_factorial.RDS"))

var_expt_levels <- var_expt[,1:6]

no_diversity <- which(rowSums(abs(var_expt_levels))==0)
max_diversty_all <- which(max(rowSums(abs(var_expt_levels))) ==
                            rowSums(abs(var_expt_levels)))
max_only_CB_diversity <- which(max(rowSums(abs(var_expt_levels[,1:2]))) ==
                            rowSums(abs(var_expt_levels[,1:2])) &
                              rowSums(abs(var_expt_levels[,3:6]))==0)
#var_expt_levels[381,]

max_only_SBPB_diversity <- which(max(rowSums(abs(var_expt_levels[,3:6]))) ==
                            rowSums(abs(var_expt_levels[,3:6])) &
                              rowSums(abs(var_expt_levels[,1:2]))==0)
#var_expt_levels[20,]
```



```r
p1  <- plot_ss_result1(var_expt,
                result_index = no_diversity,
                filename_prefix = NULL,
                save_image_file = FALSE)
p1
```

![](experiment-1_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

```r
#junk1 <- var_expt[no_diversity,]$ss_res[[1]]
```


```r
p2  <- plot_ss_result1(var_expt,
                result_index = max_diversty_all,
                filename_prefix = NULL,
                save_image_file = FALSE)
p2
```

![](experiment-1_files/figure-html/unnamed-chunk-42-1.png)<!-- -->


```r
p3  <- plot_ss_result1(var_expt,
                result_index = max_only_CB_diversity,
                filename_prefix = NULL,
                save_image_file = FALSE)
p3
```

![](experiment-1_files/figure-html/unnamed-chunk-43-1.png)<!-- -->


```r
p4  <- plot_ss_result1(var_expt,
                result_index = max_only_SBPB_diversity,
                filename_prefix = NULL,
                save_image_file = FALSE)
p4
```

![](experiment-1_files/figure-html/unnamed-chunk-44-1.png)<!-- -->



```r
p_overlay1 <- plot_ss_result2(var_expt[no_diversity,]$ss_res[[1]],
                             var_expt[max_diversty_all,]$ss_res[[1]],
                             xlims = c(-7, -1))
```

```
## `summarise()` has grouped output by 'a', 'direction', 'var_type'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'a', 'direction', 'var_type'. You can override using the `.groups` argument.
```

```r
p_overlay1
```

```
## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).
```

```
## Warning: Removed 64 row(s) containing missing values (geom_path).

## Warning: Removed 64 row(s) containing missing values (geom_path).
```

![](experiment-1_files/figure-html/unnamed-chunk-45-1.png)<!-- -->


```r
p_overlay2 <- plot_ss_result2(var_expt[no_diversity,]$ss_res[[1]],
                             var_expt[max_only_CB_diversity,]$ss_res[[1]],
                             xlims = c(-7, -1))
```

```
## `summarise()` has grouped output by 'a', 'direction', 'var_type'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'a', 'direction', 'var_type'. You can override using the `.groups` argument.
```

```r
p_overlay2
```

```
## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).
```

```
## Warning: Removed 64 row(s) containing missing values (geom_path).

## Warning: Removed 64 row(s) containing missing values (geom_path).
```

![](experiment-1_files/figure-html/unnamed-chunk-46-1.png)<!-- -->

```r
#ss_result1 <- var_expt[no_diversity,]$ss_res[[1]]
#ss_result2 <- var_expt[max_only_CB_diversity,]$ss_res[[1]]
```


```r
p_overlay3 <- plot_ss_result2(var_expt[no_diversity,]$ss_res[[1]],
                             var_expt[max_only_SBPB_diversity,]$ss_res[[1]],
                             xlims = c(-7, -1))
```

```
## `summarise()` has grouped output by 'a', 'direction', 'var_type'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'a', 'direction', 'var_type'. You can override using the `.groups` argument.
```

```r
p_overlay3
```

```
## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).

## Warning: Removed 16 row(s) containing missing values (geom_path).
```

```
## Warning: Removed 64 row(s) containing missing values (geom_path).

## Warning: Removed 64 row(s) containing missing values (geom_path).
```

![](experiment-1_files/figure-html/unnamed-chunk-47-1.png)<!-- -->

```r
#ss_result3 <- var_expt[max_only_SBPB_diversity,]$ss_res[[1]]
```



```r
tempp1 <- var_expt[no_diversity,]$ss_res[[1]] %>%
  filter(initial_N_CB == 1e10,
         log10(a_O) > -4.5, log10(a_O) < -4)
tempp2 <- var_expt[max_only_SBPB_diversity,]$ss_res[[1]] %>%
  filter(initial_N_CB == 1e10,
         log10(a_O) > -4.5, log10(a_O) < -4)

ggplot() +
  geom_line(mapping = aes(x = log10(tempp1$a_O),
                          y = log10(tempp1$O))) +
  geom_line(mapping = aes(x = log10(tempp2$a_O),
                          y = log10(tempp2$O)),
            col = "blue")
```

![](experiment-1_files/figure-html/unnamed-chunk-48-1.png)<!-- -->

```r
ggplot() +
  geom_histogram(mapping = aes(x = log10(tempp1$O) - log10(tempp2$O)),
            col = "blue")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](experiment-1_files/figure-html/unnamed-chunk-48-2.png)<!-- -->

```r
ggplot() +
  geom_line(mapping = aes(x = log10(tempp1$a_O),
    y = log10(tempp1$P) - log10(tempp2$P)),
            col = "blue")
```

![](experiment-1_files/figure-html/unnamed-chunk-48-3.png)<!-- -->




```r
CB_vars <- unique(stab_data$CB_var_gmax_s)
SB_vars <- unique(stab_data$SB_var_gmax_s)

CB_stab_data <- stab_data %>%
  filter(SB_var_gmax_s == 0) %>%
  mutate(var_treat = "CB",
         var_gmax = CB_var_gmax_s)

SBPB_stab_data <- stab_data %>%
  filter(CB_var_gmax_s == 0) %>%
  mutate(var_treat = "SB-PB",
         var_gmax = SB_var_gmax_s)

for_join <- tibble(CB_var_gmax_s = CB_vars,
                     SB_var_gmax_s = SB_vars)
CBSBPB_stab_data <- stab_data %>%
  right_join(for_join) %>%
  mutate(var_treat = "CB-SB-PB",
         var_gmax = CB_var_gmax_s)
```

```
## Joining, by = c("CB_var_gmax_s", "SB_var_gmax_s")
```

```r
all_stab_results <- CB_stab_data %>%
  bind_rows(SBPB_stab_data) %>%
#  bind_rows(results3) %>%
 # bind_rows(results4) %>%
  bind_rows(CBSBPB_stab_data)

all_stab_results <- all_stab_results %>%
    mutate(var_treat = forcats::fct_relevel(var_treat, levels = c("CB", "SB-PB", "CB-SB-PB")))
```

```
## Warning: Outer names are only allowed for unnamed scalar atomic inputs

## Warning: Outer names are only allowed for unnamed scalar atomic inputs
```

```
## Warning: Unknown levels in `f`: CB, CB-SB-PB
```

```
## Warning: Outer names are only allowed for unnamed scalar atomic inputs
```

```
## Warning: Unknown levels in `f`: CB, CB-SB-PB
```

```
## Warning: Outer names are only allowed for unnamed scalar atomic inputs
```

```
## Warning: Unknown levels in `f`: SB-PB, CB-SB-PB
```

```
## Warning: Outer names are only allowed for unnamed scalar atomic inputs
```

```
## Warning: Unknown levels in `f`: CB, SB-PB
```

```
## Warning: Outer names are only allowed for unnamed scalar atomic inputs
```

```
## Warning: Unknown levels in `f`: SB-PB, CB-SB-PB
```

```
## Warning: Outer names are only allowed for unnamed scalar atomic inputs
```

```
## Warning: Unknown levels in `f`: CB, SB-PB
```

```r
#saveRDS(all_stab_results, here("experiments/experiment summary/all_stab.RDS"))


#all_stab_results <- readRDS(here("experiments/experiment summary/all_stab.RDS"))
```

### raw

```r
all_stab_results %>%
  filter(Species == "O") %>%
  ggplot(aes(x = var_gmax, y = hyst_range_raw, col=var_treat, shape = as_factor(sim_length))) +
  geom_point() +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Extent of bistability region\n[log10 oxygen diffusivity]") +
  labs(col = "Variation in\nonly these\nfunctional groups")
```

![](experiment-1_files/figure-html/unnamed-chunk-50-1.png)<!-- -->

```r
##ggsave("manuscript/figures/extent_of_bistab1.pdf", height = 4)

all_stab_results %>%
  #filter(var_treat == "CB") %>%
  filter(Species == "O",
         sim_length == 1e6) %>%
  ggplot(aes(x = var_gmax,
             ymin = hyst_min_raw,
             ymax = hyst_max_raw,
             fill=var_treat)) +
  geom_ribbon(alpha = 0.5) +
  facet_wrap( ~ var_treat, nrow = 3) +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Oxygen diffusivity\n[log10 uM per hour]") +
  labs(fill = "Variation in\nonly these\nfunctional groups") +
  coord_flip() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
```

![](experiment-1_files/figure-html/unnamed-chunk-50-2.png)<!-- -->

```r
##ggsave("manuscript/figures/extent_of_bistab2.pdf", height = 4)
stab_data %>%
  filter(Species == "O",
         sim_length == 1e6) %>%
  ggplot(aes(x = CB_var_gmax_s, y = SB_var_gmax_s, fill = hyst_range_raw, col = hyst_range_raw)) +
  geom_point(size = 3)
```

![](experiment-1_files/figure-html/unnamed-chunk-50-3.png)<!-- -->

```r
##ggsave("manuscript/figures/extent_of_bistab2.pdf", height = 4)

stab_data %>%
  filter(Species == "O",
         sim_length == 1e6) %>%
  ggplot(aes(x = CB_var_gmax_s, y = hyst_range_raw, col = as.factor(SB_var_gmax_s))) +
  geom_line(size = 2)
```

![](experiment-1_files/figure-html/unnamed-chunk-50-4.png)<!-- -->

### log

```r
all_stab_results %>%
  filter(Species == "O") %>%
  ggplot(aes(x = var_gmax, y = hyst_range_log, col=var_treat, shape = as_factor(sim_length))) +
  geom_point() +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Extent of bistability region\n[log10 oxygen diffusivity]") +
  labs(col = "Variation in\nonly these\nfunctional groups")
```

![](experiment-1_files/figure-html/unnamed-chunk-51-1.png)<!-- -->

```r
##ggsave("manuscript/figures/extent_of_bistab1.pdf", height = 4)

all_stab_results %>%
  #filter(var_treat == "CB") %>%
  filter(Species == "O",
         sim_length == 1e6) %>%
  ggplot(aes(x = var_gmax,
             ymin = hyst_min_log,
             ymax = hyst_max_log,
             fill=var_treat)) +
  geom_ribbon(alpha = 0.5) +
  facet_wrap( ~ var_treat, nrow = 3) +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Oxygen diffusivity\n[log10 uM per hour]") +
  labs(fill = "Variation in\nonly these\nfunctional groups") +
  coord_flip() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
```

![](experiment-1_files/figure-html/unnamed-chunk-51-2.png)<!-- -->

```r
##ggsave("manuscript/figures/extent_of_bistab2.pdf", height = 4)
stab_data %>%
  filter(Species == "O",
         sim_length == 1e6) %>%
  ggplot(aes(x = CB_var_gmax_s, y = SB_var_gmax_s, fill = hyst_range_log, col = hyst_range_log)) +
  geom_point(size = 3)
```

![](experiment-1_files/figure-html/unnamed-chunk-51-3.png)<!-- -->

```r
##ggsave("manuscript/figures/extent_of_bistab2.pdf", height = 4)

stab_data %>%
  filter(Species == "O",
         sim_length == 1e6) %>%
  ggplot(aes(x = CB_var_gmax_s, y = hyst_range_log, col = as.factor(SB_var_gmax_s))) +
  geom_line(size = 2)
```

![](experiment-1_files/figure-html/unnamed-chunk-51-4.png)<!-- -->

## What effect of changing the length of the simulations
No Experimental data.


```r
var_expt1 <- readRDS(here("experiments/experiment 1/data/ss_data_1000000_20factorial.RDS"))
var_expt2 <- readRDS(here("experiments/experiment 1/data/ss_data_80000.RDS"))

p1_short  <- plot_ss_result1(var_expt2,
                result_index = max_diversty_all,
                filename_prefix = NULL,
                save_image_file = FALSE)
p1_short

p1_long  <- plot_ss_result1(var_expt1,
                result_index = max_diversty_all,
                filename_prefix = NULL,
                save_image_file = FALSE)
p1_long
```


# FROM HERE NOT CHANGED TO NEW FORMAT!!!!!!
# Some puzzles

## Puzzle 1

When the environment ameliorates for the sulphur bacteria, there is no strain replacement (in final state) along the oxygen diffusivity gradient -- either the system is oxic or the least tolerant strain SB9 dominates. And yet the switch to anoxic occurs earlier than when there is no diversity, which suggests there is some role of the more tolerant strains. Indeed Uriah showed that the presence of only the most tolerant strain is sufficient to give an earlier switch, even though it is not present in the final state when less tolerant strains are present. And he showed that the presence of only the least tolerant strain creates a later switch than when there are more tolerant strains.

The explanation is that the most tolerant strain does play a role, but only a transient one. The following dynamics are for the system starting oxic, and with a value of oxygen diffusivity (log10(a_O) = -4.8) for which the system remains oxic if there is no diversity, but switches to anoxic if there is diversity. There is only diversity in the sulphur bacteria. The most tolerant strain does at first grow, but is then outcompeted by less tolerant strains as the environment ameliorates (temporally).

TODO CHECK THE PARAMETER


```r
var_expt <- readRDS(here("experiments/experiment 1/data/ss_data_1e6_x2x6_factorial.RDS"))
var_expt_levels <- var_expt[,1:6]
no_diversity <- which(rowSums(abs(var_expt_levels))==0)
max_diversty_all <- which(max(rowSums(abs(var_expt_levels))) ==
                            rowSums(abs(var_expt_levels)))
max_only_CB_diversity <- which(max(rowSums(abs(var_expt_levels[,1:2]))) ==
                            rowSums(abs(var_expt_levels[,1:2])) &
                              rowSums(abs(var_expt_levels[,3:6]))==0)
max_only_SBPB_diversity <- which(max(rowSums(abs(var_expt_levels[,3:6]))) ==
                            rowSums(abs(var_expt_levels[,3:6])) &
                              rowSums(abs(var_expt_levels[,1:2]))==0)
```


```r
parameter <- readRDS(here("experiments/experiment 1/data/sim_res_novar3.RDS"))
parameter$sim_duration <- 100000
parameter$result <- NULL
```


```r
parameter$log10a_series <- c(-4.8, -4.8)
parameter$strain_parameter$initial_state <- new_initial_state(
  nrow(parameter$strain_parameter$CB),
  nrow(parameter$strain_parameter$PB),
  nrow(parameter$strain_parameter$PB),
  values = "bush_ssfig3"
)
parameter$strain_parameter$initial_state[grep("CB_", names(parameter$strain_parameter$initial_state))] <- 10^10/nrow(parameter$strain_parameter$CB)


sim_res_novar1 <- run_simulation(parameter)
saveRDS(sim_res_novar1, here("experiments/experiment 1/data/puzzle1_1.RDS"))
```



```r
sim_res_novar1 <- readRDS(here("experiments/experiment 1/data/puzzle1_1.RDS"))
plot_dynamics(sim_res_novar1)
#ggsave(here("simulations/expt2/figures/switching_novar.pdf"), width = 10)
```



```r
sim_res_novar1 <- run_simulation(parameter_values = var_expt$pars[[max_only_SBPB_diversity]],
                          initial_state = initial_state)
saveRDS(sim_res_novar1, here("experiments/experiment 1/data/puzzle1_2.RDS"))
```


```r
sim_res_novar1 <- readRDS(here("experiments/experiment 1/data/puzzle1_2.RDS"))
plot_dynamics(sim_res_novar1)
#ggsave(here("simulations/expt2/figures/switching_novar.pdf"), width = 10)
```


# Zoom in on SS


```r
a_Os <- 10^seq(-2.479, -2.4785, length=grid_num_a) ## sequence of a_0 values
initial_CBs <- 1#10^seq(0, 0, length=grid_num_N) ## sequence of N values
initial_PBs <- 1e8 ## not varied
initial_SBs <- 1e8 ## not varied
# next line creates all possible combinations
ss_expt <- expand.grid(N_CB = initial_CBs,
                      N_PB = initial_PBs,
                      N_SB = initial_SBs,
                      a_O = a_Os)
var_expt_master <- var_expt
var_expt <- var_expt_master[381,]
```



```r
#var_expt <- run_ss_var_experiment()
#saveRDS(var_expt, here("experiments/experiment 1/data/ss_data_zoom.RDS"))
```



```r
library(here)
zoom <- readRDS(here("experiments/experiment 1/data/ss_data_zoom.RDS"))

p1  <- plot_ss_result1(zoom,
                result_index = 1,
                filename_prefix = NULL,
                save_image_file = FALSE)
p1
```








# Negative abundance investigation

I (Owen) found that the sampling interval had an effect on the stability of the simulation. If the sampling interval was long, then in some rare cases (see below) the odesolver failed, with negative abundances occuring. I think this is due to abundances becoming very small, and then the computer having trouble with precision. I guess that when a sample is taken, the abundance is somehow altered if it is very low, probably by some rounding.


```r
var_expt$pars[[1]]
dd <- var_expt$ss_res[[1]]
dd1 <- filter(dd, PB_1<(-0.0001))
dd1$a_O

ss_expt_master <- ss_expt
ss_expt <- ss_expt_master[abs(ss_expt_master$a_O - 1.336984e-05)<1e-10,]

var_expt_master <- var_expt
#var_expt <- var_expt[1,]
var_expt_test <- run_ss_var_experiment()
res <- var_expt_test$ss_res[[1]]

test1 <- ss_by_a_N(ss_expt, var_expt$pars[[1]])
x <- ss_expt[2,]
param <- var_expt$pars[[1]]
get_final_states_a_N(x, param)
ssfind_parameters <- param
ssfind_simulation_sampling_interval <- 1000
## now run inside the function "get_final_states_a_N"
simres1 <- simres
ssfind_simulation_sampling_interval <- 5000
## now run inside the function "get_final_states_a_N"
simres2 <- simres  # this fails

## now run inside the function "get_final_states_a_N"
plot_dynamics(simres2)

ggplot() +
  geom_line(data = simres1$result,
             mapping = aes(x = time, y = log10(PB_1))) +
  geom_point(data = simres2$result,
             mapping = aes(x = time, y = log10(PB_1))) +
  xlim(c(0, 250000))

ccc <- simres2$result

simres2$result$PB_1
simres2$result$time


log10_a <- log10(ss_expt$a_O[1]) ## very slowly goes anoxic
#log10_a <- log10(a_Os[354]) ## very slowly goes anoxic
#log10_a <- log10(a_Os[356]) ## very very very  slowly goes anoxic
#log10_a <- log10(a_Os[357]) ## does not go anoxic



default_dynamic_model <- bushplus_dynamic_model
default_event_definition <- event_definition_1
default_event_interval <- ssfind_simulation_duration
default_noise_sigma <- 0
default_minimum_abundances <- ssfind_minimum_abundances
default_sim_duration <- ssfind_simulation_duration
default_sim_sample_interval <- ssfind_simulation_duration
#initial_pars_from <- "bush_ssfig3"


default_log10a_series <- c(log10_a, log10_a)
initial_state <- new_initial_state(num_CB_strains,
                                   num_PB_strains,
                                   num_SB_strains,
                                   values = "bush_ssfig3")
initial_state[grep("CB_", names(initial_state))] <- 10^10/num_CB_strains
sim_res_novar <- run_simulation(parameter_values = var_expt$pars[[1]],
                          initial_state = initial_state)
plot_dynamics(sim_res_novar)

simulation_result <- sim_res_novar
every_n <- 1

chk <- sim_res_novar$result

sim_res_novar$result %>%
  ggplot() +
  geom_line(mapping = aes(x = time, y = PB_1))

#ggsave(here("simulations/expt2/figures/switching_novar.pdf"), width = 10)
```


# Understand about relative and absolute variation in traits



<!-- With the CB diversity multiplier set at ` CB_var_multiplier` and the SB/PB multiplier set at ` SBPB_var_multiplier` the range of realised growth rates of CB is ` CB_gr_range`, range of SB is ` SB_gr_range`, and range of PB is ` PB_gr_range`. -->











