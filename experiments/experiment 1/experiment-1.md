---
title: "Experiment 1"
author: "Owen Petchey"
date: "6/25/2021"
output: 
  html_document: 
    keep_md: yes
---


# Setup

## R


```r
rm(list = ls())

knitr::opts_knit$set(progress = TRUE, verbose = TRUE, cache = TRUE)

# devtools::install_github("opetchey/microxanox",
#                              ref="main",
#                              auth_token = "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",
#                              build_vignettes = FALSE,
#                              force = TRUE)

library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.4     ✓ purrr   0.3.4
## ✓ tibble  3.1.2     ✓ dplyr   1.0.7
## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(microxanox)
library(patchwork)
library(here)
```

```
## here() starts at /Users/owenpetchey/Desktop/microxanox/diversity_envresp1
```

```r
source(here("experiments/r functions/various_useful_functions.r"))
zero <- 0 ## don't change
unity <- 1 ## don't change!!!
options(mc.cores = 8)
```

## General simulation conditions


```r
default_dynamic_model <- bushplus_dynamic_model
default_event_definition <- event_definition_1
default_event_interval <- 100
default_noise_sigma <- 0
default_minimum_abundances <- rep(1, 3)
names(default_minimum_abundances) <- c("CB", "PB", "SB") 
default_sim_duration <- 80000
default_sim_sample_interval <- 100
initial_pars_from <- "bush_ssfig3"
## note that next line (log10a_series is over-ridden with getting stable states)
#default_log10a_series <- c(-2, -2, -2, -2, -10, -10, -10, -10, -10)
```


## Define diversity


```r
num_CB_strains <- 9
num_SB_strains <- 9
num_PB_strains <- 9
CB_gmax_div <- 0.015789474
CB_h_div <- -0.08
SB_gmax_div <- 0.015789474
SB_h_div <- -0.323
PB_gmax_div <- 0.015789474
PB_h_div <- -0.323

num_div_treatment_levels <- 2
```

## Create diversity


```r
default_9strain <- new_starter(n_CB = num_CB_strains,
                             n_SB = num_SB_strains,
                             n_PB = num_PB_strains,
                             values_initial_state = initial_pars_from)
CB_var_gmax_s <- seq(zero, unity, length=num_div_treatment_levels) * CB_gmax_div
CB_var_h_s = seq(zero, unity, length=num_div_treatment_levels) * CB_h_div
SB_var_gmax_s <- seq(zero, unity, length=num_div_treatment_levels) * SB_gmax_div
SB_var_h_s = seq(zero, unity, length=num_div_treatment_levels) * SB_h_div
PB_var_gmax_s <- seq(zero, unity, length=num_div_treatment_levels) * PB_gmax_div
PB_var_h_s = seq(zero, unity, length=num_div_treatment_levels) * PB_h_div
var_expt <- tibble(CB_var_gmax_s,
                   CB_var_h_s,
                   SB_var_gmax_s,
                   SB_var_h_s,
                   PB_var_gmax_s,
                   PB_var_h_s)
var_expt <- var_expt %>%
  nest_by(CB_var_gmax_s, CB_var_h_s,
          SB_var_gmax_s, SB_var_h_s,
          PB_var_gmax_s, PB_var_h_s) %>%
  mutate(pars = list(add_strain_var(default_9strain,
                                    CB_var_gmax = CB_var_gmax_s,
                                    CB_var_h = CB_var_h_s,
                                    SB_var_gmax = SB_var_gmax_s,
                                    SB_var_h = SB_var_h_s,
                                    PB_var_gmax = PB_var_gmax_s,
                                    PB_var_h = PB_var_h_s)))
```

## Display diversity


```r
display_diversity()
```

![](experiment-1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


# Temporal switching

## Oxic to anoxic

### No diversity


```r
default_sim_duration <- 80000
```


```r
sim_number <- 1
default_log10a_series <- c(-1, -7, -7)
initial_state <- new_initial_state(num_CB_strains,
                                   num_PB_strains,
                                   num_SB_strains,
                                   values = "bush_ssfig3")
initial_state[grep("CB_", names(initial_state))] <- 10^10/num_CB_strains
sim_res_novar <- run_simulation(parameter_values = var_expt$pars[[sim_number]],
                          initial_state = initial_state)
plot_dynamics(sim_res_novar)
```

![](experiment-1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
#ggsave(here("simulations/expt2/figures/switching_novar.pdf"), width = 10)
```

### Maximum diversity


```r
sim_number <- 2
sim_res_highvar <- run_simulation(parameter_values = var_expt$pars[[sim_number]],
                          initial_state = initial_state)
plot_dynamics(sim_res_highvar)
```

![](experiment-1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
#ggsave(here("simulationsexpt2/figures/switching_highvar.pdf"), width = 10)
```

## Anoxic to oxic

### No diversity


```r
default_sim_duration <- 60000
```



```r
sim_number <- 1
default_log10a_series <- c(-5, -3, -1, -1)
initial_state <- new_initial_state(num_CB_strains,
                                   num_PB_strains,
                                   num_SB_strains,
                                   values = "bush_ssfig3")
initial_state[grep("CB_", names(initial_state))] <- 10/num_CB_strains
sim_res_novar <- run_simulation(parameter_values = var_expt$pars[[sim_number]],
                          initial_state = initial_state)
plot_dynamics(sim_res_novar)
```

![](experiment-1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
#ggsave(here("simulations/expt2/figures/switching_novar.pdf"), width = 10)
```

### Maximum diversity


```r
sim_number <- 2
sim_res_highvar <- run_simulation(parameter_values = var_expt$pars[[sim_number]],
                          initial_state = initial_state)
plot_dynamics(sim_res_highvar)
```

![](experiment-1_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#ggsave(here("simulationsexpt2/figures/switching_highvar.pdf"), width = 10)
```


## Anoxic to oxic to anoxic

### No diversity


```r
default_sim_duration <- 1000000
```


```r
default_minimum_abundances <- rep(100, 3)
names(default_minimum_abundances) <- c("CB", "PB", "SB") 
```


```r
sim_number1 <- 1
default_log10a_series <- c(-1, -8, -1)
initial_state <- new_initial_state(num_CB_strains,
                                   num_PB_strains,
                                   num_SB_strains,
                                   values = "bush_ssfig3")
initial_state[grep("CB_", names(initial_state))] <- 10/num_CB_strains
sim_res_novar <- run_simulation(parameter_values = var_expt$pars[[sim_number1]],
                          initial_state = initial_state)
plot_dynamics(sim_res_novar)
```

![](experiment-1_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
#ggsave(here("simulations/expt2/figures/switching_novar.pdf"), width = 10)
```

### Maximum diversity


```r
sim_number2 <- 2
sim_res_highvar <- run_simulation(parameter_values = var_expt$pars[[sim_number2]],
                          initial_state = initial_state)
plot_dynamics(sim_res_highvar)
```

![](experiment-1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
#ggsave(here("simulationsexpt2/figures/switching_highvar.pdf"), width = 10)
```

### Visualise


```r
tmp_novar <- sim_res_novar$result %>%
  filter(time != 0) %>%
  select(-time) %>%
  mutate(direction = rep(c("down", "up"), each = n()/2)) %>%
  pivot_longer(1:(num_CB_strains+num_SB_strains+num_PB_strains+4),
               names_to = "Species", values_to = "Quantity") %>%
  mutate(var_type=ifelse(grepl("B_", Species), "Organism", "Substrate"),
         functional_group = case_when(str_detect(Species, "CB_") ~ "CB",
                                      str_detect(Species, "SB_") ~ "SB",
                                      str_detect(Species, "PB_") ~ "PB"),
         functional_group = ifelse(is.na(functional_group), Species, functional_group)) %>%
  group_by(functional_group, a, var_type, direction) %>%
  summarise(Total_quantity = sum(Quantity))
```

```
## `summarise()` has grouped output by 'functional_group', 'a', 'var_type'. You can override using the `.groups` argument.
```

```r
tmp_highvar <- sim_res_highvar$result %>%
  filter(time != 0) %>%
  select(-time) %>%
  mutate(direction = rep(c("down", "up"), each = n()/2)) %>%
  pivot_longer(1:(num_CB_strains+num_SB_strains+num_PB_strains+4),
               names_to = "Species", values_to = "Quantity") %>%
  mutate(var_type=ifelse(grepl("B_", Species), "Organism", "Substrate"),
         functional_group = case_when(str_detect(Species, "CB_") ~ "CB",
                                      str_detect(Species, "SB_") ~ "SB",
                                      str_detect(Species, "PB_") ~ "PB"),
         functional_group = ifelse(is.na(functional_group), Species, functional_group)) %>%
  group_by(functional_group, a, var_type, direction) %>%
  summarise(Total_quantity = sum(Quantity))
```

```
## `summarise()` has grouped output by 'functional_group', 'a', 'var_type'. You can override using the `.groups` argument.
```

```r
soi <- "O"
ggplot() +
  geom_path(data = filter(tmp_novar,functional_group == soi),
            aes(x=a, y = log10(Total_quantity), group = direction)) +
  geom_path(data = filter(tmp_highvar,functional_group == soi),
            aes(x=a, y = log10(Total_quantity), group = direction),
            linetype = "dashed", lwd=2, col="red") +
  ggtitle("Solid line is with no intraspecific diversity.\nDashed line is with intraspecific diversity")
```

![](experiment-1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
##ggsave(here("simulations/figures/switching_comparison.pdf"), width = 5, height = 4)
```



# Stable state finding

## Setup


```r
options(mc.cores = 8)
```



```r
ssfind_minimum_abundances <- rep(0, 3)
names(ssfind_minimum_abundances) <- c("CB", "PB", "SB")
ssfind_simulation_duration <- default_sim_duration
ssfind_simulation_sampling_interval <- ssfind_simulation_duration
ssfind_event_interval <- ssfind_simulation_duration
grid_num_a <- 20 ## number of a_0 values
a_Os <- 10^seq(-7, -1, length=grid_num_a) ## sequence of a_0 values
grid_num_N <- 2 ## number of N values
initial_CBs <- 10^seq(0, 10, length=grid_num_N) ## sequence of N values
initial_PBs <- 1e8 ## not varied
initial_SBs <- 1e8 ## not varied
# next line creates all possible combinations
ss_expt <- expand.grid(N_CB = initial_CBs,
                      N_PB = initial_PBs,
                      N_SB = initial_SBs,
                      a_O = a_Os)
```


```r
var_expt <- run_ss_var_experiment()
saveRDS(var_expt, here("experiments/experiment 1/data/ss_data.RDS"))
```


```r
result_index1 <- 1
p1  <- plot_ss_result1(var_expt,
                result_index = result_index1,
                filename_prefix = NULL,
                save_image_file = FALSE)
p1
```

![](experiment-1_files/figure-html/unnamed-chunk-20-1.png)<!-- -->


```r
result_index2 <- 2
p2  <- plot_ss_result1(var_expt,
                result_index = result_index2,
                filename_prefix = NULL,
                save_image_file = FALSE)
p2
```

![](experiment-1_files/figure-html/unnamed-chunk-21-1.png)<!-- -->


```r
p_overlay <- plot_ss_result2(var_expt[result_index1,]$ss_res[[1]],
                             var_expt[result_index2,]$ss_res[[1]],
                             xlims = c(-7, -1))
```

```
## `summarise()` has grouped output by 'a', 'direction', 'var_type'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'a', 'direction', 'var_type'. You can override using the `.groups` argument.
```

```r
p_overlay
```

![](experiment-1_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```r
ss_result1 <- var_expt[result_index1,]$ss_res[[1]]
ss_result2 <- var_expt[result_index2,]$ss_res[[1]]
xlims = c(-7, -1)
```

