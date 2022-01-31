## Figures for main manuscript
## 
## 
## 
## 
## this file -- version 2 -- makes graphs with all combinations
## the other file -- version 1 -- makes only graphs for CB, SB-PB, and CB-SB-PB


## Prelims ----
library(microxanox)
library(tidyverse)
library(patchwork)
library(here)

colfunc_CB <- colorRampPalette(c("#024F17", "#B5FFC9"))
colfunc_SB <- colorRampPalette(c("#7D1402", "#FCBEB3"))
colfunc_PB <- colorRampPalette(c("#6E0172", "#F9AEFC"))

source(here("R/various_useful_functions.r"))
source(here("experiments/1_figures_main_ms/ms_figure_functions.R"))

## Figure 1 ----
## Powerpoint
## Conceptual figure

## Figure 2 ----
## Keynote
## Model system

## Figure 3 ----
## Effects of diversity on position of tipping points and effect sizes
## Two column

stab_2s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/stab_data_2strains_waittime1e+06_event_definition_2.RDS"))
stab_3s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/stab_data_3strains_waittime1e+06_event_definition_2.RDS"))
stab_6s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/stab_data_6strains_waittime1e+06_event_definition_2.RDS"))
stab_9s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/stab_data_9strains_waittime1e+06_event_definition_2.RDS"))

all_stab <- stab_2s %>%
  #bind_rows(stab_2s_sub1) %>%
  bind_rows(stab_3s) %>%
  #bind_rows(stab_3s_sub1) %>%
  bind_rows(stab_6s) %>%
  #bind_rows(stab_6s_sub1) %>%
  bind_rows(stab_9s)
#bind_rows(stab_9s_sub1) %>%
#bind_rows(stab_9s_diff3)

all_stab <- stab_9s


p1 <- fig_div_vs_o2diff_1strain_7row(all_stab,
                                     which_strain = 9,
                                     figure_title = "1e6 wait time, 9 strains")
#p1
p2 <- fig_resilience_vs_div(all_stab, which_strain = 9, figure_title = " ") 
#p2
p1 + p2

ggsave(here("experiments/1_figures_main_ms/Figure_3.pdf"),
       width = 6, height = 9)



## Figure 4 ----
## Stable state with diversity in all functional groups.

ss_9s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/ss_data_9strains_waittime1e+06_event_definition_2.RDS"))
sort(unique(ss_9s$CB_var_gmax_s))
sort(unique(ss_9s$SB_var_gmax_s))
ss_result <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.028254848) < 0.0001,
         abs(SB_var_gmax_s - 0.084764545) < 0.0001,
         abs(PB_var_gmax_s - 0.084764545) < 0.0001)


fig_state_vs_o2diff_sidebyside(ss_result)
ggsave(here("experiments/1_figures_main_ms/figure_4.pdf"))





## Not used anymore ----

# ss_9s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/ss_data_9strains_waittime1e+06_event_definition_2.RDS"))
# sort(unique(ss_9s$CB_var_gmax_s))
# sort(unique(ss_9s$SB_var_gmax_s))
# ss_result <- ss_9s %>%
#   filter(abs(CB_var_gmax_s - 0) < 0.0001,
#          abs(SB_var_gmax_s - 0) < 0.0001,
#          abs(PB_var_gmax_s - 0) < 0.0001)
# 
# fig_state_vs_o2diff_sidebyside(ss_result)
# ggsave(here("experiments/1_figures_main_ms/figure_2b-f.pdf"))
# 



## Not used any more ----
## Shows effect of no diversity versus diversity in all functional groups.

# ss_9s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/ss_data_9strains_waittime1e+06_event_definition_2.RDS"))
# 
# sort(unique(ss_9s$CB_var_gmax_s))
# sort(unique(ss_9s$SB_var_gmax_s))
# ss_result_none <- ss_9s %>%
#   filter(abs(CB_var_gmax_s - 0.0) < 0.0001,
#          abs(SB_var_gmax_s - 0.0) < 0.0001,
#          abs(PB_var_gmax_s - 0.0) < 0.0001)
# ss_result_CB <- ss_9s %>%
#   filter(abs(CB_var_gmax_s - 0.014958449) < 0.0001,
#          abs(SB_var_gmax_s - 0.0) < 0.0001,
#          abs(PB_var_gmax_s - 0.0) < 0.0001)
# ss_result_SB <- ss_9s %>%
#   filter(abs(CB_var_gmax_s - 0.0) < 0.0001,
#          abs(SB_var_gmax_s - 0.044875347) < 0.0001,
#          abs(PB_var_gmax_s - 0.0) < 0.0001)
# ss_result_PB <- ss_9s %>%
#   filter(abs(CB_var_gmax_s - 0.0) < 0.0001,
#          abs(SB_var_gmax_s - 0.0) < 0.0001,
#          abs(PB_var_gmax_s - 0.044875347) < 0.0001)
# ss_result_CBSB <- ss_9s %>%
#   filter(abs(CB_var_gmax_s - 0.014958449) < 0.0001,
#          abs(SB_var_gmax_s - 0.044875347) < 0.0001,
#          abs(PB_var_gmax_s - 0.0) < 0.0001)
# ss_result_CBPB <- ss_9s %>%
#   filter(abs(CB_var_gmax_s - 0.014958449) < 0.0001,
#          abs(SB_var_gmax_s - 0.0) < 0.0001,
#          abs(PB_var_gmax_s - 0.044875347) < 0.0001)
# ss_result_SBPB <- ss_9s %>%
#   filter(abs(CB_var_gmax_s - 0.0) < 0.0001,
#          abs(SB_var_gmax_s - 0.044875347) < 0.0001,
#          abs(PB_var_gmax_s - 0.044875347) < 0.0001)
# ss_result_CBSBPB <- ss_9s %>%
#   filter(abs(CB_var_gmax_s - 0.014958449) < 0.0001,
#          abs(SB_var_gmax_s - 0.044875347) < 0.0001,
#          abs(PB_var_gmax_s - 0.044875347) < 0.0001)
# 
# 
# 
# temp_none <- ss_result_none$ssfind_result[[1]] %>%
#   mutate(a = 10^a_O) %>%
#   # arrange(a) %>%
#   # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
#   #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
#   # select(-initial_N_CB, -a_O) %>%
#   gather(species, quantity, 2:(ncol(.) - 2)) %>%
#   mutate(
#     var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
#     functional_group = case_when(
#       str_detect(species, "CB_") ~ "CB",
#       str_detect(species, "SB_") ~ "SB",
#       str_detect(species, "PB_") ~ "PB"
#     ),
#     log10_quantity = log10(quantity + 1)
#   )
# 
# temp_CB <- ss_result_CB$ssfind_result[[1]] %>%
#   mutate(a = 10^a_O) %>%
#   # arrange(a) %>%
#   # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
#   #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
#   # select(-initial_N_CB, -a_O) %>%
#   gather(species, quantity, 2:(ncol(.) - 2)) %>%
#   mutate(
#     var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
#     functional_group = case_when(
#       str_detect(species, "CB_") ~ "CB",
#       str_detect(species, "SB_") ~ "SB",
#       str_detect(species, "PB_") ~ "PB"
#     ),
#     log10_quantity = log10(quantity + 1)
#   )
# temp_SB <- ss_result_SB$ssfind_result[[1]] %>%
#   mutate(a = 10^a_O) %>%
#   # arrange(a) %>%
#   # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
#   #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
#   # select(-initial_N_CB, -a_O) %>%
#   gather(species, quantity, 2:(ncol(.) - 2)) %>%
#   mutate(
#     var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
#     functional_group = case_when(
#       str_detect(species, "CB_") ~ "CB",
#       str_detect(species, "SB_") ~ "SB",
#       str_detect(species, "PB_") ~ "PB"
#     ),
#     log10_quantity = log10(quantity + 1)
#   )
# temp_PB <- ss_result_PB$ssfind_result[[1]] %>%
#   mutate(a = 10^a_O) %>%
#   # arrange(a) %>%
#   # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
#   #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
#   # select(-initial_N_CB, -a_O) %>%
#   gather(species, quantity, 2:(ncol(.) - 2)) %>%
#   mutate(
#     var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
#     functional_group = case_when(
#       str_detect(species, "CB_") ~ "CB",
#       str_detect(species, "SB_") ~ "SB",
#       str_detect(species, "PB_") ~ "PB"
#     ),
#     log10_quantity = log10(quantity + 1)
#   )
# temp_CBSB <- ss_result_CBSB$ssfind_result[[1]] %>%
#   mutate(a = 10^a_O) %>%
#   # arrange(a) %>%
#   # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
#   #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
#   # select(-initial_N_CB, -a_O) %>%
#   gather(species, quantity, 2:(ncol(.) - 2)) %>%
#   mutate(
#     var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
#     functional_group = case_when(
#       str_detect(species, "CB_") ~ "CB",
#       str_detect(species, "SB_") ~ "SB",
#       str_detect(species, "PB_") ~ "PB"
#     ),
#     log10_quantity = log10(quantity + 1)
#   )
# temp_CBPB <- ss_result_CBPB$ssfind_result[[1]] %>%
#   mutate(a = 10^a_O) %>%
#   # arrange(a) %>%
#   # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
#   #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
#   # select(-initial_N_CB, -a_O) %>%
#   gather(species, quantity, 2:(ncol(.) - 2)) %>%
#   mutate(
#     var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
#     functional_group = case_when(
#       str_detect(species, "CB_") ~ "CB",
#       str_detect(species, "SB_") ~ "SB",
#       str_detect(species, "PB_") ~ "PB"
#     ),
#     log10_quantity = log10(quantity + 1)
#   )
# temp_SBPB <- ss_result_SBPB$ssfind_result[[1]] %>%
#   mutate(a = 10^a_O) %>%
#   # arrange(a) %>%
#   # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
#   #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
#   # select(-initial_N_CB, -a_O) %>%
#   gather(species, quantity, 2:(ncol(.) - 2)) %>%
#   mutate(
#     var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
#     functional_group = case_when(
#       str_detect(species, "CB_") ~ "CB",
#       str_detect(species, "SB_") ~ "SB",
#       str_detect(species, "PB_") ~ "PB"
#     ),
#     log10_quantity = log10(quantity + 1)
#   )
# temp_CBSBPB <- ss_result_CBSBPB$ssfind_result[[1]] %>%
#   mutate(a = 10^a_O) %>%
#   # arrange(a) %>%
#   # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
#   #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
#   # select(-initial_N_CB, -a_O) %>%
#   gather(species, quantity, 2:(ncol(.) - 2)) %>%
#   mutate(
#     var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
#     functional_group = case_when(
#       str_detect(species, "CB_") ~ "CB",
#       str_detect(species, "SB_") ~ "SB",
#       str_detect(species, "PB_") ~ "PB"
#     ),
#     log10_quantity = log10(quantity + 1)
#   )
# 
# line_width <- 2.5
# 
# ## a
# p1 <- temp_none %>%
#   dplyr::filter(var_type == "Substrate", species == "O") %>%
#   ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
#   geom_path(lwd = line_width) +
#   geom_path(data = filter(temp_CB, var_type == "Substrate", species == "O"),
#             col = "black", lwd = line_width - 1.5) +
#   ##ylab("log10(quantity [cells])") +
#   ylab("Log(Quantity)") +
#   xlab("Oxygen diffusivity") +
#   theme(legend.position="none") +
#   ggtitle("(a) No diversity versus diversity\nin only CB") +
#   geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)+
#   theme(plot.title = element_text(size = 10))
# p2 <- temp_none %>%
#   dplyr::filter(var_type == "Substrate", species == "O") %>%
#   ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
#   geom_path(lwd = line_width) +
#   geom_path(data = filter(temp_SB, var_type == "Substrate", species == "O"),
#             col = "black", lwd = line_width - 1.5) +
#   ##ylab("log10(quantity [cells])") +
#   ylab("Log(Quantity)") +
#   xlab("Oxygen diffusivity") +
#   theme(legend.position="none") +
#   ggtitle("(a) No diversity versus diversity\nin only SB") +
#   geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)+
#   theme(plot.title = element_text(size = 10))
# p3 <- temp_none %>%
#   dplyr::filter(var_type == "Substrate", species == "O") %>%
#   ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
#   geom_path(lwd = line_width) +
#   geom_path(data = filter(temp_PB, var_type == "Substrate", species == "O"),
#             col = "black", lwd = line_width - 1.5) +
#   ##ylab("log10(quantity [cells])") +
#   ylab("Log(Quantity)") +
#   xlab("Oxygen diffusivity") +
#   theme(legend.position="none") +
#   ggtitle("(a) No diversity versus diversity\nin only PB") +
#   geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)+
#   theme(plot.title = element_text(size = 10))
# p4 <- temp_none %>%
#   dplyr::filter(var_type == "Substrate", species == "O") %>%
#   ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
#   geom_path(lwd = line_width) +
#   geom_path(data = filter(temp_CBSB, var_type == "Substrate", species == "O"),
#             col = "black", lwd = line_width - 1.5) +
#   ##ylab("log10(quantity [cells])") +
#   ylab("Log(Quantity)") +
#   xlab("Oxygen diffusivity") +
#   theme(legend.position="none") +
#   ggtitle("(b) No diversity versus diversity\nin only CB & SB") +
#   geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)+
#   theme(plot.title = element_text(size = 10))
# p5 <- temp_none %>%
#   dplyr::filter(var_type == "Substrate", species == "O") %>%
#   ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
#   geom_path(lwd = line_width) +
#   geom_path(data = filter(temp_CBPB, var_type == "Substrate", species == "O"),
#             col = "black", lwd = line_width - 1.5) +
#   ##ylab("log10(quantity [cells])") +
#   ylab("Log(Quantity)") +
#   xlab("Oxygen diffusivity") +
#   theme(legend.position="none") +
#   ggtitle("(b) No diversity versus diversity\nin only CB & PB") +
#   geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)+
#   theme(plot.title = element_text(size = 10))
# p6 <- temp_none %>%
#   dplyr::filter(var_type == "Substrate", species == "O") %>%
#   ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
#   geom_path(lwd = line_width) +
#   geom_path(data = filter(temp_SBPB, var_type == "Substrate", species == "O"),
#             col = "black", lwd = line_width - 1.5) +
#   ##ylab("log10(quantity [cells])") +
#   ylab("Log(Quantity)") +
#   xlab("Oxygen diffusivity") +
#   theme(legend.position="none") +
#   ggtitle("(b) No diversity versus diversity\nin only SB & PB") +
#   geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)+
#   theme(plot.title = element_text(size = 10))
# 
# p7 <- temp_none %>%
#   dplyr::filter(var_type == "Substrate", species == "O") %>%
#   ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
#   geom_path(lwd = line_width) +
#   geom_path(data = filter(temp_CBSBPB, var_type == "Substrate", species == "O"),
#             col = "black", lwd = line_width - 1.5) +
#   ##ylab("log10(quantity [cells])") +
#   ylab("Log(Quantity)") +
#   xlab("Oxygen diffusivity") +
#   theme(legend.position="none") +
#   ggtitle("(c) No diversity versus diversity\nin all functional groups") +
#   geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3) +
#   theme(plot.title = element_text(size = 10))
# 
# ( p1 + p2 + p3 ) / ( p4 + p5 + p6 ) / (plot_spacer() + p7 + plot_spacer())
#  
# ggsave(here("experiments/1_figures_main_ms/figure_4.pdf"),
#        width = 6, height = 5)
# 



## Not used anymore ----
## Region of bistability by diversity and number of strains.

# 
# stab_2s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/stab_data_2strains_waittime1e+06_event_definition_2.RDS"))
# stab_3s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/stab_data_3strains_waittime1e+06_event_definition_2.RDS"))
# stab_6s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/stab_data_6strains_waittime1e+06_event_definition_2.RDS"))
# stab_9s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/stab_data_9strains_waittime1e+06_event_definition_2.RDS"))
# 
# all_stab <- stab_2s %>%
#   #bind_rows(stab_2s_sub1) %>%
#   bind_rows(stab_3s) %>%
#   #bind_rows(stab_3s_sub1) %>%
#   bind_rows(stab_6s) %>%
#   #bind_rows(stab_6s_sub1) %>%
#   bind_rows(stab_9s)
#   #bind_rows(stab_9s_sub1) %>%
#   #bind_rows(stab_9s_diff3)
# 
# all_stab <- stab_9s
# 
# fig_div_vs_o2diff_1strain(all_stab,
#                           which_strain=9,
#                           figure_title = "1e6 wait time, 9 strains") 
# ggsave(here("experiments/1_figures_main_ms/figure_5_9strain.pdf"), width = 10, height = 8)
# 
# ## Figure 5 all strains
# fig_div_vs_o2diff_multistrain(all_stab,
#                           which_strain=c(2,3,6,9),
#                           figure_title = "1e5 wait time") 

#ggsave(here("experiments/1_figures_main_ms/figure_5_v2_allstrains.pdf"), width = 10, height = 8)



## Figure 5 ----
## Assessing additivity of diversity effects ----
## By Uriah
## Owen switched to SB_tot

all_stab_results_small <- readRDS(here("experiments/0_ss_finding/temporal_method/data/must_be_updated_all_stab_results_small.RDS"))

all_stab_results9 <- all_stab_results_small %>%
  distinct() %>%  #there are some duplicated rows....? yes, some treatment combs were repeated when sub1 was made
  filter(num_strains == 9, Species == "SB_tot") 

all_stab_results9 <- all_stab_results9[with(all_stab_results9, order(var_treat, var_gmax)),]

all_stab_results9$method <- "simulated"

calculation_fct <- function(x,y,z=NULL, Method="arithmetic"){
  if(Method=="arithmetic") {
    if(is.null(z)) {
      w <- log10((10^x + 10^y)/2)
    } else {
      w <- log10((10^x + 10^y + 10^z)/3)
    }
    return(w)
  }    
  
  
  if(Method%in% c("arith_log","geometric")) {
    if(is.null(z)) {
      # w <- (x+y)/2 # the same as below
      w <- log10((10^x*10^y)^(1/2))
    } else {
      # w <- (x+y+z)/3
      w <- log10((10^x*10^y*10^z)^(1/3))
    }
    return(w)
  }   
  
  if(Method=="geom_log") {
    if(is.null(z)) {
      w <- (x*y)^(1/2)
      w <- -w
    } else {
      w <- (x*y*z)^(1/3)
    }
    return(w) 
  }
  
  if(Method=="EF_log") {
    w <- x + y - first(x)
    return(w) 
  }
  
  if(Method=="EF_lin") {
    w <- log10(10^x + 10^y - 10^first(x))
    return(w) 
  }
}


combine_fct <- function(Method="arithmetic"){
  agg_stab_res_single_groups_wide <- all_stab_results9 %>%
    dplyr::select(Species,hyst_min_log,hyst_max_log,var_treat,var_gmax) %>%
    dplyr::filter(Species == "SB_tot") %>%
    group_by(var_treat) %>%
    dplyr::mutate(var_gmax = rank(var_gmax)) %>%
    pivot_wider(names_from = "var_treat", values_from = c("hyst_min_log", "hyst_max_log")) %>%
    dplyr::mutate(hyst_min_log_CB_SB = calculation_fct(hyst_min_log_CB,hyst_min_log_SB, Method=Method),
                  hyst_max_log_CB_SB = calculation_fct(hyst_max_log_CB,hyst_max_log_SB, Method=Method),
                  hyst_min_log_CB_PB = calculation_fct(hyst_min_log_CB,hyst_min_log_PB, Method=Method),
                  hyst_max_log_CB_PB = calculation_fct(hyst_max_log_CB,hyst_max_log_PB, Method=Method),
                  hyst_min_log_SB_PB = calculation_fct(hyst_min_log_SB,hyst_min_log_PB, Method=Method),
                  hyst_max_log_SB_PB = calculation_fct(hyst_max_log_SB,hyst_max_log_PB, Method=Method),
                  # hyst_min_log_CB_SB_PB = calculation_fct(hyst_min_log_CB,hyst_min_log_SB,hyst_min_log_PB, Method=Method),
                  # hyst_max_log_CB_SB_PB = calculation_fct(hyst_min_log_CB,hyst_max_log_SB,hyst_max_log_PB, Method=Method),
                  hyst_min_log_CB_SBPB = calculation_fct(`hyst_min_log_SB-PB`,hyst_min_log_CB, Method=Method),
                  hyst_max_log_CB_SBPB = calculation_fct(`hyst_max_log_SB-PB`,hyst_max_log_CB, Method=Method))
  
  agg_stab_strain9 <- all_stab_results9 %>%
    dplyr::filter(num_strains==9, Species == "SB_tot") 
  
  ### CB + SB
  agg_stab_strain9_CB_SB <- agg_stab_strain9 %>%
    dplyr::filter(var_treat=="CB-SB")
  
  agg_stab_strain9_CB_SB$method <- "calculated"
  agg_stab_strain9_CB_SB$hyst_min_log <- agg_stab_res_single_groups_wide$hyst_min_log_CB_SB
  agg_stab_strain9_CB_SB$hyst_max_log <- agg_stab_res_single_groups_wide$hyst_max_log_CB_SB
  
  ### CB + PB
  agg_stab_strain9_CB_PB <- agg_stab_strain9 %>%
    dplyr::filter(var_treat=="CB-PB")
  
  agg_stab_strain9_CB_PB$method <- "calculated"
  agg_stab_strain9_CB_PB$hyst_min_log <- agg_stab_res_single_groups_wide$hyst_min_log_CB_PB
  agg_stab_strain9_CB_PB$hyst_max_log <- agg_stab_res_single_groups_wide$hyst_max_log_CB_PB
  
  ### SB + PB
  agg_stab_strain9_SB_PB <- agg_stab_strain9 %>%
    dplyr::filter(var_treat=="SB-PB")
  
  agg_stab_strain9_SB_PB$method <- "calculated"
  agg_stab_strain9_SB_PB$hyst_min_log <- agg_stab_res_single_groups_wide$hyst_min_log_SB_PB
  agg_stab_strain9_SB_PB$hyst_max_log <- agg_stab_res_single_groups_wide$hyst_max_log_SB_PB
  
  ### CB + SB + PB
  # agg_stab_strain9_CB_SB_PB <- agg_stab_strain9 %>%
  #   dplyr::filter(var_treat=="CB-SB-PB")
  # 
  # agg_stab_strain9_CB_SB_PB$method <- "calculated CB+SB+PB"
  # agg_stab_strain9_CB_SB_PB$hyst_min_log <- agg_stab_res_single_groups_wide$hyst_min_log_CB_SB_PB
  # agg_stab_strain9_CB_SB_PB$hyst_max_log <- agg_stab_res_single_groups_wide$hyst_max_log_CB_SB_PB
  
  ### CB + SBPB
  agg_stab_strain9_CB_SBPB <- agg_stab_strain9 %>%
    dplyr::filter(var_treat=="CB-SB-PB")
  
  agg_stab_strain9_CB_SBPB$method <- "calculated CB+SBPB"
  agg_stab_strain9_CB_SBPB$hyst_min_log <- agg_stab_res_single_groups_wide$hyst_min_log_CB_SBPB
  agg_stab_strain9_CB_SBPB$hyst_max_log <- agg_stab_res_single_groups_wide$hyst_max_log_CB_SBPB
  
  ### merge 
  agg_stab_strain9 <- rbind(agg_stab_strain9, agg_stab_strain9_CB_SB, agg_stab_strain9_CB_PB, agg_stab_strain9_SB_PB,
                            # agg_stab_strain9_CB_SB_PB,
                            agg_stab_strain9_CB_SBPB)
  return(agg_stab_strain9)
}

agg_stab_strain9 <- combine_fct("arithmetic")

agg_stab_strain9 %>%
  filter(Species == "SB_tot", num_strains==9) %>%
  ggplot(aes(x = var_gmax, col=method)) +
  geom_hline(yintercept = c(-8, 0), col = "black") +
  geom_line(aes(y = hyst_min_log), lwd = 1, alpha=0.6) +
  geom_line(aes(y = hyst_max_log), lwd = 1, alpha=0.6) +
  facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
  labs(x="Amount of trait variation\n[see text for units]",
       y="Amount of trait variation\n[see text for units]",
       title = "Arithmetic mean on linear scale",
       col="Method") +
  coord_flip() +
  theme_bw()+
  theme(legend.position="top")


agg_stab_strain9 <- combine_fct("geometric") # same as agg_stab_strain9 <- combine_fct("arith_log")

agg_stab_strain9 %>%
  filter(Species == "SB_tot", num_strains==9) %>%
  ggplot(aes(x = var_gmax, col=method)) +
  geom_hline(yintercept = c(-8, 0), col = "black") +
  geom_line(aes(y = hyst_min_log), lwd = 1, alpha=0.6) +
  geom_line(aes(y = hyst_max_log), lwd = 1, alpha=0.6) +
  facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
  labs(x="Amount of trait variation\n[see text for units]",
       y="Amount of trait variation\n[see text for units]",
       title = "Geometric mean on linear scale",
       col="Method") +
  coord_flip() +
  theme_bw()+
  theme(legend.position="top")


agg_stab_strain9 <- combine_fct("geom_log")

agg_stab_strain9 %>%
  filter(Species == "SB_tot", num_strains==9) %>%
  ggplot(aes(x = var_gmax, col=method)) +
  geom_hline(yintercept = c(-8, 0), col = "black") +
  geom_line(aes(y = hyst_min_log), lwd = 1, alpha=0.6) +
  geom_line(aes(y = hyst_max_log), lwd = 1, alpha=0.6) +
  facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
  labs(x="Amount of trait variation\n[see text for units]",
       y="Amount of trait variation\n[see text for units]",
       title = "Geometric mean on log scale",
       col="Method") +
  coord_flip() +
  theme_bw()+
  theme(legend.position="top")



## based on effect sizes 


agg_stab_strain9 <- combine_fct("EF_log")

agg_stab_strain9 %>%
  filter(Species == "SB_tot", num_strains==9) %>%
  ggplot(aes(x = var_gmax, col=method)) +
  geom_hline(yintercept = c(-8, 0), col = "black") +
  geom_line(aes(y = hyst_min_log), lwd = 1, alpha=0.6) +
  geom_line(aes(y = hyst_max_log), lwd = 1, alpha=0.6) +
  facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
  labs(x="Amount of trait variation\n[see text for units]",
       y="Amount of trait variation\n[see text for units]",
       title = "Addition of effect sizes and starting position (log)",
       col="Method") +
  coord_flip() +
  theme_bw()+
  theme(legend.position="top")



agg_stab_strain9 <- combine_fct("EF_lin")

agg_stab_strain9 %>%
  filter(Species == "SB_tot", num_strains==9) %>%
  ggplot(aes(x = var_gmax, col=method)) +
  geom_hline(yintercept = c(-8, 0), col = "black") +
  geom_line(aes(y = hyst_min_log), lwd = 1, alpha=0.6) +
  geom_line(aes(y = hyst_max_log), lwd = 1, alpha=0.6) +
  facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
  labs(x="Amount of trait variation\n[see text for units]",
       y="Amount of trait variation\n[see text for units]",
       title = "Addition of effect sizes and starting position (linear)",
       col="Method") +
  coord_flip() +
  theme_bw()+
  theme(legend.position="top")

ggsave(here("experiments/1_figures_main_ms/Figure_5.pdf"),
       width = 6, height = 9)



###### End of insert




