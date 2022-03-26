## Figures for main manuscript
## 
## 
## 
## 
## this file -- version 2 -- makes graphs with all combinations
## the other file -- version 1 -- makes only graphs for CB, SB-PB, and CB-SB-PB


## Prelims ----
rm(list=ls())
library(microxanox)
library(tidyverse)
library(patchwork)
library(here)

colfunc_CB <- colorRampPalette(c("#024F17", "#B5FFC9"))
colfunc_SB <- colorRampPalette(c("#7D1402", "#FCBEB3"))
colfunc_PB <- colorRampPalette(c("#6E0172", "#F9AEFC"))

source(here("R/various_useful_functions.r"))
source(here("R/ms_figure_functions.R"))

## Figure 1 ----
## Powerpoint
## Conceptual figure

## Figure 2 ----
## Keynote
## Model system

## Figure 3 ----
## Effects of diversity on position of tipping points and effect sizes
## Two column


all_stab <- readRDS(here("data/0_ss_finding/temporal_method/processed_data/stab_data_temporal_method.RDS"))

wait_time <- 1e6
num_strains <- 9
plot_here <- all_stab %>%
  filter(waittime == wait_time)
p1 <- fig_div_vs_o2diff_1strain_7row(plot_here,
                                     which_strain = num_strains,
                                     figure_title = NULL
                                     # figure_title = paste0(wait_time,
                                     #                       " wait time,\n",
                                     #                       num_strains,
                                     #                       " strains")
                                     )
p2 <- fig_resilience_vs_div(plot_here,
                            which_strain = num_strains,
                            figure_title = NULL
                            ) 
p1 + p2
ggsave(here("experiments/1_figures_main_ms/Figure_3.pdf"),
       width = 6, height = 9)



## Figure 4 ----
## Stable state with diversity in all functional groups.

ss_9s <- readRDS(here("data/0_ss_finding/temporal_method/ss_data_9strains_waittime1e+06_event_definition_2.RDS"))
sort(unique(ss_9s$CB_var_gmax_s))
sort(unique(ss_9s$SB_var_gmax_s))
ss_result <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.028254848) < 0.0001,
         abs(SB_var_gmax_s - 0.084764545) < 0.0001,
         abs(PB_var_gmax_s - 0.084764545) < 0.0001)



fig_state_vs_o2diff_sidebyside_dots(ss_result)
ggsave(here("experiments/1_figures_main_ms/figure_4.pdf"),
       width = 10, height = 10)






#### Uriah insert for figure 4

fig_state_vs_o2diff_sidebyside_alternative(ss_result)

ggsave(here("experiments/1_figures_main_ms/figure_4_alternative.pdf"),
       width = 10, height = 10)

## Figure 5 ----
## Assessing additivity of diversity effect sizes
## By Uriah
## Owen switched to SB_tot

all_stab_results_small <- readRDS(here("data/0_ss_finding/temporal_method/processed_data/stab_data_temporal_method.RDS"))

wait_time <- 1e6
all_stab_results9 <- all_stab_results_small %>%
  distinct() %>%  #there are some duplicated rows....? yes, some treatment combs were repeated when sub1 was made
  filter(num_strains == 9,
         Species == "SB_tot",
         waittime == wait_time) 

all_stab_results9 <- all_stab_results9[with(all_stab_results9, order(var_treat, var_gmax)),]

all_stab_results9$method <- "Simulated"


### Things used for plotting ----

# the legend
library(ggpubr)
fig5legend <- agg_stab_strain9 %>%
  filter(Species == "SB_tot", num_strains==9) %>%
  ggplot(aes(x = stand_var, linetype=Method, col=Calculation)) +
  geom_line(aes(y = hyst_min_log), size = 1) +
  geom_line(aes(y = hyst_max_log), size = 1) +
  scale_color_manual(values = c("black","#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#a65628")) +
  scale_linetype_manual(values = c(1,2))+
  coord_flip() +
  theme_bw() +
  theme(legend.position="right")

fig5legend <- get_legend(fig5legend)

# the y and x labels
# ylab <- ggplot(data.frame(l = "            Standardised amount of trait variation", x = 1, y = 4.1)) +
ylab <- ggplot(data.frame(l = "Standardised amount of trait variation", x = 1, y = 4.1)) +
  geom_text(aes(x, y, label = l), angle = 90, size=4.5) + 
  theme_void() +
  coord_cartesian(clip = "off")

xlab <- ggplot(data.frame(x = 1, y = 1)) +
  geom_text(aes(x, y), label = expression('log'[10]*"(oxygen diffusivity) (h"^{-1}*")"), size=4.5) + 
  theme_void() +
  coord_cartesian(clip = "off")

# colours and tags used
colours <- c(Simulated="black",`CB+SB`="#e41a1c",`CB+PB`="#377eb8",
             `SB+PB`="#4daf4a",`CB+SBPB`="#984ea3",`PB+CBSB`="#ff7f00",
             `SB+CBPB`="#a65628")

tags <- c(`CB`="a",`SB`="b",`PB`="c",
          `CB-SB`="d",`CB-PB`="e",`SB-PB`="f",
          `CB-SB-PB & CB+SBPB`="g",`CB-SB-PB & PB+CBSB`="h",`CB-SB-PB & SB+CBPB`="i")


### Fig 5 - log ----

agg_stab_strain9 <- combine_fct("EF_log")

agg_stab_strain9_temp <- agg_stab_strain9 %>%
  dplyr::filter(var_treat=="CB-SB-PB")

agg_stab_strain9 <- agg_stab_strain9 %>%
  dplyr::filter(var_treat!="CB-SB-PB")

agg_stab_strain9 <- rbind(agg_stab_strain9,
                          agg_stab_strain9_temp %>% filter(method %in% c("CB+SBPB","Simulated")) %>% mutate(var_treat="CB-SB-PB & CB+SBPB"),
                          agg_stab_strain9_temp %>% filter(method %in% c("PB+CBSB","Simulated")) %>% mutate(var_treat="CB-SB-PB & PB+CBSB"),
                          agg_stab_strain9_temp %>% filter(method %in% c("SB+CBPB","Simulated")) %>% mutate(var_treat="CB-SB-PB & SB+CBPB"))

agg_stab_strain9 <- agg_stab_strain9 %>%
  mutate(method2 = ifelse(agg_stab_strain9$method=="Simulated","Simulated","Calculated")) %>%
  rename(Calculation=method, Method=method2)%>%
  mutate(Calculation = factor(Calculation, levels = c("Simulated","CB+SB","CB+PB","SB+PB",
                                                      "CB+SBPB","PB+CBSB","SB+CBPB")),
         Method = factor(Method, levels = c("Simulated","Calculated")))


# The figure

fig5_log <- split(agg_stab_strain9, f=agg_stab_strain9$var_treat, drop = T)

fig5_log <- lapply(fig5_log, function(df){
  tag <- tags[as.character(unique(df$var_treat))]
  plot <- df %>%
    filter(Species == "SB_tot", num_strains==9) %>%
    ggplot(aes(x = stand_var, linetype=Method, col=Calculation)) +
    geom_hline(yintercept = c(-8, 0), col = "black", linetype=3) +
    geom_line(aes(y = hyst_min_log), size = 1) +
    geom_line(aes(y = hyst_max_log), size = 1) +
    facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
    labs(y="Oxygen diffusivity\n[log10 uM per hour]",
         x="Amount of trait variation\n[see text for units]",
         linetype="Method",col="Calculation", tag=tag) +
    scale_color_manual(values = colours[unique(df$Calculation)]) +
    scale_linetype_manual(values = c(1,2))+
    coord_flip() +
    theme_bw()+
    theme(legend.position="none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  
  if(tag %in% c("a","d")){
    plot <- plot + theme(axis.text.y = element_text(),
                         axis.ticks.y = element_line())
  }
  if(tag %in% c("h","i")){
    plot <- plot + theme(axis.text.x = element_text(),
                         axis.ticks.x = element_line())
  }
  if(tag == "g"){
    plot <- plot + theme(axis.text = element_text(),
                         axis.ticks = element_line())
  }
  plot
})


ylab + Reduce("+",fig5_log) + fig5legend +
  plot_spacer() + xlab + plot_spacer() +
  plot_layout(ncol = 3, heights = c(25,1), widths = c(1,21,3))

ggsave(here("experiments/1_figures_main_ms/Figure_5_log.pdf"),
       width = 8, height = 8)



### Fig 5 - linear ----

agg_stab_strain9 <- combine_fct("EF_lin")

agg_stab_strain9_temp <- agg_stab_strain9 %>%
  dplyr::filter(var_treat=="CB-SB-PB")

agg_stab_strain9 <- agg_stab_strain9 %>%
  dplyr::filter(var_treat!="CB-SB-PB")

agg_stab_strain9 <- rbind(agg_stab_strain9,
                          agg_stab_strain9_temp %>% filter(method %in% c("CB+SBPB","Simulated")) %>% mutate(var_treat="CB-SB-PB & CB+SBPB"),
                          agg_stab_strain9_temp %>% filter(method %in% c("PB+CBSB","Simulated")) %>% mutate(var_treat="CB-SB-PB & PB+CBSB"),
                          agg_stab_strain9_temp %>% filter(method %in% c("SB+CBPB","Simulated")) %>% mutate(var_treat="CB-SB-PB & SB+CBPB"))

agg_stab_strain9 <- agg_stab_strain9 %>%
  mutate(method2 = ifelse(agg_stab_strain9$method=="Simulated","Simulated","Calculated")) %>%
  rename(Calculation=method, Method=method2)%>%
  mutate(Calculation = factor(Calculation, levels = c("Simulated","CB+SB","CB+PB","SB+PB",
                                                      "CB+SBPB","PB+CBSB","SB+CBPB")),
         Method = factor(Method, levels = c("Simulated","Calculated")))



fig5_lin <- split(agg_stab_strain9, f=agg_stab_strain9$var_treat, drop = T)

fig5_lin <- lapply(fig5_lin, function(df){
  tag <- tags[as.character(unique(df$var_treat))]
  plot <- df %>%
    filter(Species == "SB_tot", num_strains==9) %>%
    ggplot(aes(x = stand_var, linetype=Method, col=Calculation)) +
    geom_hline(yintercept = c(-8, 0), col = "black", linetype=3) +
    geom_line(aes(y = hyst_min_log), size = 1) +
    geom_line(aes(y = hyst_max_log), size = 1) +
    facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
    labs(y="Oxygen diffusivity\n[log10 uM per hour]",
         x="Amount of trait variation\n[see text for units]",
         linetype="Method",col="Calculation", tag=tag) +
    scale_color_manual(values = colours[unique(df$Calculation)]) +
    scale_linetype_manual(values = c(1,2))+
    coord_flip() +
    theme_bw()+
    theme(legend.position="none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  
  if(tag %in% c("a","d")){
    plot <- plot + theme(axis.text.y = element_text(),
                         axis.ticks.y = element_line())
  }
  if(tag %in% c("h","i")){
    plot <- plot + theme(axis.text.x = element_text(),
                         axis.ticks.x = element_line())
  }
  if(tag == "g"){
    plot <- plot + theme(axis.text = element_text(),
                         axis.ticks = element_line())
  }
  plot
})

ylab + Reduce("+",fig5_lin) + fig5legend +
  plot_spacer() + xlab + plot_spacer() +
  plot_layout(ncol = 3, heights = c(25,1), widths = c(1,21,3))


ggsave(here("experiments/1_figures_main_ms/Figure_5_lin.pdf"),
       width = 8, height = 8)




# 
# agg_stab_strain9 <- combine_fct("arithmetic")
# 
# agg_stab_strain9 %>%
#   filter(Species == "SB_tot", num_strains==9) %>%
#   ggplot(aes(x = var_gmax, col=method)) +
#   geom_hline(yintercept = c(-8, 0), col = "black") +
#   geom_line(aes(y = hyst_min_log), lwd = 1, alpha=0.6) +
#   geom_line(aes(y = hyst_max_log), lwd = 1, alpha=0.6) +
#   facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
#   labs(x="Amount of trait variation\n[see text for units]",
#        y="Amount of trait variation\n[see text for units]",
#        title = "Arithmetic mean on linear scale",
#        col="Method") +
#   coord_flip() +
#   theme_bw()+
#   theme(legend.position="top")
# 
# 
# agg_stab_strain9 <- combine_fct("geometric") # same as agg_stab_strain9 <- combine_fct("arith_log")
# 
# agg_stab_strain9 %>%
#   filter(Species == "SB_tot", num_strains==9) %>%
#   ggplot(aes(x = var_gmax, col=method)) +
#   geom_hline(yintercept = c(-8, 0), col = "black") +
#   geom_line(aes(y = hyst_min_log), lwd = 1, alpha=0.6) +
#   geom_line(aes(y = hyst_max_log), lwd = 1, alpha=0.6) +
#   facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
#   labs(x="Amount of trait variation\n[see text for units]",
#        y="Amount of trait variation\n[see text for units]",
#        title = "Geometric mean on linear scale",
#        col="Method") +
#   coord_flip() +
#   theme_bw()+
#   theme(legend.position="top")
# 
# 
# agg_stab_strain9 <- combine_fct("geom_log")
# 
# agg_stab_strain9 %>%
#   filter(Species == "SB_tot", num_strains==9) %>%
#   ggplot(aes(x = var_gmax, col=method)) +
#   geom_hline(yintercept = c(-8, 0), col = "black") +
#   geom_line(aes(y = hyst_min_log), lwd = 1, alpha=0.6) +
#   geom_line(aes(y = hyst_max_log), lwd = 1, alpha=0.6) +
#   facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
#   labs(x="Amount of trait variation\n[see text for units]",
#        y="Amount of trait variation\n[see text for units]",
#        title = "Geometric mean on log scale",
#        col="Method") +
#   coord_flip() +
#   theme_bw()+
#   theme(legend.position="top")

## Not used anymore ----

# ss_9s <- readRDS(here("data/0_ss_finding/temporal_method/ss_data_9strains_waittime1e+06_event_definition_2.RDS"))
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

# ss_9s <- readRDS(here("data/0_ss_finding/temporal_method/ss_data_9strains_waittime1e+06_event_definition_2.RDS"))
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
# stab_2s <- readRDS(here("data/0_ss_finding/temporal_method/stab_data_2strains_waittime1e+06_event_definition_2.RDS"))
# stab_3s <- readRDS(here("data/0_ss_finding/temporal_method/stab_data_3strains_waittime1e+06_event_definition_2.RDS"))
# stab_6s <- readRDS(here("data/0_ss_finding/temporal_method/stab_data_6strains_waittime1e+06_event_definition_2.RDS"))
# stab_9s <- readRDS(here("data/0_ss_finding/temporal_method/stab_data_9strains_waittime1e+06_event_definition_2.RDS"))
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







