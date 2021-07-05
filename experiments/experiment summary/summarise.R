rm(list = ls())

library(here)
library(tidyverse)
library(microxanox)

CBSBPB_data <- readRDS(here("experiments/experiment 1/data/ss_data.RDS"))
CB_data <- readRDS(here("experiments/experiment 2/data/ss_data.RDS"))
SBPB_data <- readRDS(here("experiments/experiment 3/data/ss_data.RDS"))



CBSBPB_stab_data <- CBSBPB_data %>%
  group_by(CB_var_gmax_s, CB_var_h_s,
           SB_var_gmax_s, SB_var_h_s,
           PB_var_gmax_s, PB_var_h_s) %>%
  do(stability_measures = get_stability_measures(.$ss_res[[1]]))
results1 <- unnest(CBSBPB_stab_data, cols = c(stability_measures)) %>%
  mutate(var_treat = "CB-SB-PB",
         var_gmax = CB_var_gmax_s)

CB_stab_data <- CB_data %>%
  group_by(CB_var_gmax_s, CB_var_h_s,
           SB_var_gmax_s, SB_var_h_s,
           PB_var_gmax_s, PB_var_h_s) %>%
  do(stability_measures = get_stability_measures(.$ss_res[[1]]))
results2 <- unnest(CB_stab_data, cols = c(stability_measures))  %>%
  mutate(var_treat = "CB",
         var_gmax = CB_var_gmax_s)


SBPB_stab_data <- SBPB_data %>%
  group_by(CB_var_gmax_s, CB_var_h_s,
           SB_var_gmax_s, SB_var_h_s,
           PB_var_gmax_s, PB_var_h_s) %>%
  do(stability_measures = get_stability_measures(.$ss_res[[1]]))
results5 <- unnest(SBPB_stab_data, cols = c(stability_measures))  %>%
  mutate(var_treat = "SB-PB",
         var_gmax = SB_var_gmax_s)


all_stab_results <- results1 %>%
  bind_rows(results2) %>%
#  bind_rows(results3) %>%
 # bind_rows(results4) %>%
  bind_rows(results5)

all_stab_results<- all_stab_results %>%
    mutate(var_treat = forcats::fct_relevel(var_treat, levels = c("CB", "SB-PB", "CB-SB-PB")))
#mutate(var_treat = forcats::fct_relevel(var_treat, levels = c("CB", "SB", "PB", "SB-PB", "CB-SB-PB")))

saveRDS(all_stab_results, here("experiments/experiment summary/all_stab.RDS"))


all_stab_results <- readRDS(here("experiments/experiment summary/all_stab.RDS"))

all_stab_results %>%
  filter(Species == "O") %>%
  ggplot(aes(x = var_gmax, y = hyst_range, col=var_treat)) +
  geom_point() +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Extent of bistability region\n[log10 oxygen diffusivity]") +
  labs(col = "Variation in\nonly these\nfunctional groups")
ggsave("manuscript/figures/extent_of_bistab1.pdf", height = 4)

all_stab_results %>%
  #filter(var_treat == "CB") %>%
  filter(Species == "O") %>%
  ggplot(aes(x = var_gmax,
             ymin = hyst_min,
             ymax = hyst_max,
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

ggsave("manuscript/figures/extent_of_bistab2.pdf", height = 4)

