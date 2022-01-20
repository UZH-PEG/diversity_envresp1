## Figures for main manuscript
## 
## 


## this file -- version 1 -- makes only graphs for CB, SB-PB, and CB-SB-PB
## the other file -- version 2 -- makes graphs with all combinations

## Prelims ----
library(microxanox)
library(tidyverse)
library(patchwork)
library(here)

colfunc_CB <- colorRampPalette(c("#024F17", "#B5FFC9"))
colfunc_SB <- colorRampPalette(c("#7D1402", "#FCBEB3"))
colfunc_PB <- colorRampPalette(c("#6E0172", "#F9AEFC"))

source(here("R/various_useful_functions.r"))

## Figure 1 ----
## Powerpoint

## Figure 2a ----
## Keynote

## Figure 2b-f ----

ss_9s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/9_strain_SS_data_1e6.RDS"))
sort(unique(ss_9s$CB_var_gmax_s))
sort(unique(ss_9s$SB_var_gmax_s))
ss_result <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0) < 0.0001,
         abs(SB_var_gmax_s - 0) < 0.0001,
         abs(PB_var_gmax_s - 0) < 0.0001)



#temp <- ss_result$ssfind_result[[1]] %>%
  
#ss_result <- ss_9s$ssfind_result[[25]]

temp <- ss_result$ssfind_result[[1]] %>%
  mutate(a = 10^a_O) %>%
  # arrange(a) %>%
  # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
  #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
  # select(-initial_N_CB, -a_O) %>%
  gather(species, quantity, 2:(ncol(.) - 2)) %>%
  mutate(
    var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
    functional_group = case_when(
      str_detect(species, "CB_5") ~ "CB",
      str_detect(species, "SB_5") ~ "SB",
      str_detect(species, "PB_5") ~ "PB"
    ),
    log10_quantity = log10(quantity + 1)
  )

num_strains <- temp %>%
  group_by(functional_group) %>%
  summarise(num = length(unique(species))) %>%
  na.omit()

num_CB_strains <- num_strains$num[num_strains$functional_group == "CB"]
num_SB_strains <- num_strains$num[num_strains$functional_group == "SB"]
num_PB_strains <- num_strains$num[num_strains$functional_group == "PB"]

line_width <- 2

p1 <- temp %>%
  dplyr::filter(functional_group == "CB")  %>%
  mutate(species = factor(species, levels = unique(species))) %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  scale_colour_manual(values = colfunc_CB(num_CB_strains)) +
  guides(colour = guide_legend(ncol = 3)) +
  theme(legend.position="none") +
  ggtitle("(b) Cyanobacteria")
# p1

p2 <- temp %>%
  dplyr::filter(functional_group == "SB")  %>%
  mutate(species = factor(species, levels = unique(species))) %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  scale_colour_manual(values = colfunc_SB(num_SB_strains)) +
  guides(colour = guide_legend(ncol = 3)) +
  theme(legend.position="none") +
  ggtitle("(c) Sulfate reducing bacteria")

p3 <- temp %>%
  dplyr::filter(functional_group == "PB")  %>%
  mutate(species = factor(species, levels = unique(species))) %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  scale_colour_manual(values = colfunc_PB(num_PB_strains)) +
  guides(colour = guide_legend(ncol = 3)) +
  theme(legend.position="none") +
  ggtitle("(d) Phototrophic sulfur bacteria")

p4 <- temp %>%
  dplyr::filter(var_type == "Substrate", species == "O") %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  theme(legend.position="none") +
  ggtitle("(e) Oxygen concentration")

p5 <- temp %>%
  dplyr::filter(var_type == "Substrate", species == "SR") %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width, col = 6) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  theme(legend.position="none") +
  ggtitle("(f) Sulfide concentration")

patchwork <- p1 / p2 / p3 / p4 / p5
patchwork
ggsave(here("experiments/1_figures_main_ms/figure_2b-f_v1.pdf"))


## Figure 3 ----
## Stable state with diversity in all functional groups.

ss_9s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/9_strain_SS_data_1e6.RDS"))
sort(unique(ss_9s$CB_var_gmax_s))
sort(unique(ss_9s$SB_var_gmax_s))
ss_result <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.028254848) < 0.0001,
         abs(SB_var_gmax_s - 0.084764545) < 0.0001,
         abs(PB_var_gmax_s - 0.084764545) < 0.0001)

temp <- ss_result$ssfind_result[[1]] %>%
  mutate(a = 10^a_O) %>%
  # arrange(a) %>%
  # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
  #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
  # select(-initial_N_CB, -a_O) %>%
  gather(species, quantity, 2:(ncol(.) - 2)) %>%
  mutate(
    var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
    functional_group = case_when(
      str_detect(species, "CB_") ~ "CB",
      str_detect(species, "SB_") ~ "SB",
      str_detect(species, "PB_") ~ "PB"
    ),
    log10_quantity = log10(quantity + 1)
  )

num_strains <- temp %>%
  group_by(functional_group) %>%
  summarise(num = length(unique(species))) %>%
  na.omit()

num_CB_strains <- num_strains$num[num_strains$functional_group == "CB"]
num_SB_strains <- num_strains$num[num_strains$functional_group == "SB"]
num_PB_strains <- num_strains$num[num_strains$functional_group == "PB"]

line_width <- 2

p1 <- temp %>%
  dplyr::filter(functional_group == "CB")  %>%
  mutate(species = factor(species, levels = unique(species))) %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  scale_colour_manual(values = colfunc_CB(num_CB_strains)) +
  guides(colour = guide_legend(ncol = 3)) +
  theme(legend.position="none") +
  ggtitle("(a) Cyanobacteria")
#p1# p1

p2 <- temp %>%
  dplyr::filter(functional_group == "SB")  %>%
  mutate(species = factor(species, levels = unique(species))) %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  scale_colour_manual(values = colfunc_SB(num_SB_strains)) +
  guides(colour = guide_legend(ncol = 3)) +
  theme(legend.position="none") +
  ggtitle("(b) Sulfate reducing bacteria")

p3 <- temp %>%
  dplyr::filter(functional_group == "PB")  %>%
  mutate(species = factor(species, levels = unique(species))) %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  scale_colour_manual(values = colfunc_PB(num_PB_strains)) +
  guides(colour = guide_legend(ncol = 3)) +
  theme(legend.position="none") +
  ggtitle("(c) Phototrophic sulfur bacteria")

p4 <- temp %>%
  dplyr::filter(var_type == "Substrate", species == "O") %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  theme(legend.position="none") +
  ggtitle("(d) Oxygen concentration")

p5 <- temp %>%
  dplyr::filter(var_type == "Substrate", species == "SR") %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width, col = 6) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  theme(legend.position="none") +
  ggtitle("(e) Sulfide concentration")

patchwork <- p1 / p2 / p3 / p4 / p5
patchwork
ggsave(here("experiments/1_figures_main_ms/figure_3_v1.pdf"))


## Figure 4 ----
## Shows effect of no diversity versus diversity in all functional groups.

ss_9s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/9_strain_SS_data_1e6.RDS"))
sort(unique(ss_9s$CB_var_gmax_s))
sort(unique(ss_9s$SB_var_gmax_s))
ss_result_none <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.0) < 0.0001,
         abs(SB_var_gmax_s - 0.0) < 0.0001,
         abs(PB_var_gmax_s - 0.0) < 0.0001)
ss_result_CB <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.014958449) < 0.0001,
         abs(SB_var_gmax_s - 0.0) < 0.0001,
         abs(PB_var_gmax_s - 0.0) < 0.0001)
ss_result_SB <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.0) < 0.0001,
         abs(SB_var_gmax_s - 0.044875347) < 0.0001,
         abs(PB_var_gmax_s - 0.0) < 0.0001)
ss_result_PB <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.0) < 0.0001,
         abs(SB_var_gmax_s - 0.0) < 0.0001,
         abs(PB_var_gmax_s - 0.044875347) < 0.0001)
ss_result_CBSB <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.014958449) < 0.0001,
         abs(SB_var_gmax_s - 0.044875347) < 0.0001,
         abs(PB_var_gmax_s - 0.0) < 0.0001)
ss_result_CBPB <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.014958449) < 0.0001,
         abs(SB_var_gmax_s - 0.0) < 0.0001,
         abs(PB_var_gmax_s - 0.044875347) < 0.0001)
ss_result_SBPB <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.0) < 0.0001,
         abs(SB_var_gmax_s - 0.044875347) < 0.0001,
         abs(PB_var_gmax_s - 0.044875347) < 0.0001)
ss_result_CBSBPB <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.014958449) < 0.0001,
         abs(SB_var_gmax_s - 0.044875347) < 0.0001,
         abs(PB_var_gmax_s - 0.044875347) < 0.0001)



temp_none <- ss_result_none$ssfind_result[[1]] %>%
  mutate(a = 10^a_O) %>%
  # arrange(a) %>%
  # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
  #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
  # select(-initial_N_CB, -a_O) %>%
  gather(species, quantity, 2:(ncol(.) - 2)) %>%
  mutate(
    var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
    functional_group = case_when(
      str_detect(species, "CB_") ~ "CB",
      str_detect(species, "SB_") ~ "SB",
      str_detect(species, "PB_") ~ "PB"
    ),
    log10_quantity = log10(quantity + 1)
  )

temp_CB <- ss_result_CB$ssfind_result[[1]] %>%
  mutate(a = 10^a_O) %>%
  # arrange(a) %>%
  # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
  #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
  # select(-initial_N_CB, -a_O) %>%
  gather(species, quantity, 2:(ncol(.) - 2)) %>%
  mutate(
    var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
    functional_group = case_when(
      str_detect(species, "CB_") ~ "CB",
      str_detect(species, "SB_") ~ "SB",
      str_detect(species, "PB_") ~ "PB"
    ),
    log10_quantity = log10(quantity + 1)
  )
temp_SB <- ss_result_SB$ssfind_result[[1]] %>%
  mutate(a = 10^a_O) %>%
  # arrange(a) %>%
  # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
  #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
  # select(-initial_N_CB, -a_O) %>%
  gather(species, quantity, 2:(ncol(.) - 2)) %>%
  mutate(
    var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
    functional_group = case_when(
      str_detect(species, "CB_") ~ "CB",
      str_detect(species, "SB_") ~ "SB",
      str_detect(species, "PB_") ~ "PB"
    ),
    log10_quantity = log10(quantity + 1)
  )
temp_PB <- ss_result_PB$ssfind_result[[1]] %>%
  mutate(a = 10^a_O) %>%
  # arrange(a) %>%
  # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
  #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
  # select(-initial_N_CB, -a_O) %>%
  gather(species, quantity, 2:(ncol(.) - 2)) %>%
  mutate(
    var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
    functional_group = case_when(
      str_detect(species, "CB_") ~ "CB",
      str_detect(species, "SB_") ~ "SB",
      str_detect(species, "PB_") ~ "PB"
    ),
    log10_quantity = log10(quantity + 1)
  )
temp_CBSB <- ss_result_CBSB$ssfind_result[[1]] %>%
  mutate(a = 10^a_O) %>%
  # arrange(a) %>%
  # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
  #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
  # select(-initial_N_CB, -a_O) %>%
  gather(species, quantity, 2:(ncol(.) - 2)) %>%
  mutate(
    var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
    functional_group = case_when(
      str_detect(species, "CB_") ~ "CB",
      str_detect(species, "SB_") ~ "SB",
      str_detect(species, "PB_") ~ "PB"
    ),
    log10_quantity = log10(quantity + 1)
  )
temp_CBPB <- ss_result_CBPB$ssfind_result[[1]] %>%
  mutate(a = 10^a_O) %>%
  # arrange(a) %>%
  # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
  #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
  # select(-initial_N_CB, -a_O) %>%
  gather(species, quantity, 2:(ncol(.) - 2)) %>%
  mutate(
    var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
    functional_group = case_when(
      str_detect(species, "CB_") ~ "CB",
      str_detect(species, "SB_") ~ "SB",
      str_detect(species, "PB_") ~ "PB"
    ),
    log10_quantity = log10(quantity + 1)
  )
temp_SBPB <- ss_result_SBPB$ssfind_result[[1]] %>%
  mutate(a = 10^a_O) %>%
  # arrange(a) %>%
  # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
  #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
  # select(-initial_N_CB, -a_O) %>%
  gather(species, quantity, 2:(ncol(.) - 2)) %>%
  mutate(
    var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
    functional_group = case_when(
      str_detect(species, "CB_") ~ "CB",
      str_detect(species, "SB_") ~ "SB",
      str_detect(species, "PB_") ~ "PB"
    ),
    log10_quantity = log10(quantity + 1)
  )
temp_CBSBPB <- ss_result_CBSBPB$ssfind_result[[1]] %>%
  mutate(a = 10^a_O) %>%
  # arrange(a) %>%
  # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
  #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
  # select(-initial_N_CB, -a_O) %>%
  gather(species, quantity, 2:(ncol(.) - 2)) %>%
  mutate(
    var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
    functional_group = case_when(
      str_detect(species, "CB_") ~ "CB",
      str_detect(species, "SB_") ~ "SB",
      str_detect(species, "PB_") ~ "PB"
    ),
    log10_quantity = log10(quantity + 1)
  )

line_width <- 2.5

## a
p1 <- temp_none %>%
  dplyr::filter(var_type == "Substrate", species == "O") %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  geom_path(data = filter(temp_CB, var_type == "Substrate", species == "O"),
            col = "black", lwd = line_width - 1.5) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  theme(legend.position="none") +
  ggtitle("(a) No diversity versus diversity in only CB") +
  geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)
p2 <- temp_none %>%
  dplyr::filter(var_type == "Substrate", species == "O") %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  geom_path(data = filter(temp_SB, var_type == "Substrate", species == "O"),
            col = "black", lwd = line_width - 1.5) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  theme(legend.position="none") +
  ggtitle("(a) No diversity versus diversity in only SB") +
  geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)
p3 <- temp_none %>%
  dplyr::filter(var_type == "Substrate", species == "O") %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  geom_path(data = filter(temp_PB, var_type == "Substrate", species == "O"),
            col = "black", lwd = line_width - 1.5) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  theme(legend.position="none") +
  ggtitle("(a) No diversity versus diversity in only PB") +
  geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)
p4 <- temp_none %>%
  dplyr::filter(var_type == "Substrate", species == "O") %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  geom_path(data = filter(temp_CBSB, var_type == "Substrate", species == "O"),
            col = "black", lwd = line_width - 1.5) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  theme(legend.position="none") +
  ggtitle("(b) No diversity versus diversity in only CB & SB") +
  geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)
p5 <- temp_none %>%
  dplyr::filter(var_type == "Substrate", species == "O") %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  geom_path(data = filter(temp_CBPB, var_type == "Substrate", species == "O"),
            col = "black", lwd = line_width - 1.5) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  theme(legend.position="none") +
  ggtitle("(b) No diversity versus diversity in only CB & PB") +
  geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)
p6 <- temp_none %>%
  dplyr::filter(var_type == "Substrate", species == "O") %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  geom_path(data = filter(temp_SBPB, var_type == "Substrate", species == "O"),
            col = "black", lwd = line_width - 1.5) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  theme(legend.position="none") +
  ggtitle("(b) No diversity versus diversity in only SB & PB") +
  geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)

p7 <- temp_none %>%
  dplyr::filter(var_type == "Substrate", species == "O") %>%
  ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
  geom_path(lwd = line_width) +
  geom_path(data = filter(temp_CBSBPB, var_type == "Substrate", species == "O"),
            col = "black", lwd = line_width - 1.5) +
  ##ylab("log10(quantity [cells])") +
  ylab("Log(Quantity)") +
  xlab("Oxygen diffusivity") +
  theme(legend.position="none") +
  ggtitle("(c) No diversity versus diversity all functional groups") +
  geom_vline(xintercept = c(-8, 0), col = "grey", lwd = 3)

p1 / p6 / p7
 
ggsave(here("experiments/1_figures_main_ms/figure_4_v1.pdf"),
       width = 6, height = 5)


## Figure 5 ----
## Region of bistability by diversity and number of strains.

ss_2s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/2_strain_SS_data_1e6.RDS"))
stab_2s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/2_strain_stab_data_1e6.RDS")) %>%
  mutate(num_strains = 2)
ss_2s_sub1 <- readRDS(here("experiments/0_ss_finding/temporal_method/data/2_strain_SS_data_1e6_sub1.RDS"))
stab_2s_sub1 <- readRDS(here("experiments/0_ss_finding/temporal_method/data/2_strain_stab_data_1e6_sub1.RDS")) %>%
  mutate(num_strains = 2)


ss_3s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/3_strain_SS_data_1e6.RDS"))
stab_3s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/3_strain_stab_data_1e6.RDS")) %>%
  mutate(num_strains = 3) 
ss_3s_sub1 <- readRDS(here("experiments/0_ss_finding/temporal_method/data/3_strain_SS_data_1e6_sub1.RDS"))
stab_3s_sub1 <- readRDS(here("experiments/0_ss_finding/temporal_method/data/3_strain_stab_data_1e6_sub1.RDS")) %>%
  mutate(num_strains = 3)




ss_6s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/6_strain_SS_data_1e6.RDS"))
stab_6s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/6_strain_stab_data_1e6.RDS")) %>%
  mutate(num_strains = 6) 
ss_6s_sub1 <- readRDS(here("experiments/0_ss_finding/temporal_method/data/6_strain_SS_data_1e6_sub1.RDS"))
stab_6s_sub1 <- readRDS(here("experiments/0_ss_finding/temporal_method/data/6_strain_stab_data_1e6_sub1.RDS")) %>%
  mutate(num_strains = 6)


ss_9s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/9_strain_SS_data_1e6.RDS"))
stab_9s <- readRDS(here("experiments/0_ss_finding/temporal_method/data/9_strain_stab_data_1e6.RDS")) %>%
  mutate(num_strains = 9) 
ss_9s_sub1 <- readRDS(here("experiments/0_ss_finding/temporal_method/data/9_strain_SS_data_1e6_sub1.RDS"))
stab_9s_sub1 <- readRDS(here("experiments/0_ss_finding/temporal_method/data/9_strain_stab_data_1e6_sub1.RDS")) %>%
  mutate(num_strains = 9)



all_stab <- stab_2s %>%
  bind_rows(stab_2s_sub1) %>%
  bind_rows(stab_3s) %>%
  bind_rows(stab_3s_sub1) %>%
  bind_rows(stab_6s) %>%
  bind_rows(stab_6s_sub1) %>%
  bind_rows(stab_9s) %>%
  bind_rows(stab_9s_sub1)

#all_stab <- stab_9s

CB_vars <- unique(all_stab$CB_var_gmax_s)
SB_vars <- unique(all_stab$SB_var_gmax_s)
PB_vars <- unique(all_stab$PB_var_gmax_s)

CB_all_stab <- all_stab %>%
  filter(SB_var_gmax_s == 0,
         PB_var_gmax_s == 0) %>%
  ungroup() %>%
  mutate(var_treat = "CB",
         var_gmax = CB_var_gmax_s,
         stand_var = var_gmax / max(var_gmax))
SB_all_stab <- all_stab %>%
  filter(CB_var_gmax_s == 0,
         PB_var_gmax_s == 0)  %>%
  ungroup() %>%
  mutate(var_treat = "SB",
         var_gmax = SB_var_gmax_s,
         stand_var = var_gmax / max(var_gmax))
PB_all_stab <- all_stab %>%
  filter(CB_var_gmax_s == 0,
         SB_var_gmax_s == 0) %>%
  ungroup()  %>%
  mutate(var_treat = "PB",
         var_gmax = PB_var_gmax_s,
         stand_var = var_gmax / max(var_gmax))

for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
                   SB_var_gmax_s = sort(SB_vars))
CBSB_all_stab <- all_stab %>%
  right_join(for_join)  %>%
  ungroup() %>%
  mutate(var_treat = "CB-SB",
         var_gmax = CB_var_gmax_s,
         stand_var = var_gmax / max(var_gmax)) %>%
  filter(PB_var_gmax_s == 0)

for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
                   PB_var_gmax_s = sort(PB_vars))
CBPB_all_stab <- all_stab %>%
  right_join(for_join)  %>%
  ungroup() %>%
  mutate(var_treat = "CB-PB",
         var_gmax = CB_var_gmax_s,
         stand_var = var_gmax / max(var_gmax)) %>%
  filter(SB_var_gmax_s == 0)

for_join <- tibble(SB_var_gmax_s = sort(SB_vars),
                   PB_var_gmax_s = sort(PB_vars))
SBPB_all_stab <- all_stab %>%
  right_join(for_join)  %>%
  ungroup() %>%
  mutate(var_treat = "SB-PB",
         var_gmax = SB_var_gmax_s,
         stand_var = var_gmax / max(var_gmax)) %>%
  filter(CB_var_gmax_s == 0)



for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
                   SB_var_gmax_s = sort(SB_vars),
                   PB_var_gmax_s = sort(PB_vars))
CBSBPB_all_stab <- all_stab %>%
  right_join(for_join)  %>%
  ungroup() %>%
  mutate(var_treat = "CB-SB-PB",
         var_gmax = CB_var_gmax_s,
         stand_var = var_gmax / max(var_gmax))

all_stab_results <- CB_all_stab %>%
  bind_rows(SB_all_stab) %>%
  bind_rows(PB_all_stab) %>%
  bind_rows(CBSB_all_stab) %>%
  bind_rows(CBPB_all_stab) %>%
  bind_rows(SBPB_all_stab) %>%
  bind_rows(CBSBPB_all_stab)

all_stab_results <- all_stab_results %>%
  mutate(var_treat = forcats::fct_relevel(var_treat, levels = c("CB",
                                                                "SB",
                                                                "PB",
                                                                "CB-SB",
                                                                "CB-PB",
                                                                "SB-PB",
                                                                "CB-SB-PB")))

all_stab_results_small <- select(all_stab_results, -7:-12)

saveRDS(all_stab_results_small, here("experiments/0_ss_finding/temporal_method/data/all_stab_results_small.RDS"))

all_stab_results %>%
  filter(var_treat %in% c("CB", "SB-PB", "CB-SB-PB")) %>%
  filter(num_strains == 9) %>%
  filter(Species == "CB_tot") %>%
  ggplot(aes(x = stand_var,
             #col = as.factor(num_strains)
             )) +
  geom_line(aes(y = hyst_min_log), lwd = 1, alpha = 0.3) +
  geom_line(aes(y = hyst_max_log), lwd = 1, alpha = 0.3) +
  facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
  xlab("Standardised amount of trait variation\n[see text for units]") +
  ylab("Oxygen diffusivity\n[log10 uM per hour]") +
  labs(fill = "Variation in\nonly these\nfunctional groups") +
  coord_flip() +
  guides(col = guide_legend(title="Number of strains")) +
  theme(
    legend.position="top"
    #strip.background = element_blank(),
    #strip.text.x = element_blank()
  ) +
  geom_hline(yintercept = c(-8, 0), col = "grey", lwd = 3)

ggsave(here("experiments/1_figures_main_ms/figure_5_v1_9strain_only.pdf"),
       width = 10, height = 8)


## Figure 5 all strains ----

all_stab_results %>%
  filter(var_treat %in% c("CB", "SB-PB", "CB-SB-PB")) %>%
  #filter(num_strains == 9) %>%
  filter(Species == "CB_tot") %>%
  ggplot(aes(x = stand_var,
             col = as.factor(num_strains)
  )) +
  geom_line(aes(y = hyst_min_log), lwd = 1, alpha = 0.3) +
  geom_line(aes(y = hyst_max_log), lwd = 1, alpha = 0.3) +
  facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
  xlab("Standardised amount of trait variation\n[see text for units]") +
  ylab("Oxygen diffusivity\n[log10 uM per hour]") +
  labs(fill = "Variation in\nonly these\nfunctional groups") +
  coord_flip() +
  guides(col = guide_legend(title="Number of strains")) +
  theme(
    legend.position="top"
    #strip.background = element_blank(),
    #strip.text.x = element_blank()
  ) +
  geom_hline(yintercept = c(-8, 0), col = "grey", lwd = 3)

ggsave(here("experiments/1_figures_main_ms/figure_5_v1_allstrains.pdf"),
       width = 10, height = 8)





agg_stab_res <- all_stab_results_small %>%
  mutate(Strain = str_sub(Species, 1, 2)) %>%
  group_by(Strain, var_treat, var_gmax, num_strains) %>%
  summarise(hyst_min_log = min(hyst_min_log),
            hyst_max_log = max(hyst_max_log))
agg_stab_res %>%
  #filter(var_treat == "CB") %>%
  filter(Strain == "CB") %>%
  ggplot(aes(x = var_gmax,
             col = as.factor(num_strains))) +
  geom_line(aes(y = hyst_min_log), lwd = 1, alpha = 0.3) +
  geom_line(aes(y = hyst_max_log), lwd = 1, alpha = 0.3) +
  facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
  xlab("Amount of trait variation\n[see text for units]") +
  ylab("Oxygen diffusivity\n[log10 uM per hour]") +
  labs(fill = "Variation in\nonly these\nfunctional groups") +
  coord_flip() +
  guides(col = guide_legend(title="Number of strains")) +
  theme(
    legend.position="top"
    #strip.background = element_blank(),
    #strip.text.x = element_blank()
  ) +
  geom_hline(yintercept = c(-8, 0), col = "grey", lwd = 3)




