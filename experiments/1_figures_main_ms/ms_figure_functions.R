## figure functions
## (since same figures appear in main text and supplement, just with different data)
##



fig_div_vs_o2diff_1strain <- function(all_stab, which_strain, figure_title) {
  
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
  
  #saveRDS(all_stab_results_small, here("experiments/0_ss_finding/temporal_method/data/all_stab_results_small.RDS"))
  
  
  
  p1 <- all_stab_results %>%
    #filter(var_treat == "CB") %>%
    filter(num_strains == which_strain) %>%
    filter(Species == "SB_tot") %>%
    ggplot(aes(x = stand_var
               #col = as.factor(num_strains))
               )) +
    geom_line(aes(y = hyst_min_log), lwd = 0.5, alpha = 0.8) +
    geom_line(aes(y = hyst_max_log), lwd = 0.5, alpha = 0.8) +
    geom_ribbon(aes(ymin = hyst_min_log, ymax = hyst_max_log),
                fill = "green", alpha = 0.2) +
    facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
    xlab("Standardised amount of trait variation\n[see text for units]") +
    ylab("Oxygen diffusivity\n[log10 uM per hour]") +
    #labs(fill = "Variation in\nonly these\nfunctional groups") +
    coord_flip() +
    #guides(col = guide_legend(title="Number of strains"),
    #       fill = guide_none()) +
    theme_bw() +
    theme(
      legend.position="top",
      panel.background = element_blank(),
      #strip.text.x = element_blank()
    ) +
    geom_hline(yintercept = c(-8, 0), col = "grey", lwd = 3) +
    ggtitle(figure_title)
  
 p1
  
}

fig_div_vs_o2diff_multistrain <- function(all_stab, which_strains, figure_title) {
  
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
  
  #saveRDS(all_stab_results_small, here("experiments/0_ss_finding/temporal_method/data/all_stab_results_small.RDS"))
  
  
  
  p1 <- all_stab_results %>%
    #filter(var_treat == "CB") %>%
    filter(num_strains %in% which_strains) %>%
    filter(Species == "SB_tot") %>%
    ggplot(aes(x = stand_var,
               col = as.factor(num_strains))) +
    geom_line(aes(y = hyst_min_log), lwd = 0.5, alpha = 0.8) +
    geom_line(aes(y = hyst_max_log), lwd = 0.5, alpha = 0.8) +
    geom_ribbon(aes(ymin = hyst_min_log, ymax = hyst_max_log,
                    col = as.factor(num_strains),
                    fill = as.factor(num_strains)), alpha = 0.2) +
    facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
    xlab("Standardised amount of trait variation\n[see text for units]") +
    ylab("Oxygen diffusivity\n[log10 uM per hour]") +
    #labs(fill = "Variation in\nonly these\nfunctional groups") +
    coord_flip() +
    guides(col = guide_legend(title="Number of strains"),
           fill = guide_none()) +
    theme_bw() +
    theme(
      legend.position="top",
      panel.background = element_blank(),
      #strip.text.x = element_blank()
    ) +
    geom_hline(yintercept = c(-8, 0), col = "grey", lwd = 3) +
    ggtitle(figure_title)
  
  p1
  
}

fig_state_vs_o2diff <- function(ss_result){
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
}

fig_state_vs_o2diff_sidebyside <- function(ss_result){
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
  
  line_width <- 1
  
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
    ggtitle("(a) Cyanobacteria") +
    facet_grid(. ~ direction) 
  
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
    ggtitle("(b) Sulfate reducing bacteria")+
    facet_grid(. ~ direction) +
    theme(strip.text = element_blank())
  
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
    ggtitle("(c) Phototrophic sulfur bacteria")+
    facet_grid(. ~ direction) +
    theme(strip.text = element_blank())
  
  p4 <- temp %>%
    dplyr::filter(var_type == "Substrate", species == "O") %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_path(lwd = line_width) +
    ##ylab("log10(quantity [cells])") +
    ylab("Log(Quantity)") +
    xlab("Oxygen diffusivity") +
    theme(legend.position="none") +
    ggtitle("(d) Oxygen concentration")+
    facet_grid(. ~ direction) +
    theme(strip.text = element_blank())
  
  p5 <- temp %>%
    dplyr::filter(var_type == "Substrate", species == "SR") %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_path(lwd = line_width, col = 6) +
    ##ylab("log10(quantity [cells])") +
    ylab("Log(Quantity)") +
    xlab("Oxygen diffusivity") +
    theme(legend.position="none") +
    ggtitle("(e) Sulfide concentration")+
    facet_grid(. ~ direction) +
    theme(strip.text = element_blank())
  
  patchwork <- p1 / p2 / p3 / p4 / p5
  patchwork
}


fig_resilience_vs_div <- function(all_stab, which_strain, figure_title) {
  
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
  
  #saveRDS(all_stab_results_small, here("experiments/0_ss_finding/temporal_method/data/all_stab_results_small.RDS"))
  
  resilience <- all_stab_results_small %>%
    #filter(var_treat == "CB") %>%
    filter(num_strains == which_strain) %>%
    filter(Species == "SB_tot") %>%
    select(1:7, 16, 17, 20:23) %>%
    group_by(var_treat) %>%
    mutate(rel_trans_pos_ao = hyst_max_log-hyst_max_log[stand_var==0],
           rel_trans_pos_oa = -hyst_min_log+hyst_min_log[stand_var==0]) %>%
    select(-8:-9) %>%
    pivot_longer(names_to = "which_transition",
                 values_to = "rel_log_trans_pos",
                 12:13)
    
  p1 <- resilience %>%
    ggplot(aes(x = stand_var, y = rel_log_trans_pos, col = which_transition)) +
    geom_line(show.legend = FALSE) +
    facet_grid(var_treat ~ ., scales = "fixed") +
    theme_bw() +
    theme(legend.position="top",
      panel.background = element_blank(),
      #strip.text.x = element_blank()
      ) +
    ylab("Resilience\n[see text for units]") +
    xlab("Standardised amount of trait variation\n[see caption for units]") +
    scale_color_manual(values = c("blue", "red"))
  
  p1
  
}

fig_div_vs_o2diff_1strain_7row <- function(all_stab, which_strain, figure_title) {
  
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
  
  #saveRDS(all_stab_results_small, here("experiments/0_ss_finding/temporal_method/data/all_stab_results_small.RDS"))
  
  
  
  p1 <- all_stab_results %>%
    #filter(var_treat == "CB") %>%
    filter(num_strains == which_strain) %>%
    filter(Species == "SB_tot") %>%
    ggplot(aes(x = stand_var
               #col = as.factor(num_strains))
    )) +
    geom_line(aes(y = hyst_min_log), lwd = 0.5, alpha = 0.8, col="red") +
    geom_line(aes(y = hyst_max_log), lwd = 0.5, alpha = 0.8, col="blue") +
    geom_ribbon(aes(ymin = hyst_min_log, ymax = hyst_max_log),
                fill = "green", alpha = 0.1) +
    facet_grid(var_treat ~ ., scales = "fixed") +
    xlab("Standardised amount of trait variation\n[see caption for units]") +
    ylab("Oxygen diffusivity\n[log10 uM per hour]") +
    #labs(fill = "Variation in\nonly these\nfunctional groups") +
    coord_flip() +
    #guides(col = guide_legend(title="Number of strains"),
    #       fill = guide_none()) +
    theme_bw() +
    theme(
      legend.position="top",
      panel.background = element_blank(),
      #strip.text.x = element_blank()
    ) +
    geom_hline(yintercept = c(-8, 0), col = "grey", lwd = 3) +
    ggtitle(figure_title)
  
  p1
  
}

