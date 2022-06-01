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
    xlab("Standardised amount of trait variation]") +
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

fig_div_vs_o2diff_multistrain <- function(all_stab,
                                          which_strains,
                                          which_wait_time,
                                          figure_title) {
  
  # CB_vars <- unique(all_stab$CB_var_gmax_s)
  # SB_vars <- unique(all_stab$SB_var_gmax_s)
  # PB_vars <- unique(all_stab$PB_var_gmax_s)
  # 
  # CB_all_stab <- all_stab %>%
  #   filter(SB_var_gmax_s == 0,
  #          PB_var_gmax_s == 0) %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # SB_all_stab <- all_stab %>%
  #   filter(CB_var_gmax_s == 0,
  #          PB_var_gmax_s == 0)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "SB",
  #          var_gmax = SB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # PB_all_stab <- all_stab %>%
  #   filter(CB_var_gmax_s == 0,
  #          SB_var_gmax_s == 0) %>%
  #   ungroup()  %>%
  #   mutate(var_treat = "PB",
  #          var_gmax = PB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # 
  # for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
  #                    SB_var_gmax_s = sort(SB_vars))
  # CBSB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB-SB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax)) %>%
  #   filter(PB_var_gmax_s == 0)
  # 
  # for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
  #                    PB_var_gmax_s = sort(PB_vars))
  # CBPB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB-PB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax)) %>%
  #   filter(SB_var_gmax_s == 0)
  # 
  # for_join <- tibble(SB_var_gmax_s = sort(SB_vars),
  #                    PB_var_gmax_s = sort(PB_vars))
  # SBPB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "SB-PB",
  #          var_gmax = SB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax)) %>%
  #   filter(CB_var_gmax_s == 0)
  # 
  # 
  # 
  # for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
  #                    SB_var_gmax_s = sort(SB_vars),
  #                    PB_var_gmax_s = sort(PB_vars))
  # CBSBPB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB-SB-PB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # 
  # all_stab_results <- CB_all_stab %>%
  #   bind_rows(SB_all_stab) %>%
  #   bind_rows(PB_all_stab) %>%
  #   bind_rows(CBSB_all_stab) %>%
  #   bind_rows(CBPB_all_stab) %>%
  #   bind_rows(SBPB_all_stab) %>%
  #   bind_rows(CBSBPB_all_stab)
  # 
  # all_stab_results <- all_stab_results %>%
  #   mutate(var_treat = forcats::fct_relevel(var_treat, levels = c("CB",
  #                                                                 "SB",
  #                                                                 "PB",
  #                                                                 "CB-SB",
  #                                                                 "CB-PB",
  #                                                                 "SB-PB",
  #                                                                 "CB-SB-PB")))
  # 
  # all_stab_results_small <- select(all_stab_results, -7:-12)
  # 
  # #saveRDS(all_stab_results_small, here("experiments/0_ss_finding/temporal_method/data/all_stab_results_small.RDS"))
  # 
  
  
  p1 <- all_stab %>%
    #filter(var_treat == "CB") %>%
    filter(num_strains %in% which_strains, 
           waittime == which_wait_time) %>%
    filter(Species == "SB_tot") %>%
    ggplot(aes(x = stand_var,
               col = as.factor(num_strains))) +
    facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
    geom_line(aes(y = hyst_min_log, col = as.factor(num_strains)), lwd = 0.5, alpha = 0.8) +
    geom_line(aes(y = hyst_max_log), lwd = 0.5, alpha = 0.8) +
    geom_ribbon(aes(ymin = hyst_min_log, ymax = hyst_max_log,
                    col = as.factor(num_strains),
                    fill = as.factor(num_strains)), alpha = 0.2) +
    facet_wrap( ~ var_treat, scales = "free_y", nrow = 3) +
    xlab("Standardised amount of trait variation") +
    ylab(expression(log[10](oxygen~diffusivity)~(h^{-1}))) +
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
    #geom_point() +
    ##ylab("log10(quantity [cells])") +
    ylab("Log(Quantity)") +
    xlab("Oxygen diffusivity") +
    scale_colour_manual(values = colfunc_CB(num_CB_strains)) +
    guides(colour = guide_legend(ncol = 3)) +
    theme(legend.position="none") +
    ggtitle("(a) Cyanobacteria")
  p1
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
}

fig_state_vs_o2diff_sidebyside_dots <- function(ss_result){

  #browser()
  is_repl <- "ss_by_a_N_result" %in% names(ss_result)

  if (is_repl) {
    ss_result$ssfind_result <- ss_result$ss_res
  }
  
  temp1 <- ss_result %>%
    mutate(ssfind_result = list(get_total_bio(ssfind_result)))

  if (is_repl){
    temp1 <- microxanox:::get_stability_measures.replication_ssfind_result(
      temp1$ssfind_result[[1]],
      threshold_diff_log10scale = 3
    )
  } else {
    temp1 <- microxanox:::get_stability_measures.temporal_ssfind_result(
      temp1$ssfind_result[[1]],
      threshold_diff_log10scale = 3
    )
  }
 
  stab_measures <- temp1 %>% 
    filter(Species == "SB_tot") %>%
    select(down=hyst_min_log, up=hyst_max_log) %>%
    pivot_longer(names_to = "direction", values_to = "aO_flip", 1:2) %>%
    mutate(y_start = c(2.5, 7.5),
           y_end = c(7.5, 2.5))
  
  rm(temp1)

  if (is_repl) {
    gathcols <- 1:(31)
    ss_result$ssfind_result[[1]] <- ss_result$ssfind_result[[1]] %>%
      mutate(a_O = log10(a_O),
             direction = ifelse(initial_N_CB == 1, "up", "down"))
  } else {
    gathcols <- 2:(ncol(ss_result$ssfind_result[[1]]) - 2)
  }
  
  #browser()
  temp <- ss_result$ssfind_result[[1]] %>%
    mutate(a = 10^a_O) %>%
    # arrange(a) %>%
    # mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
    #mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
    # select(-initial_N_CB, -a_O) %>%
    gather(species, quantity, gathcols) %>%
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
  
  line_width <- 0.2
  point_size <- 0.6
  arrow_lwd <- 1

  temp$direction[temp$direction == "up"] <- "Increasing oxygen diffusivity"
  temp$direction[temp$direction == "down"] <- "Decreasing oxygen diffusivity"
  stab_measures$direction[stab_measures$direction == "up"] <- "Increasing oxygen diffusivity"
  stab_measures$direction[stab_measures$direction == "down"] <- "Decreasing oxygen diffusivity"
  
  p1 <- temp %>%
    dplyr::filter(functional_group == "CB")  %>%
    mutate(species = factor(species, levels = unique(species))) %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_segment(data = stab_measures,
               aes(x = aO_flip, xend = aO_flip,
                   y = y_start, yend = y_end),
               col = "#bbbbbbff", lwd = arrow_lwd,
               arrow = arrow(length=unit(0.30,"cm"),
                             ends="first", type = "closed")) +
    geom_path(lwd = line_width) +
    geom_point(size = point_size) +
    ##ylab("log10(quantity [cells])") +
    ylab(expression(atop(log[10](dens), (cells~L^{~1})))) +
    xlab(NULL) +
    scale_colour_manual(values = colfunc_CB(num_CB_strains)) +
    guides(colour = guide_legend(ncol = 3)) +
    theme_bw() +
    theme(legend.position="none",
          plot.title = element_text(size = 10)) +
    ggtitle("(a) Cyanobacteria") +
    facet_grid(. ~ direction) 
  
  p2 <- temp %>%
    dplyr::filter(functional_group == "SB")  %>%
    mutate(species = factor(species, levels = unique(species))) %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_segment(data = stab_measures,
                 aes(x = aO_flip, xend = aO_flip,
                     y = c(7.5, 2), yend = c(2, 7.5)),
                 col = "#bbbbbbff", lwd = arrow_lwd,
                 arrow = arrow(length=unit(0.30,"cm"),
                               ends="first", type = "closed")) +
    geom_path(lwd = line_width) +
    geom_point(size = point_size) +
    ##ylab("log10(quantity [cells])") +
    ylab(expression(atop(log[10](dens), (cells~L^{~1})))) +
    xlab(NULL) +
    scale_colour_manual(values = colfunc_SB(num_SB_strains)) +
    guides(colour = guide_legend(ncol = 3)) +
    theme_bw() +
    theme(legend.position="none",
          plot.title = element_text(size = 10)) +
    ggtitle("(b) Sulfate reducing bacteria")+
    facet_grid(. ~ direction) +
    theme(strip.text = element_blank())
  
  p3 <- temp %>%
    dplyr::filter(functional_group == "PB")  %>%
    mutate(species = factor(species, levels = unique(species))) %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_segment(data = stab_measures,
                 aes(x = aO_flip, xend = aO_flip,
                     y = c(7, 2), yend = c(2, 7)),
                 col = "#bbbbbbff", lwd = arrow_lwd,
                 arrow = arrow(length=unit(0.30,"cm"),
                               ends="first", type = "closed")) +
    geom_path(lwd = line_width) +
    geom_point(size = point_size) +
    ##ylab("log10(quantity [cells])") +
    ylab(expression(atop(log[10](dens), (cells~L^{~1})))) +
    xlab(NULL) +
    scale_colour_manual(values = colfunc_PB(num_PB_strains)) +
    guides(colour = guide_legend(ncol = 3)) +
    theme_bw() +
    theme(legend.position="none",
          plot.title = element_text(size = 10)) +
    ggtitle("(c) Phototrophic sulfur bacteria")+
    facet_grid(. ~ direction) +
    theme(strip.text = element_blank())
  
  p4 <- temp %>%
    dplyr::filter(var_type == "Substrate", species == "O") %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_segment(data = stab_measures,
                 aes(x = aO_flip, xend = aO_flip,
                     y = c(0.5, 2), yend = c(2, 0.5)),
                 col = "#bbbbbbff", lwd = arrow_lwd,
                 arrow = arrow(length=unit(0.30,"cm"),
                               ends="first", type = "closed")) +
    geom_path(lwd = line_width) +
    geom_point(size = point_size) +
    ##ylab("log10(quantity [cells])") +
    ylab(expression(atop(log[10](conc), (mu~M)))) +
    xlab(NULL) +
    theme_bw() +
    theme(legend.position="none",
          plot.title = element_text(size = 10)) +
    ggtitle("(d) Oxygen concentration")+
    facet_grid(. ~ direction) +
    theme(strip.text = element_blank())
  
  p5 <- temp %>%
    dplyr::filter(var_type == "Substrate", species == "SR") %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_segment(data = stab_measures,
                 aes(x = aO_flip, xend = aO_flip,
                     y = c(2.5, 1.5), yend = c(1.5, 2.5)),
                 col = "#bbbbbbff", lwd = arrow_lwd,
                 arrow = arrow(length=unit(0.30,"cm"),
                               ends="first", type = "closed")) +
    geom_path(lwd = line_width, col = 6) +
    geom_point(size = point_size, col = 6) +
    ##ylab("log10(quantity [cells])") +
    ylab(expression(atop(log[10](conc), (mu~M)))) +
    xlab(expression(log[10](oxygen~diffusivity)~h^{-1})) +
    theme_bw() +
    theme(legend.position="none",
          plot.title = element_text(size = 10)) +
    ggtitle("(e) Sulfide concentration") +
    facet_grid(. ~ direction) +
    theme(strip.text = element_blank())
  
  patchwork <- p1 / p2 / p3 / p4 / p5
  patchwork <- patchwork + plot_annotation(tag_levels = 'a')
  patchwork
}



fig_state_vs_o2diff_sidebyside_alternative <- function(ss_result){
  
  is_repl <- "ss_by_a_N_result" %in% names(ss_result)
  
  if (is_repl) {
    ss_result$ssfind_result <- ss_result$ss_res
  }
  
  temp1 <- ss_result %>%
    mutate(ssfind_result = list(get_total_bio(ssfind_result)))
  
  if (is_repl){
    temp1 <- microxanox:::get_stability_measures.replication_ssfind_result(
      temp1$ssfind_result[[1]],
      threshold_diff_log10scale = 3
    )
  } else {
    temp1 <- microxanox:::get_stability_measures.temporal_ssfind_result(
      temp1$ssfind_result[[1]],
      threshold_diff_log10scale = 3
    )
  }
  
  stab_measures <- temp1 %>% 
    filter(Species == "SB_tot") %>%
    select(down=hyst_min_log, up=hyst_max_log) %>%
    pivot_longer(names_to = "direction", values_to = "aO_flip", 1:2) %>%
    mutate(y_start = c(2.5, 7.5),
           y_end = c(7.5, 2.5))
  
  rm(temp1)
  
  if (is_repl) {
    gathcols <- 1:(31)
    ss_result$ssfind_result[[1]] <- ss_result$ssfind_result[[1]] %>%
      mutate(a_O = log10(a_O),
             direction = ifelse(initial_N_CB == 1, "up", "down"))
  } else {
    gathcols <- 2:(ncol(ss_result$ssfind_result[[1]]) - 2)
  }
  
  temp <- ss_result$ssfind_result[[1]] %>%
    mutate(a = 10^a_O) %>%
    gather(species, quantity, gathcols) %>%
    mutate(
      var_type = ifelse(grepl("B_", species), "Organism", "Substrate"),
      functional_group = case_when(
        str_detect(species, "CB_") ~ "CB",
        str_detect(species, "SB_") ~ "SB",
        str_detect(species, "PB_") ~ "PB"),
      log10_quantity = log10(quantity + 1),
      species = factor(species, levels = unique(species)),
      functional_group2 = case_when(
        functional_group == "CB" ~ "Cyanobacteria",
        functional_group == "SB" ~ "Sulfate-reducing\nbacteria",
        functional_group == "PB" ~ "Phototrophic\nsulfur bacteria"),
      substrate2 = case_when(
        var_type == "Substrate" & species == "SR" ~ "Sulfide\nconcentration",
        var_type == "Substrate" & species == "O" ~ "Oxygen\nconcentration")
    )
  
  num_strains <- temp %>%
    group_by(functional_group) %>%
    summarise(num = length(unique(species))) %>%
    na.omit()
  
  num_CB_strains <- num_strains$num[num_strains$functional_group == "CB"]
  num_SB_strains <- num_strains$num[num_strains$functional_group == "SB"]
  num_PB_strains <- num_strains$num[num_strains$functional_group == "PB"]
  
  
  line_width <- 0.2
  point_size <- 0.6
  arrow_lwd <- 1

  p1CB <-
    temp %>%
    dplyr::filter(functional_group == "CB")  %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_segment(data = stab_measures,
                 aes(x = aO_flip, xend = aO_flip,
                     y = y_start, yend = y_end),
                 col = "#bbbbbbff", lwd = arrow_lwd,
                 arrow = arrow(length=unit(0.30,"cm"),
                               ends="first", type = "closed")) +
    geom_path(lwd = line_width) +
    geom_point(size = point_size) +
    scale_colour_manual(values = colfunc_CB(num_CB_strains)) +
    guides(colour = guide_legend(ncol = 3)) +
    labs(tag="a")+
    theme_bw() +
    theme(legend.position="none",
          axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          strip.background = element_rect(fill = "white"),
          strip.background.x = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_text(size=12, margin = margin(l=11, r=11)),
          plot.margin = margin(t=-1, b=-1)) +
    facet_grid(functional_group2 ~ direction)
  
  p2SB <- temp %>%
    dplyr::filter(functional_group == "SB")  %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_segment(data = stab_measures,
                 aes(x = aO_flip, xend = aO_flip,
                     y = c(7.5, 2), yend = c(2, 7.5)),
                 col = "#bbbbbbff", lwd = arrow_lwd,
                 arrow = arrow(length=unit(0.30,"cm"),
                               ends="first", type = "closed")) +
    geom_path(lwd = line_width) +
    geom_point(size = point_size) +
    scale_colour_manual(values = colfunc_SB(num_SB_strains)) +
    guides(colour = guide_legend(ncol = 3)) +
    labs(tag="b")+
    theme_bw() +
    theme(legend.position="none",
          axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          strip.background = element_rect(fill = "white"),
          strip.background.x = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_text(size=12),
          plot.margin = margin(t=-1, b=-1)) +
    facet_grid(functional_group2 ~ direction)
  
  
  p3PB <- temp %>%
    dplyr::filter(functional_group == "PB")  %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_segment(data = stab_measures,
                 aes(x = aO_flip, xend = aO_flip,
                     y = c(7, 2), yend = c(2, 7)),
                 col = "#bbbbbbff", lwd = arrow_lwd,
                 arrow = arrow(length=unit(0.30,"cm"),
                               ends="first", type = "closed")) +
    geom_path(lwd = line_width) +
    geom_point(size = point_size) +
    scale_colour_manual(values = colfunc_PB(num_PB_strains)) +
    guides(colour = guide_legend(ncol = 3)) +
    labs(tag="c") +
    theme_bw() +
    theme(legend.position="none",
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          strip.background = element_rect(fill = "white"),
          strip.background.x = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_text(size=12),
          plot.margin = margin(t=-1, b=-1)) +
    facet_grid(functional_group2 ~ direction)
  
  
  
  p4O <- temp %>%
    dplyr::filter(var_type == "Substrate", species == "O") %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_segment(data = stab_measures,
                 aes(x = aO_flip, xend = aO_flip,
                     y = c(0.5, 2), yend = c(2, 0.5)),
                 col = "#bbbbbbff", lwd = arrow_lwd,
                 arrow = arrow(length=unit(0.30,"cm"),
                               ends="first", type = "closed")) +
    geom_path(lwd = line_width) +
    geom_point(size = point_size) +
    labs(tag="d")+
    theme_bw() +
    theme(legend.position="none",
          axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          strip.background = element_rect(fill = "white"),
          strip.background.x = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_text(size=12),
          plot.margin = margin(t=-1, b=-1)) +
    facet_grid(substrate2 ~ direction)
  
  
  
  p5SR <- temp %>%
    dplyr::filter(var_type == "Substrate", species == "SR") %>%
    ggplot(aes(x = log10(a), y = log10_quantity, col = species)) +
    geom_segment(data = stab_measures,
                 aes(x = aO_flip, xend = aO_flip,
                     y = c(2.5, 1.5), yend = c(1.5, 2.5)),
                 col = "#bbbbbbff", lwd = arrow_lwd,
                 arrow = arrow(length=unit(0.30,"cm"),
                               ends="first", type = "closed")) +
    geom_path(lwd = line_width, col=6) +
    geom_point(size = point_size, col=6) +
    labs(tag="e", x=expression('log'[10]*"(oxygen diffusivity) (h"^{-1}*")"))+
    theme_bw() +
    theme(legend.position="none",
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=13),
          strip.background = element_rect(fill = "white"),
          strip.background.x = element_blank(),
          strip.text.x = element_blank(),
          strip.text.y = element_text(size=12),
          plot.margin = margin(t=-1, b=-1)) +
    facet_grid(substrate2 ~ direction)
  
  
  
  ylab1 <- ggplot(data.frame(x = 1, y = 4.1)) +
    geom_text(aes(x, y),label= expression('log'[10]*"(density) (cells L"^{-1}*")"),
              angle = 90, size=4.5) + 
    theme_void() +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(t=-1, b=-1, l=-1))
  
  
  ylab2 <- ggplot(data.frame(x = 1, y = 4.1)) +
    geom_text(aes(x, y),label= expression('log'[10]*"(concentration) ("*mu*"M)"),
              angle = 90, size=4.5) + 
    theme_void() +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(t=-1, b=-1, l=-1))
  
  
  direction.facet <- ggplot(data.frame(x = 1, y = 1,direction = c("Decreasing oxygen diffusivity","Increasing oxygen diffusivity"))) +
    geom_text(aes(x, y, label = direction), size=4.5) +
    facet_wrap(~direction)+
    theme_bw()+
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.margin = margin(t=-2, b=-1))
  
  
  patchwork <- plot_spacer() + direction.facet +
    ylab1 + (p1CB/p2SB/p3PB) +
    ylab2 + (p4O/p5SR) +
    plot_layout(ncol = 2, heights = c(1,15,10), widths = c(1,30))
  
  return(patchwork)
}








fig_resilience_vs_div <- function(all_stab, which_strain, figure_title) {
  
  # CB_vars <- unique(all_stab$CB_var_gmax_s)
  # SB_vars <- unique(all_stab$SB_var_gmax_s)
  # PB_vars <- unique(all_stab$PB_var_gmax_s)
  # 
  # CB_all_stab <- all_stab %>%
  #   filter(SB_var_gmax_s == 0,
  #          PB_var_gmax_s == 0) %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # SB_all_stab <- all_stab %>%
  #   filter(CB_var_gmax_s == 0,
  #          PB_var_gmax_s == 0)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "SB",
  #          var_gmax = SB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # PB_all_stab <- all_stab %>%
  #   filter(CB_var_gmax_s == 0,
  #          SB_var_gmax_s == 0) %>%
  #   ungroup()  %>%
  #   mutate(var_treat = "PB",
  #          var_gmax = PB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # 
  # for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
  #                    SB_var_gmax_s = sort(SB_vars))
  # CBSB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB-SB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax)) %>%
  #   filter(PB_var_gmax_s == 0)
  # 
  # for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
  #                    PB_var_gmax_s = sort(PB_vars))
  # CBPB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB-PB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax)) %>%
  #   filter(SB_var_gmax_s == 0)
  # 
  # for_join <- tibble(SB_var_gmax_s = sort(SB_vars),
  #                    PB_var_gmax_s = sort(PB_vars))
  # SBPB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "SB-PB",
  #          var_gmax = SB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax)) %>%
  #   filter(CB_var_gmax_s == 0)
  # 
  # 
  # 
  # for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
  #                    SB_var_gmax_s = sort(SB_vars),
  #                    PB_var_gmax_s = sort(PB_vars))
  # CBSBPB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB-SB-PB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # 
  # all_stab_results <- CB_all_stab %>%
  #   bind_rows(SB_all_stab) %>%
  #   bind_rows(PB_all_stab) %>%
  #   bind_rows(CBSB_all_stab) %>%
  #   bind_rows(CBPB_all_stab) %>%
  #   bind_rows(SBPB_all_stab) %>%
  #   bind_rows(CBSBPB_all_stab)
  # 
  # all_stab_results <- all_stab_results %>%
  #   mutate(var_treat = forcats::fct_relevel(var_treat, levels = c("CB",
  #                                                                 "SB",
  #                                                                 "PB",
  #                                                                 "CB-SB",
  #                                                                 "CB-PB",
  #                                                                 "SB-PB",
  #                                                                 "CB-SB-PB")))
  # 
  #all_stab_results_small <- select(all_stab_results, -7:-12)
  
  #saveRDS(all_stab_results_small, here("experiments/0_ss_finding/temporal_method/data/all_stab_results_small.RDS"))
  
  
  resilience <- all_stab %>%
    #filter(var_treat == "CB") %>%
    filter(num_strains == which_strain) %>%
    filter(Species == "SB_tot") %>%
    select(1:7, 16, 17, 20:24) %>%
    group_by(var_treat) %>%
    mutate(rel_trans_pos_ao = hyst_max_log-hyst_max_log[stand_var==0],
           rel_trans_pos_oa = -hyst_min_log+hyst_min_log[stand_var==0]) %>%
    select(-8:-9) %>%
    pivot_longer(names_to = "which_transition",
                 values_to = "rel_log_trans_pos",
                 13:14)
    
  resilience$label <- NA
  resilience$label[resilience$var_treat == "CB"] <- "b"
  resilience$label[resilience$var_treat == "SB"] <- "d"
  resilience$label[resilience$var_treat == "PB"] <- "f"
  resilience$label[resilience$var_treat == "CB-SB"] <- "h"
  resilience$label[resilience$var_treat == "CB-PB"] <- "k"
  resilience$label[resilience$var_treat == "SB-PB"] <- "m"
  resilience$label[resilience$var_treat == "CB-SB-PB"] <- "o"

  ## remove horizontal unless all 0 ----
  
  res <- split(resilience, resilience$var_treat)
  res <- lapply(
    res,
    function(x){
      x <- x %>% arrange(x$stand_var)
      ao <- which(x$which_transition == "rel_trans_pos_ao")
      oa <- which(x$which_transition == "rel_trans_pos_oa")
      
      for (i in (length(ao):2)){
        if (x[ao[i],]$rel_log_trans_pos == 0){
          break()
        }
        if (x[ao[i],]$rel_log_trans_pos == x[ao[i-1],]$rel_log_trans_pos) {
          x[ao[i],]$rel_log_trans_pos <- NA
        } else {
          break
        }      
      }
      
      for (i in (length(oa):2)){
        if (x[oa[i],]$rel_log_trans_pos == 0){
          break()
        }
        if (x[oa[i],]$rel_log_trans_pos == x[oa[i-1],]$rel_log_trans_pos) {
          x[oa[i],]$rel_log_trans_pos <- NA
        } else {
          break
        }      
      }
      
      return(x)
    }
  )
  
  resilience <- do.call(rbind, res)
  

  ## plot ----
  
  p1 <- resilience %>%
    ggplot(aes(x = stand_var, y = rel_log_trans_pos, col = which_transition)) +
    geom_line(show.legend = FALSE) +
    facet_grid(var_treat ~ ., scales = "fixed") +
    theme_bw() +
    theme(legend.position="top",
      panel.background = element_blank(),
      #strip.text.x = element_blank()
      ) +
    ylab(expression(atop(Effect~size~of~trait~variation~on~resilience, (log[10](oxygen~diffusivity)~(h^{-1}))))) +
    xlab("Standardised amount of trait variation") +
    geom_text(aes(x = 0, y = 2.85, label = label), label.size = 0, colour = "black") +
    scale_color_manual(values = c("#38ACC4", "#C43926")) +
    ylim(-0.9, 3.1)
  
  p1
  
}

fig_div_vs_o2diff_1strain_7row <- function(all_stab, which_strain, figure_title) {
  
  # CB_vars <- unique(all_stab$CB_var_gmax_s)
  # SB_vars <- unique(all_stab$SB_var_gmax_s)
  # PB_vars <- unique(all_stab$PB_var_gmax_s)
  # 
  # CB_all_stab <- all_stab %>%
  #   filter(SB_var_gmax_s == 0,
  #          PB_var_gmax_s == 0) %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # SB_all_stab <- all_stab %>%
  #   filter(CB_var_gmax_s == 0,
  #          PB_var_gmax_s == 0)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "SB",
  #          var_gmax = SB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # PB_all_stab <- all_stab %>%
  #   filter(CB_var_gmax_s == 0,
  #          SB_var_gmax_s == 0) %>%
  #   ungroup()  %>%
  #   mutate(var_treat = "PB",
  #          var_gmax = PB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # 
  # for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
  #                    SB_var_gmax_s = sort(SB_vars))
  # CBSB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB-SB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax)) %>%
  #   filter(PB_var_gmax_s == 0)
  # 
  # for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
  #                    PB_var_gmax_s = sort(PB_vars))
  # CBPB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB-PB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax)) %>%
  #   filter(SB_var_gmax_s == 0)
  # 
  # for_join <- tibble(SB_var_gmax_s = sort(SB_vars),
  #                    PB_var_gmax_s = sort(PB_vars))
  # SBPB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "SB-PB",
  #          var_gmax = SB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax)) %>%
  #   filter(CB_var_gmax_s == 0)
  # 
  # 
  # 
  # for_join <- tibble(CB_var_gmax_s = sort(CB_vars),
  #                    SB_var_gmax_s = sort(SB_vars),
  #                    PB_var_gmax_s = sort(PB_vars))
  # CBSBPB_all_stab <- all_stab %>%
  #   right_join(for_join)  %>%
  #   ungroup() %>%
  #   mutate(var_treat = "CB-SB-PB",
  #          var_gmax = CB_var_gmax_s,
  #          stand_var = var_gmax / max(var_gmax))
  # 
  # all_stab_results <- CB_all_stab %>%
  #   bind_rows(SB_all_stab) %>%
  #   bind_rows(PB_all_stab) %>%
  #   bind_rows(CBSB_all_stab) %>%
  #   bind_rows(CBPB_all_stab) %>%
  #   bind_rows(SBPB_all_stab) %>%
  #   bind_rows(CBSBPB_all_stab)
  # 
  # all_stab_results <- all_stab_results %>%
  #   mutate(var_treat = forcats::fct_relevel(var_treat, levels = c("CB",
  #                                                                 "SB",
  #                                                                 "PB",
  #                                                                 "CB-SB",
  #                                                                 "CB-PB",
  #                                                                 "SB-PB",
  #                                                                 "CB-SB-PB")))
  # 
  # all_stab_results_small <- select(all_stab_results, -7:-12)
  # 
  #saveRDS(all_stab_results_small, here("experiments/0_ss_finding/temporal_method/data/all_stab_results_small.RDS"))
  
  
  all_stab$label <- NA
  all_stab$label[all_stab$var_treat == "CB"] <- "a"
  all_stab$label[all_stab$var_treat == "SB"] <- "c"
  all_stab$label[all_stab$var_treat == "PB"] <- "e"
  all_stab$label[all_stab$var_treat == "CB-SB"] <- "g"
  all_stab$label[all_stab$var_treat == "CB-PB"] <- "i"
  all_stab$label[all_stab$var_treat == "SB-PB"] <- "l"
  all_stab$label[all_stab$var_treat == "CB-SB-PB"] <- "n"
  p1 <- all_stab %>%
    #filter(var_treat == "CB") %>%
    filter(num_strains == which_strain) %>%
    filter(Species == "SB_tot") %>%
    ggplot(aes(x = stand_var
               #col = as.factor(num_strains))
    )) +
    geom_line(aes(y = hyst_min_log), lwd = 0.5, alpha = 0.8, col="#C43926") +
    geom_line(aes(y = hyst_max_log), lwd = 0.5, alpha = 0.8, col="#38ACC4") +
    geom_ribbon(aes(ymin = hyst_min_log, ymax = hyst_max_log),
                fill = "green", alpha = 0.1) +
    facet_grid(var_treat ~ ., scales = "fixed") +
    xlab("Standardised amount of trait variation") +
    ylab(expression(log[10](oxygen~diffusivity)~(h^{-1}))) +
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
    geom_text(aes(x = 0.95, y = -8.1, label = label), colour = "black") +
    ggtitle(figure_title)

  p1
  
}

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
                  hyst_max_log_CB_SBPB = calculation_fct(`hyst_max_log_SB-PB`,hyst_max_log_CB, Method=Method),
                  # new (1) 20220303
                  hyst_min_log_CBSB_PB = calculation_fct(`hyst_min_log_CB-SB`,hyst_min_log_PB, Method=Method),
                  hyst_max_log_CBSB_PB = calculation_fct(`hyst_max_log_CB-SB`,hyst_max_log_PB, Method=Method),
                  # new (2) 20220303
                  hyst_min_log_CBPB_SB = calculation_fct(`hyst_min_log_CB-PB`,hyst_min_log_SB, Method=Method),
                  hyst_max_log_CBPB_SB = calculation_fct(`hyst_max_log_CB-PB`,hyst_max_log_SB, Method=Method))
  
  agg_stab_strain9 <- all_stab_results9 %>%
    dplyr::filter(num_strains==9, Species == "SB_tot") 
  
  ### CB + SB
  agg_stab_strain9_CB_SB <- agg_stab_strain9 %>%
    dplyr::filter(var_treat=="CB-SB")
  
  agg_stab_strain9_CB_SB$method <- factor("CB+SB")
  agg_stab_strain9_CB_SB$hyst_min_log <- agg_stab_res_single_groups_wide$hyst_min_log_CB_SB
  agg_stab_strain9_CB_SB$hyst_max_log <- agg_stab_res_single_groups_wide$hyst_max_log_CB_SB
  
  ### CB + PB
  agg_stab_strain9_CB_PB <- agg_stab_strain9 %>%
    dplyr::filter(var_treat=="CB-PB")
  
  agg_stab_strain9_CB_PB$method <- factor("CB+PB")
  agg_stab_strain9_CB_PB$hyst_min_log <- agg_stab_res_single_groups_wide$hyst_min_log_CB_PB
  agg_stab_strain9_CB_PB$hyst_max_log <- agg_stab_res_single_groups_wide$hyst_max_log_CB_PB
  
  ### SB + PB
  agg_stab_strain9_SB_PB <- agg_stab_strain9 %>%
    dplyr::filter(var_treat=="SB-PB")
  
  agg_stab_strain9_SB_PB$method <- factor("SB+PB")
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
  
  agg_stab_strain9_CB_SBPB$method <- factor("CB+SBPB")
  agg_stab_strain9_CB_SBPB$hyst_min_log <- agg_stab_res_single_groups_wide$hyst_min_log_CB_SBPB
  agg_stab_strain9_CB_SBPB$hyst_max_log <- agg_stab_res_single_groups_wide$hyst_max_log_CB_SBPB
  
  ### CBSB + PB
  agg_stab_strain9_CBSB_PB <- agg_stab_strain9 %>%
    dplyr::filter(var_treat=="CB-SB-PB")
  
  agg_stab_strain9_CBSB_PB$method <- factor("PB+CBSB")
  agg_stab_strain9_CBSB_PB$hyst_min_log <- agg_stab_res_single_groups_wide$hyst_min_log_CBSB_PB
  agg_stab_strain9_CBSB_PB$hyst_max_log <- agg_stab_res_single_groups_wide$hyst_max_log_CBSB_PB
  
  ### CBPB + SB
  agg_stab_strain9_CBPB_SB <- agg_stab_strain9 %>%
    dplyr::filter(var_treat=="CB-SB-PB")
  
  agg_stab_strain9_CBPB_SB$method <- factor("SB+CBPB")
  agg_stab_strain9_CBPB_SB$hyst_min_log <- agg_stab_res_single_groups_wide$hyst_min_log_CBPB_SB
  agg_stab_strain9_CBPB_SB$hyst_max_log <- agg_stab_res_single_groups_wide$hyst_max_log_CBPB_SB
  
  ### merge 
  agg_stab_strain9 <- rbind(agg_stab_strain9, agg_stab_strain9_CB_SB, agg_stab_strain9_CB_PB, agg_stab_strain9_SB_PB,
                            # agg_stab_strain9_CB_SB_PB,
                            agg_stab_strain9_CB_SBPB, agg_stab_strain9_CBSB_PB, agg_stab_strain9_CBPB_SB)
  return(agg_stab_strain9)
}
