add_strain_var <- function(x,
                               CB_var_gmax=0, CB_var_h=0,
                               SB_var_gmax=0, SB_var_h=0,
                               PB_var_gmax=0, PB_var_h=0) {
  
  new_x <- x
  new_x$CB$g_max_CB <- x$CB$g_max_CB * 2^(seq(-CB_var_gmax, CB_var_gmax, length=length(new_x$CB$g_max_CB)))
  new_x$CB$h_SR_CB <- x$CB$h_SR_CB * 2^(seq(-CB_var_h, CB_var_h, length=length(new_x$CB$h_SR_CB)))
  new_x$SB$g_max_SB <- x$SB$g_max_SB * 2^(seq(-SB_var_gmax, SB_var_gmax, length=length(new_x$SB$g_max_SB)))
  new_x$SB$h_O_SB <- x$SB$h_O_SB * 2^(seq(-SB_var_h, SB_var_h, length=length(new_x$SB$h_O_SB)))
  new_x$PB$g_max_PB <- x$PB$g_max_PB * 2^(seq(-PB_var_gmax, PB_var_gmax, length=length(new_x$PB$g_max_PB)))
  new_x$PB$h_O_PB <- x$PB$h_O_PB * 2^(seq(-PB_var_h, PB_var_h, length=length(new_x$PB$h_O_PB)))
  new_x
}


plot_and_save <- function(CB_var_gmax, CB_var_h,
                          SB_var_gmax, SB_var_h,
                          PB_var_gmax, PB_var_h,
                          sim_res,
                          file_path_and_prefix) {
  plot_dynamics(sim_res)
  ggsave(paste0(file_path_and_prefix,
                "-CB_", round(CB_var_gmax,3), "_", round(CB_var_h,3),
                "-SB_", round(SB_var_gmax,3), "_", round(SB_var_h,3),
                "-PB_", round(PB_var_gmax,3), "_", round(PB_var_h,3),
                ".pdf"),
         width = 10)
  NULL
}


run_ss_var_experiment <- function() {
  
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
  #var_expt$pars[[1]]$CB
  #var_expt$pars[[2]]$CB
  #var_expt$pars[[1]]$PB
  #var_expt$pars[[2]]$PB
  
  #var_expt <- var_expt[2,]
  
  # ggplot(var_expt$pars[[20]]$CB,
  #        aes(x = g_max_CB, y = h_SR_CB)) +
  #   geom_point()
  # m1 <- lm(h_SR_CB ~ g_max_CB, data = var_expt$pars[[20]]$CB)
  # par(mfrow = c(2,2))
  # plot(m1)
  
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
  var_expt
}




add_strain_var_old <- function(x,
                           CB_var_gmax=0, CB_var_h=0,
                           SB_var_gmax=0, SB_var_h=0,
                           PB_var_gmax=0, PB_var_h=0) {
  
  new_x <- x
  new_x$CB$g_max_CB <- x$CB$g_max_CB*(1+seq(-CB_var_gmax, CB_var_gmax, length=length(new_x$CB$g_max_CB)))
  new_x$CB$h_SR_CB <- x$CB$h_SR_CB*(1+seq(-CB_var_h, CB_var_h, length=length(new_x$CB$h_SR_CB)))
  new_x$SB$g_max_SB <- x$SB$g_max_SB*(1+seq(-SB_var_gmax, SB_var_gmax, length=length(new_x$SB$g_max_SB)))
  new_x$SB$h_O_SB <- x$SB$h_O_SB*(1+seq(-SB_var_h, SB_var_h, length=length(new_x$SB$h_O_SB)))
  new_x$PB$g_max_PB <- x$PB$g_max_PB*(1+seq(-PB_var_gmax, PB_var_gmax, length=length(new_x$PB$g_max_PB)))
  new_x$PB$h_O_PB <- x$PB$h_O_PB*(1+seq(-PB_var_h, PB_var_h, length=length(new_x$PB$h_O_PB)))
  new_x
}


plot_and_save <- function(CB_var_gmax, CB_var_h,
                          SB_var_gmax, SB_var_h,
                          PB_var_gmax, PB_var_h,
                          sim_res,
                          file_path_and_prefix) {
  plot_dynamics(sim_res)
  ggsave(paste0(file_path_and_prefix,
                "-CB_", round(CB_var_gmax,3), "_", round(CB_var_h,3),
                "-SB_", round(SB_var_gmax,3), "_", round(SB_var_h,3),
                "-PB_", round(PB_var_gmax,3), "_", round(PB_var_h,3),
                ".pdf"),
         width = 10)
  NULL
}


plot_ss_result1 <- function(ss_exp_result,
                            result_index,
                            filename_prefix,
                            save_image_file = TRUE) {
  
  colfunc_CB <- colorRampPalette(c("#B5FFC9", "#024F17"))
  colfunc_SB <- colorRampPalette(c("#FCBEB3", "#7D1402"))
  colfunc_PB <- colorRampPalette(c("#F9AEFC", "#6E0172"))
  
  ss_result <- ss_exp_result[result_index,]$ss_res[[1]]
  
  temp <- ss_result %>%
    mutate(a = 10^a) %>%
    #arrange(a) %>%
    #mutate(direction = rep(c("up", "down"), nrow(ss_result)/2)) %>%
    mutate(direction = ifelse(initial_N_CB == 1, "up", "down")) %>%
    #select(-initial_N_CB, -a_O) %>%
    gather(species, quantity, 1:(ncol(.)-6)) %>% 
    mutate(var_type=ifelse(grepl("B_", species), "Organism", "Substrate"),
           functional_group = case_when(str_detect(species, "CB_") ~ "CB",
                                        str_detect(species, "SB_") ~ "SB",
                                        str_detect(species, "PB_") ~ "PB"),
           log10_quantity=log10(quantity+1))
  
  
  num_strains <- temp %>%
    group_by(functional_group) %>%
    summarise(num = length(unique(species))) %>%
    na.omit()
  
  num_CB_strains <- num_strains$num[num_strains$functional_group=="CB"]
  num_SB_strains <- num_strains$num[num_strains$functional_group=="SB"]
  num_PB_strains <- num_strains$num[num_strains$functional_group=="PB"]
  
  p1 <- temp %>%
    dplyr::filter(functional_group == "CB") %>%
    ggplot(aes(x=log10(a), y=log10_quantity, col=species, linetype = direction)) +
    geom_path() +
    ylab('log10(quantity [cells])') +
    xlab('a') +
    scale_colour_manual(values = colfunc_CB(num_CB_strains)) +
    guides(colour = guide_legend(ncol = 3))
  #p1
  
  p2 <- temp %>%
    dplyr::filter(functional_group == "SB") %>%
    ggplot(aes(x=log10(a), y=log10_quantity, col=species, linetype = direction)) +
    geom_path() +
    ylab('log10(quantity [cells])') +
    xlab('a') +
    scale_colour_manual(values = colfunc_SB(num_SB_strains)) +
    guides(colour = guide_legend(ncol = 3))
  
  p3 <- temp %>%
    dplyr::filter(functional_group == "PB") %>%
    ggplot(aes(x=log10(a), y=log10_quantity, col=species, linetype = direction)) +
    geom_path() +
    ylab('log10(quantity [cells])') +
    xlab('a') +
    scale_colour_manual(values = colfunc_PB(num_PB_strains)) +
    guides(colour = guide_legend(ncol = 3))
  
  p4 <- temp %>%
    dplyr::filter(var_type == "Substrate") %>%
    ggplot(aes(x=log10(a), y=log10_quantity, col=species, linetype = direction)) +
    geom_path() +
    ylab('log10(quantity [cells])') +
    xlab('a') 
  
  patchwork <- p1 / p2 / p3 / p4 +
    plot_annotation(
    title = paste0("CB_gmax_var = ", as.numeric(ss_exp_result[result_index,"CB_var_gmax_s"]),
                   " CB_h_var =", as.numeric(ss_exp_result[result_index,"CB_var_h_s"]),
                   "\nSB_gmax_var = ", as.numeric(ss_exp_result[result_index,"SB_var_gmax_s"]),
                   " SB_h_var =", as.numeric(ss_exp_result[result_index,"SB_var_h_s"]),
                   "\nPB_gmax_var = ", as.numeric(ss_exp_result[result_index,"PB_var_gmax_s"]),
                   " PB_h_var =", as.numeric(ss_exp_result[result_index,"PB_var_h_s"])))
  
  if(save_image_file) {
    
    
    ggsave(paste0(filename_prefix,
                  "-CB_", round(CB_var_gmax,3), "_", round(CB_var_h,3),
                  "-SB_", round(SB_var_gmax,3), "_", round(SB_var_h,3),
                  "-PB_", round(PB_var_gmax,3), "_", round(PB_var_h,3),
                  ".pdf"),
           width = 10)
  }
  
  if(!save_image_file)
    return(patchwork)
      
  
}


plot_ss_result2 <- function(ss_result1,
                            ss_result2,
                            xlims) {
  
  species_colours <- c(CB = "#024F17", SB = "#7D1402", PB = "#6E0172")
  
  #colfunc_CB <- colorRampPalette(c("#B5FFC9", "#024F17"))
  #colfunc_SB <- colorRampPalette(c("#FCBEB3", "#7D1402"))
  #colfunc_PB <- colorRampPalette(c("#F9AEFC", "#6E0172"))
  
  temp1 <- ss_result1 %>%
    mutate(direction = ifelse(initial_N_CB == 1, "up", "down"),
           a = 10^a) %>%
    gather(species, quantity, 1:(ncol(.)-6)) %>% 
    mutate(var_type=ifelse(grepl("B_", species), "Organism", "Substrate"),
           functional_group = case_when(str_detect(species, "CB_") ~ "CB",
                                        str_detect(species, "SB_") ~ "SB",
                                        str_detect(species, "PB_") ~ "PB"),
           functional_group = ifelse(is.na(functional_group), species, functional_group)) %>%
    group_by(a, direction, var_type, functional_group) %>%
    summarise(total_quantity = sum(quantity, na.rm = TRUE)) %>%
    mutate(log10_total_quantity = log10(total_quantity+1))
  
  temp2 <- ss_result2 %>%
    mutate(direction = ifelse(initial_N_CB == 1, "up", "down"),
           a = 10^a) %>%
    gather(species, quantity, 1:(ncol(.)-6)) %>% 
    mutate(var_type=ifelse(grepl("B_", species), "Organism", "Substrate"),
           functional_group = case_when(str_detect(species, "CB_") ~ "CB",
                                        str_detect(species, "SB_") ~ "SB",
                                        str_detect(species, "PB_") ~ "PB"),
           functional_group = ifelse(is.na(functional_group), species, functional_group)) %>%
    group_by(a, direction, var_type, functional_group) %>%
    summarise(total_quantity = sum(quantity, na.rm = TRUE)) %>%
    mutate(log10_total_quantity = log10(total_quantity+1),
           log1 = log10(a))
  
  
  num_CB_strains <- 1
  num_SB_strains <- 1
  num_PB_strains <- 1
  
  
  plot_fg_oi <- function(temp1, temp2, fg_oi) {
  temp1 %>%
    dplyr::filter(functional_group == fg_oi) %>%
    arrange(a, direction) %>%
    ggplot(aes(x=log10(a), y=log10_total_quantity, group=direction)) +
    geom_path(col = species_colours[fg_oi]) +
    ylab('log10(quantity [cells])') +
    xlab('a')  +
    guides(colour = guide_legend(ncol = 3)) +
    geom_path(data = dplyr::filter(temp2, functional_group == fg_oi),
              linetype = "dashed", lwd=1.5,
              col = species_colours[fg_oi]) 
  }
  p_CB <- plot_fg_oi(temp1, temp2, "CB") + xlim(xlims[1], xlims[2])
  p_SB <- plot_fg_oi(temp1, temp2, "SB") + xlim(xlims[1], xlims[2])
  p_PB <- plot_fg_oi(temp1, temp2, "PB") + xlim(xlims[1], xlims[2])
  #p_CB 
  #p_SB
  #p_PB
  
  p_Substrate <- temp1 %>%
    dplyr::filter(var_type == "Substrate") %>%
    arrange(functional_group, a, direction) %>%
    ggplot(aes(x=log10(a), y=log10_total_quantity,
               col=functional_group, group = paste(direction, functional_group))) +
    geom_path() +
    ylab('log10(quantity [cells])') +
    xlab('a') +
    geom_path(data = dplyr::filter(temp2, var_type == "Substrate"),
              linetype = "dashed", lwd = 1.5) +
    xlim(xlims[1], xlims[2])
  #p4
  patchwork_graph <- p_CB / p_SB / p_PB / p_Substrate
  
  patchwork_graph 
  
}


display_diversity <- function() {
  
  colfunc_CB <- colorRampPalette(rev(c("#B5FFC9", "#024F17")))
  colfunc_SB <- colorRampPalette(rev(c("#FCBEB3", "#7D1402")))
  colfunc_PB <- colorRampPalette(rev(c("#F9AEFC", "#6E0172")))
  
  linewd <- 1.5
  ptsize <- 2
  
  CBtraits <- var_expt$pars[[num_div_treatment_levels]]$CB
  P <- 1 #10^seq(-5, 5, 0.1)
  SR <- 10^seq(0, 3, 0.1)
  gr_rateCB1 <- growth1(P, CBtraits$g_max_CB[1], CBtraits$k_CB_P[1]) * inhibition(SR, CBtraits$h_SR_CB[1])
  gr_rateCB9 <- growth1(P, CBtraits$g_max_CB[num_CB_strains], CBtraits$k_CB_P[num_CB_strains]) * inhibition(SR, CBtraits$h_SR_CB[num_CB_strains])
  
  CB_TO1 <- ggplot(CBtraits) +
    geom_point(aes(x = h_SR_CB, y = g_max_CB, col = strain_name), size = ptsize) +
    xlab("Tolerance\n(cyanobacteria to reduced sulphur)") +
    ylab("Maximum growth rate\n(cyanobacteria)") +
    scale_colour_manual(values = colfunc_CB(num_CB_strains)) +
    theme(legend.position="none")
  CB_TO2 <- ggplot() +
    geom_line(aes( x = log10(SR), y = gr_rateCB1), col = colfunc_CB(num_CB_strains)[1], size = linewd) +
    geom_line(aes( x = log10(SR), y = gr_rateCB9), col = colfunc_CB(num_CB_strains)[9], size = linewd) +
    xlab("Reduced sulphur concentration\nin the environment\n(log10 uM per litre)") +
    ylab("Realised growth rate\n(cyanobacteria)")
  
  
  
  SBtraits <- var_expt$pars[[num_div_treatment_levels]]$SB
  P <- 1 #10^seq(-5, 5, 0.1)
  SO <- 1
  O <- 10^seq(-2.5, 2.5, 0.1)
  gr_rateSB1 <- growth2(P, SO, SBtraits$g_max_SB[1], SBtraits$k_SB_P[1], SBtraits$k_SB_SO[1]) *
    inhibition(O, SBtraits$h_O_SB[1])
  gr_rateSB9 <- growth2(P, SO, SBtraits$g_max_SB[9], SBtraits$k_SB_P[9], SBtraits$k_SB_SO[9]) *
    inhibition(O, SBtraits$h_O_SB[9])
  
  SB_TO1 <- ggplot(SBtraits) +
    geom_point(aes(x = h_O_SB, y = g_max_SB, col = strain_name), size = ptsize)+
    xlab("Tolerance\n(sulphate reducing bacteria to oxygen)") +
    ylab("Maximum growth rate\n(sulphate reducing bacteria)") +
    scale_colour_manual(values = colfunc_SB(num_SB_strains))+
    theme(legend.position="none")
  SB_TO2 <- ggplot() +
    geom_line(aes( x = log10(O), y = gr_rateSB1), col = colfunc_SB(num_SB_strains)[1], size = linewd) +
    geom_line(aes( x = log10(O), y = gr_rateSB9), col = colfunc_SB(num_SB_strains)[9], size = linewd)+
    xlab("Oxygen concentration\nin the environment\n(log10 uM per litre)") +
    ylab("Realised growth rate\n(sulphate reducing bacteria)")
  
  PBtraits <- var_expt$pars[[num_div_treatment_levels]]$PB
  P <- 1 #10^seq(-5, 5, 0.1)
  SO <- 1
  O <- 10^seq(-2.5, 2.5, 0.1)
  gr_ratePB1 <- growth2(P, SO, PBtraits$g_max_PB[1], PBtraits$k_PB_P[1], PBtraits$k_PB_SR[1]) *
    inhibition(O, PBtraits$h_O_PB[1])
  gr_ratePB9 <- growth2(P, SO, PBtraits$g_max_PB[9], PBtraits$k_PB_P[9], PBtraits$k_PB_SR[9]) *
    inhibition(O, PBtraits$h_O_PB[9])
  
  PB_TO1 <- ggplot(PBtraits) +
    geom_point(aes(x = h_O_PB, y = g_max_PB, col = strain_name), size = ptsize)+
    xlab("Tolerance\n(phototrophic sulphur bacteria to oxygen)") +
    ylab("Maximum growth rate\n(phototrophic sulphur bacteria") +
    scale_colour_manual(values = colfunc_PB(num_PB_strains))+
    theme(legend.position="none")
  PB_TO2 <- ggplot() +
    geom_line(aes( x = log10(O), y = gr_ratePB1), col = colfunc_PB(num_SB_strains)[1], size = linewd) +
    geom_line(aes( x = log10(O), y = gr_ratePB9), col = colfunc_PB(num_SB_strains)[9], size = linewd)+
    xlab("Oxygen concentration\nin the environment\n(log10 uM per litre)") +
    ylab("Realised growth rate\n(phototrophic sulphur bacteria)")
  
  (CB_TO1 + SB_TO1 + PB_TO1) / (CB_TO2 + SB_TO2 + PB_TO2)
  
}

