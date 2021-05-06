
add_strain_var <- function(x,
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


plot_ss_result1 <- function(ss_result,
                            CB_var_gmax, CB_var_h,
                            SB_var_gmax, SB_var_h,
                            PB_var_gmax, PB_var_h,
                            filename_prefix,
                            save_image_file = TRUE) {
  
  colfunc_CB <- colorRampPalette(c("#B5FFC9", "#024F17"))
  colfunc_SB <- colorRampPalette(c("#FCBEB3", "#7D1402"))
  colfunc_PB <- colorRampPalette(c("#F9AEFC", "#6E0172"))
  
  temp <- ss_result %>%
    select(-initial_N_CB, -a_O) %>%
    mutate(a = 10^a) %>%
    gather(species, quantity, 2:ncol(.)) %>% 
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
    ggplot(aes(x=log10(a), y=log10_quantity, col=species)) +
    geom_path() +
    ylab('log10(quantity [cells])') +
    xlab('a') +
    scale_colour_manual(values = colfunc_CB(num_CB_strains)) +
    guides(colour = guide_legend(ncol = 3))
  p1
  
  p2 <- temp %>%
    dplyr::filter(functional_group == "SB") %>%
    ggplot(aes(x=log10(a), y=log10_quantity, col=species)) +
    geom_path() +
    ylab('log10(quantity [cells])') +
    xlab('a') +
    scale_colour_manual(values = colfunc_SB(num_SB_strains)) +
    guides(colour = guide_legend(ncol = 3))
  
  p3 <- temp %>%
    dplyr::filter(functional_group == "PB") %>%
    ggplot(aes(x=log10(a), y=log10_quantity, col=species)) +
    geom_path() +
    ylab('log10(quantity [cells])') +
    xlab('a') +
    scale_colour_manual(values = colfunc_PB(num_PB_strains)) +
    guides(colour = guide_legend(ncol = 3))
  
  p4 <- temp %>%
    dplyr::filter(var_type == "Substrate") %>%
    ggplot(aes(x=log10(a), y=log10_quantity, col=species)) +
    geom_path() +
    ylab('log10(quantity [cells])') +
    xlab('a') 
  
  patchwork <- p1 / p2 / p3 / p4
  
  if(save_image_file) {
    
    patchwork + plot_annotation(
      title = paste0("CB_gmax_var = ", CB_var_gmax,
                     " CB_h_var =", CB_var_h,
                     "\nSB_gmax_var = ", SB_var_gmax,
                     " SB_h_var =", SB_var_h,
                     "\nPB_gmax_var = ", PB_var_gmax,
                     " PB_h_var =", PB_var_h))
    
    ggsave(paste0(filename_prefix,
                  "-CB_", round(CB_var_gmax,3), "_", round(CB_var_h,3),
                  "-SB_", round(SB_var_gmax,3), "_", round(SB_var_h,3),
                  "-PB_", round(PB_var_gmax,3), "_", round(PB_var_h,3),
                  ".pdf"),
           width = 10)
  }
  
  if(!save_image_file)
    return(
      
      patchwork + plot_annotation(
        title = paste0("CB_gmax_var = ", CB_var_gmax,
                       " CB_h_var =", CB_var_h,
                       "\nSB_gmax_var = ", SB_var_gmax,
                       " SB_h_var =", SB_var_h,
                       "\nPB_gmax_var = ", PB_var_gmax,
                       " PB_h_var =", PB_var_h))
      
      
    )
  
}

