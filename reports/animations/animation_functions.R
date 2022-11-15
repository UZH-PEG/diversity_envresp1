## animation functions

anim_save_wrap <- function(plot_anim, this_fg, this_div, this_substrate = NA) {
  if(is.na(this_substrate))
  anim_save(here::here(paste0("reports/animations/gifs/",
                             this_fg ,"_", this_div, ".gif")),
            p1_ann,
            nframes = nframes,
            fps = fps,
            height = height, width = width, units = units,
            res = res)
  if(!is.na(this_substrate))
    anim_save(here::here(paste0("reports/animations/gifs/",
                                this_substrate ,"_", this_div, ".gif")),
              p1_ann,
              nframes = nframes,
              fps = fps,
              height = height, width = width, units = units,
              res = res)
  
}

plot_ann <- function(ss_result, func_group = "CB", this_substrate = NA) {
  
  dd <-  ss_result[1,]$ssfind_result[[1]] %>%
    mutate(time = ifelse(direction == "up", time,
                         time + max(time))) %>%
    mutate(a = 10^a_O) %>%
    gather(species, quantity, c(2:32)) %>%
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
  
  
  num_strains <- dd %>%
    group_by(functional_group) %>%
    summarise(num = length(unique(species))) %>%
    na.omit()
  
  
  num_CB_strains <- num_strains$num[num_strains$functional_group == "CB"]
  num_SB_strains <- num_strains$num[num_strains$functional_group == "SB"]
  num_PB_strains <- num_strains$num[num_strains$functional_group == "PB"]
  
  line_width <- 0.2
  point_size <- 0.6
  arrow_lwd <- 1
  
  ## here filter for the required functional group / substrate
  if(is.na(this_substrate)) {
  plot_dd <-  dd %>%
    dplyr::filter(functional_group == func_group) 
  }
  
  if(!is.na(this_substrate)) {
    plot_dd <-  dd %>%
      dplyr::filter(species == this_substrate) 
  }
  
  
  
  this_colfunc <- ifelse(func_group == "CB", colfunc_CB,
                         ifelse(func_group == "SB", colfunc_SB,
                                colfunc_PB))
  this_numstrains <- case_when(func_group == "CB" ~ num_CB_strains,
                               func_group == "SB" ~ num_SB_strains,
                               func_group == "PB" ~ num_PB_strains)
   
  if(!is.na(this_substrate))
    this_colfunc <- function(x) "blue"
  
  p1 <-
    plot_dd %>%
    ggplot(aes(x = log10(a), y = log10_quantity,
               col = species, group = species)) +
    #geom_segment(data = stab_measures,
    #             aes(x = aO_flip, xend = aO_flip,
    #                 y = y_start, yend = y_end),
    #             col = "#bbbbbbff", lwd = arrow_lwd,
    #             arrow = arrow(length=unit(0.30,"cm"),
    #                           ends="first", type = "closed")) +
    geom_path(lwd = line_width, alpha = 0.5, data = select(plot_dd, -time)) +
    geom_point(size = point_size) +
    scale_colour_manual(values = this_colfunc(this_numstrains)) +
    guides(colour = guide_legend(ncol = 3)) +
    ##labs(tag="a")+
    theme_bw() +
    scale_y_continuous(
      labels = label_number(accuracy = 0.1)) +
    theme(legend.position="none",
          axis.title = element_blank(),
          #axis.ticks.x = element_blank(),
          #axis.text.x = element_blank(),
          strip.background = element_rect(fill = "white"),
          strip.background.x = element_blank(),
          strip.text.x = element_blank(),
          #strip.text.y = element_text(size=12, margin = margin(l=11, r=11)),
          plot.margin = margin(t=0, b=0)) +
    facet_wrap(vars(functional_group2))
  p1
  
  p1_ann <- p1 +
    transition_reveal(time) +
    shadow_wake(wake_length = 0.1, size = 1,
                alpha = TRUE) 
  p1_ann
}

