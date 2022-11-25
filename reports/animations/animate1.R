## Prelims ----
rm(list = ls())
library(tidyverse)
library(microxanox)
library(patchwork)
library(gganimate)
library(here)
library(scales)
source(here("r", "various_useful_functions.R"))
source(here("reports/animations/animation_functions.R"))
colfunc_CB <- colorRampPalette(c("#024F17", "#B5FFC9"))
colfunc_SB <- colorRampPalette(c("#7D1402", "#FCBEB3"))
colfunc_PB <- colorRampPalette(c("#6E0172", "#F9AEFC"))
zero <- 0
unity <- 1


microxanox_version_here <- "0.9.1"
microxanox_version_path <- paste0("microxanox_v", microxanox_version_here)
if (packageVersion("microxanox") < package_version(microxanox_version_here)) {
  stop(paste0("microxanox version needs to be at least ", microxanox_version_here))
}
print(paste0("Version of microxanox: ", microxanox_version_here))


## load data ----
ss_9s <- readRDS(here("data",
                      microxanox_version_path,
                      "temporal_method",
                      "event_definition_2",
                      "ss_data_9strains_waittime1e+06_event_definition_2.RDS"))

## set animation render parameters
nframes <- 500#100
fps <- 30
height <- 2
width <- 5
units <- "in"
res <- 600

## no diversity ----
ss_result <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.0) < 0.0001,
         abs(SB_var_gmax_s - 0.0) < 0.0001,
         abs(PB_var_gmax_s - 0.0) < 0.0001)
this_div <- "no_div"

this_func_group <- "CB"
p1_ann <- plot_ann(ss_result, func_group = this_func_group)
anim_save_wrap(p1_ann, this_func_group, this_div)

this_func_group <- "SB"
p1_ann <- plot_ann(ss_result, func_group = this_func_group)
anim_save_wrap(p1_ann, this_func_group, this_div)

this_func_group <- "PB"
p1_ann <- plot_ann(ss_result, func_group = this_func_group)
anim_save_wrap(p1_ann, this_func_group, this_div)

this_func_group <- NA
this_substrate <- "O"
p1_ann <- plot_ann(ss_result, func_group = this_func_group, this_substrate = this_substrate)
anim_save_wrap(p1_ann, this_func_group, this_div, this_substrate)

this_substrate <- "SO"
p1_ann <- plot_ann(ss_result, func_group = this_func_group, this_substrate = this_substrate)
anim_save_wrap(p1_ann, this_func_group, this_div, this_substrate)

this_substrate <- "SR"
p1_ann <- plot_ann(ss_result, func_group = this_func_group, this_substrate = this_substrate)
anim_save_wrap(p1_ann, this_func_group, this_div, this_substrate)

this_substrate <- "P"
p1_ann <- plot_ann(ss_result, func_group = this_func_group, this_substrate = this_substrate)
anim_save_wrap(p1_ann, this_func_group, this_div, this_substrate)


## medium diversity ----
ss_result <- ss_9s %>%
  filter(abs(CB_var_gmax_s - 0.028254848) < 0.0001,
         abs(SB_var_gmax_s - 0.084764545) < 0.0001,
         abs(PB_var_gmax_s - 0.084764545) < 0.0001)
this_div <- "medium_div"

this_func_group <- "CB"
p1_ann <- plot_ann(ss_result, func_group = this_func_group)
anim_save_wrap(p1_ann, this_func_group, this_div)

this_func_group <- "SB"
p1_ann <- plot_ann(ss_result, func_group = this_func_group)
anim_save_wrap(p1_ann, this_func_group, this_div)

this_func_group <- "PB"
p1_ann <- plot_ann(ss_result, func_group = this_func_group)
anim_save_wrap(p1_ann, this_func_group, this_div)

this_func_group <- NA
this_substrate <- "O"
p1_ann <- plot_ann(ss_result, func_group = this_func_group, this_substrate = this_substrate)
anim_save_wrap(p1_ann, this_func_group, this_div, this_substrate)

this_substrate <- "SO"
p1_ann <- plot_ann(ss_result, func_group = this_func_group, this_substrate = this_substrate)
anim_save_wrap(p1_ann, this_func_group, this_div, this_substrate)

this_substrate <- "SR"
p1_ann <- plot_ann(ss_result, func_group = this_func_group, this_substrate = this_substrate)
anim_save_wrap(p1_ann, this_func_group, this_div, this_substrate)

this_substrate <- "P"
p1_ann <- plot_ann(ss_result, func_group = this_func_group, this_substrate = this_substrate)
anim_save_wrap(p1_ann, this_func_group, this_div, this_substrate)

#files <- list.files(here::here(paste0("reports/animations")), full.names = T)
#file.remove(files[grep(".gif", files)])


