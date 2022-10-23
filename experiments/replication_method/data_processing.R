

library(here)
library(tidyverse)



## Process the stability data to a smaller file ----

read_plus <- function(flnm) {
  print(flnm)
  readRDS(flnm) %>% 
    mutate(filename = flnm)
}

file_list <- list.files(path = here("data",
                                    microxanox_path,
                                    "replication_method",
                                    event_definition),
                        pattern = "stab_data", 
                        full.names = TRUE,
                        recursive = FALSE)

#file_list <- file_list[1:3]

tbl1 <-  file_list %>%
  map_df(~read_plus(.))

keep_tbl1 <- tbl1

#keep_tbl1$filename[1]

all_stab <- tbl1 %>%
  select(-data, -pars, -ss_by_a_N_result, -ss_res) %>%
  separate(filename , sep = "_", into = c(paste0("junk", 1:9), "simlength", paste0("junk", 10:12))) %>%
  select(-starts_with("junk")) %>%
  mutate(simlength = parse_number(simlength))



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


saveRDS(all_stab_results, here("data",
                                     microxanox_path,
                                     "replication_method",
                                     event_definition,
                                     "processed_data",
                                     "all_stab_data.RDS"))



