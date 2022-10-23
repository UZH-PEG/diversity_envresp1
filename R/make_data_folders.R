## make data folder structure

result <- NA

if(dir.exists(here("data",
                   microxanox_path)))
  result <- paste0("Data folders exist already for results using ", microxanox_path)

if(!dir.exists(here("data",
                   microxanox_path))) {
  result <- paste0("New folders created for results using ", microxanox_path)
  res1 <- dir.create(here("data",
                          microxanox_path,
                          "temporal_method",
                          "event_definition_1",
                          "processed_data"),
                     recursive = TRUE)
  res2 <- dir.create(here("data",
                          microxanox_path,
                          "temporal_method",
                          "event_definition_2",
                          "processed_data"),
                     recursive = TRUE)
  res3 <- dir.create(here("data",
                          microxanox_path,
                          "temporal_method",
                          "puzzles"))
  res4 <- dir.create(here("data",
                          microxanox_path,
                          "temporal_method",
                          "single_sims"))
  if(res1 & res2 & res3 & res4)
    result <- paste0("New folders created for results using ", microxanox_path)
}