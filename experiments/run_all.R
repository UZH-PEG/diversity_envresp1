# Preliminaries -----

rm(list = ls())

## Load some libraries and source files of functions ----
library(here)
library(microxanox)
library(tidyverse)
source(here("R/various_useful_functions.r"))

## Set and check microxanox version ----
microxanox_version_required <- "0.9.1"
source(here("R/check_microxanox_version.R"))

# **************************************************************
# Only continue if previous check of required microxanox passes.
# **************************************************************

# Should the simulations be run in test_mode (take a day or so on 32 core machine)
test_mode <- TRUE

# Note that the version specific path component has been set to:
microxanox_path

## Make data folder structure, if required ----
source(here("R/make_data_folders.R"))
result


# Run temporal method stable state finding ----
run_flag_1 <- TRUE
if(run_flag_1) {
  
  ## event_definition_2 ----
  event_definition <- "event_definition_2"
  
  ### different wait times ----
  wait_time <- 1e6
  source(here("experiments/temporal_method/run_experiment.R"))
  
  wait_time <- 1e5
  source(here("experiments/temporal_method/run_experiment.R"))
  
  wait_time <- 1e4
  source(here("experiments/temporal_method/run_experiment.R"))
  
  wait_time <- 1e3
  source(here("experiments/temporal_method/run_experiment.R"))
  
  wait_time <- 1e2
  source(here("experiments/temporal_method/run_experiment.R"))
  
  ### Only most tolerant strain ----
  wait_time <- 1e6
  source(here("experiments/temporal_method/run_experiment_onestrain.R"))
  
  ### Process and save data ----
  source(here("experiments/temporal_method/data_processing.R"))
  
}


### Make a couple of individual simulations, one with longer duration ----
run_flag_2 <- TRUE
if(run_flag_2) {
  source(here("experiments/temporal_method/run_single_sims.R"))
}


## event_definition_1 ----
run_flag_3 <- TRUE
if(run_flag_3) {
  event_definition <- "event_definition_1"
  ### Only one wait time ----
  wait_time <- 1e6
  source(here("experiments/temporal_method/run_experiment.R"))
  ## Process and save data ----
  source(here("experiments/temporal_method/data_processing.R"))
}


# Run replication method stable state finding ----
run_flag_4 <- TRUE
if(run_flag_4) {
  event_definition <- "event_definition_2"
  sim_length <- 1e6
  source(here("experiments/replication_method/run_experiment.R"))
  source(here("experiments/replication_method/data_processing.R"))
}


# Run animations code ----
# Look in the reports/animations folder and run the code there