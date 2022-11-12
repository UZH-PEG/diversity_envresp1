# Preliminaries -----

rm(list = ls())

## Load some libraries and source files of functions ----
library(here)
library(microxanox)
library(tidyverse)
source(here("R/various_useful_functions.R"))

## Set and check microxanox version ----
microxanox_version_required <- "0.9.1"
source(here("R/check_microxanox_version.R"))

# **************************************************************
# Only continue if previous check of required microxanox passes.
# **************************************************************

# Note that the version specific path component has been set to:
microxanox_path

## Make data folder structure, if required ----
source(here("R/make_data_folders.R"))
result


# Run temporal method stable state finding ----

## event_definition_2 ----
event_definition <- "event_definition_2"

### different wait times ----
wait_time <- 1e6
#source(here("experiments/temporal_method/run_experiment.R"))

wait_time <- 1e5
#source(here("experiments/temporal_method/run_experiment.R"))

wait_time <- 1e4
#source(here("experiments/temporal_method/run_experiment.R"))

wait_time <- 1e3
#source(here("experiments/temporal_method/run_experiment.R"))

wait_time <- 1e2
#source(here("experiments/temporal_method/run_experiment.R"))

### Only most tolerant strain ----
wait_time <- 1e6
#source(here("experiments/temporal_method/run_experiment_onestrain.R"))

### Process and save data ----
#source(here("experiments/temporal_method/data_processing.R"))

### Make a couple of individual simulations, one with longer duration ----
#source(here("experiments/temporal_method/run_single_sims.R"))

## event_definition_1 ----
event_def <- "event_definition_1"

### Only one wait time ----
wait_time <- 1e6
#source(here("experiments/temporal_method/run_experiment.R"))

## Process and save data ----
#source(here("experiments/temporal_method/data_processing.R"))



# Run replication method stable state finding ----

event_def <- "event_definition_2"
sim_length <- 1e6
#source(here("experiments/replication_method/run_experiment.R"))
#source(here("experiments/replication_method/data_processing.R"))



# Run animations code ----
# Look in the reports/animations folder and run the code there