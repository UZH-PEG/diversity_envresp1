---
title: "README File"
format:
  html:
    self-contained: yes
    toc: yes
    toc-location: left
---

TO DO: set package versions everywhere to the one used in the release (1.0.0)

TO DO: delete `old` and other not needed directories (before this we should tag this)

# Introduction

This is a repository containing simulation, analysis, and visualisation code for the publication **Functional diversity can facilitate the collapse of an undesirable ecosystem state**.

It contains two folders:

1.  `R` -- a folder of containing a file or files of useful R functions. The files are sourced from other scripts; there is no need to directly work with them.
2.  `experiments` -- code for running simulations, processing the data produces, and visualising the results.


A third folder named `data` is created or populated by either running the experiments or downloading the data from a repository (see [below for details](#getting-the-data)). It will contain the data in `.RDS` format readable from R.

# Getting the data

To get the data, you have two options: either run experiments (i.e. simulations) yourself (which takes many days) or use the data as used for the publication **TO DO - publication information**.

The data used is available as a 10GB compressed zip file under the DOI [10.5281/zenodo.6334147](https://doi.org/10.5281/zenodo.6334147) on [Zenodo](https://zenodo.org) (the latest version). This zip file contains the data folder which should be placed in the same directory which contains the folder `R` and `experiments`. Extraction of the zip file will create a folder named `data` in the same directory with the data generated by the scripts in the `experiment` folder.

Ensure that the first folder in this directory is `0_ss_finding` and not `data`.

You can download and extract the data automatically by executing the commands below, which will install the package `zen4R` if not installed yet, download the data package to the working directory (this will take some time, since the file is 10 GB), unzip it, and delete the downloaded file "data.zip":

```{r, eval = FALSE}
if (system.file(package = "zen4R") == "") {
install.packages("zen4R")
}
zen4R::download_zenodo("10.5281/zenodo.6334147")
unzip("data.zip")
rm("data.zip")
```

or with base R:

```{r, eval = FALSE}
# Increase the timeout if it does not download correctly
opt <- options(timeout = 6000)
download.file("https://zenodo.org/record/6334147/files/data.zip?download=1", "data.zip")
options(timeout = opt)

chksum <- tools::md5sum("./data.zip")
if (chksum != "e96386282fb57b58ebeb23b217ac96f6") {
  stop(
    "Checksums are not identical, download likely corrupted!\n",
    "Please try to re-download!\n",
    "If the error persists, try to download from the URL given above!\n",
    "Checksum: ", chksum, "\n",
    "Expected: e96386282fb57b58ebeb23b217ac96f6"
  )
}

unzip("data.zip")
unlink("./data.zip")
```

A complete guide to all of the data files is not provided. Please work through the code for the Supplement and also the various R scripts to develop your understanding. Of course, please also get in touch with the authors, preferably via submitting and Issue in the github repo.


# Required packages

To be able to use the scripts in the `experiments` folder, the package `microxanox` version `1.0.0` is needed. **Compatibility with newer versions can not be guaranteed. Of the same main release (1.x.x), compatibility is very likely, with other main releases (2.x.x, 3.x.x, ...) extremely unlikely!**

This repository contains a file called `microxanox_1.0.0.tar.gz` which contains the R package in the correct version.

The R package can be installed by either installing from this file by using


```{r}
remotes::install_local("./microxanox_0.9.0.tar.gz", upgrade = FALSE)
```

You can also install it from the github repository by using

```{r}
remotes::install_github( "UZH-PEG/microxanox", ref = "v0.9.0", build_vignettes = TRUE, upgrade = FALSE)
```

The newest version csan also be installer from the [R-Universe](https://r-universe.dev) as follows:

    install.packages("microxanox", repos = c('https://XXX.r-universe.dev', 'https://cloud.r-project.org'))

To re-iterate: there is a good chance that the versions on the [R-Universe](https://r-universe.dev) are not compatible with the one used in this code!

# Running the scripts and RMD files

The **bold** scripts need to be run, the non-bold are called from the **bold** scripts. The scripts in **bold** need to be executed in the order of their appearance in the subfolders.

## Running simulations and processing data

Beware that running simulations can take a long time (days). All data generated by the scripts and RMarkdown files is contained in the data deposit with the DOI [10.5281/zenodo.6334147](https://doi.org/10.5281/zenodo.6334147). See the section on [Getting the Data](#getting-the-data) to learn how you can get the data, if not done already.

-   `replication_method/`

    -   `replication_method/setup_experiment.R`: Setup the experiment including all parameters needed
    -   **`replication_method/run_experiment.R`**: Run the experiment.
    -   **`replication_method/data_processing.R`**: Process the data for further processing and analysis.

-   `temporal_method/`

    -   `temporal_method/check_microxanox_version.R`: Check the version of the installed `microxanox` package.
    -   `temporal_method/setup_experiment.R`: Setup the experiment including all parameters needed
    -   **`temporal_method/run_single_sim.R`**: Run a single simulation.
    -   **`temporal_method/run_experiment.R`**: Run the experiment.
    -   **`temporal_method/data_processing.R`**: Process the data for further processing and analysis.
    -   **`temporal_method/get_div_effects.R`**: Calculate the effects of diversity in each of the combinations of diversity treatments

## Making visualisations and the supplement

-   `1_figures_main_ms/`

    -   **`1_figures_main_ms/figures_main_ms_v1.R`**: Script to generate the graphs from the publication

-   `2_supplement/`

    -   `2_supplement/supplement.Rmd`: RMarkdown file used to generate the supplement to the publication **TO DO: add publication ref**
    
-   `3_animate/`

    -   Various code files to make animations of results. `report.RmD` puts the animations together into `report.html`.

