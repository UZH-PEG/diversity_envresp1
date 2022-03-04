# Introduction

This is a repository containing simulation, analysis, and visualisation code for the publication **add publication information**.

It contains three folders:

1.  `R` -- a folder of containing a file or files of useful R functions. The files are sourced from other scripts; there is no need to directly work with them.
2.  `experiments` -- code for running simulations, processing the data produces, and visualising the results.
3.  `data` -- an empty folder (apart from a short readme file) into which the data should be placed.

# Getting the data

Data is currently available from the group file server: [`smb://ieu-fs.uzh.ch`](smb://ieu-fs.uzh.ch) in `Gr_Petchey/4 Projects/diversity_envresp1/data`

\*\* TO DO The following to be updated once there is a public repo of the data, and the above to be removed.\*\*

Running the simulations takes many days. Therefore the data produced is available in this repository **TO DO**. Into the `data` folder please place the data from the repository. Ensure that the first folder in this directory is `0_ss_finding` and not `data`.

# Required packages

**TO DO**: add text for how to install microxanox, and remove any unnecessary text below.

From '?remotes::install_github\`: To install from a private repo, generate a personal access token (PAT) in"<https://github.com/settings/tokens>" and use the following

`remotes::install_github( "opetchey/microxanox", ref = microxanox_release, auth_token = "ENTER YOUR TOKEN or PROVED AS ENVIRONMENT VARIABLE", build_vignettes = FALSE, force = TRUE, upgrade = FALSE, lib = tmplib )`
