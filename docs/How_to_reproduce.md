---
title: "How to reproduce the research, the manuscript figures, and the supplementary document."
format:
  html:
    self-contained: true
    toc: true
    toc-location: left
---

24. November 2022

To do once the paper is accepted:

* Set package versions everywhere to the one used in the release (1.0.0)
* Delete `old` and other not needed directories (before this we should tag this)
* Search for XXX for items to update.


# Introduction

Please note that we did not design and write all scripts with other users primarily in mind. Reproducing the work will require some commitment! Please get in touch via Issues on github if you need any assistance.

Reproducing the research requires the *microxanox* R package (location given below) and the code to run simulation, analysis, and visualisations in [this `diversity_envresp1` repository](https://github.com/UZH-PEG/diversity_envresp1). There is also the option to get the data produced by the simulations (see below), rather than having to produce it by running the simulations (which can take a long time).


# Step 1: Get acquainted with the *microxanox* R package

To reproduce the research, the package `microxanox` version `0.9.1` is needed.  Familarise yourself with the [*microxanox* R package](https://uzh-peg.r-universe.dev/ui#package:microxanox) that was created to facilitate the research. The two vignettes in that package should be useful. The package is also described in this article (link to follow XXX).

[The `diversity_envresp1` repository](https://github.com/UZH-PEG/diversity_envresp1) contains a file called `microxanox_0.9.1.tar.gz` which contains the R package in the correct version.

The R package can be installed by either installing from this file by using


```r
remotes::install_local("./microxanox_0.9.1.tar.gz", upgrade = FALSE)
```

You can also install it from the github repository by using

```r
remotes::install_github( "UZH-PEG/microxanox", ref = "v0.9.1", build_vignettes = TRUE, upgrade = FALSE)
```

The newest version csan also be installed from the [R-Universe](https://r-universe.dev) as follows:

```r
install.packages("microxanox", repos = c('https://XXX.r-universe.dev', 'https://cloud.r-project.org'))
```

(Compatibility with any newer versions is not guaranteed. Of the same main release (1.x.x), compatibility is likely; with other main releases (2.x.x, 3.x.x, ...) is unlikely!)

# Step 2: Get the code

The code to run simulations, analyses, and visualisations is in [the `diversity_envresp1` repository](https://github.com/UZH-PEG/diversity_envresp1) from where it can be downloaded. It is also available on Zenodo [https://zenodo.org/record/7360272](https://zenodo.org/record/7360272). It includes:

1.  `R` -- a folder of containing a file or files of useful R functions. The files are sourced from other scripts; there is no need to directly work with them.
2.  `experiments` -- a folder contain folders of code for running simulations and processing the data produces.
3. `data` -- a folder that will contain all the data created by the simulations.
4. `reports` -- a folder containing folders for making different reports (i.e., main manuscript figures, supplementary report, and some animations).
5. `docs` -- a folder containing this document.
6. Some additional files in the root, e.g. the R project file and `index.md` file.


# Step 3: Get the data

To get the data, there are two options: either run experiments (i.e. simulations) yourself (which may take many days) or get the copy of the data that is available as a 10GB compressed zip file under the DOI [10.5281/zenodo.7356406](https://zenodo.org/record/7356406#.Y4CYUezMJqs) (the latest version). This zip file contains the data folder which should be placed in the same directory which contains the folder `R`, `experiments`, `reports`, and `docs`. Extraction of the zip file will create a folder named `data` (Ensure that the first folder in the `data` folder is `microxanox_v0.9.1` and not `data`.

Alternatively, the data can be downloaded and extracted by executing the commands below, which will install the package `zen4R` if not installed yet, download the data package to the working directory (this will take some time, since the file is 10 GB), unzip it, and delete the downloaded file "data.zip":

```r
if (system.file(package = "zen4R") == "") {
install.packages("zen4R")
}
zen4R::download_zenodo("10.5281/zenodo.7356406")
unzip("data.zip")
rm("microxanox_v0.9.1.zip")
```

or with base R:

```r
# Increase the timeout if it does not download correctly
opt <- options(timeout = 6000)
download.file("https://zenodo.org/record/7356406/files/data.zip?download=1", "data.zip")
options(timeout = opt)

chksum <- tools::md5sum("./data.zip")
if (chksum != "59a46ea515e96db7a30a043b9315268d") {
  stop(
    "Checksums are not identical, download likely corrupted!\n",
    "Please try to re-download!\n",
    "If the error persists, try to download from the URL given above!\n",
    "Checksum: ", chksum, "\n",
    "Expected: 59a46ea515e96db7a30a043b9315268d"
  )
}

unzip("data.zip")
unlink("./data.zip")
```

To understand the structure of the data repository and data files therein please work through the *microxanox* package vignettes, and the various codes in this repository. Of course, please also get in touch with the authors, preferably via submitting and Issue in the github repo.


# Step 4: Use the reproduction scripts

## Running simulations and processing data

Be aware that running simulations can take a long time (days). All data generated by the scripts and RMarkdown files is contained in the data deposit with the DOI [10.5281/zenodo.7356406](https://zenodo.org/record/7356406#.Y4CYUezMJqs) . See the section above about getting the data, if not done already.

The script file `run_all.r` contains code to run all simulations, and brief description of what is being done. Please look there for further information.

## Making visualisations and the supplement

To make the data-based figures of the main report (i.e. published paper): `reports/manuscript/figures_main_ms_v1.R`

To make the supplementary report: `reports/supplement/supplement_0.9.1.Rmd`: RMarkdown file used to generate [the supplementary report (HTML page)](https://uzh-peg.github.io/diversity_envresp1/reports/supplement/supplement_0.9.1.Rmd) to the publication **XXXTO DO: add publication ref**.

To make graphs of stable states against oxygen diffusivity (e.g. versions of figure 4 in the main manuscript) knit the rmd files in the `reports/stable_states_graphs` sub-folders.

To make some animations use code in the `reports/animations` folder. Eventually you will be able to make this [`report.html` HTML page.](https://uzh-peg.github.io/diversity_envresp1/reports/animations/report.html)

