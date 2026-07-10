# PRISM Shiny App 
[![DOI](https://zenodo.org/badge/1296663117.svg)](https://doi.org/10.5281/zenodo.21299034)

PRISM is an R Shiny application for phasor-based Raman image segmentation and analysis. It provides an interactive workflow for importing Raman data, applying preprocessing, selecting clusters, and exporting spectra or image-based results.

## What this app does

- Import Raman data from WITec header files or RData files
- Visualize spectral phasor plots and false-color Raman images
- Apply baseline correction and vector normalization
- Manually select or automatically cluster regions of interest
- Export mean spectra and image/spectral matrices

## Requirements

The app is written in R and requires the following packages:

- shiny
- shinyFiles
- plotly
- hyperSpec
- tidyverse
- fs
- shinyjs

You can install them in R with:

```r
install.packages(c("shiny", "shinyFiles", "plotly", "hyperSpec", "tidyverse", "fs", "shinyjs"))
```

## Running locally

From the project directory, start the app with:

```r
shiny::runApp()
```

If you are working from the folder containing this README, the app should launch directly.

## Data formats

The app currently supports:

- WITec header files with the pattern \(Header).txt
- RData files containing hyperSpec objects

## Notes on privacy and private code

This repository intentionally excludes the private helper file private_processing_func.R from the public release. It is listed in the repository ignore rules and is not part of the published archive.

## Citation and archival

If you want to archive this work on Zenodo, the repository now includes:

- a license file
- a citation file
- Zenodo metadata
- a repository ignore file

## License

This project is licensed under the MIT License. See the LICENSE file for details.

