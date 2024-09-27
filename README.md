# OPTIMA-survey

This notebook contains data and code to reproduce the analysis for the "Report
on academic integrity awareness and Open Science recognition levels in Ukraine 
(2021-2023)". 

# Dependencies
The analysis relies on multiple packages, which are listed in 
`R/dependencies.R`. Make sure all are installed before attempting to run the 
pipeline.

The package `ggchicklet` is not available from CRAN. You can find options to 
install it at https://github.com/hrbrmstr/ggchicklet. As of 2024-09-27, you
could for example install it via 
`remotes::install_git("https://git.sr.ht/~hrbrmstr/ggchicklet")` or
`remotes::install_github("hrbrmstr/ggchicklet")`.

The notebook `sample_characteristics.qmd` requires shapefiles which we cannot 
redistribute. 

The simplest way to acquire them is to use the function 
`download_and_prepare_map()` which is 
available once you run `source("R/functions.R")`. It will download the required
shapefiles and move them into the right location. Note, however, that the 
function may break in case the link structure at GADM changes.

In such cases, inspect the source code of 
`download_and_prepare_map()` to adapt the link.

You can download the shapefiles [here](https://gadm.org/download_country.html).

To acquire the files, select "Ukraine" as the country, and download the files
by clicking "Shapefile". You need all files for level 1 with the basename
`gadm41_UKR_1.*`. Then move them into the right location (`data/additional/map/`).


# Steps to reproduce the analysis
The analysis is reproducible with the `targets` package. Running the following
code should rebuild all outputs:

```r
targets::tar_make()
```

# Analysis notebooks
The directory `analysis_notebooks` contains the source notebooks (`*.qmd`), 
alongside with rendered outputs. 


# Funding
With the support of the Erasmus+ Programme of the European Union under funding 
No.OPTIMA - 618940-EPP-1-2020-1-UA-EPPKA2-CBHE-JP	
