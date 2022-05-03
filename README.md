
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ru-covid19-regional-excess-mortality

<!-- badges: start -->
<!-- badges: end -->

This repository contains the supplementary materials (code, data and
plots) and will be published on GitHub as part of the paper “SPATIAL
MODELLING OF KEY REGIONAL-LEVEL FACTORS OF COVID-19 MORTALITY IN
RUSSIA”.

You can see the published html version of the paper supplements at
<https://e-kotov.github.io/ru-covid19-regional-excess-mortality/>.

## Contents of the supplementray materials

-   `/codebook/codebook.xlsx` - contains description of the variables

-   `/paper` - a folder with the main script for performing the analysis

    -   `paper/paper.Rmd` - the main script that can be rendered into
        html or pdf file

    -   `paper/paper.html` - **the pre-rendered version of the analysis
        script** - this is what you want to open if you just want to
        read the results summary and see the plots

-   `/R` - a folder with custom analysis functions and package
    installation scripts written in R

-   `/data/` - data folder

    -   `region_borders_with_data.gpkg` - GIS data set with all
        attributes and regional boundaries geometry

    -   `region_borders_with_data.gal` - the spatial weights matrix file
        used for spatial autocorrelation tests and spatial modelling

    -   `regions_data.csv` - a copy of the regions attributes in a
        plain-text csv file without regional boundaries geometry

-   `/plots/` - folder with plots exported for the final paper

-   `/summaries/` - folder with model summaries and coefficients

## Reproducing the analysis

If you want to re-run the analysis from scratch:

1.  Install R <https://cran.r-project.org> and RStudio
    <https://www.rstudio.com>.

2.  Extract all the files to a folder.

3.  Open the `ru-covid19-regional-excess-mortality.Rproj` file in
    RStudio.

4.  Open the `paper/paper.Rmd` file within RStudio file browser.

5.  Press Knit button at the top to run the analysis. All packages
    should install automatically, the analysis should run and you should
    get an regenerated `paper/paper.html` file.

## ABSTRACT

will be added once the paper is published

## KEY WORDS

COVID-19, spatial models, socio-economic factors, climatic factors,
excess mortality, Russian regions
