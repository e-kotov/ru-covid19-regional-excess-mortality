
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ru-covid19-regional-excess-mortality

<!-- badges: start -->

Source code of the paper **Kotov, E., Goncharov, R., Kulchitsky, Y.,
Molodtsova, V., Nikitin, B. (2022). Spatial Modelling of Key
Regional-Level Factors of COVID-19 Mortality in Russia. GEOGRAPHY,
ENVIRONMENT, SUSTAINABILITY, 15(2), 71–83.
<https://doi.org/10.24057/2071-9388-2021-076>**

Current data and code:
[![DOI](https://zenodo.org/badge/471431313.svg)](https://zenodo.org/badge/latestdoi/471431313)

<!-- github_id received via accessing https://api.github.com/repos/e-kotov/ru-covid19-regional-excess-mortality -->
<!-- badges: end -->

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/e-kotov/ru-covid19-regional-excess-mortality/HEAD?urlpath=rstudio)
to run the analysis in the cloud (due to multiple packages used in the
analysis, you may have to wait about 10 minutes for the container to set
up.)

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

Intensive socio-economic interactions are a prerequisite for the
innovative development of the economy, but at the same time, they may
lead to increased epidemiological risks. Persistent migration patterns,
the socio-demographic composition of the population, income level, and
employment structure by type of economic activity determine the
intensity of socio-economic interactions and, therefore, the spread of
COVID-19.

We used the excess mortality (mortality from April 2020 to February 2021
compared to the five-year mean) as an indicator of deaths caused
directly and indirectly by COVID-19. Similar to some other countries,
due to irregularities and discrepancies in the reported infection rates,
excess mortality is currently the only available and reliable indicator
of the impact of the COVID-19 pandemic in Russia.

We used the regional level data and fit regression models to identify
the socio-economic factors that determined the impact of the pandemic.
We used ordinary least squares as a baseline model and a selection of
spatial models to account for spatial autocorrelation of dependent and
independent variables as well as the error terms.

Based on the comparison of AICc (corrected Akaike information criterion)
and standard error values, it was found that SEM (spatial error model)
is the best option with reliably significant coefficients. Our results
show that the most critical factors that increase the excess mortality
are the share of the elderly population and the employment structure
represented by the share of employees in manufacturing (C economic
activity according to European Skills, Competences, and Occupations
(ESCO) v1 classification). High humidity as a proxy for temperature and
a high number of retail locations per capita reduce the excess
mortality. Except for the share of the elderly, most identified factors
influence the opportunities and necessities of human interaction and the
associated excess mortality.

## KEY WORDS

COVID-19, spatial models, socio-economic factors, climatic factors,
excess mortality, Russian regions

## FUNDING

The reported study was funded by RFBR according to the research project
№ 20-04-60490 “Ensuring balanced regional development during a pandemic
with spatially differentiated regulation of socio-economic interaction,
sectoral composition of the economy and local labour markets”.
