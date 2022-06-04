package_list <- c(
                  "gtsummary",  "jtools",  "sandwich",  "ggstance",  "broom.mixed",  "huxtable", "Hmisc", "xtable",
                  "data.table", "tidyverse",
                  "sf", "lwgeom",
                  "spdep","spatialreg", "qpcR",
                  "readxl",
                  "ggpubr", "gridExtra", "ggfortify", "ggResidpanel", "performance", "see",
                  "corrgram", "GGally",
                  "patchwork",
                  "tidymodels", "yardstick", "broom",
                  "rticles", "here", "knitr", "formatR", "DT",
                  "officer", "flextable", "openxlsx"
                  )



packages_to_load <- c(package_list)

if( !"renv" %in% installed.packages()[,'Package'] )  { install.packages("renv") }
if( !"here" %in% installed.packages()[,'Package'] )  { install.packages("here") }
if( !"rmarkdown" %in% installed.packages()[,'Package'] )  { install.packages("rmarkdown") }

invisible({capture.output({
  renv::install(package_list, type = "binary")
})})

invisible({capture.output({
  renv::install("usethis", type = "binary")
})})
