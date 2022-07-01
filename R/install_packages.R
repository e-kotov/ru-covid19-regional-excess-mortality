if( !"renv" %in% installed.packages()[,'Package'] )  { install.packages("renv") }
if( !"here" %in% installed.packages()[,'Package'] )  { install.packages("here") }
if( !"rmarkdown" %in% installed.packages()[,'Package'] )  { install.packages("rmarkdown") }

source(here::here("R", "package_list.R"))

missing_packages <- packages_to_load[!packages_to_load %in% installed.packages()]

if( length(missing_packages) > 0 ) {
  renv::install(missing_packages)
}


# 
# invisible({capture.output({
#   renv::install(package_list, type = "binary")
# })})
# 
# invisible({capture.output({
#   renv::install("usethis", type = "binary")
# })})



# renv::snapshot()
# renv::status()
