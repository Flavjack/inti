[![Build status](https://ci.appveyor.com/api/projects/status/7x7cr66jq4pb83eu?svg=true)](https://ci.appveyor.com/project/omarbenites/fieldbook)

[![Build Status](https://travis-ci.org/Flavjack/fieldbook.svg?branch=master)](https://travis-ci.org/Flavjack/fieldbook)


# fieldbook
Data anlysis application for statistical research


# ToDo

How to install:



library(devtools)
install_github("flavjack/fieldbook")

#In case of some missing packages
paquetes <- c("leaps", "scatterplot3d", "flashClust", "knitr","klar",
                  "spedep", "AlgDesign", "DBI", "Rcpp","assertthat","tibble",
                  "gtable", "scales", "ggrepel", "cellranger", "htmlwidgets",
                  "httpuv", "xtable", "rhansontable", "ggpubr", "htmltools")
                 
fieldbook::fieldbook()
  
