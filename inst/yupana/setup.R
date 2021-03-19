# -------------------------------------------------------------------------
# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/yupanapro/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-03-19
# -------------------------------------------------------------------------

cran <- c(
  "devtools"
  , "shiny"
  , "metathis"
  , "tidyverse"
  , "googlesheets4"
  , "googleAuthR"
  , "shinydashboard"
  , "ggpubr"
  , "FactoMineR"
  , "corrplot"
  , "bslib"
  )

git <- c("Flavjack/inti")

suppressPackageStartupMessages({
  
  for (pkg in cran) { 
    if( !require(pkg, character.only = T) ) {
      install.packages(pkg)
      library(pkg, character.only = T)
    } 
  }
  
  for (pkg in git) { 
    if( !require(sub(".*/", "", pkg), character.only = T) ) {
      devtools::install_github(pkg, upgrade = T)
      library(sub(".*/", "", pkg), character.only = T)
    } 
  }
  
})

rm(cran, git, pkg)

