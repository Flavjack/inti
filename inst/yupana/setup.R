# -------------------------------------------------------------------------
# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/yupanapro/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-09-30
# -------------------------------------------------------------------------

cran <- c(
  "devtools"
  , "bslib"
  , "metathis"
  , "googlesheets4"
  , "googleAuthR"
  , "shinydashboard"
  , "cowplot"
  , "psych"
  , "tidyverse"
  , "FactoMineR"
  , "ggpmisc"
  )

git <- c("Flavjack/inti")

suppressPackageStartupMessages({
  
  for (pkg in cran) { 
    if( !require(pkg, character.only = TRUE) ) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    } 
  }
  
  for (pkg in git) { 
    if( !require(sub(".*/", "", pkg), character.only = TRUE) ) {
      devtools::install_github(pkg, upgrade = TRUE)
      library(sub(".*/", "", pkg), character.only = TRUE)
    } 
  }
  
  rm(cran, git, pkg)
  
})
