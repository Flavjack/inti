# -------------------------------------------------------------------------
# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/tarpuy/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2020-04-08
# -------------------------------------------------------------------------

cran <- c(
  "devtools"
  , "bslib"
  , "metathis"
  , "googlesheets4"
  , "googleAuthR"
  , "shinydashboard"
  , "tidyverse"
  , "stringi"
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

remove(cran, git, pkg)
