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
  
  remove(cran, git, pkg)
  
})

