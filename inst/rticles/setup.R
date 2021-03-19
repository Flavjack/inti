# -------------------------------------------------------------------------
# rticles -----------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/rticles/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-03-19
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

cran <- c(
  "devtools"
  , "bslib"
  , "metathis"
  , "shinydashboard"
  , "miniUI"
  , "shinyFiles"
  , "zip"
  , "fs"
  )

git <-  c(
  "crsh/citr" 
  , "Flavjack/inti" 
  )

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

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

library(inti)
library(bslib)
library(metathis)
library(shinydashboard)
library(miniUI)
library(shinyFiles)
library(zip)
library(fs)

# -------------------------------------------------------------------------
# references --------------------------------------------------------------
# -------------------------------------------------------------------------

# open https://realfavicongenerator.net/

