# -------------------------------------------------------------------------
# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/tarpuy/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2020-03-19
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

cran <- c(
  "devtools"
  , "shiny"
  , "metathis"
  , "tidyverse"
  , "googlesheets4"
  , "googleAuthR"
  , "shinydashboard"
  , "stringi"
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

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

library(shiny)
library(inti)
library(metathis)
library(tidyverse)
library(googlesheets4)
library(googleAuthR)
library(bslib)
library(shinydashboard)
library(stringi)

# -------------------------------------------------------------------------
# references --------------------------------------------------------------
# -------------------------------------------------------------------------

# open https://console.cloud.google.com/apis/credentials

# open https://realfavicongenerator.net/

# open https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# open https://code.markedmondson.me/googleAuthR/articles/google-authentication-types.html#gar_shiny_-functions-example

# open https://github.com/MarkEdmondson1234/googleAuthR/issues/169

# open https://code.markedmondson.me/googleAuthR/reference/with_shiny.html

# open https://googlesheets4.tidyverse.org/reference/sheets_auth.html

# open https://github.com/MarkEdmondson1234/googleAuthR/issues/111

# open https://googlesheets4.tidyverse.org/articles/articles/drive-and-sheets.html

# open https://support.rstudio.com/hc/en-us/articles/217952868-Generating-OAuth-tokens-for-a-server-using-httr

