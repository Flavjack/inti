# -------------------------------------------------------------------------
# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/yupanapro/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2020-11-18
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
  , "ggpubr"
  , "FactoMineR"
  , "corrplot"
  )

git <- c(
  "rstudio/bslib"
  , "MarkEdmondson1234/googleID"
  , "Flavjack/inti" 
  )

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

# -------------------------------------------------------------------------
# references --------------------------------------------------------------
# -------------------------------------------------------------------------

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# https://rpubs.com/therimalaya/43190

# open https://realfavicongenerator.net/

# https://googlesheets4.tidyverse.org/reference/sheets_auth.html

