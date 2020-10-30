# -------------------------------------------------------------------------
# rticles -----------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/rticles/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2020-10-30
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

cran <-  c(
  "devtools"
  , "shiny"
  , "miniUI"
  , "shinyFiles"
  , "utils"
  , "fs"
  , "metathis"
  , "BiocManager"
  )

git <-  c(
  "crsh/citr" 
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
    devtools::install_github(pkg)
    library(sub(".*/", "", pkg), character.only = TRUE)
  } 
}

rm(cran, git, pkg)

# -------------------------------------------------------------------------
# references --------------------------------------------------------------
# -------------------------------------------------------------------------

# open https://realfavicongenerator.net/

