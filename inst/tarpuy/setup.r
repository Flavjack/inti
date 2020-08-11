# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/tarpuy/

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

cran <- c("devtools"
         , "shiny"
         , "metathis"
         , "tidyverse"
         , "googlesheets4"
         , "googleAuthR"
         , "shinydashboard"
         )

git <- c(
  "Flavjack/inti" # Tools and Statistical Procedures in Plant Science
  )

installed <- c(cran, sub(".*/", "", git)) %in% rownames(installed.packages())

if (any(installed == FALSE)) {

  cran_missing <- cran %in% c(cran, sub(".*/", "", git))[!installed == TRUE]
  cran_install <- c(cran, sub(".*/", "", git))[cran_missing == TRUE]
  install.packages( cran_install )

}

invisible(lapply(git, devtools::install_github))
invisible(lapply(c(cran, sub(".*/", "", git)), library, character.only = TRUE))
rm(cran, git, installed)

# References .:

# https://code.markedmondson.me/googleAuthR/articles/google-authentication-types.html#gar_shiny_-functions-example

# https://github.com/MarkEdmondson1234/googleAuthR/issues/169

# https://code.markedmondson.me/googleAuthR/reference/with_shiny.html

# https://googlesheets4.tidyverse.org/reference/sheets_auth.html

# https://console.cloud.google.com/apis/credentials

# open https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# http://xip.io/

# https://politicadeprivacidadplantilla.com/

# https://github.com/MarkEdmondson1234/googleAuthR/issues/111
