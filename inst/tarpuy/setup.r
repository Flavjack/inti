# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/tarpuy/

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

pkgs <- list(

  cran = c(
    "shiny"
    , "metathis"
    , "tidyverse"
    , "googlesheets4"
    , "googleAuthR"
  ),

  git = c(
    "inti" # Tools and Statistical Procedures in Plant Science
  )
)

gitrepo <- c("Flavjack/inti")
installed <- unlist(pkgs) %in% rownames(installed.packages())

if (any(installed == FALSE)) {

  cran_missing <- pkgs[["cran"]] %in% as.vector(unlist(pkgs)[!installed == TRUE])
  cran_install <- unlist(pkgs)[cran_missing == TRUE]
  install.packages( cran_install )

  }

invisible(lapply(gitrepo, devtools::install_github, character.only = TRUE))
invisible(lapply(as.vector(unlist(pkgs)), library, character.only = TRUE))
rm(pkgs, gitrepo, installed)


# References .:

# https://code.markedmondson.me/googleAuthR/articles/google-authentication-types.html#gar_shiny_-functions-example

# https://github.com/MarkEdmondson1234/googleAuthR/issues/169

# https://code.markedmondson.me/googleAuthR/reference/with_shiny.html

# https://googlesheets4.tidyverse.org/reference/sheets_auth.html

# https://console.cloud.google.com/apis/credentials

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
