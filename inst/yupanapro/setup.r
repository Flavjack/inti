# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/yupanapro/

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

cran <- c("shiny"
         , "metathis"
         , "tidyverse"
         , "googlesheets4"
         , "googleAuthR"
         , "shinydashboard"
         , "ggpubr"
         , "FactoMineR"
         )

git <- c(
  "Flavjack/inti" # Tools and Statistical Procedures in Plant Science
  , "rstudio/bootstraplib"
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

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# https://rpubs.com/therimalaya/43190

