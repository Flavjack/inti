# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------

# open https://flavjack.shinyapps.io/yupanapro/
# open http://localhost:1221/

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

# source("https://raw.githubusercontent.com/Flavjack/inti/master/inst/yupanapro/setup.r")

cran <- c("devtools"
         , "shiny"
         , "metathis"
         , "tidyverse"
         , "googlesheets4"
         , "googleAuthR"
         , "shinydashboard"
         , "ggpubr"
         , "FactoMineR"
         , "corrplot"
         , "BiocManager"
         )

git <- c(
  "rstudio/bootstraplib"
  , "Flavjack/inti" # Tools and Statistical Procedures in Plant Science
  )

installed <- c(cran, sub(".*/", "", git)) %in% rownames(installed.packages())

if (any(installed == FALSE)) {

  cran_missing <- cran %in% c(cran, sub(".*/", "", git))[!installed == TRUE]
  cran_install <- c(cran, sub(".*/", "", git))[cran_missing == TRUE]
  install.packages( cran_install )

}

invisible(lapply(sub(".*/", "", git), unloadNamespace))
invisible(lapply(git, devtools::install_github))
invisible(lapply(c(cran, sub(".*/", "", git)), library, character.only = TRUE))
rm(cran, git, installed)

# References .:

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/

# https://rpubs.com/therimalaya/43190

# open https://realfavicongenerator.net/

# https://googlesheets4.tidyverse.org/reference/sheets_auth.html

