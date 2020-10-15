# rticles -----------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/rticles/

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
  "crsh/citr" # include citation in Rmarkdown
  , "Flavjack/inti" # Tools and Statistical Procedures in Plant Science
  )

installed <- c(cran, sub(".*/", "", git)) %in% rownames(installed.packages())

if (any(installed == FALSE)) {
  cran_missing <- cran %in% c(cran, sub(".*/", "", git))[!installed == TRUE]
  cran_install <- c(cran, sub(".*/", "", git))[cran_missing == TRUE]
  install.packages( cran_install )
}

invisible(lapply(sub(".*/", "", git), unloadNamespace))
invisible(lapply(git, devtools::install_github, dependencies = T))
invisible(lapply(c(cran, sub(".*/", "", git)), library, character.only = TRUE))
rm(cran, git, installed)

# References

# open https://realfavicongenerator.net/

