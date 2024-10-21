# -------------------------------------------------------------------------
# R packages dependencies and configuration -------------------------------
# -------------------------------------------------------------------------
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date   .: 2024-10-21
# -------------------------------------------------------------------------

#> source("https://inkaverse.com/setup.r")

# -------------------------------------------------------------------------
# Packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

cran <- c(
  "devtools" # Developer tools
  , "inti" # Tools and Statistical Procedures in Plant Science
  , "FactoMineR" # Multivariate data analysis
  , "psych" # Correlation plot
  , "lme4"
  , "emmeans"
  , "multcomp"
  , "huito" # label design
  , "grid" # Import images as R object
  , "gsheet" # Read open google sheets docs
  , "googlesheets4" # Read/write google sheets docs
  , "googledrive" # Download/Upload files from googledrive
  , "knitr" # Write docs using R 
  , "tidyverse" # Data manipulation
  )

git <- c("crsh/citr") # Use zotero for docs citations

suppressPackageStartupMessages({
  
  for (pkg in cran) { 
    if( !require(pkg, character.only = TRUE) ) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    } 
  }
  
  # for (pkg in git) { 
  #   if( !require(sub(".*/", "", pkg), character.only = T) ) {
  #     devtools::install_github(pkg, upgrade = T)
  #     library(sub(".*/", "", pkg), character.only = T)
  #   } 
  # }
  
})

remove(cran, git, pkg)

# -------------------------------------------------------------------------
# Knitr options -----------------------------------------------------------
# -------------------------------------------------------------------------

knitr::opts_chunk$set(
  fig.align = "center" # Center images in the export file
  , out.width = "98%" # Figure width in html
  # , echo = FALSE # Avoid print code in the export file
  , message = FALSE # Avoid print messages in the export file
  , warning = FALSE # Avoid print warnings in the export file
  , collapse = TRUE # Collapse text output into source blocks
)

# -------------------------------------------------------------------------
# Compile options ---------------------------------------------------------
# -------------------------------------------------------------------------

options(
  OutDec= "." # Use "." instead of "," in the decimal values
  , scipen = 99 # Avoid use "6e-04"
  , knitr.kable.NA = "" # NA values will appear as empty cell
  , knitr.table.format = "pipe" # Format for export tables
  , citr.use_betterbiblatex = FALSE # For zotero addin 
) 

# -------------------------------------------------------------------------
# Authorize googledrive & googlesheets ------------------------------------
# -------------------------------------------------------------------------

googlesheets4::gs4_auth(TRUE)
googledrive::drive_auth(TRUE)
