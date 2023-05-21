# -------------------------------------------------------------------------
# Configuration & R packages for rticles ----------------------------------
# -------------------------------------------------------------------------
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date   .: 2023-02-05
# -------------------------------------------------------------------------

#> source("https://inkaverse.com/rticles.r")

# -------------------------------------------------------------------------
# Packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

cran <- c("devtools", "knitr", "bookdown", "gsheet", "cowplot", "magick", "inti")

git <- c("crsh/citr") # Use zotero for docs citations

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

remove(cran, git, pkg)

# -------------------------------------------------------------------------
# Knitr options -----------------------------------------------------------
# -------------------------------------------------------------------------

knitr::opts_chunk$set(
    fig.align = "center" # Center images in the export file
  , out.width = "98%" # Figure width in html
  , echo = FALSE # Avoid print code in the export file
  , message = FALSE # Avoid print messages in the export file
  , warning = FALSE # Avoid print messages in the export file
  , collapse = TRUE # Collapse text output into source blocks
  # , tidy = TRUE # Reformat R source code
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
