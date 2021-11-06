# -------------------------------------------------------------------------
# R packages dependencies and configuration -------------------------------
# -------------------------------------------------------------------------
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date   .: 2021-04-28
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

cran <- c(
  "devtools" # Developer tools
  , "inti" # Tools and Statistical Procedures in Plant Science
  , "knitr" # Write docs using R 
  , "bookdown" # Write articles and technical documents
  , "tidyverse" # Data manipulation
  , "gsheet" # Read open google sheets docs
  , "readxl" # Read/write excel files
  , "googlesheets4" # Read/write google sheets docs
  , "googledrive" # Download/Upload files from googledrive
  , "agricolae" # Agriculture data analysis and designs
  , "GerminaR" # Germination analysis
  , "FactoMineR" # Multivariate data analysis
  , "heatmaply" # Correlation plot
  , "cowplot" # Layout for grid figures 
  , "magick" # Import multiple format files
  , "grid" # Import images as R object
  )

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

googlesheets4::gs4_auth(T)
googledrive::drive_auth(T)

