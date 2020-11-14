# -------------------------------------------------------------------------
# R packages dependencies and configuration -------------------------------
# -------------------------------------------------------------------------
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date   .: 2020-11-14
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

cran <- c(
  "devtools" # Developer tools
  , "knitr" # Write Docs in R 
  , "bookdown" # Write articles and technical documents
  , "tidyverse" # Data manipulation
  , "googlesheets4" # Read/write google sheets docs
  , "googledrive" # Download/Upload files from googledrive
  , "agricolae" # Agriculture data analysis and designs
  , "GerminaR" # Germination analysis
  , "FactoMineR" # Multivariate data analysis
  , "heatmaply" # Correlation plot
  , "cowplot" # Layout for grid figures 
  , "magick" # Import png files
  , "grid" # Image grob
  )

git <- c(
  "Flavjack/inti" # Tools and Statistical Procedures in Plant Science
  , "crsh/citr" # Use zotero for citations
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

