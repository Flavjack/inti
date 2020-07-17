# Info --------------------------------------------------------------------
# -------------------------------------------------------------------------
#> author .: Flavio Lozano Isla
#> web    .: https://lozanoisla.com
#> date   .: 2020-07-16
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

cran <- c("devtools" # Developer tools
           , "bookdown" # Write articles and technical documents
           , "knitr" # Base for markdown documents
           , "tidyverse" # Data manipulation
           , "googlesheets4" # Read/write google sheets docs
           , "googledrive" # Download/Upload files from googledrive
           , "agricolae" # Agriculture data analysis and designs
           , "GerminaR" # Germination analysis
           , "FactoMineR" # Multivariate data analysis
           , "heatmaply" # Correlation plot
           , "cowplot" # Layout for grid figures 
           , "grid" # For merge figures 
           , "png" # Import png files
           , "jpeg" # Import jpg files
           , "emmeans" # Estimated Marginal Means
           , "lme4" # Linear Mixed-Effects Models (LMM)
           , "lmerTest" # Tests in Linear Mixed Effects Models
           )

git <- c(
  "Flavjack/inti" # Tools and Statistical Procedures in Plant Science
  , "crsh/citr" # Use zotero for citations
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
