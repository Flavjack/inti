# -------------------------------------------------------------------------
# TARPUY: runtime packages -------------------------------------------------
# -------------------------------------------------------------------------
# This file only checks and attaches packages required by the TARPUY app.
# Package installation is handled separately by inst/tarpuy/setup.R.
# -------------------------------------------------------------------------

# Packages whose exported functions are used without an explicit `pkg::`
# prefix in ui.R, server.R, or the authentication module.
tarpuy_attach_packages <- c(
  "inti",
  "shiny",
  "metathis",
  "googlesheets4",
  "googleAuthR",
  "shinydashboard",
  "dplyr"
)

# Packages used through `pkg::function()` or loaded by TARPUY modules.
# They must be installed, but do not need to be attached to the search path.
tarpuy_namespace_packages <- c(
  "bslib",
  "DT",
  "purrr",
  "readr",
  "gargle",
  "httr",
  "assertthat",
  "cli"
)

tarpuy_required_packages <- unique(c(
  tarpuy_attach_packages,
  tarpuy_namespace_packages
))

tarpuy_missing_packages <- tarpuy_required_packages[
  !vapply(
    tarpuy_required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if(length(tarpuy_missing_packages) > 0L) {
  setup_path <- system.file("tarpuy", "setup.R", package = "inti")
  
  setup_instruction <- if(nzchar(setup_path)) {
    paste0(
      "source(",
      deparse(setup_path),
      ")"
    )
  } else {
    "install the missing packages before starting TARPUY"
  }
  
  stop(
    paste0(
      "TARPUY cannot start because these required packages are missing: ",
      paste(tarpuy_missing_packages, collapse = ", "),
      ". Run ", setup_instruction, "."
    ),
    call. = FALSE
  )
}

# Attach packages in a deterministic order. `inti` is attached first because
# TARPUY calls its exported design, trait, and plotting functions directly.
suppressPackageStartupMessages({
  for(pkg in tarpuy_attach_packages) {
    library(pkg, character.only = TRUE)
  }
})

rm(
  tarpuy_attach_packages,
  tarpuy_namespace_packages,
  tarpuy_required_packages,
  tarpuy_missing_packages,
  pkg
)