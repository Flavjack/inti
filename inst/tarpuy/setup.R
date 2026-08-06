# -------------------------------------------------------------------------
# TARPUY: install missing runtime dependencies -----------------------------
# -------------------------------------------------------------------------
# This script installs only packages that are missing. It does not update
# installed packages, install development tools, or reinstall `inti` from
# GitHub. The app loads packages later through inst/tarpuy/pkgs.R.
# -------------------------------------------------------------------------

local({
  if(getRversion() < "4.1.0") {
    stop(
      "TARPUY requires R 4.1.0 or newer.",
      call. = FALSE
    )
  }
  
  tarpuy_packages <- c(
    # Core application and interface
    "shiny",
    "bslib",
    "metathis",
    "shinydashboard",
    "DT",
    
    # Google Sheets and authentication
    "googlesheets4",
    "googleAuthR",
    "gargle",
    "httr",
    "assertthat",
    
    # Data handling used by TARPUY
    "dplyr",
    "tidyr",
    "tibble",
    "purrr",
    "stringr",
    "readr",
    
    # Plotting and application messages
    "ggplot2",
    "systemfonts",
    "cli"
  )
  
  missing_packages <- tarpuy_packages[
    !vapply(
      tarpuy_packages,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1)
    )
  ]
  
  if(length(missing_packages) == 0L) {
    message("All TARPUY runtime dependencies are already installed.")
  } else {
    repos <- getOption("repos")
    
    if(
      is.null(repos) ||
      length(repos) == 0L ||
      is.na(repos[[1L]]) ||
      identical(unname(repos[[1L]]), "@CRAN@")
    ) {
      repos <- c(CRAN = "https://cloud.r-project.org")
    } else if(
      is.null(names(repos)) ||
      !"CRAN" %in% names(repos) ||
      is.na(repos[["CRAN"]]) ||
      identical(unname(repos[["CRAN"]]), "@CRAN@")
    ) {
      repos <- c(CRAN = "https://cloud.r-project.org", repos)
    }
    
    message(
      "Installing missing TARPUY dependencies: ",
      paste(missing_packages, collapse = ", ")
    )
    
    install.packages(
      pkgs = missing_packages,
      repos = repos,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
    
    still_missing <- missing_packages[
      !vapply(
        missing_packages,
        requireNamespace,
        quietly = TRUE,
        FUN.VALUE = logical(1)
      )
    ]
    
    if(length(still_missing) > 0L) {
      stop(
        paste0(
          "TARPUY could not install these packages: ",
          paste(still_missing, collapse = ", "),
          ". Review the installation messages and system requirements, then retry."
        ),
        call. = FALSE
      )
    }
    
    message("TARPUY runtime dependencies were installed successfully.")
  }
  
  invisible(TRUE)
})

