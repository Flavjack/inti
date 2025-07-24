# -------------------------------------------------------------------------
# R packages dependencies and configuration -------------------------------
# -------------------------------------------------------------------------
#> author .: Flavio Lozano-Isla (linkedin.com/in/flozanoisla/)
#> date   .: 2025-07-23
# -------------------------------------------------------------------------

#> source("https://inkaverse.com/setup.r")

# -------------------------------------------------------------------------
# 📦 Package loading and installation -------------------------------------
# -------------------------------------------------------------------------

cran_packages <- c(
  "devtools", "inti", "FactoMineR", "psych", "lme4", "car", "emmeans",
  "multcomp", "huito", "grid", "googlesheets4", "googledrive",
  "knitr", "tidyverse", "RhpcBLASctl", "sessioninfo", "cli"
)

load_or_install <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

load_or_install(cran_packages)

# Optional GitHub packages
# git_packages <- c("crsh/citr")
# for (repo in git_packages) {
#   pkg <- sub(".*/", "", repo)
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     devtools::install_github(repo, upgrade = TRUE)
#   }
#   suppressPackageStartupMessages(library(pkg, character.only = TRUE))
# }

# -------------------------------------------------------------------------
# ⚙️ System and computation configuration ----------------------------------
# -------------------------------------------------------------------------

knitr::opts_chunk$set(
  fig.align = "center",
  out.width = "98%",
  message = FALSE,
  warning = FALSE,
  collapse = TRUE
)

options(
  OutDec = ".", scipen = 99,
  knitr.kable.NA = "",
  knitr.table.format = "pipe",
  citr.use_betterbiblatex = FALSE
)

total_cores <- parallel::detectCores(logical = TRUE)
usable_cores <- max(1, floor(total_cores * 0.8))
RhpcBLASctl::blas_set_num_threads(usable_cores)

cli::cli_alert_info("📁 Project directory: {getwd()}")
cli::cli_alert_info("🧠 Total CPU cores detected: {total_cores}")
cli::cli_alert_info("🚀 BLAS threads configured to: {RhpcBLASctl::blas_get_num_threads()}")

# -------------------------------------------------------------------------
# 🔐 Google authentication ------------------------------------------------
# -------------------------------------------------------------------------

googlesheets4::gs4_auth(cache = ".secrets", use_oob = TRUE)
googledrive::drive_auth(cache = ".secrets", use_oob = TRUE)

# -------------------------------------------------------------------------
# 📋 Environment info -----------------------------------------------------
# -------------------------------------------------------------------------

sessioninfo::session_info()

# -------------------------------------------------------------------------
# 🧹 Clean up -------------------------------------------------------------
# -------------------------------------------------------------------------

rm(cran_packages, load_or_install, total_cores, usable_cores)
# rm(git_packages)  # if defined