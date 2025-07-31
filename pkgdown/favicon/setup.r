# -------------------------------------------------------------------------
# 📄 R packages dependencies and configuration ----------------------------
# -------------------------------------------------------------------------
#> Author .: Flavio Lozano-Isla (linkedin.com/in/flozanoisla/)
#> Date   .: 2025-07-23
# -------------------------------------------------------------------------

# Optional: external setup script
# source("https://inkaverse.com/setup.r")

# -------------------------------------------------------------------------
# 📦 Package loading and installation -------------------------------------
# -------------------------------------------------------------------------

cran_packages <- c(
  "devtools", "tidyverse"
  , "lme4", "car", "emmeans", "multcomp"
  , "FactoMineR", "psych"
  , "inti", "huito"
  , "googlesheets4", "googledrive"
  , "knitr", "grid", "magick"
  , "RhpcBLASctl"
  , "sessioninfo"
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

# CPU configuration
total_cores <- parallel::detectCores(logical = TRUE)
usable_cores <- max(1, floor(total_cores * 0.8))
RhpcBLASctl::blas_set_num_threads(usable_cores)

# -------------------------------------------------------------------------
# 🔐 Google authentication ------------------------------------------------
# -------------------------------------------------------------------------

googlesheets4::gs4_auth(TRUE)
googledrive::drive_auth(TRUE)

# -------------------------------------------------------------------------
# 📋 Environment info -----------------------------------------------------
# -------------------------------------------------------------------------

# Mostrar ruta del proyecto y número de núcleos
cat("Project directory: ", getwd(), "\n")
cat("CPU cores detected: ", total_cores, "\n")
cat("CPU cores in use: ", usable_cores, "\n")

# Información de sesión
sessioninfo::session_info() %>% print()

# -------------------------------------------------------------------------
# 🧹 Clean up -------------------------------------------------------------
# -------------------------------------------------------------------------

rm(cran_packages, load_or_install, total_cores, usable_cores)
