# -------------------------------------------------------------------------
# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/tarpuy/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2026-06-14
# -------------------------------------------------------------------------

# `cli` is intentionally used with explicit namespace qualification.
# TARPUY checks that the package is installed in pkgs.R, but it does not need
# to attach it to the global search path merely to print these startup notes.
tarpuy_cli_div <- cli::cli_div(
  theme = list(
    h1 = list(
      color = "red",
      "font-weight" = "bold"
    )
  )
)

cli::cli_h1("ReadMe")
cli::cli_rule()

cli::cli_alert_info("More info:")
cli::cli_alert("{.blue https://inkaverse.com/}")

cli::cli_alert_info("Citation:")
cli::cli_alert("{.green https://inkaverse.com/authors.html#citation}")

cli::cli_h1("Tarpuy")
cli::cli_rule()

cli::cli_end(tarpuy_cli_div)
rm(tarpuy_cli_div)