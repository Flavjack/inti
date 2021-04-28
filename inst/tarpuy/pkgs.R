# -------------------------------------------------------------------------
# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/tarpuy/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2020-03-24
# -------------------------------------------------------------------------

suppressPackageStartupMessages({library(inti)})

# -------------------------------------------------------------------------
# message -----------------------------------------------------------------
# -------------------------------------------------------------------------

head <- colortext(
  "
# -------------------------------------------------------------------------
# ReadMe ------------------------------------------------------------------
# -------------------------------------------------------------------------
  "
)

end <- colortext(
  "
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
  "
)

message(
  head
  , "\n"
  , colortext("- ")
  , colortext("If is the first time running the app consider install the dependencies:")
  , "\n\n"
  , colortext("> ")
  , colortext("inti::tarpuy(dependencies = TRUE)", "green")
  , "\n\n"
  , colortext("- ")
  , colortext("More info: ")
  , colortext("https://inkaverse.com/articles/apps", "blue")
  , "\n"
  , end
)

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

suppressPackageStartupMessages({
library(inti)
library(bslib)
library(metathis)
library(googlesheets4)
library(googleAuthR)
library(shinydashboard)
library(tidyverse)
library(stringi)
})

