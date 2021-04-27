# -------------------------------------------------------------------------
# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/yupanapro/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-04-27
# -------------------------------------------------------------------------

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
  library(cowplot)
  library(corrplot)
  library(tidyverse)
  library(FactoMineR)
})

# -------------------------------------------------------------------------
# message -----------------------------------------------------------------
# -------------------------------------------------------------------------

head <- colortext(
  "
# -------------------------------------------------------------------------
# ReadMe ------------------------------------------------------------------
# -------------------------------------------------------------------------
  ", "light red"
)

end <- colortext(
  "
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
  ", "light red"
)

message(
  head
  , "\n"
  , colortext("- ")
  , colortext("If is the first time running the app consider install the dependencies:")
  , "\n\n"
  , colortext("> ")
  , colortext("inti::yupana(dependencies = TRUE)", "green")
  , "\n\n"
  , colortext("- ")
  , colortext("More info: ")
  , colortext("https://inkaverse.com/articles/apps", "blue")
  , "\n"
  , end
)