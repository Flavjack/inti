# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/tarpuy/

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

pkgs_cran <- c(
  "shiny"
  , "miniUI"
  , "shinyFiles"
  , "utils"
  , "fs"
  , "metathis"
  , "googlesheets4"
  , "googledrive"
  , "dplyr"
)

installed_cran <- pkgs_cran %in% rownames(installed.packages())
if (any(installed_cran == FALSE)) {
  install.packages(pkgs_cran[!installed_cran])
}

pkgs_git <- c(
  "inti"
)

installed_git <- pkgs_git %in% rownames(installed.packages())
if (any(installed_git == FALSE)) {
  devtools::install_github("flavjack/inti", upgrade = "always")
}

invisible(lapply(c(pkgs_cran, pkgs_git), library, character.only = TRUE))
rm(pkgs_cran, installed_cran, pkgs_git, installed_git)

library(dplyr)
library(purrr)
library(shiny)
library(miniUI)
library(shinyFiles)
library(utils)
library(fs)
library(inti)
library(metathis)
library(googlesheets4)
library(googledrive)
library(dplyr)
library(purrr)

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

fluidPage(

    tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
    tags$head(HTML('<style>* {font-size: 100%; font-family: Roboto Mono;}</style>')),

    meta() %>%
      meta_social(
        title = "Tarpuy",
        description = "create field book designs easy",
        url = "https://flavjack.shinyapps.io/tarpuy/",
        image = "https://lozanoisla.com/img/quipo.png",
        image_alt = "quipolab.com"
      ),

    fluidRow(
      column(1,

             br(),
             br(),

             HTML('

            <div id=footer style="width:100%; margin:auto;">

            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://lozanoisla.com/">
            <img src="https://raw.githubusercontent.com/Flavjack/lozanoisla/master/static/android-chrome-512x512.png?token=AB3ARRI5E4ZF7FLXM6CDQ7S7CHJ3K" style="height:50px" title="flozano"></a>
            <span style="display:block;"><small>lozanoisla.com</small></span>
            </p></div>

            </div>

                  ')

             ),

      column(2,

             HTML('<h1><a target="_blank" href="https://flavjack.shinyapps.io/tarpuy/">Tarpuy</a></h1>'),

             br(),

             numericInput(
               inputId = "nFactors"
               , label = "Factors"
               , value = 1
               , max = 5
               , min = 1
             ),

             uiOutput("design_type"),

             numericInput(inputId = "rep"
                          , label = "Replications"
                          , value = 2
                          , min = 1
                          ),

             numericInput(inputId = "serie"
                          , label = "Plot digits"
                          , value = 2
                          , max = 3
                          , min = 1
                          ),

             numericInput(inputId = "seed"
                          , label = "Seed"
                          , value = 0
                          ),

             textInput(inputId = "gsheet_name"
                       , label = "Gsheet name"
                       , value = "tarpuy"
                       ),

             actionButton(inputId = "export_fb"
                          , label = "Export"
                          )

             ),

      column(8,

             br(),

             column(width = 12,

                    h4(icon("book"), "Google SpreadSheet (URL)", width = "100%"),
                    textInput(inputId = "gsheet_url",
                              label = NULL,
                              width = "100%",
                              value = "https://docs.google.com/spreadsheets/d/1ilw0NHT7mihaM-3U48KzkuMt927xe8ukX6rNuIw2fT0/edit#gid=0")

             ),

             shinydashboard::box(

               status = "danger",
               solidHeader = T,
               width = 12,

               htmlOutput("gsheet_preview"),
               br()

             )


             ),

      column(1,

             br(),
             br(),

             HTML('

            <div id=footer style="width:100%; margin:auto;">

            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://www.quipolab.com/">
            <img src="https://lh3.googleusercontent.com/ovLKu9fnNmUfk4cG81KpwnL0w7tqZawlR93xzrNDCJRXYFZu6_uREKTgRI5u43N-pHe3Z6dt=w16383" style="height:50px" title="quipo"></a>
            <span style="display:block;"><small>quipolab.com</small></span>
            </p></div>

            </div>

                  ')

             )
      )
  )

# https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
