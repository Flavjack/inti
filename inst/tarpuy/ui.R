# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/tarpuy/

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/spreadsheets"))

if (file.exists("setup.r")) { source("setup.r") }

library(shiny)
library(inti)
library(metathis)
library(tidyverse)
library(googlesheets4)
library(googleAuthR)

gar_set_client(web_json = "files/tarpuy.json")

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

fluidPage(

    tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
    tags$head(HTML('<style>* {font-size: 100%; font-family: Roboto Mono;}</style>')),
    tags$head(includeHTML(("files/analytics.html"))),

    meta() %>%
      meta_social(
        title = "Tarpuy",
        description = "Tarpuy helps to create experimental designs",
        url = "https://flavjack.shinyapps.io/tarpuy/",
        image = "https://raw.githubusercontent.com/Flavjack/lozanoisla/master/static/android-chrome-512x512.png?token=AB3ARRI5E4ZF7FLXM6CDQ7S7CHJ3K",
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
                       , value = ""
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
                              value = "")

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

             googleAuth_jsUI("js_token"),

             br(),
             br(),
             br(),
             br(),

             HTML('

            <div id=footer style="width:100%; margin:auto;">

            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://lozanoisla.com/">
            <img src="files/youtube.png" style="height:50px" title="demo"></a>
            <span style="display:block;"><small>demo</small></span>
            </p></div>

            </div>

                  '),

             br(),
             br(),

             HTML('

            <div id=footer style="width:100%; margin:auto;">

            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://www.quipolab.com/">
            <img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/tarpuy/files/tarpuy.jpeg" style="height:80px" title="quipo"></a>
            <span style="display:block;"><small>quipolab.com</small></span>
            </p></div>

            </div>

                  ')

             )
      )
  )
