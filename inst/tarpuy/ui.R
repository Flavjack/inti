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
    tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/Flavjack/inti/master/inst/rticles/files/quipo4c.png")),

    meta() %>%
      meta_social(
        title = "Tarpuy",
        description = "Tarpuy helps to create experimental designs",
        url = "https://flavjack.shinyapps.io/tarpuy/",
        image = "https://raw.githubusercontent.com/Flavjack/inti/master/inst/rticles/files/quipo4c.png",
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
            <img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/rticles/files/quipo4c.png" style="height:50px" title="flozano"></a>
            <span style="display:block;"><small>lozanoisla.com</small></span>
            </p></div>

            </div>

                  ')

             ),

      column(2,

             HTML('<h1><a target="_blank" href="https://flavjack.shinyapps.io/tarpuy/">Tarpuy</a></h1>'),

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
                          , min = 2
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
                       , label = "Field book sheet"
                       , value = ""
                       , placeholder = "Design info"
                       ),

             textInput(inputId = "varlist_name"
                       , label = "Variables sheet"
                       , value = ""
                       , placeholder = "Optional"
             ),

             actionButton(inputId = "export_fb"
                          , label = "Export"
                          )

             ),

      column(8,

             br(),

              column(width = 12,

                    h4(icon("book"), "Google Sheets (URL)", width = "100%"),
                    textInput(inputId = "gsheet_url",
                              label = NULL,
                              width = "100%",
                              value = ""
                              , placeholder = "Insert google sheet link"
                              )

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

             HTML('

            <div id=footer style="width:100%; margin:auto;">

            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://www.youtube.com/playlist?list=PLSQMdOu57lj8XTyH5KUN9h-VL5TAEsaBC">
            <img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/tarpuy/files/youtube.png" style="height:60px" title="demo"></a>
            <span style="display:block;"><small>demo</small></span>
            </p></div>

            </div>

                  '),

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
