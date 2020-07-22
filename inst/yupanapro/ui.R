# yupanapro ---------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/yupanapro/

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
library(bootstraplib)

gar_set_client(web_json = "www/yupanapro.json")

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

bs_theme_new(version = "4+3", bootswatch = NULL)

navbarPage(title = HTML('<h3><strong><a target="_blank" href="https://flavjack.shinyapps.io/yupanapro/">Yupana</a></strong></h3>')
           , windowTitle = "Yupana 2.0"
           , theme = "bootstrap_sandstone.css"
           , selected = "Data",

           tabPanel("",

                    bootstrap(), # allow use the new bootstrap

                    tags$head(includeHTML(("www/analytics.html"))),
                    tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/Flavjack/inti/master/inst/rticles/files/quipo4c.png")),

                    meta() %>%
                      meta_social(
                        title = "Yupana",
                        description = "Yupana: platform for statistical data analysis",
                        url = "https://flavjack.shinyapps.io/tarpuy/",
                        image = "https://raw.githubusercontent.com/Flavjack/inti/master/inst/rticles/files/quipo4c.png",
                        image_alt = "quipolab.com"
                      )

           ),

           tabPanel("Data",

# data init ---------------------------------------------------------------
# -------------------------------------------------------------------------

                    fluidRow(

                      column(2,

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
                                          , min = 0
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
                                          , class = "btn btn-warning"
                             )

                      ),

                      column(width = 10,

                             column(width = 12,

                                    textInput(inputId = "gsheet_url",
                                              label = NULL,
                                              width = "100%",
                                              value = ""
                                              , placeholder = "Insert google sheet url"
                                    )

                             ),

                             shinydashboard::box(

                               status = "danger",
                               solidHeader = T,
                               width = 12,

                               htmlOutput("gsheet_preview"),

                             )

                      ),

                     )

# data - end --------------------------------------------------------------
# -------------------------------------------------------------------------

                    ),

           tabPanel("Analysis",

                    ),

           tabPanel("Graphics",


                    ),

           tabPanel("Demo",


                    ),

           tabPanel("About",


                    ),

            tabPanel( "",


            ),

           tabPanel(

             googleAuth_jsUI("js_token"
                             , login_class = "badge badge-danger"
                             , logout_class = "badge badge-success"
             )

           )

)
