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
           , selected = "Fieldbook",

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

           tabPanel("Fieldbook",

# Fieldbook summary -------------------------------------------------------
# -------------------------------------------------------------------------

                    fluidRow(

                      column(2,

                             textInput(inputId = "fbsmr_gsheet"
                                       , label = "Fieldbook sheet"
                                       , value = ""
                                       , placeholder = "Field book data"
                                       ),

                             uiOutput("last_factor"),

                             textInput(inputId = "model_facts"
                                       , label = "Model factors"
                                       , value = ""
                                       , placeholder = "block + factor1*factor2"
                                       ),

                             uiOutput("comp_facts"),

                             selectInput(inputId = "test_comp"
                                         , label = "Mean comparison test"
                                         , choices = c("SNK", "TUKEY", "DUNCAN")
                                         ),

                             numericInput(inputId = "sig_level"
                                          , label = "Significance level"
                                          , value = 0.05
                                          , step = 0.01
                                          , min = 0
                                          ),

                             actionButton(inputId = "fbsmr_generate"
                                          , label = "Generate"
                                          , class = "btn btn-warning"
                                          )

                      ),

                      column(width = 10,

                             column(width = 12,

                                    textInput(inputId = "fieldbook_url",
                                              label = NULL,
                                              width = "100%",
                                              value = "https://docs.google.com/spreadsheets/d/15uwCgQRtR01B3FJaZBE8t_0bOC_8Bbey9ccwVlZH0jg/edit#gid=56711214"
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
