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
library(shinydashboard)

gar_set_client(web_json = "www/yupanapro.json")

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

bs_theme_new(version = "4+3", bootswatch = NULL)

navbarPage(title = HTML('<h3><strong><a target="_blank" href="https://flavjack.shinyapps.io/yupanapro/">Yupana</a></strong></h3>')
           , windowTitle = "Yupana 2.0"
           , theme = "bootstrap_sandstone.css"
           , selected = "Analysis",

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

 # Yupana Info -------------------------------------------------------------
 # -------------------------------------------------------------------------

           tabPanel("Intro",


                    fluidRow(

                      column(width = 1,

                             br(),
                             br(),

                             HTML('
            <div id=footer style="width:100%; margin:auto;">
            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://lozanoisla.com/">
            <img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/rticles/www/quipo4c.png" style="height:50px" title="flozano"></a>
            <span style="display:block;"><small>lozanoisla.com</small></span>
            </p></div>
            </div>
                  ')

                      ),

                      column(width = 10,


                             box(title = h4(icon("book"), "Fieldbook Google Sheets (URL)")
                                 , width = 10
                                 , solidHeader = T
                                 , status = "primary",

                                 textInput(inputId = "fieldbook_url",
                                           label = NULL
                                           , width = "100%"
                                           , value = "https://docs.google.com/spreadsheets/d/15uwCgQRtR01B3FJaZBE8t_0bOC_8Bbey9ccwVlZH0jg/edit#gid=56711214"
                                           , placeholder = "Insert google sheet link"
                                 )

                             ),

                             box(title = "Info fieldbook"
                                 , width = 2
                                 , solidHeader = T,

                                 textInput(inputId = "fieldbook_gsheet"
                                           , label = "Fieldbook sheet"
                                           , value = "fb"
                                           , placeholder = "Fieldbook data"
                                 ),


                                 textInput(inputId = "fbsmrvars_gsheet"
                                           , label = "Fieldbook summary"
                                           , value = "fbsm"
                                           , placeholder = "Variables information"
                                 )

                             ),

                             box(title = "Auth"
                                 , width = 2
                                 , solidHeader = T
                                 , status = "primary",

                                 googleAuth_jsUI("js_token")

                             ),

                      ),

                      column(1,

                             br(),
                             br(),

                             HTML('
            <div id=footer style="width:100%; margin:auto;">
            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://www.youtube.com/playlist?list=PLSQMdOu57lj8XTyH5KUN9h-VL5TAEsaBC">
            <img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/tarpuy/www/youtube.png" style="height:60px" title="demo"></a>
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
            <img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/tarpuy/www/tarpuy.jpeg" style="height:80px" title="quipo"></a>
            <span style="display:block;"><small>quipolab.com</small></span>
            </p></div>
            </div>
                  ')

                      )

                    )


           ),


           tabPanel("Fieldbook",

# Yupana Fieldbook --------------------------------------------------------
# -------------------------------------------------------------------------

                    fluidRow(

                      column(2,

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

                             shinydashboard::box(

                               status = "danger",
                               solidHeader = T,
                               width = 12,

                               htmlOutput("fieldbook_preview"),

                             )

                      ),

                     )

# Yupana Analysis ---------------------------------------------------------
# -------------------------------------------------------------------------

                    ),

            tabPanel("Analysis",


                     fluidRow(

                       column(2,

                              radioButtons(inputId = "rpt_preview_opt"
                                           , label = "Modules"
                                           , choices = c("Gsheet"
                                                         , "Model"
                                                         , "Plots")
                                           , inline = TRUE

                              ),

                              uiOutput("rpt_variable"),

                              uiOutput("rpt_dotplot_groups"),

                              actionButton(inputId = "export_mctab"
                                           , label = "Export table"
                                           , class = "btn btn-warning"
                              )

                              ),


                       column(width = 10,

                              shinydashboard::box(

                                status = "danger",
                                solidHeader = T,
                                width = 12,

                                htmlOutput("rpt_preview"),

                              )

                       ),

                     )

# Yupana Graphics ---------------------------------------------------------
# -------------------------------------------------------------------------

            ),

           tabPanel("Graphics",


                    fluidRow(

                      column(2,

                             radioButtons(inputId = "grp_preview_opt"
                                          , label = "Modules"
                                          , choices = c("Gsheet"
                                                        , "Plots")
                                          , inline = TRUE

                             ),

                             textInput(inputId = "grp_gsheet"
                                       , label = "Graph sheet"
                                       , value = "plot"
                                       , placeholder = "Table for graph"
                             ),


                             radioButtons(inputId = "grp_type"
                                          , label = "Graph type"
                                          , choices = c("bar"
                                                        , "line"
                                                        )
                                          , inline = TRUE

                             ),

                             uiOutput("grp_xvar"),

                             uiOutput("grp_ybar"),

                             uiOutput("grp_gvar"),

                             selectInput(inputId = "grp_legend"
                                         , label = "Legend position"
                                         , choices = c("top"
                                                       , "left"
                                                       , "right"
                                                       , "bottom"
                                                       , "none"
                                                       )
                             ),

                             actionButton(inputId = "graph_create"
                                          , label = "Create Graph"
                                          , class = "btn btn-warning"
                             )

                      ),


                      column(width = 10,

                             shinydashboard::box(

                               status = "danger",
                               solidHeader = T,
                               width = 12,

                               htmlOutput("graph_preview"),

                             )

                      ),

                    )


                )

)


# Yupana end code ---------------------------------------------------------
# -------------------------------------------------------------------------
