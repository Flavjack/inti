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
           , position = "fixed-top"
           , theme = "bootstrap_sandstone.css"
           , selected = "Intro",

           tabPanel("",

                    bootstrap(), # allow use the new bootstrap

                    tags$head(includeHTML(("www/analytics.html"))),
                    tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/Flavjack/inti/master/inst/rticles/www/quipo4c.png")),

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


                      column(width = 3,


                             box(title = "Presentacion"
                                 , solidHeader = T
                                 , background = "green"
                                 , width = 12
                                 , status = "primary",

                                 HTML('

                                 Yupana es una plataforma interactiva para el análisis y gráfica de datos
                                 de diseños experimentales. Esta desarrollada con la finalidad de promover
                                 las buenas prácticas en la colecta, análisis y manipulación de datos.

                                 Yupana tiene el objetivo de "continuidad" entre el uso de la app y el software
                                 estadictico R. Los resultados pueden ser reproducidos en cualquiera de las 2 plataformas.

                                      '),
                             ),

                             br(),

                             box(title = "Caracteristicas"
                                 , solidHeader = T
                                 , background = "green"
                                 , width = 12
                                 , status = "primary",

                                 HTML('

                                 Yupana esta pensando en la reproducción de los resultados. Por lo que cada
                                 análisis realizado puede almacenarse en la hojas de cálculo privadas de cada
                                 usuario, permitiendo acceder a ellos en cualquier momento.

                                 La app esta basada en el paquete inti desarrollado en sofware estadistico R.
                                 Los mismo resultados pueden ser producidos en R usando los output de yupana.

                                 Yupana ademas permite:

                                 1. Estadistica descriptiva.
                                 2. Pruebas de comparación de medias
                                 3. Tablas con resumen de los datos.
                                 4. Gráficos de diagnostico de los modelos.
                                 5. Gráficos interactivo con opciones de personalización.

                                      '),
                             ),

                             br(),

                             box(title = "Info"
                                 , solidHeader = T
                                 , background = "green"
                                 , width = 12
                                 , status = "primary",

                                 HTML('

                                 Actualmente esta versión de Yupana está en desarrollo.
                                 Por lo que la interface y las opciones iran cambiando.
                                 La app esta basada en el paquete inti (Tools and statistical procedures
                                 for experimentals designs and plant breeding).

                                 https://github.com/Flavjack/inti

                                      '),
                             ),


                             br(),
                             br(),


                      ),

                      column(width = 7,

                             fluidRow(

                               box(title = h4(icon("google"), "Fieldbook Google Sheets (URL)")
                                   , width = 12
                                   , solidHeader = T
                                   , status = "primary",

                                   textInput(inputId = "fieldbook_url",
                                             label = NULL
                                             , width = "100%"
                                             , value = "https://docs.google.com/spreadsheets/d/15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=172957346"
                                             , placeholder = "Insert google sheet link"
                                   )

                               ),

                             ),


                             fluidRow(

                               box(title = h5("Fieldbook data")
                                   , width = 4
                                   , solidHeader = T,

                                   textInput(inputId = "fieldbook_gsheet"
                                             , label = NULL
                                             , value = "fb"
                                             , placeholder = "Sheet name ('fb')"
                                   )
                               ),

                               box(title = h5("Fieldbook summary")
                                   , width = 4
                                   , solidHeader = T,

                                   textInput(inputId = "fbsmrvars_gsheet"
                                             , label = NULL
                                             , value = "fbsm"
                                             , placeholder = "Sheet name ('fbsm')"
                                   )
                               ),

                               box(title =  h5("Gsheet access")
                                   , width = 4
                                   , solidHeader = T
                                   , status = "primary",

                                   googleAuth_jsUI("js_token")

                               )
                             ),

                             br(),

                             fluidRow(

                               box(title = "Cómo usar yupana?"
                                   , width = 6
                                   , solidHeader = T
                                   , height = "200px"
                                   , status = "primary",


                                   HTML('

                                 Para usar yupana es necesario que tus datos esten en una hoja de calculo de google.

                                 1. Introduce el URL de tu documento.

                                 2. Coloca el nombre de la hoja donde está tu base de datos.

                                 3. Coloca el nombre de la hoja donde donde esta el resumen de tus datos.
                                 Si no lo tiene aún, puedes generarla en la pestaña "Fieldbook" de la app.

                                 4. Debes dar los permisos para editar las hojas(LOG IN); ya que tu documento es privado
                                 y para poder leer y exportar la información debes dar los permisos correspondientes.
                                 Más información en la politicas de privacidad: https://lozanoisla.com/policy/

                                 5. Cuando hagas des los permisos el boton de "LOG IN" cambiara a color rojo.
                                 Lo que te permite interactuar con tu información y analizar tus datos.

                                 6. Cualquier problema o sugerencia puedes escribir en el rastreador de problemas.
                                 https://github.com/Flavjack/inti/issues

                                      '),

                               ),

                               box(title = "Recomendaciones"
                                   , width = 6
                                   , solidHeader = T
                                   , height = "200px"
                                   , status = "primary",


                                   HTML('

                                   1. Antes de iniciar a usar yupana, es bueno que guardes una copia de seguridad de tú trabajo.
                                   Para eso debes ir a tú hoja de cálculo: Historial de versiones > Asignar un nombre a la
                                   versión actual. Con eso podras tener multiples copias de tu base de datos, sin crear
                                   multiples documentos.

                                   2. Es recomendable solo tener una libro de campo "fieldbook" por cada experimento.
                                   Si tienes muchas pestañas que dificultan tú trabajo, puedes ir ocultandolas
                                   (click derecho en la pestaña > Ocultar hoja). Al momento de usar la app puedes
                                   especificar que hoja deseas utilizar y la app extrae la información de la hoja indicada.

                                   3. Si deseas crear un diseño experimental que luego puede ser usado de forma rápida en
                                   yupana, puedes usar la app Tarpuy (https://flavjack.shinyapps.io/tarpuy/).

                                   4. Mayor información: https://lozanoisla.com/projects/

                                      '),


                               )

                             ),


                             ),


                      column(1,

                             br(),
                             br(),

                             HTML('
            <div id=footer style="width:100%; margin:auto;">
            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://www.youtube.com/playlist?list=PLSQMdOu57lj9sTx5Dbff9O0g6KCU4pwCQ">
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
            <img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/yupanapro/www/yupana.png" style="height:80px" title="quipo"></a>
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

                               htmlOutput("fieldbook_preview"),

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

                              uiOutput("rpt_dotplot_groups")

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

                             uiOutput("graph_sheets"),

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
