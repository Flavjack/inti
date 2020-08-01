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
library(ggpubr)
library(FactoMineR)
library(corrplot)

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

                                 <p>
                                 Yupana es una plataforma interactiva para el análisis y gráfica de datos de diseños experimentales.
                                 Está desarrollada con la finalidad de promover las buenas prácticas en la colecta, análisis y manipulación de datos.
                                 Yupana tiene el objetivo de "continuidad" entre el uso de la aplicación y el software estadístico R.
                                 Yupana está basada en el paquete <strong><em>inti</em></strong>:
                                 “<em>Tools and statistical procedures for experimentals designs and plant breeding</em>”. </p>

                                      '),
                             ),

                             box(title = "Caracteristicas"
                                 , solidHeader = T
                                 , background = "green"
                                 , width = 12
                                 , status = "primary",

                                 HTML('

                                  <p>
                                  Yupana está pensando en la reproductibilidad de los resultados.
                                  Los análisis realizados se almacenará en la hojas de cálculo privadas de cada usuario.
                                  Los resultados de yupana pueden ser usados en R o viceversa. Yupana además permite:
                                  </p>
                                  <ul>

                                  <li>Estadística descriptiva y con resumen de los datos.

                                  <li>Pruebas de comparación de medias.

                                  <li>Gráficos de diagnóstico de los modelos.

                                  <li>Gráficos interactivo con opciones de personalización.

                                  <li>Análisis multivariados
                                  </li>
                                  </ul>

                                      '),
                             ),

                      ),

                      column(width = 7,

                             fluidRow(

                               box(title = div(h4(icon("google"), "Fieldbook Google Sheets (URL)"), align = "center")
                                   , width = 12
                                   , solidHeader = T
                                   , status = "primary",

                                   textInput(inputId = "fieldbook_url",
                                             label = NULL
                                             , width = "100%"
                                             , value = ""
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
                                             , value = ""
                                             , placeholder = "Sheet name (fb)"
                                   )
                               ),

                               box(title = h5("Fieldbook summary")
                                   , width = 4
                                   , solidHeader = T,

                                   textInput(inputId = "fbsmrvars_gsheet"
                                             , label = NULL
                                             , value = ""
                                             , placeholder = "Sheet name (fbsm)"
                                   )
                               ),

                               box(width = 1),

                               box(title = div(h4(icon("key")), align = "center")
                                   , width = 2,

                                   div(
                                     googleAuth_jsUI("js_token")
                                     , align = "center")

                               ),

                               box(width = 1)

                             ),

                             br(),

                             fluidRow(

                               box(title = "Cómo usar yupana?"
                                   , width = 6
                                   , solidHeader = T
                                   , height = "200px"
                                   , status = "primary",


                                   HTML('

                                    <p>
                                    Para usar yupana es necesario que tus datos esten en una hoja de cálculo de google.
                                    </p>
                                    <ol>

                                    <li>Introduce el URL de tu documento y coloca el nombre de la hoja donde está tu base de datos
                                    (información mínima requerida para usar la app).

                                    <li>Introduce el nombre de la hoja donde donde esta el resumen de tus datos.
                                    Si no tiene aún, puedes generarla en la pestaña "Fieldbook" de la app.

                                    <li>Debes dar los permisos para editar las hojas haciendo “LOG IN”;
                                    ya que la app requiere los permisos correspondientes para leer y exportar la información generada.
                                    Más información en la politicas de privacidad: <a href="https://lozanoisla.com/policy/">https://lozanoisla.com/policy/</a>

                                    <li>Cuando des los permisos el botón de "LOG IN" cambiará a color rojo “LOG OUT”.
                                    Lo que te permitirá interactuar con tu información y analizar tus datos.

                                    <li>Cualquier problema o sugerencia puedes escribir en el rastreador de problemas.
                                    <a href="https://github.com/Flavjack/inti/issues">https://github.com/Flavjack/inti/issues</a>
                                    </li>
                                    </ol>

                                      '),

                               ),

                               box(title = "Recomendaciones"
                                   , width = 6
                                   , solidHeader = T
                                   , height = "200px"
                                   , status = "primary",


                                   HTML('

                                  <p>
                                  Antes de iniciar a usar yupana ten en cuenta las siguientes recomendaciones.
                                  </p>
                                  <ul>

                                  <li>Crea una copia de seguridad de tu documento.
                                  Vé a tu hoja de cálculo: <em>Archivo > Historial de versiones > Asignar un nombre a la versión actual</em>.
                                  De esa manera puedes crear múltiples copias de seguridad de tu base de datos, sin crear múltiples documentos.

                                  <li>Es recomendable solo tener una libro de campo "fieldbook" por cada experimento.
                                  Si tienes muchas pestañas que dificultan tu trabajo, puedes ir ocultandolas (<em>click derecho en la pestaña > Ocultar hoja</em>).
                                  Al momento de usar la app puedes especificar qué hoja deseas utilizar y la app extrae la información de la hoja indicada.

                                  <li>Si deseas crear un diseño experimental que luego puede ser usado de forma rápida en yupana,
                                  puedes usar la app Tarpuy: <a href="https://flavjack.shinyapps.io/tarpuy/">https://flavjack.shinyapps.io/tarpuy/</a>
                                  </li>
                                  </ul>
                                      '),
                               )
                             ),
                             ),


                      column(1,

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
              <a target="_blank" href="https://flavjack.shinyapps.io/tarpuy/">
              <img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/tarpuy/www/tarpuy.jpeg" style="height:80px" title="tarpuy"></a>
              <span style="display:block;"><small>Tarpuy</small></span>
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
            <span style="display:block;"><small>quipolab</small></span>
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

                              uiOutput("rpt_dotplot_groups"),

                              actionButton(inputId = "fbsm_refresh"
                                           , label = "Refresh"
                                           , class = "btn btn-success"
                              )

                              ),


                       column(width = 10,

                                htmlOutput("rpt_preview")

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

                             numericInput(inputId = "graph_width"
                                          , label = "Width (cm)"
                                          , value = 15
                                          , step = 5
                                          , min = 5
                             ),

                             numericInput(inputId = "graph_height"
                                          , label = "Height (cm)"
                                          , value = 10
                                          , step = 5
                                          , min = 5
                             ),

                             numericInput(inputId = "graph_dpi"
                                          , label = "Resolution"
                                          , value = 100
                                          , step = 50
                                          , min = 100
                             ),

                             actionButton(inputId = "graph_create"
                                          , label = "Create"
                                          , class = "btn btn-info"
                             )

                      ),


                      column(width = 10,

                               htmlOutput("graph_preview")

                      ),

                    )


                ),

        tabPanel("Multivariate",


                 fluidRow(

                   column(2,

                          radioButtons(inputId = "mvr_module"
                                       , label = "Modules"
                                       , choices = c("PCA"
                                                     , "HCPC"
                                                     , "CORR"
                                                     )
                                       , inline = TRUE

                          ),

                          uiOutput("mvr_facts"),

                          uiOutput("mvr_groups"),

                          numericInput(inputId = "mvr_width"
                                       , label = "Width (cm)"
                                       , value = 15
                                       , step = 5
                                       , min = 5
                          ),

                          numericInput(inputId = "mvr_height"
                                       , label = "Height (cm)"
                                       , value = 15
                                       , step = 5
                                       , min = 5
                          ),

                          numericInput(inputId = "mvr_dpi"
                                       , label = "Resolution"
                                       , value = 100
                                       , step = 50
                                       , min = 100
                          ),

                          actionButton(inputId = "mvr_refresh"
                                       , label = "Refresh"
                                       , class = "btn btn-success"
                          )
                   ),


                   column(width = 10,

                          htmlOutput("mvr_preview")

                   ),

                  br()

                 )


        )


# Yupana end code ---------------------------------------------------------
# -------------------------------------------------------------------------


)


