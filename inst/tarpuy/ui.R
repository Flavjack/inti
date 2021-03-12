# -------------------------------------------------------------------------
# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/tarpuy/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-02-17
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

#> devtools::install_github("flavjack/inti")

if (file.exists("setup.R")) { source("setup.R") }

  # library(shiny)
  # library(inti)
  # library(metathis)
  # library(tidyverse)
  # library(googlesheets4)
  # library(googleAuthR)
  # library(bslib)
  # library(shinydashboard)
  # library(stringi)

options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/spreadsheets"
                                          , "https://www.googleapis.com/auth/userinfo.email"
                                          ))
options(gargle_oob_default = TRUE)
options(shiny.port = 1221)

if (file.exists("www/cloud.json")) gar_set_client(web_json = "www/cloud.json", activate = "web")

# -------------------------------------------------------------------------
# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

navbarPage(title = HTML('<h3><strong><a target="_blank" href="https://inkaverse.com/">Tarpuy</a></strong></h3>')
           , windowTitle = "Tarpuy • app"
           , position = "fixed-top"
           , selected = "Intro"
           , theme = "bootstrap_sandstone.css"  #!
           , 

# -------------------------------------------------------------------------
# Yupana Info -------------------------------------------------------------
# -------------------------------------------------------------------------

           tabPanel("Intro"
                    
                    , bs_theme_dependencies("flatly") #!
                    
                    , includeCSS("www/custom.css")
                    , tags$head(includeHTML(("www/analytics.html")))
                    , tags$head(tags$link(rel="shortcut icon"
                                          , href="https://flavjack.github.io/inti/reference/figures/tarpuy.png")),
                    
                    meta() %>%
                      meta_social(
                        title = "Tarpuy",
                        description = "Tarpuy: Easy way to create fieldbook experimental designs.",
                        url = "https://flavjack.shinyapps.io/tarpuy/",
                        image = "https://flavjack.github.io/inti/reference/figures/tarpuy.png",
                        image_alt = "inkaverse.com"
                        ),
                    
                    fluidRow(

                      column(width = 1,

                             HTML('
            <div id=footer style="width:100%; margin:auto;">
            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://flavjack.github.io/inti/index.html">
            <img src="https://flavjack.github.io/inti/reference/figures/biologia.png" style="height:50px" title="flozano"></a>
            <span style="display:block;"><small>project</small></span>
            </p></div>
            </div>
                  ')

                      ),

                      column(width = 3,

                             box(title = "Presentación"
                                 , solidHeader = T
                                 , background = "green"
                                 , width = 12
                                 , status = "primary",

                                 HTML('
                                 <p>
                                 Tarpuy es una plataforma interactiva para el planeamiento de experimentos (PLEX).
                                 Está desarrollada con la finalidad de promover las buenas prácticas en la colecta, análisis y manipulación de datos.
                                 Tarpuy tiene el objetivo de "continuidad" entre el uso de la aplicación y el software estadístico R.
                                 Tarpuy está basada en el paquete <strong><em>inti</em></strong>:
                                 “<em>Tools and statistical procedures for experimentals designs and plant breeding</em>”.
                                      </p>
                                      '),
                             ),

                             box(title = "Características"
                                 , solidHeader = T
                                 , background = "green"
                                 , width = 12
                                 , status = "primary",

                                 HTML('
                                 <p>
                                 Tarpuy está pensando en la elaboración de proyectos experimentales de forma rápida e intuitiva.
                                 Los resultados se almacenará en la hojas de cálculo privadas de cada usuario.Tarpuy además permite:
                                 </p>
                                 <ul>
                                 <li>Genera la información mínima del proyecto de investigación.
                                 <li>Generación de libreta de campo (fieldbook) para colecta de datos.
                                 <li>Diseños de campo para su establecimiento.
                                 <li>Lista de variables a evaluar en los experimentos.
                                 </li>
                                 </ul>
                                      '),
                             ),

                      ),

                      column(width = 7,

                             fluidRow(
                               
                               box(title = div(h4(icon("key")), align = "right")
                                   , width = 1,

                                   div(
                                     
                                     uiOutput("login")
                                     
                                     , align = "center")

                               ),

                               box(title = div(h4(icon("google"), "Fieldbook Google Sheets (URL)"), align = "center")
                                   , width = 11
                                   , solidHeader = T
                                   , status = "primary",

                                   textInput(inputId = "fieldbook_url",
                                             label = NULL,
                                             width = "100%",
                                             value = ""
                                             , placeholder = "Insert google sheet link"
                                   )

                               ),

                             ),


                             fluidRow(

                               box(title = h5("Info")
                                   , width = 2
                                   , solidHeader = T,

                                   textInput(inputId = "gsheet_info"
                                             , label = NULL
                                             , value = "info"
                                             , placeholder = "Sheet name"
                                   )
                               ),

                               box(title = h5("Design")
                                   , width = 2
                                   , solidHeader = T,

                                   textInput(inputId = "gsheet_design"
                                             , label = NULL
                                             , value = "dsg"
                                             , placeholder = "Sheet name"
                                   ),

                               ),

                               box(title = h5("Varibles")
                                   , width = 2
                                   , solidHeader = T,

                                   textInput(inputId = "gsheet_varlist"
                                             , label = NULL
                                             , value = "var"
                                             , placeholder = "Sheet name"
                                   ),

                               ),

                               box(title = h5("Fieldbook")
                                   , width = 2
                                   , solidHeader = T,

                                   textInput(inputId = "gsheet_fb"
                                             , label = NULL
                                             , value = "fb"
                                             , placeholder = "Sheet name"
                                   ),

                               ),

                               box(title = div(h5("Create GS"), align = "center")
                                   , width = 2
                                   , solidHeader = T,

                                   div(


                                     actionButton(inputId = "create_sheet"
                                                  , label =  "Create"
                                                  , class = "btn btn-success"
                                                  )

                                     , align = "center")


                                   ),

                               box(title = div(h5("Open GS"), align = "center")
                                       , width = 2
                                       , solidHeader = T,

                                   div(

                                     uiOutput("open_url")

                                    , align = "center")

                             ),

                             ),

                             br(),

                             fluidRow(

                               box(title = "Cómo usar Tarpuy?"
                                   , width = 6
                                   , solidHeader = T
                                   , height = "200px"
                                   , status = "primary",


                                   HTML('
                                   <p>
                                   Para usar Tarpuy es necesario tener una hoja de cálculo de google vacía.
                                   </p>
                                   <ol>
                                   <li>Introduce el URL de tu documento de google spreadsheets.
                                   <li>Introduce el nombre de la hoja donde donde se creará/cargará la información.
                                   <li>Debes dar los permisos para editar las hojas haciendo “LOG IN”;
                                   ya que la app requiere los permisos correspondientes para leer y exportar la información generada.
                                   Más información en la politicas de privacidad: <a href="https://inkaverse.com/articles/policy"> https://inkaverse.com/articles/policy</a>
                                   <li>Cuando des los permisos el botón de "LOG IN" cambiará a color rojo “LOG OUT”.
                                   Lo que te permitirá interactuar con tu información y analizar tus datos.
                                   <li>Cualquier problema o sugerencia puedes escribir en el rastreador de problemas.
                                   <a href="https://github.com/Flavjack/inti/issues">https://github.com/flavjack/inti/issues</a>
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
                                  Antes de iniciar a usar Tarpuy ten en cuenta las siguientes recomendaciones.
                                  </p>
                                  <ul>
                                  <li>Si tu hoja ya contiene información. Crea una copia de seguridad de tu documento.
                                  Vé a tu hoja de cálculo: <em>Archivo > Historial de versiones > Asignar un nombre a la versión actual</em>.
                                  De esa manera puedes crear múltiples copias de seguridad de tu base de datos, sin crear múltiples documentos.
                                  <li>Es recomendable solo tener una libro de campo "fieldbook" por cada experimento.
                                  Si tienes muchas pestañas que dificultan tu trabajo, puedes ir ocultandolas (<em>click derecho en la pestaña > Ocultar hoja</em>).
                                  Al momento de usar la app puedes especificar qué hoja deseas utilizar y la app extrae la información de la hoja indicada.
                                  <li>Si deseas analizar los datos experimentales de tu proyecto, puedes usar la app Yupana
                                  (<a href="https://flavjack.shinyapps.io/yupanapro/">https://flavjack.shinyapps.io/yupanapro/</a>)
                                  </li>
                                  </ul>
                                  <p>
                                      '),
                               )
                             ),
                      ),


                      column(1,

                              HTML('

                             <div id=footer style="width:100%; margin:auto;">

                             <div style="display:inline-block; width:100%">
                             <p style="text-align:center">
                             <a target="_blank" href="https://www.youtube.com/playlist?list=PLSQMdOu57lj8XTyH5KUN9h-VL5TAEsaBC">
                             <img src="https://flavjack.github.io/inti/reference/figures/youtube.png" style="height:60px" title="demo"></a>
                             <span style="display:block;"><small>demo</small></span>
                             </p></div>

                             </div>

                                   '),

                              br(),

                              HTML('

                             <div id=footer style="width:100%; margin:auto;">

                             <div style="display:inline-block; width:100%">
                             <p style="text-align:center">
                             <a target="_blank" href="https://flavjack.shinyapps.io/yupanapro/">
                             <img src="https://flavjack.github.io/inti/reference/figures/yupana.png" style="height:80px" title="yupana"></a>
                             <span style="display:block;"><small>Yupana</small></span>
                             </p></div>

                             </div>

                                   ')

                      )

                    )


           ),

           tabPanel("Plex",

                    # Yupana Fieldbook --------------------------------------------------------
                    # -------------------------------------------------------------------------

                    fluidRow(

                      column(width = 2,

                             checkboxGroupInput(inputId = "plex_fields"
                                                , label = h5(icon("file-signature"), "Fieldbook fields")
                                                , choices = c("manager"
                                                              , "location"
                                                              , "dates"
                                                              , "about"
                                                              , "environment"
                                                              , "institutions"
                                                              , "researchers"
                                                              , "altitude"
                                                              , "georeferencing"
                                                              , "fieldbook"
                                                              , "album"
                                                              , "github"
                                                              )
                                                , selected = c("manager"
                                                               , "location"
                                                               , "dates"
                                                               , "about"
                                                               , "environment"
                                                               )
                             ),

                             textInput(inputId = "plex_logbook"
                                       , label = "Logbook (optinal)"
                                       , width = "100%"
                                       , value = NA
                                       , placeholder = "sheet name"
                             ),

                             textInput(inputId = "plex_timetable"
                                       , label = "Timetable (optional)"
                                       , width = "100%"
                                       , value = NA
                                       , placeholder = "sheet name"
                             ),

                             textInput(inputId = "plex_budget"
                                       , label = "Budget (optional)"
                                       , width = "100%"
                                       , value = NA
                                       , placeholder = "sheet name"
                             ),

                             actionButton(inputId = "plex_generate"
                                          , label = "Generate"
                                          , class = "btn btn-success"
                             )

                             ),

                      column(width = 3,

                             h5(icon("feather-alt"), "Information"),

                             # conditional panel "%in%" --> .includes("values")

                             conditionalPanel(condition =  ' input["plex_fields"].includes("manager") ',

                                              textInput(inputId = "plex_manager"
                                                        , label = "Project manager"
                                                        , width = "100%"
                                                        , value = NA
                                              )

                             ),

                             conditionalPanel(condition = ' input["plex_fields"].includes("location") ',

                                              textInput(inputId = "plex_location"
                                                        , label = "Location"
                                                        , width = "100%"
                                                        , value = NA
                                              )

                             ),

                             conditionalPanel(condition =  " input.plex_fields.includes('dates') ",

                                              dateRangeInput(inputId = "plex_dates"
                                                             , label = "Experiment dates (start/end)"
                                                             , end = NA
                                                             , width = "100%"
                                                             )
                             ),

                             conditionalPanel(condition =  ' input["plex_fields"].includes("about") ',

                                              textInput(inputId = "plex_about"
                                                        , label = "About"
                                                        , width = "100%"
                                                        , placeholder = "Short project description"
                                                        , value = NA
                                              )

                             ),

                             conditionalPanel(condition =  ' input["plex_fields"].includes("environment") ',

                                              selectInput(inputId = "plex_environment"
                                                        , label = "Environment"
                                                        , choices = c(""
                                                                      , "Field"
                                                                      , "Greenhouse"
                                                                      , "Laboratory"
                                                                      )
                                                        , width = "100%"
                                                        )

                             ),

                             conditionalPanel(condition =  ' input["plex_fields"].includes("institutions") ',

                                                textInput(inputId = "plex_institutions"
                                                          , label = "Institutions"
                                                          , width = "100%"
                                                          , value = NA
                                                )

                             ),

                             conditionalPanel(condition =  ' input["plex_fields"].includes("researchers") ',

                                                textInput(inputId = "plex_researchers"
                                                          , label = "Researchers"
                                                          , width = "100%"
                                                          , value = NA
                                                )

                             ),

                             conditionalPanel(condition = ' input["plex_fields"].includes("altitude") ',

                                              textInput(inputId = "plex_altitude"
                                                        , label = "Altitude (m.a.s.l)"
                                                        , width = "100%"
                                                        , value = NA
                                              )

                             ),

                             conditionalPanel(condition =  ' input["plex_fields"].includes("georeferencing") ',

                                              textInput(inputId = "plex_georeferencing"
                                                        , label = "Georeferencing"
                                                        , width = "100%"
                                                        , value = NA
                                              )

                             ),

                             conditionalPanel(condition =  ' input["plex_fields"].includes("fieldbook") ',

                                              textInput(inputId = "plex_fieldbook"
                                                        , label = "Fieldbook name"
                                                        , value = NA
                                                        , width = "100%"
                                              )

                             ),

                             conditionalPanel(condition =  ' input["plex_fields"].includes("album") ',

                                              textInput(inputId = "plex_album"
                                                        , label = "Album"
                                                        , width = "100%"
                                                        , placeholder = "url or link"
                                                        , value = NA
                                              )

                             ),

                             conditionalPanel(condition =  ' input["plex_fields"].includes("github") ',

                                              textInput(inputId = "plex_github"
                                                        , label = "Github"
                                                        , width = "100%"
                                                        , placeholder = "url or link"
                                                        , value = NA
                                              )

                             )
                      ),

                      column(width = 5,

                             h5(icon("flask"), "Experimental plan"),

                             textAreaInput(inputId = "plex_idea"
                                           , label = "Idea"
                                           , placeholder = "How the idea was born."
                                           , width = "100%"
                             ),

                             textAreaInput(inputId = "plex_goal"
                                           , label = "Goal"
                                           , placeholder = "The main goal of the project."
                             ),

                             textAreaInput(inputId = "plex_hypothesis"
                                           , label = "Hypothesis"
                                           , placeholder = "What are the expected results."
                             ),

                             textAreaInput(inputId = "plex_rationale"
                                           , label = "Rationale"
                                           , placeholder = "Based in which evidence is planned the experiment."

                             ),

                             textAreaInput(inputId = "plex_objectives"
                                           , label = "Objectives"
                                           , placeholder = "Objectives of the project."
                             ),

                             textAreaInput(inputId = "plex_plan"
                                           , label = "Project plan"
                                           , placeholder = "General plan description of the project (M & M)."
                             )

                      ),

                      column(width = 2,

                             h5(icon("pencil-ruler"), "Experimental design"),

                             numericInput(
                               inputId = "plex_nfactors"
                               , label = "Factors number"
                               , value = 1
                               , max = 5
                               , min = 1
                             ),

                             uiOutput("plex_design"),

                             numericInput(inputId = "plex_rep"
                                          , label = "Replications"
                                          , value = 3
                                          , min = 2
                             ),

                             numericInput(inputId = "plex_serie"
                                          , label = "Plot digits"
                                          , value = 2
                                          , max = 3
                                          , min = 1
                             ),

                             numericInput(inputId = "plex_seed"
                                          , label = "Seed"
                                          , value = 0
                                          , min = 0
                             )
                             
                             )
                      )

# Tarpuy fb  --------------------------------------------------------------
# -------------------------------------------------------------------------

           ),

           tabPanel("Fieldbook",

                    fluidRow(

                      column(2,

                             h5(icon("pencil-ruler"), "Experimental design"),

                             numericInput(
                               inputId = "design_nfactors"
                               , label = "Factors number"
                               , value = 1
                               , max = 5
                               , min = 1
                             ),

                             uiOutput("plex_location"),

                             uiOutput("design_type"),

                             numericInput(inputId = "design_rep"
                                          , label = "Replications"
                                          , value = 3
                                          , min = 2
                                          ),

                             numericInput(inputId = "design_serie"
                                          , label = "Plot digits"
                                          , value = 2
                                          , max = 3
                                          , min = 1
                                          ),

                             numericInput(inputId = "design_seed"
                                          , label = "Seed"
                                          , value = 0
                                          , min = 0
                                          ),
                             
                             textInput(inputId = "design_qr"
                                       , label = "QR label"
                                       , placeholder = "QR prefix"
                                       , value = "FB"
                             ),

                             actionButton(inputId = "export_design"
                                          , label = "Generate"
                                          , class = "btn btn-success"
                                          )

                             ),

                      column(10,

                             uiOutput("gsheet_preview_design"),

                             br(),
                             br()

                             )
                      )


# Tarpuy sketch -----------------------------------------------------------
# -------------------------------------------------------------------------

           ),

tabPanel("Sketch",

         fluidRow(

           column(2,

                  div(h5(icon("drafting-compass"), "Skecth design")),

                  radioButtons(inputId = "sketch_preview_opt"
                               , label = "Modules"
                               , choices = c("Gsheet"
                                             , "Sketch")
                               , inline = TRUE
                               , selected = "Sketch"
                               ),

                  uiOutput("sketch_options"),

                  actionButton(inputId = "update_sketch"
                               , label = "Refresh"
                               , class = "btn btn-success"
                               )

           ),

           column(10,

                  uiOutput("sketch_modules"),

                  br(),
                  br()

           )

           ),


         )

# Tarpuy end code ---------------------------------------------------------
# -------------------------------------------------------------------------

)
