# -------------------------------------------------------------------------
# Tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/tarpuy/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2024-02-05
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

#> devtools::install_github("flavjack/inti")

source("www/msgs.R")
suppressPackageStartupMessages({source("pkgs.R")})

options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/spreadsheets"
                                          , "https://www.googleapis.com/auth/userinfo.email"
                                          ))

options(gargle_oob_default = TRUE)
options(shiny.port = 1221)

if (file.exists("www/cloud.json")) gar_set_client(web_json = "www/cloud.json", activate = "web")

# -------------------------------------------------------------------------
# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

navbarPage(title = HTML('<strong><a target="_blank" href="https://inkaverse.com/">TARPUY</a></strong>')
  , windowTitle = "Tarpuy • app"
  , selected = "Intro"
  , theme =  bslib::bs_theme(version = 5, bootswatch = 'sandstone')
  , tags$style(HTML("p{ text-align: justify; }"))
  , position = "fixed-top"
  , tags$style(HTML("body {padding-top: 50px;}"))
  ,

# -------------------------------------------------------------------------
# Yupana Info -------------------------------------------------------------
# -------------------------------------------------------------------------

           tabPanel("Intro", icon = icon("home") 
                    , tags$head(includeHTML(("www/analytics.html")))
                    , tags$head(tags$link(rel="shortcut icon"
                                          , href="https://flavjack.github.io/inti/img/inkaverse.png")),
                    
                    meta() %>%
                      meta_social(
                        title = "Tarpuy",
                        description = "Tarpuy: Easy way to create fieldbook experimental designs.",
                        url = "https://flavjack.shinyapps.io/tarpuy/",
                        image = "https://flavjack.github.io/inti/img/tarpuy.png",
                        image_alt = "inkaverse.com"
                        ),
                    
                    fluidRow(

                      column(width = 1,
                             
                             HTML('
              <div id=footer style="width:100%; margin:auto;">
              <div style="display:inline-block; width:100%">
              <p style="text-align:center">
              <a target="_blank" href="https://inkaverse.com/articles/tarpuy.html">
              <img src="https://flavjack.github.io/inti/img/tarpuy.png" style="height:80px" title="Tarpuy"></a>
              <span style="display:block;"><small>Tarpuy</small></span>
              </p></div>
              </div>
                    '),
              
              br(),

                             HTML('
            <div id=footer style="width:100%; margin:auto;">
            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://flavjack.github.io/inti/index.html">
            <img src="https://flavjack.github.io/inti/logo.png" style="height:80px" title="flozano"></a>
            <span style="display:block;"><small>project</small></span>
            </p></div>
            </div>
                  ')

                      ),

                      column(width = 3,

                             box(title = "Presentación"
                                 , solidHeader = T
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
                                 , width = 12
                                 , status = "primary",

                                 HTML('
                                 <p>
                                 Tarpuy está diseñado pensando en la elaboración de proyectos experimentales de forma rápida e intuitiva.
                                 Los resultados se almacenará en la hojas de cálculo privadas de cada usuario. Tarpuy además permite:
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

                               box(title = h5("Information")
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
                                             , value = "design"
                                             , placeholder = "Sheet name"
                                   ),

                               ),

                               box(title = h5("Varibles")
                                   , width = 2
                                   , solidHeader = T,

                                   textInput(inputId = "gsheet_varlist"
                                             , label = NULL
                                             , value = "traits"
                                             , placeholder = "Sheet name"
                                   ),

                               ),

                               box(title = #h5("Fieldbook")
                                   , width = 2
                                   , solidHeader = T,
                                   
                                   #> gap()

                               ),

                               box(title = div(h5("Create GS"), align = "center")
                                   , width = 2
                                   , solidHeader = T,

                                   div(


                                     actionButton(inputId = "create_sheet"
                                                  , label =  "Create"
                                                  , class = "btn btn-warning"
                                                  , width = "80%"
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

                               box(title = "¿Cómo usar Tarpuy?"
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
                                   
                                   <li>En la versión web, cuando des los permisos el botón de "LOG IN" cambiará a “LOG OUT” (color rojo).
                                    Lo que significa que ahora puedes interactuar con tu información y generar tus datos.
                                   
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
                                  <li>Si deseas analizar los datos experimentales de tu proyecto, puedes usar la app Yupana:
                                  <a href="https://flavjack.shinyapps.io/yupana/">https://flavjack.shinyapps.io/yupana/</a>
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
                             <img src="https://flavjack.github.io/inti/img/youtube.png" style="height:60px" title="demo"></a>
                             <span style="display:block;"><small>demo</small></span>
                             </p></div>

                             </div>

                                   '),

                              br(),

                              HTML('

                             <div id=footer style="width:100%; margin:auto;">

                             <div style="display:inline-block; width:100%">
                             <p style="text-align:center">
                             <a target="_blank" href="https://flavjack.shinyapps.io/yupana/">
                             <img src="https://flavjack.github.io/inti/img/yupana.png" style="height:80px" title="yupana"></a>
                             <span style="display:block;"><small>Yupana</small></span>
                             </p></div>

                             </div>

                                   '),
                             
                             br(),
                             
                             HTML('
              <div id=footer style="width:100%; margin:auto;">
              <div style="display:inline-block; width:100%">
              <p style="text-align:center">
              <a target="_blank" href="https://huito.inkaverse.com/">
              <img src="https://huito.inkaverse.com/logo.png" style="height:80px" title="huito"></a>
              <span style="display:block;"><small>Huito</small></span>
              </p></div>
              </div>
                    ')
              
                      )
            
                    )
            
           ),

           tabPanel("Plex", icon = icon("seedling"), 
             
             # div(h5(icon("seedling"),  "Plex")),

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
                                                              , "repository"
                                                              , "project"
                                                              , "manuscript"
                                                              , "album"
                                                              )
                                                , selected = c("manager"
                                                               , "location"
                                                               , "dates"
                                                               , "about"
                                                               , "environment"
                                                               , "project"
                                                               )
                             ),

                             textInput(inputId = "plex_logbook"
                                       , label = "Logbook (optional)"
                                       , width = "100%"
                                       , value = "logbook"
                                       , placeholder = "sheet name"
                             ),

                             textInput(inputId = "plex_timetable"
                                       , label = "Timetable (optional)"
                                       , width = "100%"
                                       , value = "schedule"
                                       , placeholder = "sheet name"
                             ),

                             textInput(inputId = "plex_budget"
                                       , label = "Budget (optional)"
                                       , width = "100%"
                                       , value = "budget"
                                       , placeholder = "sheet name"
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
                                                             , width = "100%"
                                                             , end = NA
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
                                                                      , "Others"
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
                             
                             conditionalPanel(condition =  ' input["plex_fields"].includes("project") ',
                                              
                                              textInput(inputId = "plex_project"
                                                        , label = "Project"
                                                        , width = "100%"
                                                        , placeholder = "url or link"
                                                        , value = NA
                                              )
                                              
                             ),

                             conditionalPanel(condition =  ' input["plex_fields"].includes("repository") ',

                                              textInput(inputId = "plex_repository"
                                                        , label = "Repository"
                                                        , width = "100%"
                                                        , placeholder = "url or link"
                                                        , value = NA
                                              )

                             ),
                             
                             conditionalPanel(condition =  ' input["plex_fields"].includes("manuscript") ',
                                              
                                              textInput(inputId = "plex_manuscript"
                                                        , label = "Manuscript"
                                                        , width = "100%"
                                                        , placeholder = "url or link"
                                                        , value = NA
                                              )
                                              
                             ),
                             
                             conditionalPanel(condition =  ' input["plex_fields"].includes("album") ',
                                              
                                              textInput(inputId = "plex_album"
                                                        , label = "Album"
                                                        , width = "100%"
                                                        , placeholder = "url or link"
                                                        , value = NA
                                              )
                                              
                             )
                             
                             
                      ),

                      column(width = 5,

                             h5(icon("flask"), "Experimental plan"),

                             textAreaInput(inputId = "plex_title"
                                           , label = "Title"
                                           , placeholder = "Project title"
                                           , width = "100%"
                             ),
                             
                             textAreaInput(inputId = "plex_objectives"
                                           , label = "Objectives"
                                           , placeholder = "Objectives of the project."
                                           , width = "100%"
                             ),

                             textAreaInput(inputId = "plex_hypothesis"
                                           , label = "Hypothesis"
                                           , placeholder = "What are the expected results."
                                           , width = "100%"
                             ),

                             textAreaInput(inputId = "plex_rationale"
                                           , label = "Rationale"
                                           , placeholder = "Based in which evidence is planned the experiment."
                                           , width = "100%"

                             ),
                             
                             textAreaInput(inputId = "plex_references"
                                           , label = "References"
                                           , placeholder = "References for the project."
                                           , width = "100%"
                             ),

                             textAreaInput(inputId = "plex_plan"
                                           , label = "Project plan"
                                           , placeholder = "General plan description of the project (M & M)."
                                           , width = "100%"
                             )

                      ),

                      column(width = 2,

                             h5(icon("pencil-ruler"), "Experimental design"),
                             
                             fluidRow(
                               
                               column(7,
                                      
                                      numericInput(
                                        inputId = "plex_nfactors"
                                        , label = "Factors number"
                                        , value = 1
                                        , max = 5
                                        , min = 1
                                        )
                                      
                                      ),
                               
                               column(5,
                                      
                                      numericInput(inputId = "plex_rep"
                                                   , label = "Replications"
                                                   , value = 3
                                                   , min = 1
                                                   )
                                      )
                               ),
                             
                             fluidRow( 
                               
                               column(7,
                                      
                                      uiOutput("plex_design")
                                      
                                      ),
                               
                               column(5,
                                      
                                      selectizeInput(
                                        inputId = "plex_zigzag",
                                        label = "Zigzag",
                                        choices = c("FALSE", "TRUE")
                                      )
                               ),
                               
                               
                               ), 
                             
                             fluidRow(

                               column(6,
                                      
                                      numericInput(inputId = "plex_serie"
                                                   , label = "Plots serie"
                                                   , value = 100
                                                   , min = 100
                                                   )
                                      
                                      ),
                               
                               column(6,
                                      
                                      numericInput(inputId = "plex_seed"
                                                   , label = "Seed"
                                                   , value = 0
                                                   , min = 0
                                                   )
                                      
                                      )
                               ),
                             
                             br(),
                             
                             fluidRow(
                               
                               column(12,
                                      
                                      uiOutput("plex_sheets2create"),

                               ),
                                      
                               
                               column(12,
                                      
                                      actionButton(inputId = "plex_generate"
                                                   , label = "Generate"
                                                   , class = "btn btn-success"
                                                   )
                               )
                             )
                             
                             )
                      )

# Tarpuy fb  --------------------------------------------------------------
# -------------------------------------------------------------------------

           ),

           tabPanel("Fieldbook", icon = icon("book"),
             
             # div(h5(icon("book"), "Fieldbook")),

                    fluidRow(

                      column(2,

                             h5(icon("pencil-ruler"), "Experimental design"),
                             
                             fluidRow(
                               
                               column(7,
                                      
                                      numericInput(
                                        inputId = "design_nfactors"
                                        , label = "Factors number"
                                        , value = 1
                                        , max = 5
                                        , min = 1
                                        ),
                                      
                                      ),
                                      

                               column(5,
                                      
                                      numericInput(inputId = "design_rep"
                                                   , label = "Replications"
                                                   , value = 3
                                                   , min = 2
                                                   ),
                                      ),
                               ),
                             
                             uiOutput("design_type"),
                             
                             fluidRow(
                               
                               column(6,
                                      
                                      numericInput(inputId = "design_serie"
                                                   , label = "Plots serie"
                                                   , value = 100
                                                   , min = 0
                                                   ),
                                      
                                      ),
                               
                               
                               column(6,
                                      
                                      numericInput(inputId = "design_seed"
                                                   , label = "Seed"
                                                   , value = 0
                                                   , min = 0
                                                   ),
                                      
                                      
                                      )
                               ),

                             
                             textInput(inputId = "design_qr"
                                       , label = "QR label"
                                       , placeholder = "QR prefix"
                                       , value = "TARPUY"
                                       ),
                             
                             fluidRow(
                               
                               column(6,
                                      
                                      selectInput(inputId = "design_zigzag"
                                                  , label = "Zigzag"
                                                  , choices = c("FALSE",  "TRUE")
                                      )
                               ),
                               
                               column(6,
                                      
                                      textInput(inputId = "fb2export"
                                                , label = "Sheet export"
                                                , placeholder = "sheet name"
                                                , value = "fb"
                                      ),
                                      
                               ),
                               
                               column(6,
                                      
                                      actionButton(inputId = "export_design"
                                                   , label = "Generate"
                                                   , class = "btn btn-success"
                                                   )
                                      ),
                               
                               column(6,
                                      
                                      radioButtons(inputId = "export_design_overwrite"
                                                   , label = "Overwrite"
                                                   , inline = TRUE
                                                   , choices = c("no", "yes")
                                      )
                               ),
                               
                               ),
                             
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

tabPanel("Sketch", icon = icon("pen-ruler"),
  
  # div(h5(icon("pen-ruler"), "Sketch")) ,

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
                  
                  
                  uiOutput("sketch_sheets"), 

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


         ),


# connect -----------------------------------------------------------------
# -------------------------------------------------------------------------

tabPanel("Mobile", icon = icon("plug-circle-check"),
  
  # div(h5(icon("plug-circle-check"), "Mobile")),
         
         fluidRow(
           
           column(2,
                  
                  h5(icon("mobile-screen-button"), HTML('<a target="_blank" href="https://play.google.com/store/apps/details?id=com.fieldbook.tracker&hl=en_US">Field-Book (PhenoApp)</a>')),
                  
                  br(),
                  
                  radioButtons(inputId = "connection_sheet_preview"
                               , label = h5(icon("magnifying-glass"), "Preview") 
                               , choices = c("Traits"
                                             , "Field-Book")
                               , inline = TRUE
                               , selected = "Traits"
                  ),
                  
                  fluidRow(
                    
                    h5(icon("wheat-awn"), "Traits"),
                    
                    uiOutput("connection_sheet_traits"),
                    
                  ),
                  
                  fluidRow(
                    
                    h5(icon("book"), "Field-Book"),
                    
                    uiOutput("connection_sheet_fieldbook"),
                    
                    uiOutput("connection_fieldbook_lastfactor"),
                    
                  ),
                  
                  br(),
                  
                  fluidRow(
                    
                    h5(icon("cloud-arrow-down"), "Download Files"),
                    
                    column(6,
                           
                           uiOutput("connection_traits_download"),
                           
                           ),
                    
                    column(6,
                           
                           uiOutput("connection_fieldbook_download"),
                           
                    ),
                    
                  ),
                  
                  
           ),
           
           column(10,
                  
                  uiOutput("connection_sheet_preview"),
                  
                  br(),
                  br()
                  
           )
           
         )

),

bslib::nav_spacer(),
bslib::nav_item(
  tags$a(
    shiny::icon("heart"), "support",
    href = "https://github.com/sponsors/flavjack",
    target = "_blank"
  )),

bslib::nav_item(
  tags$a(
    shiny::icon("github"), paste("Inti ", packageVersion('inti')),
    href = "https://inkaverse.com/news/",
    target = "_blank"
  )),

# Tarpuy end code ---------------------------------------------------------
# -------------------------------------------------------------------------

)