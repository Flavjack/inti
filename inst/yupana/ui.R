# -------------------------------------------------------------------------
# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/yupanapro/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-05-24
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

#> devtools::install_github("flavjack/inti")

source("msgs.R")
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

navbarPage(title = HTML('<h3><strong><a target="_blank" href="https://inkaverse.com/">Yupana</a></strong></h3>')
           , windowTitle = "Yupana • app"
           , selected = "Intro"
           , theme = "bootstrap_sandstone.css" #!
           , 

# -------------------------------------------------------------------------
# Yupana Info -------------------------------------------------------------
# -------------------------------------------------------------------------

           tabPanel("Intro"
                    
                    , bs_theme_dependencies("flatly") #!
                    
                    , includeCSS("www/custom.css")
                    , tags$head(includeHTML(("www/analytics.html")))
                    , tags$head(tags$link(rel="shortcut icon"
                                          , href="https://flavjack.github.io/inti/img/inkaverse.png")),
                    
                    meta() %>%
                      meta_social(
                        title = "Yupana",
                        description = "Yupana: platform for statistical data analysis",
                        url = "https://flavjack.shinyapps.io/yupanapro/",
                        image = "https://flavjack.github.io/inti/img/yupana.png",
                        image_alt = "inkaverse.com"
                      )
                    
                    , fluidRow(

                      column(width = 1,
                             
                             HTML('
              <div id=footer style="width:100%; margin:auto;">
              <div style="display:inline-block; width:100%">
              <p style="text-align:center">
              <a target="_blank" href="https://inkaverse.com/articles/yupana.html">
              <img src="https://flavjack.github.io/inti/img/yupana.png" style="height:80px" title="Yupana"></a>
              <span style="display:block;"><small>Yupana</small></span>
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
                                  Yupana está diseñado pensando en la reproductibilidad de los resultados.
                                  Los análisis realizados se almacenará en la hojas de cálculo privadas de cada usuario.
                                  Los resultados de yupana pueden ser usados en R o viceversa. Yupana además permite:
                                  </p>
                                  <ul>

                                  <li>Estadística descriptiva y resumen de los datos.

                                  <li>Pruebas de comparación de medias.

                                  <li>Gráficos de diagnóstico de los modelos.

                                  <li>Gráficos interactivo con opciones de personalización.

                                  <li>Análisis multivariados.
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
                                             label = NULL
                                             , width = "100%"
                                             , value = ""
                                             , placeholder = "Insert google sheet url"
                                   )

                               ),

                             ),

                             fluidRow(

                               box(title = h5("Fieldbook data")
                                   , width = 2
                                   , solidHeader = T,

                                   uiOutput("fieldbook_gsheet")
                                   
                               ),
                               
                               box(title = h5("Last factor")
                                   , width = 2
                                   , solidHeader = T,
                                 
                                   uiOutput("fb_last_factor")
                                 
                               ),
                               
                               box(title = h5("Model factors (optional)")
                                   , width = 6
                                   , solidHeader = T,
                                   
                                   textInput(inputId = "fb_model_factors"
                                             , label = NULL
                                             , width = "100%"
                                             , placeholder = "e.g. block + factor1*factor2"
                                             )
                                   
                               ),

                               box(title = div(h5("Open GS"), align = "center")
                                   , width = 2
                                   , solidHeader = T,

                                   div(

                                     uiOutput("open_url")

                                     , align = "center")
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
                                    Más información en la politicas de privacidad: <a href="https://inkaverse.com/articles/policy"> https://inkaverse.com/articles/policy </a>

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
              <a target="_blank" href="https://flavjack.shinyapps.io/tarpuy/">
              <img src="https://flavjack.github.io/inti/img/tarpuy.png" style="height:80px" title="tarpuy"></a>
              <span style="display:block;"><small>Tarpuy</small></span>
              </p></div>
              </div>
                    ')

                      )

                    )

           ),

# Exploratory -------------------------------------------------------------
# -------------------------------------------------------------------------

tabPanel("Exploratory",
         
         fluidRow(
           
           column(2,
                  
                  fixedRow(
                    
                    column(12, 
                           
                           selectInput(
                             inputId = "raw_type"
                             , label = "Type"
                             , choices = c("boxplot", "scatterplot")
                           )
                           
                    ),
                    
                    column(12, 
                           
                           uiOutput("raw_response"),
                           
                    ),
                    
                    column(12, 
                           
                           uiOutput("raw_x"),
                           
                    ),
                    
                    column(12, 
                           
                           uiOutput("raw_group"),
                           
                    ),
                    
                    column(6, 
                           
                           textInput(
                             inputId ="raw_ylimits"
                             , label = "Y limits"
                             , placeholder = "0*100*20"
                           )
                           
                    ),
                    
                    column(6, 
                           
                           textInput(
                             inputId ="raw_xrotation"
                             , label = "X rotation"
                             , value = "0*0.5*0.5"
                             , placeholder = "angle*h*v"
                           )
                           
                    ),
                    
                    column(12, 
                           
                           textInput(
                             inputId = "raw_dimension"
                             , label = "Dimensions (W*H*dpi)"
                             , placeholder = "W*H*dpi"
                             , value = "20*10*100"
                           )
                           
                    )
                    
                  )
                  
           ),
           
           column(8,
                  
                  uiOutput("plot_raw"),
                  
           ),
           
           column(2,
                  
                  fixedRow(
                    
                    column(6, 
                           
                           selectInput(
                             inputId ="raw_color"
                             , label = "Color"
                             , choices = c("yes", "no")
                           )
                           
                    ),   
                    
                    column(6, 
                           
                           selectInput(
                             inputId = "raw_legend"
                             , label = "Legend"
                             , choices = c("top", "bottom", "left", "right", "none")
                           )
                           
                    ),
                    
                    column(12, 
                           
                           textInput(
                             inputId ="raw_ylab"
                             , label = "Y label"
                           )
                           
                    ),
                    
                    column(12, 
                           
                           textInput(
                             inputId ="raw_xlab"
                             , label = "X label"
                           )
                           
                    ),
                    
                    column(12, 
                           
                           textInput(
                             inputId ="raw_glab"
                             , label = "Group label"
                           )
                           
                    ),
                    
                    column(12, 
                           
                           textInput(
                             inputId ="raw_gtext"
                             , label = "Group brake labels (,)"
                           )
                           
                    ),
                    
                    column(12, 
                           
                           textInput(
                             inputId ="raw_xtext"
                             , label = "X brake labels (,)"
                           )
                           
                    ),
                    
                    column(12, 
                           
                           textInput(
                             inputId ="raw_opt"
                             , label = "Opt"
                             , placeholder = "extra layers"
                           )
                           
                    )
                  )
                  
           )
         )
         
),

# Yupana Analysis ---------------------------------------------------------
# -------------------------------------------------------------------------

tabPanel("Analysis",
         
fluidRow(
  
  column(2, 
                
        radioButtons(inputId = "analysis_preview_opt"
                     , label = "Modules"
                     , choices = c("Gsheet"
                                   , "Model"
                                   , "Diagnostic"
                                   )
                     , inline = TRUE
                     , selected = "Model"
                     ),
        
        uiOutput("analysis_last_factor"),
        
        uiOutput("analysis_response"),
        
        uiOutput("analysis_comparison"),
        
        uiOutput("analysis_model_factors"),

        selectInput(
          inputId = "analysis_test_comparison"
          , label = "Test comparison"
          , choices = c("SNK", "TUKEY", "DUNCAN")
        ),
        
        fluidRow(
          
          column(7,
                 
                 numericInput(
                   inputId = "analysis_sig_level"
                   , label = "Significance level"
                   , value = 0.05
                   , step = 0.01
                   , min = 0
                   , max = 0.5
                 )
                 
          ),
          
          column(5,
                 
                 numericInput(
                   inputId = "analysis_digits"
                   , label = "Digits"
                   , value = 2
                   , min = 0
                   , step = 1
                 )
                 
          )
          
        )
        
        ),
 
 column(10,
        
        uiOutput("analysis_preview")
        
        )
 
 )
                     
         
# Yupana Graphics ---------------------------------------------------------
# -------------------------------------------------------------------------

),

tabPanel("Graphics",
         
         fluidRow(
           
           column(2,

                  fixedRow(

                    column(12,
                           
                           radioButtons(inputId = "smr_preview_opt"
                                        , label = "Modules"
                                        , choices = c("Gsheet"
                                                      , "Plots")
                                        , selected = "Plots"
                                        , inline = TRUE)
                    ),
                    
                    column(12, 
                           
                           uiOutput("smr_type")
                           
                           ),
                           

                    column(12,

                           uiOutput("smr_response")

                           ),

                    column(12,

                           uiOutput("smr_x")

                           ),

                    column(12,

                           uiOutput("smr_group")

                           ),
                    
                    column(6, 
                           
                           uiOutput("smr_sig")
                           
                           ),
                    
                    column(6, 
                           
                           uiOutput("smr_error")
                           
                    ),
                    
                    column(6, 
                           
                           uiOutput("smr_ylimits")
                           
                    ),
                    
                    column(6, 
                           
                           uiOutput("smr_xrotation")
                           
                    ),
                    
                    column(12,
                           
                           uiOutput("smr_dimension")
                           
                    ),
                    
                    column(6,
                           
                           uiOutput("graph_sheet_save"),
                           
                           ),
                    
                    column(6,
                           
                           radioButtons(inputId = "graph_smr_overwrite"
                                        , label = "Overwrite"
                                        , inline = TRUE
                                        , choices = c("no", "yes")
                                        )
                           ),
                    
                    column(6,
                           
                           actionButton(inputId = "graph_smr_load"
                                        , label = "Load"
                                        , class = "btn btn-primary"
                                        , width = "100%"
                                        )
                           
                           ),
                    
                    column(6,
                           
                           actionButton(inputId = "graph_smr_save"
                                        , label = "Save"
                                        , class = "btn btn-warning"
                                        , width = "100%"
                                        )
                           
                           ),
                    
                    br(),
                    br()
                    
                    )
                  
           ),
           
           column(8,
                  
                  uiOutput("plot_smr"),
                  
           ),
           
           column(2,
                  
                  fixedRow(
                    
                    column(12,
                           
                           HTML('<p>Model</p>'),
                           
                           verbatimTextOutput("analysis_model")
                           
                    ),
                    
                    column(6, 
                           
                           uiOutput("plot_color"),
                           
                           ),   
                    
                    column(6, 
                           
                           uiOutput("plot_error"),
                           
                           ),
                    
                    column(12, 
                           
                           uiOutput("plot_ylab")
                           
                    ),
                    
                    column(12, 
                           
                           uiOutput("plot_xlab")
                           
                    ),
                    
                    column(12, 
                           
                           uiOutput("plot_glab")
                           
                    ),
                    
                    column(12, 
                           
                           uiOutput("plot_gtext")
                           
                    ),
                    
                    column(12, 
                           
                           uiOutput("plot_xtext")
                           
                    ),
                    
                    column(12, 
                           
                           
                           uiOutput("plot_opt")
                           
                           )
                  )
                  
           )
         )
                    
# multivariate ------------------------------------------------------------
# -------------------------------------------------------------------------

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
                          
                          uiOutput("mvr_last_factor"),

                          uiOutput("mvr_facts"),

                          uiOutput("mvr_groups"),
                          
                          uiOutput("mvr_variables"),

                          textInput(
                            inputId ="mvr_dimension"
                            , label = "Dimensions (cm)"
                            , placeholder = "w*h*dpi"
                            , value = "14*14*100"
                            )
                          
                          ),

                   column(width = 10,

                          uiOutput("mvr_preview")

                   ),

                  br()

                 )
        ),

# Yupana Fieldbook --------------------------------------------------------
# -------------------------------------------------------------------------

tabPanel("Fieldbook",
         
         fluidRow(
           
           column(2,
                  
                  radioButtons(inputId = "fb_preview_opt"
                               , label = "Modules"
                               , choices = c("Reshape")
                               , inline = TRUE
                               
                  ),
                  
                  uiOutput("fb_modules"),
                  
                  br(),
                  
           ),
           
           column(width = 10,
                  
                  uiOutput("fieldbook_preview")
                  
           ),
           
         )
         
)

# Yupana end code ---------------------------------------------------------
# -------------------------------------------------------------------------

)
