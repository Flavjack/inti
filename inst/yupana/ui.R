# -------------------------------------------------------------------------
# Yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/yupana/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2024-02-05
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

#> devtools::install_github("flavjack/inti")

source("www/msgs.R")
suppressPackageStartupMessages({source("pkgs.R")})

# -------------------------------------------------------------------------

options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/spreadsheets"
                                          , "https://www.googleapis.com/auth/userinfo.email"
                                          ))

options(gargle_oob_default = TRUE)
options(shiny.port = 1221)

if (file.exists("www/cloud.json")) gar_set_client(web_json = "www/cloud.json", activate = "web")

# -------------------------------------------------------------------------
# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

navbarPage(title = HTML('<strong><a target="_blank" href="https://inkaverse.com/">YUPANA</a></strong>')
           , windowTitle = "Yupana • app"
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
                        title = "Yupana",
                        description = "Yupana: platform for statistical data analysis",
                        url = "https://flavjack.shinyapps.io/yupana/",
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
                                 , width = 12
                                 , status = "primary",

                                 HTML('

                                  <p>
                                  Yupana está diseñado pensando en la reproductibilidad de los resultados.
                                  Los análisis realizados se almacenará en la hojas de cálculo privadas de cada usuario.
                                  Los resultados de Yupana pueden ser usados en R o viceversa. Yupana además permite:
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

                               box(title = h5("Fieldbook")
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

                               box(title = "¿Cómo usar Yupana?"
                                   , width = 6
                                   , solidHeader = T
                                   , height = "200px"
                                   , status = "primary",


                                   HTML('

                                    <p>
                                    Para usar Yupana es necesario que tus datos esten en una hoja de cálculo de google.
                                    </p>
                                    <ol>

                                    <li>Introduce el URL de tu documento y selecciona el nombre de la hoja donde está tu base de datos
                                    (información mínima requerida para usar la app).

                                    <li>Debes dar los permisos para editar las hojas haciendo “LOG IN”;
                                    ya que la app requiere los permisos correspondientes para leer y exportar la información generada.
                                    Más información en la politicas de privacidad: <a href="https://inkaverse.com/articles/policy"> https://inkaverse.com/articles/policy </a>

                                    <li>En la versión web, cuando des los permisos el botón de "LOG IN" cambiará a “LOG OUT” (color rojo).
                                    Lo que significa que ahora puedes interactuar con tu información y analizar tus datos.

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
                                  Antes de iniciar a usar Yupana ten en cuenta las siguientes recomendaciones.
                                  </p>
                                  <ul>

                                  <li>Crea una copia de seguridad de tu documento.
                                  Vé a tu hoja de cálculo: <em>Archivo > Historial de versiones > Asignar un nombre a la versión actual</em>.
                                  De esa manera puedes crear múltiples copias de seguridad de tu base de datos, sin crear múltiples documentos.

                                  <li>Es recomendable solo tener una libro de campo "fieldbook" por cada experimento.
                                  Si tienes muchas pestañas que dificultan tu trabajo, puedes ir ocultandolas (<em>click derecho en la pestaña > Ocultar hoja</em>).
                                  Al momento de usar la app puedes especificar qué hoja deseas utilizar y la app extrae la información de la hoja indicada.

                                  <li>Si deseas crear un diseño experimental que luego puede ser usado de forma rápida en Yupana,
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

# Exploratory -------------------------------------------------------------
# -------------------------------------------------------------------------

tabPanel("Exploratory", icon = icon("magnifying-glass-chart"),
  
  # h5(icon("magnifying-glass-chart"), "Exploratory"),
         
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

tabPanel("Analysis", icon = icon("chart-simple"),
  
  # h5(icon("chart-simple"), "Analysis"),
         
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

tabPanel("Graphics", icon = icon("feather-pointed"),
  
  # h5(icon("feather-pointed"), "Graphics"),
         
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
                    
                    )
                  
           ),
           
           #>
           
           column(8,
                  
                  fluidRow(
                    
                    box(width = 2),
                    
                    box(width = 1,
                          
                          div(
                            class = "plotsave",
                            
                            div(
                              actionButton(inputId = "graph_smr_load"
                                           , label = "Load"
                                           , class = "btn btn-primary"
                                           ),
                            ))),
                    
                    box(width = 3,
                            
                            div(
                              uiOutput("graph_sheet_save"),
                              ),
                          ),
                    
                            
                      box(width = 2,
                            
                            div(
                              radioButtons(inputId = "graph_smr_overwrite"
                                           , label = "Overwrite"
                                           , inline = TRUE
                                           , choices = c("no", "yes")
                                           ),
                              )
                          ),
                    
                    box(width = 1,
                            
                              div(
                                actionButton(inputId = "graph_smr_save"
                                             , label = "Save"
                                             , class = "btn btn-warning"
                                             ),
                                ),
                            ),
                    
                    box(width = 3),
                    
                  ),
                          
                      br(),
                      br(),
                       
                box(width = 12, 
                       
                       uiOutput("plot_smr"),
                       
                       ),
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

        tabPanel("Multivariate", icon = icon("gears"),
          
          # h5(icon("gears"), "Multivariate"),

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
                          
                          conditionalPanel(
                            condition = "input.mvr_module == 'PCA'",
                            
                            textInput(
                              inputId ="mvr_dimension_pca_var"
                              , label = "Dimensions (W*H*dpi) - PCA var"
                              , placeholder = "w*h*dpi"
                              , value = "16*16*100"
                            ),
                            
                            textInput(
                              inputId ="mvr_dimension_pca_ind"
                              , label = "Dimensions (W*H*dpi) - PCA ind"
                              , placeholder = "w*h*dpi"
                              , value = "16*16*100"
                            ),
                            
                          ),
                          
                          conditionalPanel(
                            condition = "input.mvr_module == 'HCPC'",
                            
                            textInput(
                              inputId ="mvr_dimension_hcp_tree"
                              , label = "Dimensions (W*H*dpi) - HCPC tree"
                              , placeholder = "w*h*dpi"
                              , value = "16*16*100"
                            ),
                            
                            textInput(
                              inputId ="mvr_dimension_hcp_map" 
                              , label = "Dimensions (W*H*dpi) - HCPC map"
                              , placeholder = "w*h*dpi"
                              , value = "16*16*100"
                            ),
                            
                          ),
                          
                          conditionalPanel(
                            condition = "input.mvr_module == 'CORR'",
                            
                            selectInput(
                              inputId = "mvr_cor_method"
                              , label = "Correlation method"
                              , choices = c("pearson","spearman","kendall")
                              ),
                            
                            radioButtons(inputId = "mvr_cor_scale"
                                         , label = "Correlation scale"
                                         , choices = c("no" = FALSE
                                                       , "yes" = TRUE
                                                       )
                                         , inline = TRUE
                            ),
                            
                            textInput(
                              inputId ="mvr_dimension_cor"
                              , label = "Dimensions (W*H*dpi) - Correlation"
                              , placeholder = "w*h*dpi"
                              , value = "18*18*100"
                            )
                            
                            
                          ),

                          ),

                   column(width = 10,

                          uiOutput("mvr_preview")

                   ),

                  br()

                 )
        ),

# Yupana Fieldbook --------------------------------------------------------
# -------------------------------------------------------------------------

tabPanel("Fieldbook", icon = icon("book"),
  
  # h5(icon("book"), "Fieldbook"),
         
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
    href = "https://github.com/sponsors/flavjack",
    target = "_blank"
  )),

# Yupana end code ---------------------------------------------------------
# -------------------------------------------------------------------------

)
