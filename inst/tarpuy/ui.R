# -------------------------------------------------------------------------
# Tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/tarpuy/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2026-06-14
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

#> devtools::install_github("flavjack/inti")

suppressPackageStartupMessages({source("pkgs.R")})
source("www/msgs.R")

options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/spreadsheets"
                                          , "https://www.googleapis.com/auth/userinfo.email"
))

options(gargle_oob_default = TRUE)
options(shiny.port = 1221)

if (file.exists("www/cloud.json")) gar_set_client(web_json = "www/cloud.json", activate = "web")

# -------------------------------------------------------------------------
# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

navbarPage(
  title = HTML(
    '<strong><a target="_blank" href="https://inkaverse.com/">TARPUY</a></strong>'
  ),
  windowTitle = "Tarpuy • app",
  selected = "Intro",
  theme = bslib::bs_theme(version = 5, bootswatch = "sandstone"),
  position = "fixed-top",
  
  header = tags$head(
    
    tags$style(HTML("
  p { text-align: justify; }
  body { padding-top: 50px; }

  html {
    overflow-y: scroll;
    scrollbar-gutter: stable;
  }

  html.gsheet-scroll-lock {
    overflow-y: clip !important;
  }

  .gsheet-preview-wrapper {
    height: 600px;
    width: 100%;
    overflow: hidden;
    overscroll-behavior: contain;
  }

  .gsheet-preview-frame {
    height: 100%;
    width: 100%;
    border: 0;
    border-radius: 8px;
    display: block;
  }
  
  .datepicker,
  .datepicker-dropdown {
    z-index: 99999 !important;
  }
  
")),
    
    tags$script(HTML("
  (function() {

    let locked = false;
    let lockTimer = null;
    let unlockTimer = null;

    function lockPageScroll() {
      clearTimeout(unlockTimer);

      lockTimer = setTimeout(function() {
        if (locked) return;

        locked = true;
        document.documentElement.classList.add('gsheet-scroll-lock');
      }, 120);
    }

    function unlockPageScroll() {
      clearTimeout(lockTimer);

      unlockTimer = setTimeout(function() {
        if (!locked) return;

        locked = false;
        document.documentElement.classList.remove('gsheet-scroll-lock');
      }, 250);
    }

    $(document).on('mouseenter', '.gsheet-preview-wrapper', lockPageScroll);
    $(document).on('mouseleave', '.gsheet-preview-wrapper', unlockPageScroll);

  })();
"))
    
  ),
  
  # -------------------------------------------------------------------------
  # Panel intro
  # -------------------------------------------------------------------------
  tabPanel(
    "Intro",
    icon = icon("home")
    ,
    tags$head(includeHTML((
      "www/analytics.html"
    )))
    ,
    tags$head(
      tags$link(rel = "shortcut icon"
                , href = "https://flavjack.github.io/inti/img/inkaverse.png")
    ),
    
    meta() %>%
      meta_social(
        title = "Tarpuy",
        description = "Tarpuy: Easy way to create fieldbook experimental designs.",
        url = "https://flavjack.shinyapps.io/tarpuy/",
        image = "https://flavjack.github.io/inti/img/tarpuy.png",
        image_alt = "inkaverse.com"
      ),
    
    fluidRow(
      column(
        width = 1,
        
        HTML(
          '
              <div id=footer style="width:100%; margin:auto;">
              <div style="display:inline-block; width:100%">
              <p style="text-align:center">
              <a target="_blank" href="https://inkaverse.com/articles/tarpuy.html">
              <img src="https://flavjack.github.io/inti/img/tarpuy.png" style="height:80px" title="Tarpuy"></a>
              <span style="display:block;"><small>Tarpuy</small></span>
              </p></div>
              </div>
                    '
        ),
        
        br(),
        
        HTML(
          '
            <div id=footer style="width:100%; margin:auto;">
            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://flavjack.github.io/inti/index.html">
            <img src="https://flavjack.github.io/inti/logo.png" style="height:80px" title="flozano"></a>
            <span style="display:block;"><small>project</small></span>
            </p></div>
            </div>
                  '
        )
        
      ),
      
      column(
        width = 3,
        
        box(
          title = "Presentación"
          ,
          solidHeader = T
          ,
          width = 12
          ,
          status = "primary",
          
          HTML(
            '
                                 <p>
                                 Tarpuy es una plataforma interactiva para el planeamiento de experimentos (PLEX).
                                 Está desarrollada con la finalidad de promover las buenas prácticas en la colecta, análisis y manipulación de datos.
                                 Tarpuy tiene el objetivo de "continuidad" entre el uso de la aplicación y el software estadístico R.
                                 Tarpuy está basada en el paquete <strong><em>inti</em></strong>:
                                 “<em>Tools and statistical procedures for experimentals designs and plant breeding</em>”.
                                      </p>
                                      '
          ),
        ),
        
        box(
          title = "Características"
          ,
          solidHeader = T
          ,
          width = 12
          ,
          status = "primary",
          
          HTML(
            '
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
                                      '
          ),
        ),
        
      ),
      
      column(
        width = 7,
        
        fluidRow(
          box(
            title = div(h4(icon("key")), align = "right")
            ,
            width = 1,
            
            div(uiOutput("login")
                
                , align = "center")
            
          ),
          
          box(
            title = div(h4(
              icon("google"), "Fieldbook Google Sheets (URL)"
            ), align = "center")
            ,
            width = 11
            ,
            solidHeader = T
            ,
            status = "primary",
            
            textInput(
              inputId = "fieldbook_url",
              label = NULL,
              width = "100%",
              value = ""
              ,
              placeholder = "Insert google sheet link"
            )
            
          ),
          
        ),
        
        
        fluidRow(
          box(
            title = h5("Information")
            ,
            width = 2
            ,
            solidHeader = T,
            
            textInput(
              inputId = "gsheet_info"
              ,
              label = NULL
              ,
              value = "info"
              ,
              placeholder = "Sheet name"
            )
          ),
          
          box(
            title = h5("Design")
            ,
            width = 2
            ,
            solidHeader = T,
            
            textInput(
              inputId = "gsheet_design"
              ,
              label = NULL
              ,
              value = "design"
              ,
              placeholder = "Sheet name"
            ),
            
          ),
          
          box(
            title = h5("Varibles")
            ,
            width = 2
            ,
            solidHeader = T,
            
            textInput(
              inputId = "gsheet_varlist"
              ,
              label = NULL
              ,
              value = "traits"
              ,
              placeholder = "Sheet name"
            ),
            
          ),
          
          box(
            title = #h5("Fieldbook")
              ,
            width = 2
            ,
            solidHeader = T,
            
            #> gap()
            
          ),
          
          box(
            title = div(h5("Create GS"), align = "center")
            ,
            width = 2
            ,
            solidHeader = T,
            
            div(
              actionButton(
                inputId = "create_sheet"
                ,
                label =  "Create"
                ,
                class = "btn btn-warning"
                ,
                width = "80%"
              )
              
              ,
              align = "center"
            )
            
            
          ),
          
          box(
            title = div(h5("Open GS"), align = "center")
            ,
            width = 2
            ,
            solidHeader = T,
            
            div(uiOutput("open_url")
                
                , align = "center")
            
          ),
          
        ),
        
        br(),
        
        fluidRow(
          box(
            title = "¿Cómo usar Tarpuy?"
            ,
            width = 6
            ,
            solidHeader = T
            ,
            height = "200px"
            ,
            status = "primary",
            
            
            HTML(
              '
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
                                      '
            ),
            
          ),
          
          box(
            title = "Recomendaciones"
            ,
            width = 6
            ,
            solidHeader = T
            ,
            height = "200px"
            ,
            status = "primary",
            
            
            HTML(
              '
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
                                      '
            ),
          )
        ),
      ),
      
      
      column(
        1,
        
        HTML(
          '

                             <div id=footer style="width:100%; margin:auto;">

                             <div style="display:inline-block; width:100%">
                             <p style="text-align:center">
                             <a target="_blank" href="https://www.youtube.com/playlist?list=PLSQMdOu57lj8XTyH5KUN9h-VL5TAEsaBC">
                             <img src="https://flavjack.github.io/inti/img/youtube.png" style="height:60px" title="demo"></a>
                             <span style="display:block;"><small>demo</small></span>
                             </p></div>

                             </div>

                                   '
        ),
        
        br(),
        
        HTML(
          '

                             <div id=footer style="width:100%; margin:auto;">

                             <div style="display:inline-block; width:100%">
                             <p style="text-align:center">
                             <a target="_blank" href="https://flavjack.shinyapps.io/yupana/">
                             <img src="https://flavjack.github.io/inti/img/yupana.png" style="height:80px" title="yupana"></a>
                             <span style="display:block;"><small>Yupana</small></span>
                             </p></div>

                             </div>

                                   '
        ),
        
        br(),
        
        HTML(
          '
              <div id=footer style="width:100%; margin:auto;">
              <div style="display:inline-block; width:100%">
              <p style="text-align:center">
              <a target="_blank" href="https://huito.inkaverse.com/">
              <img src="https://huito.inkaverse.com/logo.png" style="height:80px" title="huito"></a>
              <span style="display:block;"><small>Huito</small></span>
              </p></div>
              </div>
                    '
        )
        
      )
      
    )
    
  ),
  
  # module plex
  
  tabPanel(
    "Plex",
    icon = icon("seedling"),
    
    tags$head(
      tags$style(
        HTML(
          "
      .tarpuy-card {
        background: #ffffff;
        border: 1px solid #e5e7eb;
        border-radius: 12px;
        padding: 12px;
        margin-bottom: 10px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.04);
      
        min-height: 100px;
        
        display: flex;
        flex-direction: column;
        justify-content: center;
      }

      .tarpuy-card-title {
        font-weight: 700;
        font-size: 18px;
        margin-bottom: 8px;
      }

      .tarpuy-section-title {
        font-weight: 700;
        color: #000000;
        margin-bottom: 8px;
      }

      .tarpuy-generate {
        font-weight: 700;
        min-width: 150px;
      }
      "
        )
      )
    ),
    
    fluidRow(
      style = "margin-bottom:8px;",
      
      column(
        width = 8,
        
        div(
          class = "tarpuy-card",
          
          div(
            style = "font-size:22px; font-weight:600; line-height:1.15;margin-bottom:10px;",
            icon("seedling"),
            " Experimental Plan - PLEX"
          ),
          
          tags$small(
            "Configure your experimental plan and generate all required sheets."
          )
        )
      ),
      
      # Experimental Project
      column(
        width = 4,
        div(
          class = "tarpuy-card",
          
          div(
            class = "tarpuy-card-title",
            icon("lightbulb"),
            " Experimental Project"
          ),
          
          div(
            style = "
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:16px;
        ",
            
            div(
              style = "flex:1;",
              uiOutput("plex_sheets2create")
            ),
            
            actionButton(
              inputId = "plex_generate",
              label = "GENERATE",
              class = "btn btn-success tarpuy-generate"
            )
          )
        )
      )
    ),
    
    # card Field Book de modulo plex
    
    fluidRow(
      column(
        width = 2,
        
        div(
          class = "tarpuy-card",
          div(class = "tarpuy-card-title", icon("file-signature"), " Field Book"),
          checkboxGroupInput(
            inputId = "plex_fields",
            label = NULL,
            choices = c(
              "manager",
              "location",
              "dates",
              "environment",
              "institutions",
              "researchers",
              "altitude",
              "georeferencing",
              "repository",
              "project",
              "manuscript",
              "album"
            ),
            selected = c(
              "manager",
              "location",
              "dates",
              "environment",
              "repository",
              "manuscript"
            )
          )
        ),
        
        div(
          class = "tarpuy-card",
          div(class = "tarpuy-card-title", icon("file-circle-plus"), " Extra Sheets"),
          checkboxGroupInput(
            inputId = "plex_sheets",
            label = NULL,
            choices = c("logbook", "timetable", "budget", "matrix", "credit"),
            selected = c("logbook", "matrix", "budget", "credit")
          )
        )
      ),
      

      column(
        width = 7,
        
        # General Information
        div(
          class = "tarpuy-card",
          div(class = "tarpuy-section-title", icon("feather-alt"), " General Information"),
          
          div(
            style = "
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
      gap: 14px;
      align-items: end;
    ",
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("manager") ',
              textInput(
                inputId = "plex_manager",
                label = "Project manager",
                width = "100%",
                value = NA
              )
            ),
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("location") ',
              textInput(
                inputId = "plex_location",
                label = "Location",
                width = "100%",
                value = NA
              )
            ),
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("dates") ',
              dateRangeInput(
                inputId = "plex_dates",
                label = "Experiment dates (start/end)",
                width = "100%",
                end = NA
              )
            ),
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("environment") ',
              selectInput(
                inputId = "plex_environment",
                label = "Environment",
                choices = c(
                  "",
                  "Field",
                  "Greenhouse",
                  "Laboratory",
                  "Others"
                ),
                width = "100%"
              )
            ),
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("repository") ',
              textInput(
                inputId = "plex_repository",
                label = "Repository",
                placeholder = "url or link",
                width = "100%",
                value = NA
              )
            ),
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("manuscript") ',
              textInput(
                inputId = "plex_manuscript",
                label = "Manuscript",
                placeholder = "url or link",
                width = "100%",
                value = NA
              )
            )
          ),
          
          div(
            style = "
      display: grid;
      grid-template-columns: 2fr 1fr;
      gap: 14px;
      align-items: start;
      margin-top: 14px;
    ",
            
            textAreaInput(
              inputId = "plex_title",
              label = "Title",
              placeholder = "Project title",
              width = "100%"
            ),
            
            textAreaInput(
              inputId = "plex_short_title",
              label = "Short title (max. 4 words)",
              placeholder = "Short title",
              width = "100%"
            )
          ),
          
          textAreaInput(
            inputId = "plex_objective",
            label = "Objective",
            placeholder = "Objectives of the project.",
            width = "100%"
          ),
          
          textAreaInput(
            inputId = "plex_references",
            label = "References",
            placeholder = "References for the project.",
            width = "100%"
          )
        ),
        
        # Advanced Information (optional)
        
        div(
          class = "tarpuy-card",
          div(class = "tarpuy-section-title", icon("circle-plus"), " Advanced Information (optional)"),
          
          div(
            style = "
            display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
            gap: 14px; align-items: end; ",
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("institutions") ',
              textInput("plex_institutions", "Institutions", value = NA)
            ),
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("researchers") ',
              textInput("plex_researchers", "Researchers", value = NA)
            ),
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("altitude") ',
              textInput("plex_altitude", "Altitude (m.a.s.l)", value = NA)
            ),
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("georeferencing") ',
              textInput("plex_georeferencing", "Georeferencing", value = NA)
            ),
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("project") ',
              textInput("plex_project", "Project", placeholder = "Name or ID for the fieldbook/project", value = NA)
            ),
            
            conditionalPanel(
              condition = ' input["plex_fields"].includes("album") ',
              textInput("plex_album", "Album", placeholder = "url or link", value = NA)
            )
          )
        )
      ),
      
      # Experimental Design
      column(
        width = 3,
        
        div(
          class = "tarpuy-card",
          div(class = "tarpuy-card-title", icon("pencil-ruler"), " Experimental Design"),
          
          uiOutput("plex_factor_selector"),
          uiOutput("plex_design_selector"),
          uiOutput("plex_design_parameters"),
          
          selectizeInput(
            inputId = "plex_zigzag",
            label = "Zigzag",
            choices = c("FALSE", "TRUE")
          ),
          
          fluidRow(column(
            6,
            numericInput("plex_serie", "Plots serie", value = 1000, min = 100)
          ), column(
            6, numericInput("plex_seed", "Seed", value = 0, min = 0)
          ))
        )
      )
    )
  ),
    
  # Module Fieldbook ----------------------------------------------------------
  
  tabPanel(
    "Fieldbook",
    icon = icon("book"),
    
    fluidRow(
      
      column(
        3,
        
        div(
          class = "tarpuy-card",
          div(
            style = "font-size:22px; font-weight:600; line-height:1.15;margin-bottom:10px;",
            icon("book"),
            " Fieldbook Generator"
          ),
          tags$small("Generate and validate fieldbooks from the design sheet")
        ),
        
        div(
          class = "tarpuy-card",
          div(class = "tarpuy-card-title", icon("gear"), " Export Options"),
          
          textInput("fb2export", "Sheet export", placeholder = "sheet name", value = "fb"),
          
          radioButtons(
            "export_design_overwrite",
            "Overwrite",
            inline = TRUE,
            choices = c("no", "yes")
          ),
          
          actionButton(
            "export_design",
            "GENERATE",
            class = "btn btn-success tarpuy-generate"
          )
        ),
        
        div(
          class = "tarpuy-card",
          div(class = "tarpuy-card-title", icon("circle-check"), " Status"),
          uiOutput("fieldbook_status")
        ),
        
        div(
          class = "tarpuy-card",
          div(class = "tarpuy-card-title", icon("chart-simple"), " Layout Summary"),
          uiOutput("fieldbook_summary")
        )
      ),
      
      column(
        9,
        
        div(
          class = "tarpuy-card",
          div(class = "tarpuy-card-title", icon("table"), " Design Preview"),
          uiOutput("gsheet_preview_design")
        )
      )
    ),
    
    # Fieldbook Preview
    fluidRow(
      column(
        12,
        
        div(
          class = "tarpuy-card",
          
          div(
            style = "
          display:flex;
          justify-content:space-between;
          align-items:center;
          margin-bottom:10px;
        ",
            
            div(
              class = "tarpuy-card-title",
              style = "margin-bottom:0px;",
              icon("table-list"),
              " Fieldbook Preview"
            ),
            
            actionButton(
              inputId = "refresh_fieldbook_preview",
              label = "Refresh",
              icon = icon("rotate"),
              class = "btn btn-default btn-sm"
            )
          ),
          
          DT::DTOutput("fieldbook_preview")
        )
      )
    )
  ),
    
  # module sketch -----------------------------------------------------------
  # -------------------------------------------------------------------------
  
  tabPanel(
    "Sketch",
    icon = icon("pen-ruler"),
    
    fluidRow(
      
      column(
        2,
        
        div(
          h5(
            icon("drafting-compass"),
            "Sketch Design"
          )
        ),
        
        radioButtons(
          inputId = "sketch_preview_opt",
          label = "Preview",
          choices = c("Gsheet", "Sketch"),
          inline = TRUE,
          selected = "Sketch"
        ),
        
        uiOutput("sketch_sheets"),
        
        uiOutput("sketch_options"),
        
        actionButton(
          inputId = "update_sketch",
          label = "Refresh",
          class = "btn btn-success"
        )
        
      ),
      
      column(
        10,
        uiOutput("sketch_modules"),
        br(),
        br()
      )
      
    )
    
  ),
    
    # connect -----------------------------------------------------------------
    # -------------------------------------------------------------------------
    
    tabPanel(
      "Mobile",
      icon = icon("plug-circle-check"),
      
      # div(h5(icon("plug-circle-check"), "Mobile")),
      
      fluidRow(
        column(
          2,
          
          h5(
            icon("mobile-screen-button"),
            HTML(
              '<a target="_blank" href="https://play.google.com/store/apps/details?id=com.fieldbook.tracker&hl=en_US">Field-Book (PhenoApp)</a>'
            )
          ),
          
          br(),
          
          radioButtons(
            inputId = "connection_sheet_preview"
            ,
            label = h5(icon("magnifying-glass"), "Preview")
            ,
            choices = c("Traits"
                        , "Field-Book")
            ,
            inline = TRUE
            ,
            selected = "Traits"
          ),
          
          fluidRow(h5(icon("wheat-awn"), "Traits"), uiOutput("connection_sheet_traits"), ),
          
          fluidRow(
            h5(icon("book"), "Field-Book"),
            
            uiOutput("connection_sheet_fieldbook"),
            
            uiOutput("connection_fieldbook_lastfactor"),
            
          ),
          
          br(),
          
          fluidRow(
            h5(icon("cloud-arrow-down"), "Download Files"),
            
            column(6, uiOutput("connection_traits_download"), ),
            
            column(6, uiOutput("connection_fieldbook_download"), ),
            
          ),
          
          
        ),
        
        column(10, uiOutput("connection_sheet_preview"), br(), br())
        
      )
      
    ),
  
  
  bslib::nav_spacer(), 
  bslib::nav_item( 
    tags$a( shiny::icon("heart"), 
            "support", 
            href = "https://github.com/sponsors/flavjack", 
            target = "_blank" ) ), 
  bslib::nav_item( 
    tags$a( shiny::icon("github"), 
            paste("Inti ", packageVersion('inti')), 
            href = "https://inkaverse.com/news/", 
            target = "_blank" ) ),
  
    
    # Tarpuy end code ---------------------------------------------------------
    # -------------------------------------------------------------------------
    
  )