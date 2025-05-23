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

shinyServer(function(input, output, session) {

# close auto local session ------------------------------------------------

  observe({

    if(Sys.getenv('SHINY_PORT') == "") {

      session$onSessionEnded(stopApp)
    }

  })
  
# auth --------------------------------------------------------------------

  source("www/auth.R")
  if (file.exists("www/analytics.r")) {source("www/analytics.r", local = T)}
  
  gar_shiny_auth(session)

# longin vs local ---------------------------------------------------------
  
  access_token <- moduleServer(id = "js_token"
                               , module = googleAuth_js)
  

# -------------------------------------------------------------------------

  output$login <- renderUI({
    
    if (file.exists("www/cloud.json")) {
      
      googleAuth_jsUI("js_token"
                      , login_text = "LogIn"
                      , logout_text = "LogOut"
                      )
      
    } else {
      
      actionButton("local_user", "Local", class = "btn-success")
      
    }
    
  })

  gs <- reactive({

    if(Sys.getenv('SHINY_PORT') == "") {

      gs4_auth(T)

    } else {

    gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets"
             , cache = FALSE
             , use_oob = TRUE
             , token = access_token()
             )

    }

    validate( need( gs4_has_token(), "LogIn and insert a url" ) )

    as_sheets_id( fieldbook_url() )

  })
  
# generate sheet url ------------------------------------------------------

  fieldbook_url <- reactive({

    validate( need( input$fieldbook_url, "LogIn and insert a url" ) )

    if ( input$fieldbook_url != "" ) {

      fieldbook_url <- input$fieldbook_url

    }

    })

  # create new sheet ---------------------------------------------------------

  gs_created <- NULL
  makeReactiveBinding("gs_created")
  observeEvent( input$create_sheet, {

    if(Sys.getenv('SHINY_PORT') == "") {
      
      gs4_auth(T)
      
    } else {
      
      gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets"
               , cache = FALSE
               , use_oob = TRUE
               , token = access_token()
      )
      
    }
    
    validate( need( gs4_has_token(), "LogIn and insert a url" ) )
    
    gs_created <<- gs4_create(
      name = paste("Tarpuy", format(Sys.time(), '%Y-%m-%d  %H:%M'))
      , sheets = "tarpuy"
      , locale = "en_US"
      )
    
    # updt link ---------------------------------------------------------------

    url <- "https://docs.google.com/spreadsheets/d/"

    id <- gs_created %>% pluck(1)

    gsheet_url  <- paste0(url, id)

    updateTextInput(session, inputId = "fieldbook_url", value = gsheet_url)

  })

  # open url ----------------------------------------------------------------

  output$open_url <- renderUI({

    if ( input$fieldbook_url == "" ) {

      link <- "https://docs.google.com/spreadsheets/u/0/"

    } else {

      link <- fieldbook_url()

    }

    open <- paste0("window.open('", link, "', '_blank')")

    actionButton(inputId = "open_sheet"
                 , label = "Open"
                 , class = "btn btn-success"
                 , width = "80%"
                 , onclick = open
    )

  })

# tarpuy plex -------------------------------------------------------------
# -------------------------------------------------------------------------

  # design type -------------------------------------------------------------

  output$plex_design <- renderUI({
    
    if(input$plex_nfactors == 1 & input$plex_rep == 1) {
      
      type <- c("sorted", "unsorted")
      
    } else if(input$plex_nfactors == 1) {

      type <- c("crd", "rcbd"
                # , "lsd", "lattice"
                )

    } else if (input$plex_nfactors == 2) {

      type <- c("crd", "rcbd"
                # , "lsd", "split-crd", "split-rcbd", "split-lsd"
                )

    } else if (input$plex_nfactors > 2) {

      type <- c("crd", "rcbd"
                # , "lsd"
                )

    }

    selectizeInput(
      inputId = "plex_design",
      label = "Design type",
      choices = type,
      multiple = FALSE
    )

  })
  

# -------------------------------------------------------------------------

  plex <- reactive({
    
    plex <- tarpuy_plex(data = NULL
                         , title = input$plex_title
                         , objectives = input$plex_objectives
                         , hypothesis = input$plex_hypothesis
                         , rationale = input$plex_rationale
                         , references = input$plex_references
                         , plan = input$plex_plan
                         , institutions = input$plex_institutions
                         , researchers = input$plex_researchers
                         , manager = input$plex_manager
                         , location = input$plex_location
                         , altitude = input$plex_altitude
                         , georeferencing = input$plex_georeferencing
                         , environment = input$plex_environment
                         , start = input$plex_dates[1]
                         , end = input$plex_dates[2]
                         , about = input$plex_about
                         , fieldbook = input$plex_fieldbook
                         , album = input$plex_album
                         , project = input$plex_project
                         , repository = input$plex_repository
                         , manuscript = input$plex_manuscript
                         , nfactor = input$plex_nfactors
                         , design = input$plex_design
                         , rep = input$plex_rep
                         , serie = input$plex_serie
                         , seed = input$plex_seed
                         , zigzag = input$plex_zigzag
                        )

  })
  
# sheets to create --------------------------------------------------------

output$plex_sheets2create <- renderUI({
  
  sheets <- c(input$gsheet_info, input$gsheet_design, input$gsheet_varlist)
  
  checkboxGroupInput(inputId = "plex_sheet2create"
                     , label = h5(icon("cloud-arrow-up"), "Experimental plan")
                     , choices = sheets
                     , selected = sheets
  )
  
  
})  
  

# -------------------------------------------------------------------------
  
observeEvent(input$plex_generate, {

    validate( need( input$fieldbook_url, "LogIn and create or insert a url" ) )


# info --------------------------------------------------------------------

    if ( !input$gsheet_info %in% sheet_names(gs()) & input$gsheet_info %in% input$plex_sheet2create) {

      sheet_add(ss = gs(), sheet = input$gsheet_info)

      plex()$plex %>% sheet_write(ss = gs(), sheet = input$gsheet_info)

    } else { print ("sheet already exist: info") }


# varlist -----------------------------------------------------------------

    if ( !input$gsheet_varlist %in% sheet_names(gs()) & input$gsheet_varlist %in% input$plex_sheet2create) {

      sheet_add(ss = gs(), sheet = input$gsheet_varlist, .after = input$gsheet_info)

      plex()$variables %>% sheet_write(ss = gs(), sheet = input$gsheet_varlist)

    } else { print ("sheet already exist: varlist") }


# design ------------------------------------------------------------------

    if ( !input$gsheet_design %in% sheet_names(gs()) & input$gsheet_design %in% input$plex_sheet2create) {

      sheet_add(ss = gs(), sheet = input$gsheet_design, .after = input$gsheet_varlist)

      plex()$design %>% sheet_write(ss = gs(), sheet = input$gsheet_design)

    } else { print ("sheet already exist: design") }

# logbook -----------------------------------------------------------------

    if ( !input$plex_logbook %in% sheet_names(gs()) & input$plex_logbook != "" ) {

      sheet_add(ss = gs(), sheet = input$plex_logbook)

      plex()$logbook %>% sheet_write(ss = gs(), sheet = input$plex_logbook)
      
      print ("sheet created: logbook")
    } 

# timetable ---------------------------------------------------------------

    if ( !input$plex_timetable %in% sheet_names(gs()) & input$plex_timetable != ""  ) {

      sheet_add(ss = gs(), sheet = input$plex_timetable)

      plex()$timetable %>% sheet_write(ss = gs(), sheet = input$plex_timetable)
      
      print ("sheet created: timetible")
    } 

# budget ------------------------------------------------------------------

    if ( !input$plex_budget %in% sheet_names(gs()) & input$plex_budget != ""  ) {

      sheet_add(ss = gs(), sheet = input$plex_budget)

      plex()$budget %>% sheet_write(ss = gs(), sheet = input$plex_budget)
      
      print ("sheet created: budget")
    } 
    

# -------------------------------------------------------------------------
    
    if ( "tarpuy" %in% sheet_names(gs()) ) { 
      
      gs() %>% sheet_delete(sheet = "tarpuy")
      
    }
    
  })

# tarpuy design -----------------------------------------------------------
# -------------------------------------------------------------------------

# preview design -----------------------------------------------------------

gsheet_design <- reactive({

  info <- gs4_get(gs())

  url <- info$spreadsheet_url

  id <- info$sheets %>%
    filter(name %in% input$gsheet_design) %>%
    pluck("id")

  plot_url  <- paste(url, id, sep = "#gid=")

})

output$gsheet_preview_design <- renderUI({

  validate( need( input$fieldbook_url, "LogIn and create or insert a url" ) )

  tags$iframe(src = gsheet_design(),
              style="height:580px; width:100%; scrolling=no")

})

# design type -------------------------------------------------------------

output$design_type <- renderUI({

  if(input$design_nfactors == 1) {

    type <- c("crd", "rcbd"
              # , "lsd", "lattice"
              )

  } else if (input$design_nfactors == 2) {

    type <- c("crd", "rcbd"
              # , "lsd", "split-crd", "split-rcbd", "split-lsd"
              )

  } else if (input$design_nfactors > 2) {

    type <- c("crd", "rcbd"
              # , "lsd"
              )

  }

  selectizeInput(
    inputId = "design_type",
    label = "Design type",
    choices = type,
    multiple = FALSE
  )

})

# export fieldbook --------------------------------------------------------

observeEvent(input$export_design, {
  
  validate(need(input$fieldbook_url, "LogIn and create or insert a url"))
  
# fieldbook ---------------------------------------------------------------
  
  if ( input$gsheet_design %in% sheet_names(gs()) ) {
    
    fb <- gs() %>%
      range_read(input$gsheet_design)
    
  } else { fieldbook <- NULL }
  
# variables ---------------------------------------------------------------

  if ( input$gsheet_varlist %in% sheet_names(gs()) ) {

    variables <- gs() %>%
      range_read(input$gsheet_varlist, col_types = "c")

  } else { variables <- NULL }

# -------------------------------------------------------------------------

  fieldbook <- fb %>%
      tarpuy_design(
        nfactors = input$design_nfactors
        , type = input$design_type
        , rep = input$design_rep
        , serie = input$design_serie
        , zigzag = as.logical(input$design_zigzag)
        , nrows = input$design_rep
        , seed = input$design_seed
        , fbname = input$design_qr
      ) 
  
  sheet_export <- input$fb2export %>% gsub("[[:space:]]", "_", .)
  
  if(!is.null(fieldbook)) {
    
    fbds <- tarpuy_traits(fieldbook = fieldbook
                          , last_factor = NULL
                          , traits = variables
                          )
    
    if (input$export_design_overwrite == "no" & !sheet_export %in% sheet_names(gs())) {
      
      fbds$fieldbook %>% 
        write_sheet(ss = gs(), sheet = sheet_export)
      
    } else if(input$export_design_overwrite == "yes") {
      
      fbds$fieldbook %>% 
        write_sheet(ss = gs(), sheet = sheet_export)
      
    } else {  print ("sheet already exist") }
    
  } else {"Insert factor levels"}
  
# -------------------------------------------------------------------------

  if (!"sketch" %in% sheet_names(gs()) & sheet_export %in% sheet_names(gs()) ) {

    sheet_add(ss = gs(), sheet = "sketch", .after = sheet_export)

  }

})

# tarpuy sketch -----------------------------------------------------------
# -------------------------------------------------------------------------


# preview sketch ----------------------------------------------------------

  gsheet_fb <- reactive({

    info <- gs4_get(gs())

    url <- info$spreadsheet_url

    id <- info$sheets %>%
      filter(name %in% input$gsheet_fb) %>%
      pluck("id")

    sketch_url  <- paste(url, id, sep = "#gid=")

  })

  output$gsheet_preview_sketch <- renderUI({

    validate( need( input$fieldbook_url, "LogIn and create or insert a url" ) )

    tags$iframe(src = gsheet_fb(),
                style="height:580px; width:100%; scrolling=no")

  })
  
  

# sketch sheets -----------------------------------------------------------

  sketch_sheets <- eventReactive(input$update_sketch,{
    
    names <- gs() %>% sheet_names()
    
  })
  
  output$sketch_sheets <-  renderUI({
    
    selectizeInput(
      inputId = "sketch_sheets"
      , label = "Fieldbook"
      , choices = c("choose" = ""
                    , sketch_sheets())
      , multiple = FALSE
    )
    
  })

  # options -----------------------------------------------------------------

  output$sketch_options <- renderUI({
    
    validate( need( input$sketch_sheets, "Insert your fieldbook" ) )

    factors <- gs() %>%
          range_read( input$sketch_sheets ) %>% names()


    tagList(

      selectInput(inputId = "sketch_factor"
                  , label = "Factor"
                  , multiple = FALSE
                  , choices = c(""
                                , factors)
                  , width = "100%"
      ),

      selectInput(inputId = "sketch_fill"
                  , label = "Fill factor"
                  , multiple = FALSE
                  , selected = "plots"
                  , choices = c(""
                                , factors)
                  , width = "100%"
      ),

    )

  })
  
fb_sketch <- reactive({
  
  if ( input$sketch_sheets %in% sheet_names(gs()) ) {
    
     gs() %>%
      range_read( input$sketch_sheets )
  }
  
})

# plot --------------------------------------------------------------------

  plot_sketch <- reactive({

    validate( need( input$fieldbook_url, "LogIn and create or insert a url" ) )
    validate( need( input$sketch_sheets, "Insert your fieldbook" ) )
    validate( need( input$sketch_factor, "Select your design factor") )
    
# -------------------------------------------------------------------------

    if ( input$sketch_xlab == "" | is.null(input$sketch_xlab) ){ sketch_xlab <- NULL } else {sketch_xlab <- input$sketch_xlab}
    if ( input$sketch_ylab == "" | is.null(input$sketch_ylab) ){ sketch_ylab <- NULL } else {sketch_ylab <- input$sketch_ylab}
    if ( input$sketch_glab == "" | is.null(input$sketch_glab) ){ sketch_glab <- NULL } else {sketch_glab <- input$sketch_glab}

    plot_sketch <-  tarpuy_plotdesign(data = fb_sketch()
                                      , factor = input$sketch_factor
                                      , fill = input$sketch_fill
                                      , xlab = sketch_xlab
                                      , ylab = sketch_ylab
                                      , glab = sketch_glab
                                      )
  })

  # -------------------------------------------------------------------------

  output$plot_sketch <- renderImage({

    dpi <- input$sketch_dpi
    ancho <- input$sketch_width
    alto <- input$sketch_height

    outfile <- tempfile(fileext = ".png")

    png(outfile, width = ancho, height = alto, units = "cm", res = dpi)
    print(plot_sketch())
    dev.off()

    list(src = outfile)

  }, deleteFile = TRUE)

  # options panel -----------------------------------------------------------

  output$sketch_modules <- renderUI({

    if ( input$sketch_preview_opt == "Gsheet" ) {

      uiOutput("gsheet_preview_sketch")

    } else if ( input$sketch_preview_opt == "Sketch" ) {

      tagList(

        fluidRow(

          box(width = 2,

              textInput(inputId = "sketch_xlab"
                        , label = "Label X"
                        , value = NA
                        , placeholder = "Exp. Units"
              )
          ),

          box(width = 2,

              textInput(inputId = "sketch_ylab"
                        , label = "Label Y"
                        , value = NA
                        , placeholder = "Blocks"
              )
          ),

          box(width = 2,

              textInput(inputId = "sketch_glab"
                        , label = "Label Groups"
                        , value = NA
                        , placeholder = "Groups"
              )
          ),

          box(width = 2,

              numericInput(inputId = "sketch_width"
                           , label = "Width (cm)"
                           , value = 20
                           , step = 5
                           , min = 5)
          ),

          box(width = 2,

              numericInput(inputId = "sketch_height"
                           , label = "Height (cm)"
                           , value = 10
                           , step = 5
                           , min = 5)
          ),

          box(width = 2,

              numericInput(inputId = "sketch_dpi"
                           , label = "Resolution"
                           , value = 100
                           , step = 50
                           , min = 100)
          )
        ),

        div(imageOutput("plot_sketch"), align = "center")

      )

    }

  })



# connection --------------------------------------------------------------
# -------------------------------------------------------------------------

output$connection_sheet_fieldbook <- renderUI({
  
  validate(need(fieldbook_url(), "LogIn and insert a url") )
  
  info <- gs4_get(gs())
  
  names <- info$sheets$name 
  
  selectInput(inputId = "connection_sheet_fieldbook"
              , label = NULL
              , choices = c("choose" = ""
                            , names)
  )
})

output$connection_sheet_traits <- renderUI({
  
  validate(need(fieldbook_url(), "LogIn and insert a url") )
  
  info <- gs4_get(gs())
  
  names <- info$sheets$name 
  
  selectInput(inputId = "connection_sheet_traits"
              , label = NULL
              , choices = c("choose" = ""
                            , names)
  )
})

# -------------------------------------------------------------------------

connection_sheet_preview <- reactive({
  
  info <- gs4_get(gs())
  
  url <- info$spreadsheet_url
  
  # id <- if(input$connection_sheet_preview == "Field-Book") {
  # 
  #   info$sheets %>%
  #     filter(name %in% input$connection_sheet_fieldbook) %>%
  #     pluck("id")
  #   
  # } else if (input$connection_sheet_preview == "Traits")  {
  #   
  #   info$sheets %>%
  #     filter(name %in% input$connection_sheet_traits) %>%
  #     pluck("id")
  #   
  # }
  
  id <- if (input$connection_sheet_preview == "Traits")  {
    
    info$sheets %>%
      filter(name %in% input$connection_sheet_traits) %>%
      pluck("id")
    
  } else {
    
    info$sheets %>%
      filter(name %in% input$connection_sheet_fieldbook) %>%
      pluck("id")
  } 
  
  preview <- paste(url, id, sep = "#gid=")
  
})

output$connection_sheet_preview <- renderUI({
  
  validate( need( input$fieldbook_url, "LogIn and create or insert a url" ) )
  
  tags$iframe(src = connection_sheet_preview(),
              style="height:580px; width:100%; scrolling=no")
  
})

# -------------------------------------------------------------------------

traits <- reactive({
  
  validate(need(input$connection_sheet_traits, "Need table with traits"))
  
  gs() %>%
    googlesheets4::range_read(sheet = input$connection_sheet_traits
                              , col_types = "c") 
  
})

fieldbook <- reactive({
  
  validate(need(input$connection_sheet_fieldbook, "Need field book table"))
  
  gs() %>%
    googlesheets4::range_read(sheet = input$connection_sheet_fieldbook) 
  
})

# -------------------------------------------------------------------------

output$connection_fieldbook_lastfactor <- renderUI({
  
  validate(need(fieldbook(), "LogIn and insert a url") )
  
  names <- fieldbook() %>% names()
  
  selectInput(inputId = "connection_fieldbook_lastfactor"
              , label = "Last Factor"
              , choices = c("choose" = ""
                            , names)
  )
})

# -------------------------------------------------------------------------

fbapp <- reactive({
  
  tarpuy_traits(fieldbook = fieldbook()
               , last_factor = input$connection_fieldbook_lastfactor
               , traits = traits()
               )
})

# -------------------------------------------------------------------------

output$connection_traits_trt <- downloadHandler(
  
  filename = function() {
    paste('traits-', Sys.Date(), '.trt', sep='')
  },
  content = function(con) {
    fbapp()$traits %>% 
      write_delim(file = con, delim = ",", quote = "all", na = '""')
  }
)

output$connection_fieldbook_csv <- downloadHandler(
  
  filename = function() {
    paste('fieldbook-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    fbapp()$fb %>% 
      write.csv(file = con)
  }
)

# -------------------------------------------------------------------------

output$connection_traits_download <- renderUI({
  
  validate(need(input$connection_fieldbook_lastfactor, ""))
  validate(need(input$connection_sheet_traits, ""))

  downloadButton(outputId = 'connection_traits_trt'
                 , label =  h6('Traits') 
                 , icon = icon("download", "fa-2x") 
  )
  
})

output$connection_fieldbook_download <- renderUI({
  
  validate(need(input$connection_fieldbook_lastfactor, ""))
  validate(need(input$connection_sheet_traits, ""))

  downloadButton(outputId = 'connection_fieldbook_csv'
                 , label = h6('FieldBook')
                 , icon = icon("download", "fa-2x")
  )
  
})

# end app -----------------------------------------------------------------
# -------------------------------------------------------------------------

})
