# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

# open https://flavjack.shinyapps.io/tarpuy/
# open http://localhost:1221/

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

options("googleAuthR.scopes.selected", scopes = c("https://www.googleapis.com/auth/spreadsheets"))

options(shiny.port = 1221 )

if (file.exists("setup.r")) { source("setup.r") }

library(shiny)
library(inti)
library(metathis)
library(tidyverse)
library(googlesheets4)
library(googleAuthR)
library(shinydashboard)
library(stringi)

options(gargle_oob_default = TRUE)

gar_set_client(web_json = "www/tarpuy.json")

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {

# auth --------------------------------------------------------------------

  gar_shiny_auth(session)

  access_token <- callModule(googleAuth_js, "js_token")

observe({

      gs4_auth(scopes = c("https://www.googleapis.com/auth/spreadsheets")
               , token = access_token()
               )
  })

gs <- reactive({ as_sheets_id( gsheet_url() ) })

# # create new sheet ---------------------------------------------------------

  gs_created <- NULL
  makeReactiveBinding("gs_created")
  observeEvent( input$create_sheet, {

    validate( need( gs4_has_token(), "LogIn" ) )

      gs_created <<- gs4_create(
        name = paste("Tarpuy", format(Sys.time(), '%Y-%m-%d  %H:%M'))
        , sheets = "tarpuy")

      })

# generate sheet url ------------------------------------------------------

  gsheet_url <- reactive({

    if ( input$gsheet_url != "" ) {

    gsheet_url <- input$gsheet_url

    } else if ( !is.null(gs_created) ) {

    url <- "https://docs.google.com/spreadsheets/d/"

    id <- gs_created %>% pluck(1)

    gsheet_url  <- paste0(url, id)

    } else {

    gsheet_url <- NULL

    }

  })

# open url ----------------------------------------------------------------

output$open_url <- renderUI({

  if ( is.null(gsheet_url() )) {

    link <- "https://docs.google.com/spreadsheets/u/0/"

  } else {

    link <- gsheet_url()

  }

  open <- paste0("window.open('", link, "', '_blank')")

  actionButton(inputId = "open_sheet"
               , label = "Open"
               , class = "btn btn-success"
               , onclick = open
               )
})

# tarpuy plex -------------------------------------------------------------
# -------------------------------------------------------------------------

# test args ---------------------------------------------------------------

observe({

  cat("--------------------------------------------------\n")

  cat("input$gsheet_url")
  print(input$gsheet_url)

  cat("input$plex_fieldbook")
  print(input$plex_fieldbook)

  cat("input$plex_location")
  print(input$plex_location)

  cat("input$plex_dates[1]")
  print(input$plex_dates[1])

  cat("input$plex_dates[2]")
  print(input$plex_dates[2])

  cat("input$plex_about")
  print(input$plex_about)

})

# design type -------------------------------------------------------------

output$plex_design <- renderUI({

  if(input$plex_nfactors == 1) {

    type <- c("crd", "rcbd", "lsd", "lattice")

  } else if (input$plex_nfactors == 2) {

    type <- c("crd", "rcbd", "lsd", "split-crd", "split-rcbd")

  } else if (input$plex_nfactors > 2) {

    type <- c("crd", "rcbd", "lsd")

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

  if( input$plex_fieldbook == "" &
      input$plex_location != "" & !is.na(input$plex_dates[1]) & input$plex_about != "") {

    fbname <- paste(strsplit(input$plex_location, ",") %>% unlist() %>% pluck(1)
                    , as.character(input$plex_dates[1])
                    , input$plex_about
                    , sep = " "
                    ) %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      str_to_upper()

  } else ( fbname <- input$plex_fieldbook )

  plex <- fieldbook_plex(data = NULL
                        , idea = input$plex_idea
                        , goal = input$plex_goal
                        , hypothesis = input$plex_hypothesis
                        , rationale = input$plex_rationale
                        , objectives = input$plex_objectives
                        , plan = input$plex_plan
                        , institutions = input$plex_institutions
                        , researchers = input$plex_researchers
                        , manager = input$plex_manager
                        , location = input$plex_location
                        , altitude = input$plex_altitude
                        , georeferencing = input$plex_georeferencing
                        , environment = input$plex_environment
                        , start = as.character(input$plex_dates[1])
                        , end = as.character(input$plex_dates[2])
                        , about = input$plex_about
                        , fieldbook = fbname
                        , album = input$plex_album
                        , github = input$plex_github
                        , nfactor = input$plex_nfactors
                        , design = input$plex_design
                        , rep = input$plex_rep
                        , serie = input$plex_serie
                        , seed = input$plex_seed
                        )

})

observeEvent(input$plex_generate, {

  if ( !input$gsheet_info %in% sheet_names(gs()) ) {

    sheet_add(ss = gs(), sheet = input$gsheet_info)

    plex()$plex %>% sheet_write(ss = gs(), sheet = input$gsheet_info)

  } else { print ("sheet already exist") }

# -------------------------------------------------------------------------

  if ( !input$gsheet_varlist %in% sheet_names(gs()) ) {

    sheet_add(ss = gs(), sheet = input$gsheet_varlist, .after = input$gsheet_info)

    plex()$variables %>% sheet_write(ss = gs(), sheet = input$gsheet_varlist)

  } else { print ("sheet already exist") }

# -------------------------------------------------------------------------

  if ( !input$gsheet_design %in% sheet_names(gs()) ) {

    sheet_add(ss = gs(), sheet = input$gsheet_design, .after = input$gsheet_varlist)

    plex()$design %>% sheet_write(ss = gs(), sheet = input$gsheet_design)

  } else { print ("sheet already exist") }

})

# tarpuy design -----------------------------------------------------------
# -------------------------------------------------------------------------

# test args ---------------------------------------------------------------

observe({

  cat("--------------------------------------------------\n")

  cat("Factores")
  print(input$design_nfactors)

  cat("Design type")
  print(input$design_type)

  cat("Replication")
  print(input$design_rep)

  cat("Plot digits")
  print(input$design_serie)

  cat("Seed")
  print(input$design_seed)

})

# preview data -----------------------------------------------------------

gsheet_design <- reactive({

  info <- gs4_get(gs())

  url <- info$spreadsheet_url

  id <- info$sheets %>%
    filter(name %in% input$gsheet_design) %>%
    pluck("id")

  plot_url  <- paste(url, id, sep = "#gid=")

})

output$gsheet_preview_design <- renderUI({

  tags$iframe(src = gsheet_design(),
              style="height:580px; width:100%; scrolling=no")

})

# design type -------------------------------------------------------------

output$design_type <- renderUI({

  if(input$design_nfactors == 1) {

    type <- c("crd", "rcbd", "lsd", "lattice")

  } else if (input$design_nfactors == 2) {

    type <- c("crd", "rcbd", "lsd", "split-crd", "split-rcbd")

  } else if (input$design_nfactors > 2) {

    type <- c("crd", "rcbd", "lsd")

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

# variables ---------------------------------------------------------------

    if ( input$gsheet_varlist %in% sheet_names(gs()) ) {

      variables <- gs() %>%
        range_read(input$gsheet_varlist)

    } else { variables <- NULL }

# fieldbook ---------------------------------------------------------------

    if ( input$gsheet_design %in% sheet_names(gs()) ) {

     fieldbook <-  gs() %>%
        range_read(input$gsheet_design)

    } else { fieldbook <- NULL }

# -------------------------------------------------------------------------

    if ( !is.null( fieldbook ) ) {

      fbds <- fieldbook %>%
        inti::fieldbook_design(
          n_factors = input$design_nfactors
          , type = input$design_type
          , rep = input$design_rep
          , serie = input$design_serie
          , seed = input$design_seed
        ) %>%
        inti::fieldbook_varlist( variables )

        fbds %>%
          pluck("design") %>%
          as.data.frame() %>%
          write_sheet(ss = gs(), sheet = input$gsheet_fb)

    } else { "Create your design" }

# -------------------------------------------------------------------------

    if ( !"sketch" %in% sheet_names(gs()) & input$gsheet_fb %in% sheet_names(gs()) ) {

      sheet_add(ss = gs(), sheet = "sketch" )

    }

  })

# tarpuy sketch -----------------------------------------------------------
# -------------------------------------------------------------------------

# test args ---------------------------------------------------------------

observe({

  cat("--------------------------------------------------\n")

  cat("input$sketch_xlab")
  print(input$sketch_xlab)

  cat("input$sketch_dim")
  print(input$sketch_dim)

  cat("input$sketch_dim2")
  print(input$sketch_dim2)

})

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

  tags$iframe(src = gsheet_fb(),
              style="height:580px; width:100%; scrolling=no")

})

# options -----------------------------------------------------------------

fb_factors <- eventReactive(input$update_sketch, {

  validate( need( input$gsheet_fb %in% sheet_names(gs())
                  , "Create your fieldbook") )

  factors <- gs() %>%
    range_read( input$gsheet_fb ) %>% names()

})

output$sketch_options <- renderUI({

  factors <- fb_factors()

  tagList(

    selectInput(inputId = "sketch_factor"
                , label = "Factor"
                , multiple = FALSE
                , choices = c(""
                              , factors)
                , width = "100%"
    ),

    selectInput(inputId = "sketch_dim"
                , label = "Block factor"
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

    textInput(inputId = "sketch_dim2"
              , label = "Block factor (optional)"
              , value = NA
              , placeholder = "NcolxNrow"
              , width = "100%"
    )
  )

})

# plot --------------------------------------------------------------------

plot_sketch <- reactive({

  validate( need( input$sketch_factor, "Select your design factor") )
  validate( need( input$sketch_dim, "Select your blocking factor") )

  if ( input$gsheet_fb %in% sheet_names(gs()) ) {

    fb_sketch <- gs() %>%
      range_read( input$gsheet_fb )
  }

  if ( input$sketch_xlab == "" | is.null(input$sketch_xlab) ){ sketch_xlab <- NULL } else {sketch_xlab <- input$sketch_xlab}
  if ( input$sketch_ylab == "" | is.null(input$sketch_ylab) ){ sketch_ylab <- NULL } else {sketch_ylab <- input$sketch_ylab}
  if ( input$sketch_glab == "" | is.null(input$sketch_glab) ){ sketch_glab <- NULL } else {sketch_glab <- input$sketch_glab}

  if ( input$sketch_dim2 != "" ) { blocking <- input$sketch_dim2 } else { blocking <- input$sketch_dim }

  plot_sketch <-  plot_design(data = fb_sketch
                             , factor = input$sketch_factor
                             , dim = blocking
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

# end app -----------------------------------------------------------------
# -------------------------------------------------------------------------

})

