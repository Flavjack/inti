# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

# open https://flavjack.shinyapps.io/tarpuy/

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
library(shinydashboard)

gar_set_client(web_json = "www/tarpuy.json")

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {

# -------------------------------------------------------------------------

  gsheet_url <- reactive({

    if ( input$gsheet_url == "" ) {

      gsheet_url <- "https://docs.google.com/spreadsheets/d/1a82XIbTeWPC4pwvu9Zjl4qrGitGQ_B-mf3-w67VS4-Q/edit#gid=1664155282"

    } else { input$gsheet_url }

  })

# auth --------------------------------------------------------------------
# -------------------------------------------------------------------------

gar_shiny_auth(session)

access_token <- callModule(googleAuth_js, "js_token")

gs <- reactive({

  if(Sys.getenv('SHINY_PORT') == "") {

    gs4_auth(T)

  } else {

    gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets"
             , cache = FALSE
             , use_oob = TRUE
             , token = access_token())
  }

  as_sheets_id( gsheet_url() )

})

# tarpuy plex -------------------------------------------------------------
# -------------------------------------------------------------------------

# test args ---------------------------------------------------------------

observe({

  cat("--------------------------------------------------\n")

  cat("input$plex_fields")
  print(input$plex_fields)

})


# conditional panel -------------------------------------------------------

output$plex_fields <- renderUI({

  tagList(

  if("location" %in% input$plex_fields ){

    textInput(inputId = "plex_location"
              , label = "Location"
              , width = "100%"
    )

  },

  if("dates" %in% input$plex_fields ){

    dateRangeInput(inputId = "plex_dates"
                   , label = "Experiment dates (start / end)"
                   , end = NA
                   , width = "100%"
    )

  },

  if("about" %in% input$plex_fields ){

    textInput(inputId = "plex_about"
              , label = "About"
              , width = "100%"
              , placeholder = "Short project description"
              )

  },

  if("fieldbook" %in% input$plex_fields ){

    textInput(inputId = "plex_fieldbook"
              , label = "Fieldbook name"
              , width = "100%"
    )

  },

  if("evaluators" %in% input$plex_fields ){

    textInput(inputId = "plex_evaluators"
              , label = "Evaluators"
              , width = "100%"
    )

  },

  if("environment" %in% input$plex_fields ){

    textInput(inputId = "plex_environment"
              , label = "Environment"
              , width = "100%"
    )

  },

  if("institutions" %in% input$plex_fields ){

    textInput(inputId = "plex_institutions"
              , label = "Institutions"
              , width = "100%"
    )

  },

  if("researchers" %in% input$plex_fields ){

    textInput(inputId = "plex_researchers"
              , label = "Researchers"
              , width = "100%"
    )

  },

  if("altitude" %in% input$plex_fields ){

    textInput(inputId = "plex_altitude"
              , label = "Altitude (m.a.s.l)"
              , width = "100%"
    )

  },

  if("georeferencing" %in% input$plex_fields ){

    textInput(inputId = "plex_georeferencing"
              , label = "Georeferencing"
              , width = "100%"
    )

  },

  if("album" %in% input$plex_fields ){

    textInput(inputId = "plex_album"
              , label = "Album"
              , width = "100%"
              , placeholder = "url or link"
    )

  },

  if("github" %in% input$plex_fields ){

    textInput(inputId = "plex_github"
              , label = "Github"
              , width = "100%"
              , placeholder = "url or link"
    )

  }

)

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

  plex <- fieldbook_plex(data = NULL
                        , idea = input$plex_idea
                        , goal = input$plex_goal
                        , hypothesis = input$plex_hypothesis
                        , rationale = input$plex_rationale
                        , objectives = input$plex_objectives
                        , plan = input$plex_plan
                        , institutions = input$plex_institutions
                        , researchers = input$plex_researchers
                        , evaluators = input$plex_evaluators
                        , location = input$plex_location
                        , altitude = input$plex_altitude
                        , georeferencing = input$plex_georeferencing
                        , environment = input$plex_environment
                        , start = as.character(input$plex_dates[1])
                        , end = as.character(input$plex_dates[2])
                        , about = input$plex_about
                        , fieldbook = input$plex_fieldbook
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

    sheet_add(ss = gs(), sheet = input$gsheet_info, .before = 1)

    plex()$plex %>% sheet_write(ss = gs(), sheet = input$gsheet_info)

  } else { print ("sheet already exist") }

# -------------------------------------------------------------------------

  if ( !input$gsheet_design %in% sheet_names(gs()) ) {

    sheet_add(ss = gs(), sheet = input$gsheet_design, .after = input$gsheet_info)

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

output$gsheet_preview <- renderUI({

  tags$iframe(src = gsheet_design(),
              style="height:550px; width:100%; scrolling=no")

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

  observeEvent(input$export_fb, {

# sketch delete -----------------------------------------------------------
# -------------------------------------------------------------------------

    if ( "sketch" %in% sheet_names(gs()) ) {

      sheet_delete(gs(), "sketch")

    }

# variables ---------------------------------------------------------------
# -------------------------------------------------------------------------

    if ( input$gsheet_varlist %in% sheet_names(gs()) ) {

      variables <- gs() %>%
        range_read(input$gsheet_varlist)

    } else { variables <- NULL }

# fieldbook ---------------------------------------------------------------
# -------------------------------------------------------------------------

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
        inti::fieldbook_varlist(variables )

      if( length(fbds) == 2 ) {

        fbds %>%
          pluck("design") %>%
          as.data.frame() %>%
          write_sheet(ss = gs(), sheet = "fb")

        fb <- fbds %>%
          pluck("sketch") %>%
          as.data.frame() %>%
          write_sheet(ss = gs(), sheet = "sketch")

      }

      if( length(fbds) == 1 ) {

        fb <- fbds %>%
          pluck("design") %>%
          as.data.frame() %>%
          write_sheet(ss = gs(), sheet = "fb")

      }

    }

  })

})
