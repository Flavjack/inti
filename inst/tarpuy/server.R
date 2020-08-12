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
library(stringi)

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

output$gsheet_preview <- renderUI({

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
