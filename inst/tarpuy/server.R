# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/tarpuy/
# http://localhost:1221/

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

gar_set_client(web_json = "files/tarpuy.json")

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {

# design type -------------------------------------------------------------
# -------------------------------------------------------------------------

  output$design_type <- renderUI({

    if(input$nFactors == 1) {

      type <- c("crd", "rcbd", "lsd", "lattice")

    } else if (input$nFactors == 2) {

      type <- c("crd", "rcbd", "lsd", "split-crd", "split-rcbd")

    } else if (input$nFactors > 2) {

      type <- c("crd", "rcbd", "lsd")

    }

    selectizeInput(
      inputId = "type",
      label = "Design type",
      choices = type,
      multiple = FALSE
    )

  })

# test code ---------------------------------------------------------------
# -------------------------------------------------------------------------

  observe({

    cat("--------------------------------------------------\n")

    cat("Factores")
    print(input$nFactors)

    cat("Design type")
    print(input$type)

    cat("Replication")
    print(input$rep)

    cat("Plot digits")
    print(input$serie)

    cat("Seed")
    print(input$seed)

    cat("Sheet name")
    print(input$gsheet_name)

    cat("access_token")
    print(access_token())

  })

# import data -----------------------------------------------------------
# -------------------------------------------------------------------------

  output$gsheet_preview <- renderUI({

    gss <- tags$iframe(src = input$gsheet_url,
                       style="height:420px; width:100%; scrolling=no; zoom:1.2")

  })

# auth --------------------------------------------------------------------
# -------------------------------------------------------------------------

gar_shiny_auth(session)

access_token <- callModule(googleAuth_js, "js_token")

# fieldbook design --------------------------------------------------------
# -------------------------------------------------------------------------

  observeEvent(input$export_fb, {

    gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets"
             , cache = FALSE
             , use_oob = TRUE
             , token = access_token()
             )

    gs <- as_sheets_id(input$gsheet_url)

    if ( input$gsheet_name %in% sheet_names(gs) ) {


      fb <- gs %>%
        range_read(input$gsheet_name)

      fbds <- fb %>%
        inti::fieldbook_design(
          nFactors = input$nFactors
          , type = input$type
          , rep = input$rep
          , serie = input$serie
          , seed = input$seed
        )

      if ( "sketch" %in% sheet_names(gs) ) {

        sheet_delete(gs, "sketch")

      }

      if( length(fbds) == 2 ) {

        fbds %>%
          pluck("design") %>%
          as.data.frame() %>%
          write_sheet(ss = gs, sheet = "fb")

        fbds %>%
          pluck("sketch") %>%
          as.data.frame() %>%
          write_sheet(ss = gs, sheet = "sketch")

      }

      if( length(fbds) == 1 ) {

        fbds %>%
          pluck("design") %>%
          as.data.frame() %>%
          write_sheet(ss = gs, sheet = "fb")

      }

    } else { print("Sheet not found") }

  })

})

