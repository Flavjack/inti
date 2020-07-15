# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/tarpuy/

# https://console.cloud.google.com/apis/credentials

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

if (file.exists("setup.r")) { source("setup.r") }

library(shiny)
library(inti)
library(metathis)
library(tidyverse)
library(googlesheets4)
library(googleAuthR)


# auth --------------------------------------------------------------------
# -------------------------------------------------------------------------

options(googleAuthR.webapp.client_id = "593859286021-stvni3pmq7mrrl8l6dber27pscs1ebrs.apps.googleusercontent.com")


# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {


  # test --------------------------------------------------------------------
  # -------------------------------------------------------------------------

  sign_ins <- shiny::callModule(googleSignIn, "demo")

  output$g_name = renderText({ sign_ins()$name })
  output$g_email = renderText({ sign_ins()$email })
  output$g_image = renderUI({ img(src=sign_ins()$image) })


  # end test ----------------------------------------------------------------
  # -------------------------------------------------------------------------


  observeEvent(input$cancel, {
    stopApp()
  })

  observeEvent(input$create, {
    stopApp()
  })


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

  })

# import data -----------------------------------------------------------

  output$gsheet_preview <- renderUI({

    gss <- tags$iframe(src = input$gsheet_url,
                       style="height:420px; width:100%; scrolling=no; zoom:1.2")

  })

  observeEvent(input$export_fb, {

    gs <- as_sheets_id(input$gsheet_url)

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

  })

})

