# yupanapro----------------------------------------------------------------
# -------------------------------------------------------------------------

# open https://flavjack.shinyapps.io/yupanapro/
# runApp('inst/tarpuy', port = 1221)
# browseURL("http://localhost:1221/")

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
library(bootstraplib)

gar_set_client(web_json = "www/yupanapro.json")

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {

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

gs <- reactive({

  gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets"
           , cache = FALSE
           , use_oob = TRUE
           , token = access_token())

  as_sheets_id(input$gsheet_url)

  })

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# module: data ------------------------------------------------------------
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

  cat("Design sheet")
  print(input$gsheet_name)

  cat("Variable sheet")
  print(input$varlist_name)

})



})
