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

    gss <- tags$iframe(src = input$fieldbook_url,
                       style="height:420px; width:100%; scrolling=no; zoom:1.2")

  })

# auth --------------------------------------------------------------------
# -------------------------------------------------------------------------

gar_shiny_auth(session)

access_token <- callModule(googleAuth_js, "js_token")

gs <- reactive({

  gs4_auth(T)

  # gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets"
  #          , cache = FALSE
  #          , use_oob = TRUE
  #          , token = access_token()
  #          )

  as_sheets_id(input$fieldbook_url)

  })

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# module: data ------------------------------------------------------------
# -------------------------------------------------------------------------

observe({

  cat("--------------------------------------------------\n")

  cat("fbsmr_gsheet")
  print(input$fbsmr_gsheet)

  cat("last_factor")
  print(input$last_factor)

  cat("model_facts")
  print(input$model_facts)

  cat("comp_facts")
  print(input$comp_facts)

  cat("test_comp")
  print(input$test_comp)

  cat("sig_level")
  print(input$sig_level)


})


# fieldbook summary -------------------------------------------------------
# -------------------------------------------------------------------------

fieldbook <- reactive({

  if ( input$fbsmr_gsheet %in% sheet_names(gs()) & input$fbsmr_gsheet != "" ) {

    gs() %>%
      range_read(input$fbsmr_gsheet)

    } else { fieldbook <- NULL }

  })

output$last_factor <- renderUI({

  if ( !is.null(fieldbook()) ) {

    fieldbook_names <- fieldbook() %>%
      names()

    selectInput(inputId = "last_factor"
                , label = "Last factor"
                , choices = c("choose" = ""
                              , fieldbook_names)
                )

    } else { print ("Insert sheet name") }

})

output$comp_facts <- renderUI({

  if ( !is.null(fieldbook()) && input$last_factor != ""  ) {

  fieldbook_fctr_names <- fieldbook() %>%
    select( 1:input$last_factor ) %>%
    names()

  selectInput(inputId = "comp_facts"
              , label = "Comparison factors"
              , multiple = TRUE
              , choices = c("choose" = ""
                            , fieldbook_fctr_names)
              )

  } else { print("Insert last factor") }

})

observeEvent(input$fbsmr_generate, {

  if ( !is.null(fieldbook()) && input$last_factor != "" )  {

  fbsmr <- fieldbook_summary(data = fieldbook()
                             , last_factor = input$last_factor
                             , model_facts = input$model_facts
                             , treat_comp = paste0(input$comp_facts, collapse = ":")
                             , test_comp = input$test_comp
                             , sig_level = input$sig_level
                             )


  if ( !"fbsmr" %in% sheet_names(gs()) ) {

    sheet_add(ss = gs(), .after = input$fbsmr_gsheet, sheet = "fbsmr")

    fbsmr %>% sheet_write(ss = gs(), sheet = "fbsmr")

  } else { print ("sheet already exist") }

  }

  })


# fieldbook report --------------------------------------------------------
# -------------------------------------------------------------------------








# end yupana --------------------------------------------------------------
# -------------------------------------------------------------------------

})
