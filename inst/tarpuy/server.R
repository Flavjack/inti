# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/tarpuy/

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

pkgs_cran <- c(
  "shiny"
  , "miniUI"
  , "shinyFiles"
  , "utils"
  , "fs"
  , "metathis"
  , "googlesheets4"
  , "googledrive"
  , "dplyr"
)

installed_cran <- pkgs_cran %in% rownames(installed.packages())
if (any(installed_cran == FALSE)) {
  install.packages(pkgs_cran[!installed_cran])
}

pkgs_git <- c(
  "inti"
)

installed_git <- pkgs_git %in% rownames(installed.packages())
if (any(installed_git == FALSE)) {
  devtools::install_github("flavjack/inti", upgrade = "always")
}

invisible(lapply(c(pkgs_cran, pkgs_git), library, character.only = TRUE))
rm(pkgs_cran, installed_cran, pkgs_git, installed_git)

library(shiny)
library(miniUI)
library(shinyFiles)
library(utils)
library(fs)
library(inti)
library(metathis)
library(googlesheets4)
library(googledrive)
library(dplyr)
library(purrr)

# auth --------------------------------------------------------------------
# -------------------------------------------------------------------------

googlesheets4::gs4_auth(T)
googledrive::drive_auth(T)

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {

  observeEvent(input$cancel, {
    stopApp()
  })

  observeEvent(input$create, {
    stopApp()
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

  })

# import data -----------------------------------------------------------

  output$gsheet_preview <- renderUI({

    gss <- tags$iframe(src = input$gsheet_url,
                       style="height:420px; width:100%; scrolling=no; zoom:1.2")

  })


  fb <- reactive  ({

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

    })


  observeEvent(input$export_fb, {

    gs <- as_sheets_id(input$gsheet_url)

    if ( "sketch" %in% sheet_names(gs) ) {

      sheet_delete(gs, "sketch")

      }

    if (is.list(fb())) {

      fb()  %>%
        pluck("design") %>%
        sheet_write(ss = gs
                    , sheet = "fb")

      fb()  %>%
        pluck("sketch") %>%
        as.data.frame() %>%
        sheet_write(ss = gs
                    , sheet = "sketch")

    } else if (is.data.frame(fb())) {

      fb() %>%
        as.data.frame() %>%
        sheet_write(ss =  gs
                    , sheet = "fb")

      }

  })

})

