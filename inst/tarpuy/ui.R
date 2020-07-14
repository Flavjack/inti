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

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

fluidPage(

    tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
    tags$head(HTML('<style>* {font-size: 100%; font-family: Roboto Mono;}</style>')),

    meta() %>%
      meta_social(
        title = "Tarpuy",
        description = "create field book designs easy",
        url = "https://flavjack.shinyapps.io/tarpuy/",
        image = "https://lozanoisla.com/img/quipo.png",
        image_alt = "quipolab.com"
      ),

    fluidRow(
      column(2,

             br(),
             br(),

             HTML('

            <div id=footer style="width:100%; margin:auto;">

            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://lozanoisla.com/">
            <img src="https://raw.githubusercontent.com/Flavjack/lozanoisla/master/static/android-chrome-512x512.png?token=AB3ARRI5E4ZF7FLXM6CDQ7S7CHJ3K" style="height:55px" title="flozano"></a>
            <span style="display:block;">lozanoisla.com</span>
            </p></div>

            </div>

                  ')

             ),

      column(2,

             HTML('<h1><a target="_blank" href="https://flavjack.shinyapps.io/rticles/">Tarpuy</a></h1>'),

             br(),

             numericInput(
               inputId = "nFactors"
               , label = "Factors"
               , value = 1
               , max = 5
               , min = 1
             ),

             conditionalPanel(

               "input.nFactors == '1'",

               selectizeInput(
                 inputId = "type",
                 label = "Design type",
                 choices = c("crd", "rcbd", "lsd", "lattice"),
                 multiple = FALSE
               ),
             ),

             conditionalPanel(

               "input.nFactors == '2'",

               selectizeInput(
                 inputId = "type",
                 label = "Design type",
                 choices = c("crd", "rcbd", "lsd"
                             ,"split-crd", "split-rcbd"
                             ),
                 multiple = FALSE
               ),
             ),

             conditionalPanel(

               "input.nFactors > '2'",

               selectizeInput(
                 inputId = "type",
                 label = "Design type",
                 choices = c("crd", "rcbd", "lsd"),
                 multiple = FALSE
               ),
             ),

             numericInput(inputId = "rep"
                          , label = "Replications"
                          , value = 2
                          , min = 1
                          ),

             numericInput(inputId = "serie"
                          , label = "Plot digits"
                          , value = 2
                          , max = 3
                          , min = 1
                          ),

             numericInput(inputId = "seed"
                          , label = "Seed"
                          , value = 0
                          ),

             ),

      column(6,

             br(),

             column(width = 12, #offset = 1,

                    h4(icon("book"), "Google SpreadSheet (URL)", width = "100%"),
                    textInput("fbdt",
                              label = NULL,
                              width = "100%",
                              value = "https://docs.google.com/spreadsheets/d/14sO81N50Zx1al5O3Iu3IPaz1_5CVncvtsx-_JRqJ_qE/edit#gid=1625775871")

             )

             ),

      column(2,

             br(),
             br(),

             HTML('

            <p> </p>
            <p> </p>

            <div id=footer style="width:100%; margin:auto;">

            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://www.quipolab.com/">
            <img src="https://lozanoisla.com/img/quipo.png" style="height:55px" title="quipo"></a>
            </p></div>

            </div>

                  ')

             ),

    )
  )

  # viewer <- dialogViewer("lozanoisla.com", width = 500, height = 450)

  # runGadget("inst/rticles/app.R", server = server, viewer = viewer)

  # browseURL("https://flavjack.shinyapps.io/rticles")
