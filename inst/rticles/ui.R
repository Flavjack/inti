# rticles -----------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/rticles/

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

if ( file.exists("setup.r") ) { source("setup.r") }

library(shiny)
library(miniUI)
library(shinyFiles)
library(utils)
library(fs)
library(metathis)
library(inti)

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

fluidPage(title = "Rticles",

    tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
    tags$head(HTML('<style>* {font-size: 100%; font-family: Roboto Mono;}</style>')),
    tags$head(includeHTML(("www/analytics.html"))),
    tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/Flavjack/inti/master/inst/icons/favicon_rticles/android-chrome-512x512.png")),

    meta() %>%
      meta_social(
        title = "Rticles",
        description = "Rticles: Create technical documents with markdown and bookdown",
        url = "https://flavjack.shinyapps.io/rticles/",
        image = "https://raw.githubusercontent.com/Flavjack/inti/master/inst/icons/favicon_rticles/android-chrome-512x512.png",
        image_alt = "quipolab.com"
      ),

    fluidRow(
      column(2,

             br(),

             HTML('

            <div id=footer style="width:100%; margin:auto;">

            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://lozanoisla.com/">
            <img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/icons/quipo4c.png" style="height:55px" title="flozano"></a>
            <span style="display:block;"><small>lozanoisla.com</small></span>
            </p></div>

            </div>

                  ')

             ),

      column(3,

             HTML('<h1><a target="_blank" href="https://flavjack.shinyapps.io/rticles/">Rticles</a></h1>'),

             br(),

             selectizeInput(
               inputId = "type",
               label = "Document type",
               choices = c("markdown", "bookdown"),
               multiple = FALSE
             ),

             textInput(
               inputId = "name",
               label = "Document name",
               value = "manuscript"
             ),

             p(),

             radioButtons(
               inputId = "project",
               label = "R project",
               choices = c("Yes", "No"),
               selected = "Yes",
               inline = T
             ),

             p(),

             conditionalPanel(

               "output.server == 'web'",

               downloadButton(outputId = "downloadData"
                              , label =  "Download")

             ),

             conditionalPanel(

               "output.server == 'local'",

               verbatimTextOutput("directorypath"),

               shinyDirButton(
                 "directory",
                 "Select folder",
                 "Please select a folder"
               ),

             ),

             br(),

             conditionalPanel(

               "output.server == 'local'",

               gadgetTitleBar(
                 title = "enjoy :)",
                 left = miniTitleBarCancelButton(),
                 right = miniTitleBarButton(
                   inputId = "create",
                   label = "Create",
                   primary = TRUE
                 ))

             )

             ),

      column(5,

             includeHTML("www/manual.html")

             ),

      column(2,

             br(),

             HTML('

            <div id=footer style="width:100%; margin:auto;">
            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://www.youtube.com/playlist?list=PLSQMdOu57lj9HTZNed2-q3WpQfIGraA1V">
            <img src="https://raw.githubusercontent.com/Flavjack/inti/master/inst/icons/youtube.png" style="height:60px" title="demo"></a>
            <span style="display:block;"><small>demo</small></span>
            </p></div>
            </div>

                  '),

             HTML('

            <h4 style="text-align: center;"><span style="color: #0a0909;">Softwares</span></h4>

            <p> </p>
            <p> </p>
            <p> </p>

            <div id=footer style="width:100%; margin:auto;">

            <div style="display:inline-block; width:100%">

            <p style="text-align:center; vertical-align:middle">
            <a target="_blank" href="https://www.zotero.org/download/">
            <img src="https://www.zotero.org/static/images/bs4theme/zotero-logo.1519312231.svg" style="height:40px" title="Zotero" alt="Zotero"></a>
            </p>

            </div>

            <p> </p>

            <div style="display:inline-block; width:100%">

            <p style="text-align:center; vertical-align:middle">
            <a target="_blank" href="https://cloud.r-project.org/">
            <img src="https://cran.r-project.org/Rlogo.svg" style="height:40px" title="R cran" alt="R cran"></a>
            </p>

            </div>

            <p> </p>

            <div style="display:inline-block; width:100%">

            <p style="text-align:center; vertical-align:middle">
            <a target="_blank" href="https://rstudio.com/products/rstudio/download/">
            <img src="https://d33wubrfki0l68.cloudfront.net/62bcc8535a06077094ca3c29c383e37ad7334311/a263f/assets/img/logo.svg" style="height:40px" title="Rstudio" alt="Rstudio"></a>
            </p>

            </div>

            <p> </p>

            <div style="display:inline-block; width:100%">

            <p style="text-align:center; vertical-align:middle">
            <a target="_blank" href="https://git-scm.com/downloads">
            <img src="https://git-scm.com/images/logo@2x.png" style="height:40px" title="Git" alt="Git"></a>
            </p>

            </div>

            <p> </p>

            <div style="display:inline-block; width:100%">

            <p style="text-align:center; vertical-align:middle">
            <a target="_blank" href="https://retorque.re/zotero-better-bibtex/">
            <img src="https://ctan.org/teaser/pkg/bibtex" style="height:40px" title="Git" alt="Git"></a>
            </p>

            </div>

            </div>

                '),

             br(),

             HTML('

            <div id=footer style="width:100%; margin:auto;">
            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://github.com/Flavjack/rticles">
            <img src="https://image.flaticon.com/icons/svg/25/25231.svg" style="height:50px" title="demo"></a>
            <span style="display:block;"><small>Github</small></span>
            </p></div>
            </div>

                  '),

             HTML('

            <div id=footer style="width:100%; margin:auto;">
            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://www.quipolab.com/">
            <img src="https://lozanoisla.com/img/quipo.png" style="height:50px" title="demo"></a>
            </p></div>
            </div>

                  ')

             ),
    )
  )

# end ---------------------------------------------------------------------
# -------------------------------------------------------------------------
