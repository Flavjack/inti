# rticles -----------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/rticles/

# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

if (file.exists("setup.r")) { source("setup.r") }

library(shiny)
library(miniUI)
library(shinyFiles)
library(utils)
library(fs)
library(metathis)
library(inti)

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {

  observeEvent(input$cancel, {
    stopApp()
  })

# arguments ---------------------------------------------------------------
# -------------------------------------------------------------------------

  project <- reactive({
    if(input$project == "Yes") {TRUE} else {FALSE}
  })

# server detect -----------------------------------------------------------
# -------------------------------------------------------------------------

  output$server <- reactive({
    if(Sys.getenv('SHINY_PORT') == "") {"local"} else {"web"}
  })
  outputOptions(output, "server", suspendWhenHidden = FALSE)

  server <- reactive({
    if(Sys.getenv('SHINY_PORT') == "") {"local"} else {"web"}
  })

# test code ---------------------------------------------------------------
# -------------------------------------------------------------------------

  observe({

    cat("--------------------------------------------------\n")

    cat("Server")
    print(server())

    cat("Project")
    print(project())

    cat("Directory")
    print(path())

    cat("Doc type")
    print(input$type)

    cat("Doc name")
    print(input$name)

  })

# Export path -------------------------------------------------------------
# -------------------------------------------------------------------------

  volumes <- c(
    Home = fs::path_home(),
    "R Installation" = R.home(),
    getVolumes()()
    )

  shinyDirChoose(input,
    "directory",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  output$directorypath <- renderPrint({
    if (is.integer(input$directory)) {
      cat("No directory has been selected")
    } else {
      parseDirPath(volumes, input$directory)
    }
  })

  path <- reactive({
    paste0(parseDirPath(volumes, input$directory), "/")
  })

# rticles -----------------------------------------------------------------
# -------------------------------------------------------------------------

  observeEvent(input$create, {

    inti::rticles(path = path()
                  , type = input$type
                  , name = input$name
                  , project = project()
                  , server = server()
                  )
    })

# download data -----------------------------------------------------------
# -------------------------------------------------------------------------

  output$downloadData <- downloadHandler(

    filename = function() {

      paste0(stringr::str_replace_all(input$name, pattern = " ", repl = "_"), ".zip")

    },

    content = function(content) {

      filelist <- inti:::rticles(type = input$type
                                 , name = input$name
                                 , project = project()
                                 , server = server()
                                 )

      zip::zipr(zipfile = content, files = filelist)

    },

    contentType = "application/zip"

  )

# end ---------------------------------------------------------------------
# -------------------------------------------------------------------------

})

# viewer <- dialogViewer("lozanoisla.com", width = 500, height = 450)
# runGadget("inst/rticles/app.R", server = server, viewer = viewer)
# browseURL("https://flavjack.shinyapps.io/rticles")
