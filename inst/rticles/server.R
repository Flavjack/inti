# rticles -----------------------------------------------------------------
# -------------------------------------------------------------------------

# https://flavjack.shinyapps.io/rticles/

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

# library(shiny)
# library(miniUI)
# library(shinyFiles)
# library(utils)
# library(fs)
# library(inti)

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {

  observeEvent(input$cancel, {
    stopApp()
  })

  observeEvent(input$create, {
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
