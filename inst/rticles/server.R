# -------------------------------------------------------------------------
# Rticles -----------------------------------------------------------------
# -------------------------------------------------------------------------
#> open https://flavjack.github.io/inti/
#> open https://flavjack.shinyapps.io/rticles/
#> author .: Flavio Lozano-Isla (lozanoisla.com)
#> date .: 2021-04-17
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

#> devtools::install_github("flavjack/inti")

source("pkgs.R")

# -------------------------------------------------------------------------
# update template ---------------------------------------------------------
# -------------------------------------------------------------------------

if (FALSE) {
  
  unlink("inst/rticles/template", recursive = T)
  dir.create("inst/rticles/template")
  inti::create_rticles(path = "inst/rticles/template", type = "book")
  file.rename(from = "inst/rticles/template/rticles.Rproj"
              , to = "inst/rticles/template/rticles.proj")
  }

# -------------------------------------------------------------------------
# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {

# close auto local session ------------------------------------------------

  observe({

    if(Sys.getenv('SHINY_PORT') == "") {

      session$onSessionEnded(stopApp)

    }

  })

# close app ---------------------------------------------------------------

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
    if(Sys.getenv('SHINY_PORT') == "") {
      "web" # local!
      } else {"web"} # always web!
  })
  
  outputOptions(output, "server", suspendWhenHidden = FALSE)

  server <- reactive({
    if(Sys.getenv('SHINY_PORT') == "") {
      "web" # local!
      } else {"web"}  
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

volumes <- c(Home = fs::path_home()
             , "R Installation" = R.home()
             , getVolumes()())

shinyDirChoose(input
               , "directory"
               , roots = volumes
               , session = session
               , restrictions = system.file(package = "base")
               , allowDirCreate = TRUE)

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

    inti::create_rticles(
      path = path()
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

      filelist <- inti:::create_rticles(
        type = input$type
        , name = input$name
        , project = project()
        , server = server()
        )

      zip::zipr(zipfile = content, files = filelist)

    },

    contentType = "application/zip"

  )

})

# end ---------------------------------------------------------------------
# -------------------------------------------------------------------------
