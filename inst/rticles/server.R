# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

# pkgs_cran <- c(
#   "shiny"
#   , "miniUI"
#   , "shinyFiles"
#   , "utils"
#   , "fs"
#   )
#
# installed_cran <- pkgs_cran %in% rownames(installed.packages())
# if (any(installed_cran == FALSE)) {
#   install.packages(pkgs_cran[!installed_cran])
# }
#
# pkgs_git <- c(
#   "inti"
# )
#
# installed_git <- pkgs_git %in% rownames(installed.packages())
# if (any(installed_git == FALSE)) {
#   devtools::install_github("flavjack/inti", upgrade = "always")
# }
#
# invisible(lapply(c(pkgs_cran, pkgs_git), library, character.only = TRUE))
# rm(pkgs_cran, installed_cran, pkgs_git, installed_git)


library(shiny)
library(miniUI)
library(shinyFiles)
library(utils)
library(fs)
library(inti)

# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyServer(function(input, output, session) {

  observeEvent(input$cancel, {
    stopApp()
  })

  observeEvent(input$create, {
    stopApp()
  })

  observe({

    cat("Directory")
    path <- paste0(parseDirPath(volumes, input$directory), "/")
    print(path)

    cat("Doc type")
    print(input$type)

    cat("Doc name")
    print(input$name)

    cat("Choose server")
    print(input$server)

    cat("Auto choose server")
    print(if(Sys.getenv('SHINY_PORT') == "") {"is_local"} else {"is_web"})

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

# rticles -----------------------------------------------------------------
# -------------------------------------------------------------------------

  observeEvent(input$create, {

    type <- input$type
    name <- input$name

    project <- switch(input$project,
      "Yes" = TRUE,
      "No" = FALSE
    )

    inti::rticles(path = path
                  , type = type
                  , name = name
                  , project = project
                  )
  })

# download data -----------------------------------------------------------
# -------------------------------------------------------------------------


  output$downloadData <- downloadHandler(

    filename = function() {

      paste0(stringr::str_replace_all(input$name, pattern = " ", repl = "_")
            ,".zip"
            )

    },

    content = function(content) {

      project <- switch(input$project,
                        "Yes" = TRUE,
                        "No" = FALSE
                        )

      filelist <- inti:::rticles(name = input$name
                                 , project = project
                                 , type = input$type
                                 , server = input$server
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
