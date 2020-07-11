# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

pkgs_cran <- c(
  "shiny"
  ,"miniUI"
  , "shinyFiles"
  , "utils"
  , "fs"
  , "inti"
)

installed_cran <- pkgs_cran %in% rownames(utils::installed.packages())
if (any(installed_cran == FALSE)) {
  install.packages(pkgs_cran[!installed_cran])
}

invisible(lapply(c(pkgs_cran), library, character.only = TRUE))
rm(pkgs_cran, installed_cran)

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

  observe({

    cat("Directory")
    path <- paste0(parseDirPath(volumes, input$directory), "/")
    print(path)

    print(input$type)

    print(input$name)

    print(input$server)


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

    filename = function(){
      paste(input$name,"zip",sep=".")
    },
    content = function(content){

      project <- switch(input$project,
                        "Yes" = TRUE,
                        "No" = FALSE
                        )

      filelist <- inti:::rticles_files(name = input$name
                                       , project = project
                                       , type = input$type
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
