#
#   # packages ----------------------------------------------------------------
#   # -------------------------------------------------------------------------
#
#   pkgs_cran <- c(
#     "shiny"
#     ,"miniUI"
#     , "shinyFiles"
#     , "utils"
#     , "fs"
#   )
#
#   installed_cran <- pkgs_cran %in% rownames(utils::installed.packages())
#   if (any(installed_cran == FALSE)) {
#     install.packages(pkgs_cran[!installed_cran])
#   }
#
#   pkgs_git <- c(
#     "inti" # Tools and Statistical Procedures in Plant Science
#     , "inserttable" # Insert table with copy and paste
#     , "citr"  # Use zotero for citations
#   )
#
#   installed_git <- pkgs_git %in% rownames(utils::installed.packages())
#   if (any(installed_git == FALSE)) {
#     devtools::install_github("Flavjack/inti", upgrade = "always")
#     devtools::install_github("lbusett/insert_table", upgrade = "always")
#     devtools::install_github("crsh/citr", upgrade = "always")
#   }
#
#   invisible(lapply(c(pkgs_cran, pkgs_git), library, character.only = TRUE))
#   rm(pkgs_cran, installed_cran, pkgs_git, installed_git)

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

  observe({
    cat("Directory")
    path <- paste0(parseDirPath(volumes, input$directory), "/")
    print(path)

    name <- stringr::str_replace_all(input$name, pattern = " ", repl = "_")

    print(input$type)

  })

  observeEvent(input$create, {

    path <- paste0(parseDirPath(volumes, input$directory), "/")

    type <- input$type

    name <- stringr::str_replace_all(input$name, pattern = " ", repl = "_")

    project <- switch(input$project,
      "Yes" = TRUE,
      "No" = FALSE
    )

    inti::rticles(
      path = path,
      type = type,
      name = name,
      project = project
    )
    stopApp()
  })

# download data -----------------------------------------------------------
# -------------------------------------------------------------------------

  output$downloadData <- downloadHandler(

    filename = function(){
      paste(input$name,"zip",sep=".")
    },
    content = function(content){
      tmpdir <- system.file("rticles/template", package = "inti")
      setwd(tmpdir)

      if(input$type == "bookdown"){

        filesToSave <- list.files(tmpdir)

      } else if(input$type == "markdown") {

        name <- stringr::str_replace_all(input$name, pattern = " ", repl = "_")

        dir.create("tmp")
        file.copy("files", "tmp", recursive = T)
        file.copy("index.rmd", "tmp/index.rmd")

        file <- list.files(tmpdir) %>%
          purrr::keep(., .p = ~stringr::str_detect(.x,"rmd"))

        file.rename(paste0("tmp/", file), paste0("tmp/", name, ".rmd"))

        filesToSave <- list.files("tmp")

      }

      zip::zipr(zipfile = content, files = filesToSave)

    },

    contentType = "application/zip"

  )

})

# viewer <- dialogViewer("lozanoisla.com", width = 500, height = 450)
# runGadget("inst/rticles/app.R", server = server, viewer = viewer)
# browseURL("https://flavjack.shinyapps.io/rticles")
