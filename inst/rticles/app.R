
  # packages ----------------------------------------------------------------
  # -------------------------------------------------------------------------

  pkgs_cran <- c(
    "shiny"
    ,"miniUI"
    , "shinyFiles"
    , "utils"
    , "fs"
  )

  installed_cran <- pkgs_cran %in% rownames(utils::installed.packages())
  if (any(installed_cran == FALSE)) {
    install.packages(pkgs_cran[!installed_cran])
  }

  pkgs_git <- c(
    "inti" # Tools and Statistical Procedures in Plant Science
    , "inserttable" # Insert table with copy and paste
    , "citr"  # Use zotero for citations
  )

  installed_git <- pkgs_git %in% rownames(utils::installed.packages())
  if (any(installed_git == FALSE)) {
    devtools::install_github("Flavjack/inti", upgrade = "always")
    devtools::install_github("lbusett/insert_table", upgrade = "always")
    devtools::install_github("crsh/citr", upgrade = "always")
  }

  invisible(lapply(c(pkgs_cran, pkgs_git), library, character.only = TRUE))
  rm(pkgs_cran, installed_cran, pkgs_git, installed_git)

  # app ---------------------------------------------------------------------
  # -------------------------------------------------------------------------

app <- shinyApp(
  ui <- miniPage(
    miniTitleBar("Rticles Template"),

    tags$p(),

    sidebarPanel(
      selectizeInput(
        inputId = "type",
        label = "Document type",
        choices = c("Markdown", "Bookdown"),
        selected = "Markdown",
        multiple = FALSE
      ),

      textInput(
        inputId = "name",
        label = "Document name",
        value = "manuscript"
      ),

      tags$p(),

      radioButtons(
        inputId = "project",
        label = "Use a R project",
        choices = c("Yes", "No"),
        selected = "Yes",
        inline = T
      ),

      verbatimTextOutput("directorypath"),

      shinyDirButton(
        "directory",
        "Folder select",
        "Please select a folder"
      ),
    ),

    gadgetTitleBar(
      title = "enjoy writing!  :)",
      left = miniTitleBarCancelButton(),
      right = miniTitleBarButton(
        inputId = "create",
        label = "Create",
        primary = TRUE
      )
    )
  ),

  server <- function(input, output, session) {

    # Export path -------------------------------------------------------------
    # -------------------------------------------------------------------------

    # shinyFilesExample()

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
      print(paste0(parseDirPath(volumes, input$directory), "/"))
    })

    observeEvent(input$create, {
      path <- paste0(parseDirPath(volumes, input$directory), "/")

      name <- paste0(input$name, ".rmd")

      # dependencies ------------------------------------------------------------
      # -------------------------------------------------------------------------

      if (!dir.exists(paste0(path, "files"))) {
        dir.create(paste0(path, "files"))
      }

      if (!file.exists(paste0(path, "files/style_rticles.docx"))) {
        download.file(
          url = "https://github.com/Flavjack/rticles/raw/master/cnfg/style_rticles_tr.docx",
          destfile = paste0(path, "files/style_rticles.docx"),
          mode = "wb"
        )
      }

      if (!file.exists(paste0(path, "files/style_thesis.docx"))) {
        download.file(
          url = "https://github.com/Flavjack/rticles/raw/master/cnfg/style_unalm.docx",
          destfile = paste0(path, "files/style_thesis.docx"),
          mode = "wb"
        )
      }

      if (!file.exists(paste0(path, "files/logo.png"))) {
        download.file(
          url = "https://raw.githubusercontent.com/Flavjack/rticles/master/cnfg/icons/unalm.png",
          destfile = paste0(path, "files/logo.png"),
          mode = "wb"
        )
      }

      if (!file.exists(paste0(path, "files/setup.r"))) {
        download.file(
          url = "https://lozanoisla.com/setup.r",
          destfile = paste0(path, "files/setup.r"),
          mode = "wb"
        )
      }

      # citations ---------------------------------------------------------------
      # -------------------------------------------------------------------------

      if (!file.exists(paste0(path, "files/book.bib"))) {
        download.file(
          url = "https://raw.githubusercontent.com/Flavjack/rticles/master/cnfg/book.bib",
          destfile = paste0(path, "files/book.bib"),
          mode = "wb"
        )
      }

      # Rproj -------------------------------------------------------------------
      # -------------------------------------------------------------------------

      if (input$project == "Yes") {
        download.file(
          url = "https://raw.githubusercontent.com/Flavjack/rticles/master/rticles.Rproj",
          destfile = paste0(path, "rticles.Rproj"),
          mode = "wb"
        )
      }

      # markdown ----------------------------------------------------------------
      # -------------------------------------------------------------------------

      if (input$type == "Markdown") {
        if (!file.exists(paste0(path, name))) {
          download.file(
            url = "https://raw.githubusercontent.com/Flavjack/rticles/master/index.Rmd",
            destfile = paste0(path, name),
            mode = "wb"
          )
        }

        # bookdown ----------------------------------------------------------------
        # -------------------------------------------------------------------------
      } else if (input$type == "Bookdown") {
        if (!file.exists(paste0(path, "index.rmd"))) {
          download.file(
            url = "https://raw.githubusercontent.com/Flavjack/rticles/master/index.Rmd",
            destfile = paste0(path, "index.rmd"),
            mode = "wb"
          )
        }

        if (!file.exists(paste0(path, "_bookdown.yml"))) {
          download.file(
            url = "https://raw.githubusercontent.com/Flavjack/rticles/master/cnfg/bookdown.yml",
            destfile = paste0(path, "_bookdown.yml"),
            mode = "wb"
          )
        }

        if (!file.exists(paste0(path, "_output.yml"))) {
          download.file(
            url = "https://raw.githubusercontent.com/Flavjack/rticles/master/cnfg/output.yml",
            destfile = paste0(path, "_output.yml"),
            mode = "wb"
          )
        }

        if (!file.exists(paste0(path, "files/style_rbooks.css"))) {
          download.file(
            url = "https://raw.githubusercontent.com/Flavjack/rticles/master/cnfg/style_rbooks.css",
            destfile = paste0(path, "files/style_rbooks.css"),
            mode = "wb"
          )
        }
      }

      stopApp()
    })

    observeEvent(input$cancel, {
      stopApp()
    })
  })


# viewer <- dialogViewer("lozanoisla.com", width = 500, height = 450)
#
# runGadget("inst/rticles/app.R", server = server, viewer = viewer)
