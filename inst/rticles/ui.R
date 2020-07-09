
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

miniPage(
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
  )

  # viewer <- dialogViewer("lozanoisla.com", width = 500, height = 450)
  #
  # runGadget("inst/rticles/app.R", server = server, viewer = viewer)
