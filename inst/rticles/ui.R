
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

fluidPage(

    tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
    tags$head(HTML('<style>* {font-size: 100%; font-family: Roboto Mono;}</style>')),

    fluidRow(
      column(2),

      column(3,

             h1("Rticles"),

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

      gadgetTitleBar(
        title = "enjoy writing!  :)",
        left = miniTitleBarCancelButton(),
        right = miniTitleBarButton(
          inputId = "create",
          label = "Create",
          primary = TRUE
        ))

             ),

      column(5,

             HTML('
                  <h1 style="text-align: justify;"><span style="color: #ff6600;">Instrucciones</span></h1>
                    <p style="text-align: justify;"><span style="color: #008000;">La siguiente app te ayudara a crear tu primer documento usando R + markdown = Rmardown (.Rmd).</span></p>
                    <ol style="text-align: justify;">
                    <li><span style="color: #008000;">Elije que tipo de documento deseas crear.</span>
                    <ul>
                    <li><span style="color: #008000;">Markdown para articulos y tesis.</span></li>
                    <li><span style="color: #008000;">Bookdown para manuales y libros.</span></li>
                    </ul>
                    </li>
                    <li><span style="color: #008000;">Selecciona el nombre para tú documento.</span></li>
                    <li><span style="color: #008000;">Selecciona el folder donde se exportará toda los documentos necesarios para ejecutar el ejemplo.</span></li>
                    <li><span style="color: #008000;">Te recomiendo usar proyectos (Use a R project) ya que te permitirá organizar mejor tus trabajos</span></li>
                    <li><span style="color: #008000;">Unas vez selecionado todos los parámetros crea las dependencias ("Create")</span></li>
                    <li><span style="color: #008000;">Despúes de crear, ve al folder que seleccionates y abre el proyecto (.Rporj)</span></li>
                    <li><span style="color: #008000;">Compila el documento con Knitr</span></li>
                    <li><span style="color: #008000;">Espera hasta que compile el documento</span></li>
                    <li><span style="color: #008000;">Ahora con ese ejemplo puedes crear tú propio documento.</span></li>
                    <li><span style="color: #008000;">A escribir los manuscritos! :)</span></li>
               </ol>
               <p> </p>
               <p> </p>
                  '),

             br(),

      ),

      column(2)

    )
  )

  # viewer <- dialogViewer("lozanoisla.com", width = 500, height = 450)
  #
  # runGadget("inst/rticles/app.R", server = server, viewer = viewer)
