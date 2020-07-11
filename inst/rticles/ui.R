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

# pkgs_git <- c(
#   "inti" # Tools and Statistical Procedures in Plant Science
#   , "inserttable" # Insert table with copy and paste
#   , "citr"  # Use zotero for citations
# )
#
# installed_git <- pkgs_git %in% rownames(utils::installed.packages())
# if (any(installed_git == FALSE)) {
#   devtools::install_github("Flavjack/inti", upgrade = "always")
#   devtools::install_github("lbusett/insert_table", upgrade = "always")
#   devtools::install_github("crsh/citr", upgrade = "always")
# }
#
# rm(pkgs_git, installed_git)


# app ---------------------------------------------------------------------
# -------------------------------------------------------------------------

fluidPage(

    tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
    tags$head(HTML('<style>* {font-size: 100%; font-family: Roboto Mono;}</style>')),

    fluidRow(
      column(2,

             br(),
             br(),

             HTML('

            <div id=footer style="width:100%; margin:auto;">

            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://lozanoisla.com/">
            <img src="https://raw.githubusercontent.com/Flavjack/lozanoisla/master/static/android-chrome-512x512.png?token=AB3ARRI5E4ZF7FLXM6CDQ7S7CHJ3K" style="height:55px" title="flozano"></a>
            <span style="display:block;">lozanoisla.com</span>
            </p></div>

            </div>

                  ')

             ),

      column(3,

             h1("Rticles"),

             selectizeInput(
               inputId = "type",
               label = "Document type",
               choices = c("Markdown" = "markdown", "Bookdown" = "bookdown"),
               selected = "Markdown",
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
               label = "Use a R project",
               choices = c("Yes", "No"),
               selected = "Yes",
               inline = T
             ),

             br(),

             conditionalPanel(

               condition = "input.server == 'web'",

               downloadButton(outputId = "downloadData"
                              , label =  "Download")

             ),

             conditionalPanel(

               condition = "input.server == 'local'",

               verbatimTextOutput("directorypath"),

               shinyDirButton(
                 "directory",
                 "Select folder",
                 "Please select a folder"
               ),

             ),

             br(),

             conditionalPanel(

               condition = "input.server == 'local'",

               gadgetTitleBar(
                 title = "enjoy writing! :)",
                 left = miniTitleBarCancelButton(),
                 right = miniTitleBarButton(
                   inputId = "create",
                   label = "Create",
                   primary = TRUE
                 ))

             )

             ),

      column(5,

             HTML('
                <h1 style="text-align: justify;"><span style="color: #ff6600;">Instrucciones</span></h1>
                <p style="text-align: justify;"><span style="color: #008000;">La siguiente app te ayudara a crear tu primer documento usando R + markdown = Rmarkdown (.Rmd).</span></p>
                <ol style="text-align: justify;">
                <li><span style="color: #008000;">Elije que tipo de documento deseas crear.</span>
                <ul>
                <li><span style="color: #008000;">Markdown para articulos y tesis.</span></li>
                <li><span style="color: #008000;">Bookdown para manuales y libros.</span></li>
                </ul>
                </li>
                <li><span style="color: #008000;">Selecciona el nombre para tú documento.</span></li>
                <li><span style="color: #008000;">Selecciona el folder donde se exportará toda los documentos necesarios para ejecutar el ejemplo.</span></li>
                <li><span style="color: #008000;">Te recomiendo usar proyectos (use a R project) ya que te permitirá organizar mejor tus trabajos.</span></li>
                <li><span style="color: #008000;">Unas vez selecionado todos los parámetros, crea las dependencias con el boton ("Create").</span></li>
                <li><span style="color: #008000;">Despúes de crear, ve al folder que seleccionates y abre el proyecto (.Rporj)</span></li>
                <li><span style="color: #008000;">Compila el documento usando el boton Knitr en Rstudio.<br /></span></li>
                <li><span style="color: #008000;">Espera hasta que compile el documento.</span></li>
                <li><span style="color: #008000;">Ahora con el ejemplo puedes crear tú propio documento.</span></li>
                <li><span style="color: #008000;">A escribir los manuscritos! :)</span></li>
                </ol>
                <p> </p>
                <p> </p>
                  '),

             br(),

      ),

      column(2,

             br(),
             br(),

             HTML('

            <div id=footer style="width:100%; margin:auto;">

            <div style="display:inline-block; width:100%">
            <p style="text-align:center">
            <a target="_blank" href="https://www.quipolab.com/">
            <img src="https://lozanoisla.com/img/quipo.png" style="height:55px" title="quipo"></a>
            <span style="display:block;">quipolab.com</span>
            </p></div>

            </div>

                  '),

             br(),
             br(),

             radioButtons(inputId = "server"
                          , label = "Server"
                          , choices = c("web", "local")
                          , selected = "local"
                          , inline = TRUE
             )

             ),

    )
  )

  # viewer <- dialogViewer("lozanoisla.com", width = 500, height = 450)

  # runGadget("inst/rticles/app.R", server = server, viewer = viewer)

  # browseURL("https://flavjack.shinyapps.io/rticles")
