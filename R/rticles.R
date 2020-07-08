#' Build markdown o bookdown template
#'
#' Invoke RStudio add-in to create markdown documents
#'
#' @details
#'
#' Create all the files in the present work directory.
#' It is recommended use ".Rproj" for bookdown documents.
#' After create the ".Rproj" file.
#' Open the ".Rprj" and you should have the possibility to compile the book.
#'
#' @return Shiny app
#'
#' @examples
#' \dontrun{
#'  rticles()
#' }
#'
#' @import miniUI
#' @import shiny
#' @importFrom utils download.file
#'
#' @export

rticles <- function() {

    ui <- miniPage(

        miniTitleBar("Rticle template"),

        textInput(inputId = "name"
                  , label = "Name for your project"
                  , value = "manuscript"
                  ),

        selectizeInput(
            inputId = "type"
            , label = "Type of document"
            , choices = c("Markdown", "Bookdown")
            , selected = "Markdown"
            , multiple = FALSE
        ),

        radioButtons(inputId = "project"
                     , label = "Create a R project"
                     , choices = c("Yes", "No")
                     , selected = "Yes"
                     , inline = T
                     ),

        gadgetTitleBar(title = "enjoy writing!  :)"
                       , left = miniTitleBarCancelButton()
                       , right = miniTitleBarButton(inputId = "create"
                                                    ,label =  "Create"
                                                    , primary = TRUE)
                       )

    )

    server <- function(input, output, session) {

        observeEvent(input$cancel, {
            stopApp("Cancelled")
        })

        observeEvent(input$create, {

          # dependencies ------------------------------------------------------------
          # -------------------------------------------------------------------------

          if(!dir.exists("files")){dir.create("files")}

          if(!file.exists("files/style_rticles.docx"))
          {download.file(url = "https://github.com/Flavjack/rticles/raw/master/cnfg/style_rticles_tr.docx"
                         , destfile = "files/style_rticles.docx", mode = "wb")}

          if(!file.exists("files/style_thesis.docx"))
          {download.file(url = "https://github.com/Flavjack/rticles/raw/master/cnfg/style_unalm.docx"
                         , destfile = "files/style_thesis.docx", mode = "wb")}

          if(!file.exists("files/logo.png"))
          {download.file(url = "https://raw.githubusercontent.com/Flavjack/rticles/master/cnfg/icons/unalm.png"
                         , destfile = "files/logo.png", mode = "wb")}

          project <- paste0(input$name, ".Rproj")

          if(!file.exists(project) & input$project == "Yes")
          {download.file(url = "https://raw.githubusercontent.com/Flavjack/rticles/master/rticles.Rproj"
                         , destfile = project, mode = "wb")}

            if(input$type == "Markdown"){

              # markdown ----------------------------------------------------------------
              # -------------------------------------------------------------------------

              name <- paste0(input$name, ".rmd")

              if(!file.exists(name))
                {download.file(url = "https://raw.githubusercontent.com/Flavjack/rticles/master/index.Rmd"
                               , destfile = name, mode = "wb")}

              } else if (input$type == "Bookdown"){

              # bookdown ----------------------------------------------------------------
              # -------------------------------------------------------------------------

              if(!file.exists("index.rmd"))
              {download.file(url = "https://raw.githubusercontent.com/Flavjack/rticles/master/index.Rmd"
                             , destfile = "index.rmd", mode = "wb")}

              if(!file.exists("_bookdown.yml"))
              {download.file(url = "https://raw.githubusercontent.com/Flavjack/rticles/master/cnfg/bookdown.yml"
                             , destfile = "_bookdown.yml", mode = "wb")}

              if(!file.exists("_output.yml"))
              {download.file(url = "https://raw.githubusercontent.com/Flavjack/rticles/master/cnfg/output.yml"
                             , destfile = "_output.yml", mode = "wb")}

              if(!file.exists("files/style_rbooks.css"))
              {download.file(url = "https://raw.githubusercontent.com/Flavjack/rticles/master/cnfg/style_rbooks.css"
                             , destfile = "files/style_rbooks.css", mode = "wb")}

              }

            # citations ---------------------------------------------------------------
            # -------------------------------------------------------------------------

            if(!file.exists("files/book.bib"))
            {download.file(url = "https://raw.githubusercontent.com/Flavjack/rticles/master/cnfg/book.bib"
                           , destfile = "files/book.bib", mode = "wb")}

        })

        observeEvent(input$create, {
            stopApp("Completed")
        })

    }

viewer <- dialogViewer("lozanoisla.com", width = 400, height = 320)
runGadget(ui, server, viewer = viewer)

}

