#' Rticles template
#'
#' Create a complete example for markdown o bookdown documents
#'
#' @param path directory for create the files.
#' @param type document type (default = "markdown").
#' @param name name of the main document (default = "manuscript").
#' @param project create a R project (default = TRUE). See details.
#' @param server How create the dependencies (default = "local").
#'
#' @details
#'
#' Create all the files in the path directory.
#' It is recommended use ".Rproj".
#' After create the project Open the ".Rprj" file.
#' Compile the document and you will see the example.
#' If you already have a folder and select such as "bookdown"
#' is necessary change the main (.rmd) file to index.rmd
#'
#' @return folder and files
#'
#' @author Flavio Lozano-Isla
#'
#' @importFrom utils download.file
#'
#' @source
#'
#' \url{https://github.com/flavjack/rticles}
#'
#' @export

rticles <- function(path = NULL
                    , type = c("markdown", "bookdown")
                    , name = "manuscript"
                    , project = TRUE
                    , server = c("local", "web")
                    ){

  # arguments ---------------------------------------------------------------
  # -------------------------------------------------------------------------

  type <- match.arg(type)
  name <- stringr::str_replace_all(name, pattern = " ", repl = "_")
  server <- match.arg(server)

  if (server == "local") {

    name.rmd <- paste0(name, ".rmd")
    if ( is.null(path) ) { path <- getwd() }
    path <- paste0(path, "/")

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
        url = "https://raw.githubusercontent.com/Flavjack/rticles/master/files/book.bib",
        destfile = paste0(path, "files/book.bib"),
        mode = "wb"
      )
    }

    # Rproj -------------------------------------------------------------------
    # -------------------------------------------------------------------------

    if (project == TRUE) {
      if (!file.exists(paste0(path, "rticles.Rproj"))) {
        download.file(
          url = "https://raw.githubusercontent.com/Flavjack/rticles/master/rticles.Rproj",
          destfile = paste0(path, "rticles.Rproj"),
          mode = "wb"
        )
      }
    }

    # markdown ----------------------------------------------------------------
    # -------------------------------------------------------------------------

    if (type == "markdown") {
      if (!file.exists(paste0(path, name.rmd))) {
        download.file(
          url = "https://raw.githubusercontent.com/Flavjack/rticles/master/index.Rmd",
          destfile = paste0(path, name.rmd),
          mode = "wb"
        )
      }

      # bookdown ----------------------------------------------------------------
      # -------------------------------------------------------------------------
    } else if (type == "bookdown") {

      message("Change the name of your main markdown file to index.rmd")

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

      path

    }

# web ---------------------------------------------------------------------
# -------------------------------------------------------------------------

  } else if (server == "web") {

    files.dir <- paste0(system.file("rticles/template", package = "inti"), "/")
    tmp.dir <- paste0(tempdir(), "/")
    path <- paste0(tmp.dir, name)

    if (dir.exists(paste0(tmp.dir, name))) {

      unlink(paste0(tmp.dir, name)
             , recursive = T
      )

      dir.create(paste0(tmp.dir, name)
                 , recursive = T
      )

    } else {

      dir.create(paste0(tmp.dir, name)
                 , recursive = T
      )

    }

    file.copy(paste0(files.dir, "files")
              , paste0(tmp.dir, name)
              , recursive = T
              , overwrite = T
    )

    file.copy(from = paste0(files.dir, "index.rmd")
              , to = paste0(tmp.dir, name , "/", "index.rmd")
              , overwrite = T
    )

    file.copy(from = paste0(files.dir, "_bookdown.yml")
              , to = paste0(tmp.dir, name , "/", "_bookdown.yml")
              , overwrite = T
    )

    file.copy(from = paste0(files.dir, "_output.yml")
              , to = paste0(tmp.dir, name , "/", "_output.yml"))

    if (project == TRUE) {

      file.copy(paste0(files.dir, "rticles.Rproj")
                , to = paste0(tmp.dir, name , "/", "rticles.Rproj")
                , overwrite = T
      )

      file.copy(paste0(files.dir, "rticles.proj")
                , to = paste0(tmp.dir, name , "/", "rticles.Rproj")
                , overwrite = T
      )

    }

    if (type == "markdown") {

      file.rename(from = paste0(tmp.dir, name , "/", "index.rmd")
                  , to = paste0(tmp.dir, name, "/", name, ".rmd")
      )

      file.remove(paste0(tmp.dir, name, "/files/", "style_rbooks.css"))
      file.remove(paste0(tmp.dir, name, "/" ,"_bookdown.yml"))
      file.remove(paste0(tmp.dir, name, "/" ,"_output.yml"))

    }

    path

  }

}

