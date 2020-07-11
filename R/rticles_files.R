#' Files paths for shiny app
#'
#' List of files for shiny app
#'
#' @param name file name
#' @param project create project
#' @param type type of document
#'
#' @return file list
#'
#' @export

rticles_files <- function(name
                         , project = TRUE
                         , type = c("markdown", "bookdown")
                         ){

  type <- match.arg(type)

    files.dir <- paste0(system.file("rticles/template", package = "inti"), "/")
    tmp.dir <- paste0(tempdir(), "/")

    dir.create(paste0(tmp.dir, name), recursive = T)

    file.copy(paste0(files.dir, "files")
              , paste0(tmp.dir, name)
              , recursive = T, overwrite = T)

    file.copy(from = paste0(files.dir, "index.rmd")
              , to = paste0(tmp.dir, name , "/", "index.rmd"))

    file.copy(from = paste0(files.dir, "_bookdown.yml")
              , to = paste0(tmp.dir, name , "/", "_bookdown.yml"))

    file.copy(from = paste0(files.dir, "_output.yml")
              , to = paste0(tmp.dir, name , "/", "_output.yml"))

    if (project == TRUE) {

      file.copy(paste0(files.dir, "rticles.Rproj")
                , to = paste0(tmp.dir, name , "/", "rticles.Rproj"))

      file.copy(paste0(files.dir, "rticles.r")
                , paste0(tmp.dir, name , "/", "rticles.Rproj"))

    }

    if (type == "markdown") {

      file.rename(from = paste0(files.dir, name , "/", "index.rmd")
                  , to = paste0(tmp.dir, name, "/", name, ".rmd"))

      file.remove(paste0(tmp.dir, name, "/files/", "style_rbooks.css"))
      file.remove(paste0(tmp.dir, name,  "_bookdown.yml"))
      file.remove(paste0(tmp.dir, name,  "_output.yml"))

    }

    files <- paste0(tmp.dir, name)

  }

