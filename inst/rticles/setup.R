# packages ----------------------------------------------------------------
# -------------------------------------------------------------------------

pkgs <- list(

  cran = c(
    "shiny"
    , "miniUI"
    , "shinyFiles"
    , "utils"
    , "fs"
    , "metathis"
  ),

  git = c(
    "inti" # Tools and Statistical Procedures in Plant Science
    , "citr"
  )
)

gitrepo <- c("Flavjack/inti", "crsh/citr")

installed <- unlist(pkgs) %in% rownames(installed.packages())

if (any(installed == FALSE)) {

  cran_missing <- pkgs[["cran"]] %in% as.vector(unlist(pkgs)[!installed == TRUE])
  cran_install <- unlist(pkgs)[cran_missing == TRUE]
  install.packages( cran_install )

  git_missing <- pkgs[["git"]] %in% as.vector(unlist(pkgs)[!installed == TRUE])

  if (any(git_missing == FALSE)) {

    invisible(lapply(gitrepo, devtools::install_github, character.only = TRUE))

  }

} else {invisible(lapply(gitrepo, devtools::install_github, character.only = TRUE))}

invisible(lapply(as.vector(unlist(pkgs)), library, character.only = TRUE))
rm(pkgs, gitrepo, installed)
