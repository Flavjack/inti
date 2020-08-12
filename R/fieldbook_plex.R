#' Fieldbook plan information
#'
#' Information for build a plan for an experiment (PLEX)
#'
#' @param data Data with the fieldbook information.
#' @param idea how the idea was born.
#' @param goal the main goal of the project.
#' @param hypothesis what are the expected results.
#' @param rationale based in which evidence is planned the experiment.
#' @param objectives the objectives of the project.
#' @param plan General description of the project (M & M).
#' @param institutions Institutions involved in the project.
#' @param researchers Persons involved in the project.
#' @param manager Persons responsible of the collection of the data.
#' @param location Location of the project.
#' @param altitude Altitude of the experiment (m.a.s.l).
#' @param georeferencing Georeferencing information.
#' @param environment Environment of the experiment (greenhouse, lab, etc).
#' @param start The date of the start of the experiments.
#' @param end The date of the end of the experiments.
#' @param album link with the photos of the project.
#' @param github link with the github repository.
#' @param about Short description of the project
#' @param fieldbook Name or ID for the fieldbook/project.
#' @param nfactor Number of factors for the design
#' @param design Type of design
#' @param rep Number of replication
#' @param serie Number of digits in the plots
#' @param seed Seed for the randomization
#'
#' @details
#'
#' It´s not necessary to provide all the information.
#'
#' @return data frame or list of arguments (info, variables, design)
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @importFrom dplyr mutate
#' @importFrom tibble tribble deframe
#' @importFrom stringr word
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' library(googlesheets4)
#' library(tidyverse)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "1gue-wSQcEu4nJigVZdUWsTfIIzhtxpDRdWAQiEHgKak/edit#gid=1981295232")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (data <- gs %>%
#'     range_read("info"))
#'
#' plex <- data %>% fieldbook_plex(.)
#'
#' data %>% sheet_write(ss = gs, sheet = "test")
#'
#' }
#'
#' @export

fieldbook_plex <- function(data = NULL
                          , idea = NULL
                          , goal = NULL
                          , hypothesis = NULL
                          , rationale = NULL
                          , objectives = NULL
                          , plan = NULL
                          , institutions = NULL
                          , researchers = NULL
                          , manager = NULL
                          , location = NULL
                          , altitude = NULL
                          , georeferencing = NULL
                          , environment = NULL
                          , start = NULL
                          , end = NULL
                          , about = NULL
                          , fieldbook = NULL
                          , album = NULL
                          , github = NULL
                          , nfactor = 2
                          , design = "rcbd"
                          , rep = 3
                          , serie = 2
                          , seed = 0
                         ) {

  # idea <- goal <- hypothesis <-  rationale <- objectives <-  plan <- NA
  # institutions <- researchers <- manager <- location <- altitude <- NA
  # georeferencing <- environment <- start <- end <- album <- github <- NA
  # fieldbook <- NA
  # nfactor = 1
  # design = "crd"
  # rep = 2
  # serie = 2
  # seed = 0

  PLEX <- INFORMATION <- NULL

# plex -----------------------------------------------------------------------

if ( is.null(data) ) {


  plex <-  c(IDEA = idea
             , GOAL = goal
             , HYPOTHESIS = hypothesis
             , RATIONALE = rationale
             , OBJECTIVES = objectives
             , PLAN = plan
             , INSTITUTIONS = institutions
             , RESEARCHERS = researchers
             , MANAGER = manager
             , LOCATION = location
             , ALTITUDE = altitude
             , GEOREFERENCING = georeferencing
             , ENVIRONMENT = environment
             , "START EXPERIMENT" = start
             , "END EXPERIMENT" = end
             , ABOUT = about
             , "FIELDBOOK NAME" = fieldbook
             , GITHUB = github
             , ALBUM = album
             ) %>%
    enframe() %>%
    rename('PLEX' = .data$name, 'INFORMATION' = .data$value)

} else if ( !is.null(data) ) { # for import to the app?

  plex <- data %>%
    mutate(across(.data$PLEX, tolower)) %>%
    mutate(PLEX = word(PLEX, 1)) %>%
    deframe()
}

# variables ---------------------------------------------------------------

var_list <- c(variable = NA
              , '{siglas}' = NA # abbreviation
              , '{evaluation}' = NA # evaluation, eval dap dat
              , '{sampling}' = NA # sampling sample subplot muestra
              , units = NA
              , description = NA
              )

# design ------------------------------------------------------------------

    factors <- c(paste0("factor", 1:nfactor))

    dsg_info <-  c(nfactors = nfactor
                  , type = design
                  , rep = rep
                  , serie = serie
                  , seed = seed
                  ) %>%
      enframe() %>%
      rename('{arguments}' = .data$name, '{values}' = .data$value)

      dsg_info[,factors] <- NA

# result ------------------------------------------------------------------

list(plex = plex
     , design = dsg_info
     , variables = var_list
     )

}
