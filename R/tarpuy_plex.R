#' Fieldbook plan information
#'
#' Information for build a plan for an experiment (PLEX)
#'
#' @param data Data with the fieldbook information.
#' @param idea How the idea was born.
#' @param goal The main goal of the project.
#' @param hypothesis What are the expected results.
#' @param rationale Based in which evidence is planned the experiment.
#' @param objectives The objectives of the project.
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
#' @param about Short description of the project.
#' @param fieldbook Name or ID for the fieldbook/project.
#' @param gdocs link for Google Docs
#' @param nfactor Number of factors for the design.
#' @param design Type of design.
#' @param rep Number of replication.
#' @param zigzag Experiment layout in zigzag [logic: F]
#' @param nrows Experimental design dimension by rows [numeric: value]
#' @param serie Number of digits in the plots.
#' @param seed Seed for the randomization.
#'
#' @details
#'
#' Provide the information available.
#'
#' @return data frame or list of arguments:
#'
#'   \enumerate{ \item info \item variables \item design \item logbook \item
#'   timetable \item budget }
#'
#' @importFrom dplyr mutate
#' @importFrom tibble tribble deframe
#' @importFrom stringr word str_to_upper
#' 
#' @export
#'
#' 

tarpuy_plex <- function(data = NULL
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
                        , start = NA
                        , end = NA
                        , about = NULL
                        , fieldbook = NULL
                        , gdocs = NULL
                        , github = NULL
                        , album = NULL
                        , nfactor = 2
                        , design = "rcbd"
                        , rep = 3
                        , zigzag = FALSE
                        , nrows = NA
                        , serie = 100
                        , seed = 0
                         ) {
  
  
  PLEX <- INFORMATION <- DAI <- NULL
  
# arguments ---------------------------------------------------------------
  
  start <- if(is.null(start) || is.na(start)) { 
    format(Sys.time(), '%Y-%m-%d') %>% 
      as.Date() } else { start %>% as.Date(format = "%Y-%m-%d")} 
  
  end <- if(is.null(end) || is.na(end) ) {
    format(Sys.time(), '%Y-%m-%d') %>% 
      as.Date() + 90 } else { end %>% as.Date(format = "%Y-%m-%d") } 

# fieldbook name ----------------------------------------------------------
  
  loc <- if(is.null(location) || is.na(location) || location == "") { "INKAVERSE" 
    } else {location} %>% 
    iconv(., to="ASCII//TRANSLIT") %>%
    toupper() %>% 
    strsplit(., "[[:punct:]]") %>% 
    unlist() %>% 
    trimws() %>% 
    gsub(" ", "-", .) %>% 
    pluck(1)
  
  info <- if(is.null(about) || is.na(about) || about == "") { "TARPUY" 
    } else {about} %>% 
    iconv(., to="ASCII//TRANSLIT") %>%
    toupper() %>% 
    strsplit(., "[[:punct:]]") %>% 
    unlist() %>% 
    trimws() %>% 
    gsub(" ", "-", .) %>% 
    pluck(1)
  
  fbname <- if(is.null(fieldbook) || is.na(fieldbook) || fieldbook == "") {
    
    paste(loc, start, info, sep = "_")
    
    } else { fieldbook } 
    
  barcode <-  paste(loc, start, sep = "_")
    
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
             , "START EXPERIMENT" = as.character.Date(start)
             , "END EXPERIMENT" = as.character.Date(end)
             , ABOUT = info
             , "FIELDBOOK NAME" = fbname
             , GDOCS = gdocs
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
  
var_list <- list(
  list(format = "numeric"
        , variable = "X"
       , trait = "X"
       , when = "X"
       , samples = NA
       , units = "X"
       , details = NA
       , minimum = "X"
       , maximum = "X"
       )
  ,  list(format = "text"
          , variable = "X"
          , trait = "X"
          , when = "X"
          , samples = NA
          , units = "X"
          , details = NA
          )
  ,  list(format = "photo"
          , variable = "X"
          , trait = "X"
          , when = "X"
          , samples = NA
          , units = "X"
          , details = NA
          )
  , list(format = "scategorical"
         , variable = "X"
         , trait = "X"
         , when = "X"
         , samples = NA
         , units = "X"
         , details = NA
         , categories = "X"
         )
  , list(format = "mcategorical"
         , variable = "X"
         , trait = "X"
         , when = "X"
         , samples = NA
         , units = "X"
         , details = NA
         , categories = "X"
         )
  ,  list(format = "location"
          , variable = "X"
          , trait = "X"
          , when = "X"
          , samples = NA
          , units = "X"
          , details = NA
          )
  ,  list(format = "date"
          , variable = "X"
          , trait = "X"
          , when = "X"
          , samples = NA
          , units = "X"
          , details = NA
          )
  ,  list(format = "counter"
          , variable = "X"
          , trait = "X"
          , when = "X"
          , samples = NA
          , units = "X"
          , details = NA
          )
  ,  list(format = "boolean"
          , variable = "X"
          , trait = "X"
          , when = "X"
          , samples = NA
          , units = "X"
          , details = NA
          )
  ,  list(format = "audio"
          , variable = "X"
          , trait = "X"
          , when = "X"
          , samples = NA
          , units = "X"
          , details = NA
          )
  ) %>% 
    dplyr::bind_rows() %>% 
    dplyr::select(.data$variable
                  , .data$trait
                  , .data$when
                  , .data$samples
                  , .data$format
                  , .data$units
                  , .data$details
                  , .data$categories
                  ) %>%
    rename('{trait}' = .data$trait
         , '{when}' = .data$when
         , '{samples}' = .data$samples
         , '{format}' = .data$format
         )

# design ------------------------------------------------------------------

factors <- c(paste0("factor", 1:nfactor))
  
seedset <- if(seed == 0) sample(1:9999, 1) else seed

nrowsx <- if(is.na(nrows)) {nrowsx <- rep} else {nrowsx <- nrows}
  
dsg_info <-  c(nfactors = nfactor
              , type = design
              , rep = rep
              , zigzag = zigzag
              , nrows = nrowsx
              , serie = serie
              , seed = seedset
              , fbname = barcode
              ) %>%
  enframe() %>%
  rename('{arguments}' = .data$name, '{values}' = .data$value)

  dsg_info[,factors] <- NA

# timetable ---------------------------------------------------------------

finish <- end - start

first_col <- c("Activities (DAI)"
               , "Material Preparation"
               , rep(NA, 5)
               , "Evaluation"
               , rep(NA, 5)
               , "Data processing"
               ) %>%
  enframe(value = "Dates") %>% select(!.data$name)

ttable <- c(DAI = seq.int(from = -30, to = finish, by = 5)) %>%
  enframe() %>%
  mutate(date =  format( .data$value + start, "%d/%b")) %>%
  select(date, DAI = .data$value) %>%
  pivot_wider(names_from = date, values_from = DAI)

timetable <- merge( first_col
                    , ttable
                    , by = 0
                    , all = TRUE
                    )  %>%
  mutate(across(.data$Row.names, as.numeric)) %>%
  arrange(.data$Row.names) %>%
  select(!.data$Row.names)

# logbook -----------------------------------------------------------------

desc <- "Day After Initiation (DAI) of experiment."

logbook <- tibble(Date = c(rep(NA, 3), as.character.Date(start), rep(NA, 3))
               , DAI = c(rep(NA, 3), 0, rep(NA, 3))
               , Activity = c(rep(NA, 3), "Init experiment", rep(NA, 3))
               , Description = c(rep(NA, 3), desc, rep(NA, 3))
               )

# budget ------------------------------------------------------------------

budget <- tibble("Material/Service" = rep(NA, 5)
                 , Unit = rep(NA, 5)
                 , Price = rep(NA, 5)
                 , Quantity = rep(NA, 5)
                 , Total = rep(NA, 5)
                 , Description = rep(NA, 5)
                 )

# result ------------------------------------------------------------------

list(plex = plex
     , design = dsg_info
     , variables = var_list
     , logbook = logbook
     , timetable = timetable
     , budget = budget
     )

}
