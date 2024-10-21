#' Fieldbook plan information
#'
#' Information for build a plan for an experiment (PLEX)
#'
#' @param data Data with the fieldbook information.
#' @param title Project title.
#' @param objectives The objectives of the project.
#' @param hypothesis What are the expected results.
#' @param rationale Based in which evidence is planned the experiment.
#' @param references References.
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
#' @param repository link to the repository.
#' @param about Short description of the project.
#' @param fieldbook Name or ID for the fieldbook/project.
#' @param project link for project.
#' @param manuscript link for manuscript.
#' @param nfactor Number of factors for the design.
#' @param design Type of design.
#' @param rep Number of replication.
#' @param zigzag Experiment layout in zigzag [logic: F]
#' @param nrows Experimental design dimension by rows [numeric: value]
#' @param serie Number of digits in the plots.
#' @param seed Seed for the randomization.
#' @param qrcode [string: "\{fbname\}\{plots\}\{factors\}"] String to concatenate the qr code.
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
                        , title = NULL
                        , objectives = NULL
                        , hypothesis = NULL
                        , rationale = NULL
                        , references = NULL
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
                        , project = NULL
                        , repository = NULL
                        , manuscript = NULL
                        , album = NULL
                        , nfactor = 2
                        , design = "rcbd"
                        , rep = 3
                        , zigzag = FALSE
                        , nrows = NA
                        , serie = 100
                        , seed = 0
                        , qrcode = "{fbname}{plots}{factors}"
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

  plex <-  c(TITLE = title
             , OBJECTIVES = objectives
             , HYPOTHESIS = hypothesis
             , RATIONALE = rationale
             , REFERENCES = references
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
             , PROJECT = project
             , GITHUB = repository
             , MANUSCRIPT = manuscript
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
              , qrcode = qrcode
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

budget <- tibble(
  Category = c("Supplies", "Supplies", "Supplies", "Materials", "Materials", 
               "Equipment", "Tools", "Equipment", "Human Resources", 
               "Human Resources", "Human Resources", "Transport and Logistics", 
               "Transport and Logistics", "Services", "Services", "Other", 
               "Other", "Other"),
  Description = c("Seeds", "Fertilizers", "Phytosanitary products", 
                  "Substrate (soil, sand, etc.)", "Laboratory materials (pipettes, Petri dishes, etc.)", 
                  "Measuring instruments (sensors, scales, etc.)", "Field tools", 
                  "Software or programs for data analysis", "Researchers", 
                  "Laboratory technicians", "Field workers", "Equipment transport", 
                  "Personnel transport", "Sequencing", "Article publication", 
                  "Electricity", "General services", "Administrative expenses"),
  Unit = c(NA, NA, NA, "kg", NA, NA, NA, NA, "months", "months", "day", 
           "trips", "trips", "samples", "articles", "months", NA, NA),
  Quantity = NA,
  `Unit Cost` = NA,
  `Total Cost` = NA,
  `Technical Specifications` = c("Variety, purity, germination (%)", 
                               "Chemical composition (NPK, etc.)", 
                               "Active ingredient, concentration", 
                               "Soil type, pH, electrical conductivity", 
                               "Brand, capacity, material", 
                               "Precision, measurement range", 
                               "Type of tool, material", 
                               "Software name, license, compatibility", 
                               "Level of specialization, required hours", 
                               "Specialization, certification", 
                               "Experience, skills", 
                               "Type of vehicle, distance", 
                               "Number of people, distance", 
                               "Type of sequencing, number of samples", 
                               "Journal, impact level", 
                               "Kilowatt per month", 
                               "Service details", 
                               "Details of administrative expenses"),
  Justification = c("Essential for experimental cultivation", 
                    "Provides essential nutrients for growth", 
                    "Prevents pests and diseases", 
                    "Necessary for seed germination", 
                    "For precise measurements and laboratory cultivation", 
                    "Measurement of environmental and growth variables", 
                    "For crop management in the field", 
                    "Processing and analysis of experimental data", 
                    "Specialized personnel for design and analysis", 
                    "Technical assistance for experiment management", 
                    "Implementation and management of field experiments", 
                    "Transport of necessary equipment to the experimental site", 
                    "Transport of personnel to the experimental site", 
                    "Genetic analysis of samples", 
                    "Publication of experimental results", 
                    "Power supply for laboratory and field equipment", 
                    "Necessary for general experiment maintenance", 
                    "Administration and management of the experimental project"),
  `Reference` = "<link to info>"
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
