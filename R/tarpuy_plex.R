#' Fieldbook plan information
#'
#' Information for build a plan for an experiment (PLEX)
#'
#' @param data Data with the fieldbook information.
#' @param title Project title.
#' @param objective The objectives of the project.
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
#' @param short_title Short description of the project.
#' @param project Name or ID for the fieldbook/project.
#' @param manuscript link for manuscript.
#' @param nfactor Number of factors for the design.
#' @param design Type of design.
#' @param rep Number of replication.
#' @param zigzag Experiment layout in zigzag `[logic: FALSE]`
#' @param nrows Experimental design dimension by rows `[numeric: value]`
#' @param serie Number of digits in the plots.
#' @param seed Seed for the randomization. A value of `0`, `NA`, or `NULL`
#'   generates and stores an effective seed in the design sheet.
#' @param qrcode QR code template used to concatenate fieldbook identifiers.
#' @param aug_blocks Optional number of blocks for augmented design.
#' @param aug_eu_block Number of plots per block for augmented design.
#' @param aug_random Logical. Randomize entries allocation in augmented design.
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
                        , short_title = NULL
                        , objective = NULL
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
                        , project = NULL
                        , repository = NULL
                        , manuscript = NULL
                        , album = NULL
                        , nfactor = 2
                        , design = "rcbd"
                        , rep = 4
                        , zigzag = FALSE
                        , nrows = NA
                        , serie = 1000
                        , seed = 0
                        , qrcode = "{project}{plots}"
                        , aug_blocks = NA
                        , aug_eu_block = NA
                        , aug_random = TRUE
) {
  
  
  PLEX <- INFORMATION <- DAI <- NULL
  
  # Internal helpers -------------------------------------------------------
  
  is_blank_scalar <- function(x) {
    is.null(x) ||
      length(x) == 0L ||
      (
        length(x) == 1L &&
          (
            is.na(x) ||
              (is.character(x) && !nzchar(trimws(x)))
          )
      )
  }
  
  parse_plex_date <- function(x, name, default) {
    
    if(is_blank_scalar(x)) {
      return(default)
    }
    
    if(length(x) != 1L) {
      stop("'", name, "' must contain one date.", call. = FALSE)
    }
    
    value <- if(inherits(x, "Date")) {
      x
    } else if(inherits(x, c("POSIXct", "POSIXlt"))) {
      as.Date(x)
    } else {
      suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))
    }
    
    if(length(value) != 1L || is.na(value)) {
      stop(
        "'", name, "' must be a valid date in YYYY-MM-DD format.",
        call. = FALSE
      )
    }
    
    value
  }
  
  positive_integer <- function(x, name, allow_missing = FALSE) {
    
    if(is_blank_scalar(x)) {
      if(isTRUE(allow_missing)) {
        return(NA_integer_)
      }
      
      stop("'", name, "' is required.", call. = FALSE)
    }
    
    value <- suppressWarnings(as.numeric(as.character(x)))
    
    if(
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 1 ||
      value != floor(value)
    ) {
      stop("'", name, "' must be a positive integer.", call. = FALSE)
    }
    
    as.integer(value)
  }
  
  logical_scalar <- function(x, name, default = FALSE) {
    
    if(is_blank_scalar(x)) {
      return(isTRUE(default))
    }
    
    if(length(x) != 1L) {
      stop("'", name, "' must be TRUE or FALSE.", call. = FALSE)
    }
    
    if(is.logical(x) && !is.na(x)) {
      return(x)
    }
    
    value <- tolower(trimws(as.character(x)))
    
    if(value %in% c("true", "t", "1", "yes", "y")) return(TRUE)
    if(value %in% c("false", "f", "0", "no", "n")) return(FALSE)
    
    stop("'", name, "' must be TRUE or FALSE.", call. = FALSE)
  }
  
  # Arguments --------------------------------------------------------------
  
  design <- normalize_tarpuy_design_type(design)
  
  if(is_blank_scalar(design)) {
    stop("'design' is required.", call. = FALSE)
  }
  
  design <- tolower(trimws(as.character(design[1L])))
  nfactor <- positive_integer(nfactor, "nfactor")
  serie <- positive_integer(serie, "serie")
  zigzag <- logical_scalar(zigzag, "zigzag", FALSE)
  aug_random <- logical_scalar(aug_random, "aug_random", TRUE)
  
  if(design == "split-rcbd" && nfactor != 2L) {
    stop("Splitplot-RCBD requires exactly two factors.", call. = FALSE)
  }
  
  rep <- if(design == "augmented" && is_blank_scalar(rep)) {
    NA_integer_
  } else {
    positive_integer(rep, "rep")
  }
  
  nrows <- positive_integer(nrows, "nrows", allow_missing = TRUE)
  aug_blocks <- positive_integer(
    aug_blocks,
    "aug_blocks",
    allow_missing = TRUE
  )
  aug_eu_block <- positive_integer(
    aug_eu_block,
    "aug_eu_block",
    allow_missing = TRUE
  )
  
  seed <- if(is_blank_scalar(seed)) 0L else suppressWarnings(
    as.numeric(as.character(seed))
  )
  
  if(
    length(seed) != 1L ||
    is.na(seed) ||
    !is.finite(seed) ||
    seed < 0 ||
    seed != floor(seed) ||
    seed > .Machine$integer.max
  ) {
    stop("'seed' must be a non-negative integer.", call. = FALSE)
  }
  
  seed <- as.integer(seed)
  
  qrcode <- if(is_blank_scalar(qrcode)) {
    "{project}{plots}"
  } else {
    trimws(as.character(qrcode[1L]))
  }
  
  start <- parse_plex_date(start, "start", Sys.Date())
  end <- parse_plex_date(end, "end", start + 90L)
  
  if(end < start) {
    stop(
      "'end' must be equal to or later than 'start'.",
      call. = FALSE
    )
  }
  
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
  
  info <- if(is.null(short_title) || is.na(short_title) || short_title == "") { "TARPUY" 
  } else {short_title} %>% 
    iconv(., to="ASCII//TRANSLIT") %>%
    toupper() %>% 
    strsplit(., "[[:punct:]]") %>% 
    unlist() %>% 
    trimws() %>% 
    gsub(" ", "-", .) %>% 
    pluck(1)
  
  project_info <- if(is.null(project) || is.na(project) || project == "") {
    
    paste(loc, start, info, sep = "_")
    
  } else { project }
  
  project_code <- paste(loc, format(start, "%Y-%m"), sep = "_")
  
  # plex -----------------------------------------------------------------------
  
  if ( is.null(data) ) {
    
    plex <-  c(TITLE = title
               , OBJECTIVE = objective
               , `SHORT TITLE` = short_title
               , MANAGER = manager
               , REFERENCES = references
               , PLAN = plan
               , INSTITUTIONS = institutions
               , RESEARCHERS = researchers
               , LOCATION = location
               , ALTITUDE = altitude
               , GEOREFERENCING = georeferencing
               , ENVIRONMENT = environment
               , "START EXPERIMENT" = as.character.Date(start)
               , "END EXPERIMENT" = as.character.Date(end)
               , PROJECT = project_info
               , GITHUB = repository
               , MANUSCRIPT = manuscript
               , ALBUM = album
    ) %>%
      enframe() %>%
      rename('PLEX' = .data$name, 'INFORMATION' = .data$value)
    
  } else if ( !is.null(data) ) { # for import to the app?
    
    plex_data <- tibble::as_tibble(data)
    
    if(!all(c("PLEX", "INFORMATION") %in% names(plex_data))) {
      stop(
        "'data' must contain the columns PLEX and INFORMATION.",
        call. = FALSE
      )
    }
    
    plex <- plex_data %>%
      dplyr::transmute(
        PLEX = stringr::word(tolower(as.character(.data$PLEX)), 1L),
        INFORMATION = .data$INFORMATION
      ) %>%
      tibble::deframe()
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
  
  # A random effective seed is stored when the user selects seed = 0.
  seedset <- if(seed == 0L) sample.int(9999L, 1L) else seed
  
  # Standard designs keep the existing default of one row per replication.
  # Splitplot-RCBD leaves nrows as NA so its design function can calculate it.
  nrowsx <- if(is.na(nrows)) rep else nrows
  nrows_split <- if(is.na(nrows)) NA_integer_ else nrows
  
  # Keep the augmented default QR structure, but respect custom QR templates
  # for every design, including Splitplot-RCBD.
  qrcode_design <- if(
    design == "augmented" &&
    identical(qrcode, "{project}{plots}")
  ) {
    "{project}{plots}{entry}"
  } else {
    qrcode
  }
  
  # design builders
  # -------------------------------------------------------------------------
  
  build_split_rcbd <- function() {
    
    dsg <- tibble::tibble(
      "{arguments}" = c(
        "nfactors",
        "type",
        "rep",
        "zigzag",
        "nrows",
        "serie",
        "seed",
        "project",
        "qrcode"
      ),
      
      "{values}" = c(
        2,
        "split-rcbd",
        rep,
        zigzag,
        nrows_split,
        serie,
        seedset,
        project_code,
        qrcode_design
      )
    )
    
    dsg$whole_plot <- NA_character_
    dsg$sub_plot   <- NA_character_
    
    dsg
  }
  
  
  build_augmented <- function() {
    
    # Preserve the current app template. The optional blocks argument is only
    # added when it was explicitly supplied to tarpuy_plex().
    dsg_arguments <- c(
      "type",
      if(!is.na(aug_blocks)) "blocks",
      "eu_block",
      "random",
      "zigzag",
      "serie",
      "seed",
      "project",
      "qrcode"
    )
    
    dsg_values <- c(
      "augmented",
      if(!is.na(aug_blocks)) aug_blocks,
      aug_eu_block,
      aug_random,
      zigzag,
      serie,
      seedset,
      project_code,
      qrcode_design
    )
    
    dsg <- tibble::tibble(
      "{arguments}" = dsg_arguments,
      "{values}" = dsg_values
    )
    
    dsg$checks  <- NA_character_
    dsg$entries <- NA_character_
    
    dsg
  }
  # -------------------------------------------------------------------------
  
  build_factorial <- function() {
    
    factors <- paste0("factor", seq_len(nfactor))
    
    dsg <- c(
      nfactors = nfactor,
      type     = design,
      rep      = rep,
      zigzag   = zigzag,
      nrows    = nrowsx,
      serie    = serie,
      seed     = seedset,
      project  = project_code,
      qrcode   = qrcode_design
    ) %>%
      tibble::enframe() %>%
      dplyr::rename(
        "{arguments}" = .data$name,
        "{values}"    = .data$value
      )
    
    for (f in factors) {
      dsg[[f]] <- NA_character_
    }
    
    dsg
  }
  
  # dispatcher
  # -------------------------------------------------------------------------
  
  if(design == "augmented") {
    
    dsg_info <- build_augmented()
    
  } else if(design == "split-rcbd") {
    
    dsg_info <- build_split_rcbd()
    
  } else {
    
    dsg_info <- build_factorial()
    
  }
  
  # timetable ---------------------------------------------------------------
  
  finish <- as.integer(end - start)
  
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
  
  
  # matrix ------------------------------------------------------------------
  
  matrix <- tibble(
    Information = c(
      "Material & Methods",
      paste("OE", 1:5),
      "Supplementaty Material"
    ),
    Variables = "",
    Presentation = "",
    `Statistical Analyses` = "",
    Results = "",
    Discussion = "",
    Limitations = ""
  )
  
  # CreDiT ------------------------------------------------------------------
  
  credit <- tibble::tibble(
    Author = paste("Author", 1:6),
    ORCID = "",
    email = "",
    Conceptualization = "",
    `Data curation` = "",
    `Formal analysis` = "",
    `Funding acquisition` = "",
    Investigation = "",
    Methodology = "",
    `Project administration` = "",
    Resources = "",
    Software = "",
    Supervision = "",
    Validation = "",
    Visualization = "",
    `Writing: original draft` = "",
    `Writing: review & editing` = ""
  )
  
  # result ------------------------------------------------------------------
  
  list(plex = plex
       , design = dsg_info
       , variables = var_list
       , logbook = logbook
       , timetable = timetable
       , budget = budget
       , credit = credit
       , matrix = matrix
  )
  
}