#' Fieldbook experimental designs
#'
#' Function to deploy experimental designs
#'
#' @param data Experimental design data frame with the factors and level. See
#'   examples.
#' @param nfactors Number of factor in the experiment(default = 1). See
#'   details.
#' @param type Type of experimental arrange `[default = "crd"]`. See details.
#' @param rep  Number of replications in the experiment (default = 3).
#' @param zigzag Experiment layout in zigzag `[logic: FALSE]`.
#' @param nrows Experimental design dimension by rows `[numeric: value]`.
#' @param serie Number to start the plot id `[numeric: 100]`.
#' @param seed Replicability of draw results `[default = 0]` always random. See
#'   details.
#' @param project Barcode prefix for data collection.
#' @param qrcode String to concatenate the QR code `[character: {project}{plots}{factors}]`.
#'
#' @details The function allows to include the arguments in the sheet that have
#'   the information of the design. You should include 2 columns in the sheet:
#'   \code{{arguments}} and \code{{values}}. See examples. The information will
#'   be extracted automatically and deploy the design. \code{nfactors} = 1:
#'   crd, rcbd, lsd, lattice. \code{nfactors} = 2 (factorial): split-crd,
#'   split-rcbd split-lsd \code{nfactors} >= 2 (factorial): crd, rcbd, lsd.
#'
#' @return A list with the fieldbook design
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#'
#' library(inti)
#' library(gsheet)
#' 
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1479851579#gid=1479851579")
#' # browseURL(url)
#' 
#' fb <- gsheet2tbl(url) 
#' 
#' dsg <- fb %>% tarpuy_design() 
#' 
#' dsg %>% 
#'   tarpuy_plotdesign()
#' 
#' }

tarpuy_design <- function(data
                          , nfactors = 1
                          , type = "crd"
                          , rep = 2
                          , zigzag = FALSE
                          , nrows = NA
                          , serie = 100
                          , seed = NULL
                          , project = NA
                          , qrcode = "{project}{plots}") {
  plots <- Row.names <- factors <- where <- NULL
  
  is_blank <- function(x) {
    is.null(x) || length(x) == 0 || is.na(x) || x == ""
  }
  
  # data <- fb
  
  # design type -------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  type <- match.arg(
    type,
    c(
      "sorted", "unsorted"
      ,
      "crd", "rcbd", "lsd", "lattice"
      ,
      "split-crd", "split-rcbd"
      , "augmented", "strip-plot"
    )
  )
  
  
  # factors -----------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  # data <- fb
  
  dt_factors <- data %>%
    dplyr::select(where( ~ !all(is.na(.)))) %>%
    dplyr::select(!starts_with("[") | !ends_with("]")) %>%
    dplyr::select(!starts_with("{") | !ends_with("}"))
  
  # -------------------------------------------------------------------------
  
  if (length(dt_factors) == 0 | length(dt_factors) < nfactors) {
    print("Factors without levels")
    
    return(fieldbook <- NULL)
    
  }
  
  # desatendido -------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  arg_opt <- data %>%
    dplyr::select(starts_with("{") | ends_with("}")) %>%
    names()
  
  data <- if (length(arg_opt) == 2) {
    data
  } else if (length(arg_opt) < 2) {
    ndata <- data %>%
      dplyr::select(!starts_with("{") | !ends_with("}")) %>%
      dplyr::select(1:{{nfactors}})
    
    opt <- list(
      nfactors = nfactors
      , type = type
      , rep = rep
      , serie = serie
      , seed = seed
      , project = project
    ) %>%
      tibble::enframe(name = "{arguments}", value = "{values}") %>%
      merge(.
            , ndata
            , by = 0
            , all = TRUE) %>%
      dplyr::select(!.data$Row.names)
    
  }
  
  # data arguments ----------------------------------------------------------
  # -------------------------------------------------------------------------
  
  arguments <- data %>%
    dplyr::select(where( ~ !all(is.na(.)))) %>%
    dplyr::select(starts_with("{") | ends_with("}")) %>%
    dplyr::rename_with( ~ gsub("\\{|\\}", "", .)) %>%
    tidyr::drop_na() %>%
    tibble::deframe() %>%
    as.list()
  
  # -------------------------------------------------------------------------
  
  nfactors <- if(is_blank(arguments$nfactors)) nfactors else as.numeric(arguments$nfactors)
  
  type <- if(is_blank(arguments$type)) type else arguments$type
  
  rep <- if(is_blank(arguments$rep)) rep else as.numeric(arguments$rep)
  
  zigzag <- if(is_blank(arguments$zigzag)) zigzag else as.logical(arguments$zigzag)
  
  nrows <- if(is_blank(arguments$nrows)) nrows else as.numeric(arguments$nrows)
  
  serie <- if(is_blank(arguments$serie)) serie else as.numeric(arguments$serie)
  
  seed <- if(is_blank(arguments$seed) || arguments$seed == "0") NULL else as.numeric(arguments$seed)
  
  
  project <- if (is.null(arguments$project) ||
                 is.na(arguments$project) || arguments$project == "") {
    project
  } else {
    arguments$project
  } %>%
    iconv(., to = "ASCII//TRANSLIT") %>%
    toupper() %>%
    gsub("[[:space:]]", "-", .)
  
  qrcode <- if(is_blank(arguments$qrcode)) qrcode else arguments$qrcode
  
  #qrcode <- if(is_blank(arguments$qrcode)) qrcode else arguments$qrcode
  
  blocks <- if(is_blank(arguments$blocks)) NULL else as.numeric(arguments$blocks)
  
  eu_block <- if(is_blank(arguments$eu_block)) NULL else as.numeric(arguments$eu_block)
  
  random <- if(is_blank(arguments$random)) TRUE else as.logical(arguments$random)
  
  
  # design
  # -------------------------------------------------------------------------
  
  
  if(type == "augmented") {
    
    if(!all(c("checks", "entries") %in% names(dt_factors))) {
      print("Columns 'checks' and 'entries' are required")
      return(NULL)
    }
    
    if(qrcode == "{project}{plots}") {
      qrcode <- "{project}{plots}{entry}"
    }
    
    checks <- dt_factors$checks %>%
      stats::na.omit() %>%
      unique() %>%
      as.character()
    
    entries <- dt_factors$entries %>%
      stats::na.omit() %>%
      unique() %>%
      as.character()
    
  } else {
    
    factor_names <- dt_factors %>%
      names() %>%
      .[1:nfactors] %>%
      stats::na.omit()
    
    if(length(factor_names) != nfactors) {
      print("Number of factors does not match columns")
      return(NULL)
    }
    
    factor_levels <- dt_factors %>%
      dplyr::select({{factor_names}}) %>%
      as.list()
  }
  
  # -------------------------------------------------------------------------
  # DESIGN DISPATCH
  # -------------------------------------------------------------------------
  
  design <- if(type == "split-rcbd") {
    
    design_split(
      nfactors = nfactors,
      factors = factor_levels,
      type = type,
      rep = rep,
      zigzag = zigzag,
      nrows = nrows,
      serie = serie,
      seed = seed,
      project = project,
      qrcode = qrcode
    ) %>%
      purrr::pluck("fieldbook")
    
  } else if(type == "augmented") {
    
    design_augmented(
      checks = checks,
      entries = entries,
      blocks = blocks,
      eu_block = eu_block,
      random = random,
      zigzag = zigzag,
      serie = serie,
      seed = seed,
      project = project,
      qrcode = qrcode
    ) %>%
      purrr::pluck("fieldbook")
    
  } else if(nfactors == 1 & rep == 1) {
    
    design_noreps(
      factors = factor_levels,
      type = type,
      zigzag = zigzag,
      nrows = nrows,
      serie = serie,
      seed = seed,
      project = project,
      qrcode = qrcode
    ) %>%
      purrr::pluck(1)
    
  } else {
    
    design_repblock(
      nfactors = nfactors,
      factors = factor_levels,
      type = type,
      rep = rep,
      zigzag = zigzag,
      nrows = nrows,
      serie = serie,
      seed = seed,
      project = project,
      qrcode = qrcode
    ) %>%
      purrr::pluck(1)
  }
}
