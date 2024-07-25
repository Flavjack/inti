#' Fieldbook experimental designs
#'
#' Function to deploy experimental designs
#'
#' @param data Experimental design data frame with the factors and level. See
#'   examples.
#' @param nfactors Number of factor in the experiment(default = 1). See
#'   details.
#' @param type Type of experimental arrange  (default = "crd"). See details.
#' @param rep  Number of replications in the experiment (default = 3).
#' @param zigzag Experiment layout in zigzag [logic: FALSE].
#' @param nrows Experimental design dimension by rows [numeric: value]
#' @param serie Number to start the plot id [numeric: 100].
#' @param seed Replicability of draw results (default = 0) always random. See
#'   details.
#' @param fbname Barcode prefix for data collection.
#' @param qrcode [string: "\{fbname\}\{plots\}\{factors\}"] String to concatenate the qr code.
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
                          , fbname = NA
                          , qrcode = "{fbname}{plots}{factors}"
                          ) {

plots <- Row.names <- factors <- where <- NULL

# data <- fb
  
# design type -------------------------------------------------------------
# -------------------------------------------------------------------------

type <- match.arg(type, c(
  "sorted", "unsorted"
  , "crd", "rcbd", "lsd", "lattice"
  , "split-crd", "split-rcbd"
  ))


# factors -----------------------------------------------------------------
# -------------------------------------------------------------------------

# data <- fb

dt_factors <- data %>%
  dplyr::select(where(~!all(is.na(.)))) %>% 
  dplyr::select(!starts_with("[") | !ends_with("]")) %>%
  dplyr::select(!starts_with("{") | !ends_with("}"))

# -------------------------------------------------------------------------

if(length(dt_factors) == 0 | length(dt_factors) < nfactors) {
  
  print("Factors without levels")
  
  return(fieldbook <- NULL)
  
}

# desatendido -------------------------------------------------------------
# -------------------------------------------------------------------------

arg_opt <- data %>% 
  dplyr::select(starts_with("{") | ends_with("}")) %>% 
  names() 

data <- if(length(arg_opt) == 2) {data} else if (length(arg_opt) < 2) {

ndata <- data %>% 
  dplyr::select(!starts_with("{") | !ends_with("}")) %>% 
  dplyr::select(1:{{nfactors}})  

opt <- list(nfactors = nfactors
            , type = type
            , rep = rep
            , serie = serie
            , seed = seed
            , fbname = fbname
            ) %>% 
  tibble::enframe(name = "{arguments}", value = "{values}") %>% 
  merge(.
        , ndata
        , by = 0
        , all = TRUE
        ) %>% 
  dplyr::select(!.data$Row.names)
  
}

# data arguments ----------------------------------------------------------
# -------------------------------------------------------------------------

arguments <- data %>%
  dplyr::select(where(~!all(is.na(.)))) %>% 
  dplyr::select(starts_with("{") | ends_with("}")) %>%
  dplyr::rename_with(~ gsub("\\{|\\}", "", .)) %>%
  tidyr::drop_na() %>% 
  tibble::deframe() %>% 
  as.list()

# -------------------------------------------------------------------------

nfactors <- if(is.null(arguments$nfactors) || is.na(arguments$nfactors) || arguments$nfactors == "") { nfactors
} else {arguments$nfactors} %>% 
  as.numeric()

type <- if(is.null(arguments$type) || is.na(arguments$type) || arguments$type == "") { type
} else {arguments$type}

rep <- if(is.null(arguments$rep) || is.na(arguments$rep) || arguments$rep == "") { rep
} else {arguments$rep} %>% as.numeric()

zigzag <- if(is.null(arguments$zigzag) || is.na(arguments$zigzag) || arguments$zigzag == "") { zigzag
} else {arguments$zigzag} %>% as.logical()

nrows <- if(is.null(arguments$nrows) || is.na(arguments$nrows) || arguments$nrows == "") { rep
} else {arguments$nrows} %>% as.numeric()

serie <- if(is.null(arguments$serie) || is.na(arguments$serie) || arguments$serie == "") { serie
} else {arguments$serie} %>% as.numeric()

seed <- if(is.null(arguments$seed) || is.na(arguments$seed) || arguments$seed == "" || arguments$seed == "0") { NULL
} else {arguments$seed %>% as.numeric()} 
  
fbname <- if(is.null(arguments$fbname) || is.na(arguments$fbname) || arguments$fbname == "") { fbname
} else {arguments$fbname} %>% 
  iconv(., to="ASCII//TRANSLIT") %>%
  toupper() %>% 
  gsub("[[:space:]]", "-", .)

qrcode <- if(is.null(arguments$fbname) || is.na(arguments$fbname) || arguments$fbname == "") { qrcode
} else {arguments$qrcode} 

# -------------------------------------------------------------------------

factor_names <- dt_factors %>%
  names() %>% 
  .[1:arguments$nfactors] %>% 
  stats::na.omit()

if(length(factor_names) != nfactors) {
  
  print("Number of factor are different with the column factors")
  
  return(fieldbook <- NULL)
  
}

factor_levels <- dt_factors %>%
  dplyr::select({{factor_names}}) %>% 
  as.list() 

design <- if(nfactors == 1 & rep == 1) {
  
  design_noreps(factors = factor_levels
                , type = type 
                , zigzag = zigzag
                , nrows = nrows
                , serie = serie
                , seed = seed
                , fbname = fbname
                , qrcode = qrcode
                ) %>% purrr::pluck(1)
  } else {
    
    design_repblock(
      nfactors = nfactors
      , factors = factor_levels
      , type = type
      , rep = rep
      , zigzag = zigzag
      , nrows = nrows
      , serie = serie
      , seed = seed
      , fbname = fbname
      , qrcode = qrcode
      )  %>% purrr::pluck(1)
  
}
  
return(design)
  
}

