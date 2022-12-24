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
#' @param zigzag Experiment layout in zigzag [logic: F].
#' @param serie Number to start the plot id [numeric: 100].
#' @param seed Replicability of draw results (default = 0) always random. See
#'   details.
#' @param fbname Barcode prefix for data collection.
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
#' @import dplyr
#' @importFrom purrr pluck as_vector
#' @importFrom stringr str_detect str_to_upper
#' @importFrom tibble tibble
#' @importFrom utils tail
#' @importFrom tidyr unite
#' @importFrom purrr discard
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
#'               , "1grAv_2po804pPGg9nj1o5nli01IcEGvSevDruq_ssHk/edit#gid=1595426169")
#' # browseURL(url)
#' 
#' fb <- gsheet2tbl(url) 
#' 
#' dsg <- fb %>% tarpuy_design() 
#' 
#' dsg %>% str()
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
                          , serie = 100
                          , seed = NULL
                          , fbname = NA
                          ) {

plots <- Row.names <- factors <- where <- NULL
  
# design type -------------------------------------------------------------
# -------------------------------------------------------------------------

type <- match.arg(type, c(
  "crd", "rcbd", "lsd", "lattice"
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
} else {arguments$rep} %>% 
  as.numeric()

zigzag <- if(is.null(arguments$zigzag) || is.na(arguments$zigzag) || arguments$zigzag == "") { zigzag
} else {arguments$zigzag} %>% as.logical()

serie <- if(is.null(arguments$serie) || is.na(arguments$serie) || arguments$serie == "") { serie
} else {arguments$serie} %>% 
  as.numeric()

seed <- if(is.null(arguments$seed) || is.na(arguments$seed) || arguments$seed == "" || arguments$seed == "0") { NULL
} else {arguments$seed %>% as.numeric()} 
  
fbname <- if(is.null(arguments$fbname) || is.na(arguments$fbname) || arguments$fbname == "") { fbname
} else {arguments$fbname} %>% 
  stringi::stri_trans_general("Latin-ASCII") %>%
  stringr::str_to_upper() %>% 
  gsub("[[:space:]]", "-", .)

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

design <- design_repblock(
  nfactors = nfactors
  , factors = factor_levels
  , type = type
  , rep = rep
  , zigzag = zigzag
  , serie = serie
  , seed = seed
  , fbname = fbname
  ) %>% purrr::pluck(1)

return(design)
  
}

