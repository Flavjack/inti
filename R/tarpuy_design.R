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
#' @param serie Digits in the plot id (default = 2).
#' @param seed Replicability of draw results (default = 0) always random. See
#'   details.
#' @param barcode Barcode prefix for data collection.
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
#'               , "1tDcLZZ9C80uBJK8RRW6_WIyxiL8bIHACStJyf8avuCU/edit#gid=59220990")
#' # browseURL(url)
#' 
#' fb <- gsheet2tbl(url) 
#' 
#' tarpuy_design(data = fb) 
#' 
#' }

tarpuy_design <- function(data
                          , nfactors = 1
                          , type = "crd"
                          , rep = 2
                          , serie = 2
                          , seed = 0
                          , barcode = NA
                             ) {

  plots <- Row.names <- factors <- where <- NULL
  
# design type -------------------------------------------------------------
# -------------------------------------------------------------------------

type <- match.arg(type, c(
  "crd", "rcbd", "lsd", "lattice"
  , "split-crd", "split-rcbd"
  ))


# data arguments ----------------------------------------------------------
# -------------------------------------------------------------------------

# data <- fb

dt_factors <- data %>%
  dplyr::select(where(~!all(is.na(.)))) %>% 
  dplyr::select(!starts_with("{") | !ends_with("}")) %>%
  dplyr::rename_with(~ gsub("\\s+|\\.", "_", .)) %>%
  dplyr::mutate(across(everything(), ~ gsub(" ", "-", .))) %>%
  dplyr::na_if("NULL") 

arguments <- data %>%
  dplyr::select(where(~!all(is.na(.)))) %>% 
  dplyr::select(starts_with("{") | ends_with("}")) %>%
  dplyr::rename_with(~ gsub("\\{|\\}", "", .)) %>%
  tidyr::drop_na() %>% 
  tibble::deframe() %>% 
  as.list()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

if(length(dt_factors) == 0) {
  
  print("Factors without levels")
  
  return(fieldbook <- NULL)
  
}

# -------------------------------------------------------------------------

nfactors <- if(is.null(arguments$nfactors) || is.na(arguments$nfactors) || arguments$nfactors == "") { nfactors
} else {arguments$nfactors} %>% 
  as.numeric()

type <- if(is.null(arguments$type) || is.na(arguments$type) || arguments$type == "") { type
} else {arguments$type}

rep <- if(is.null(arguments$rep) || is.na(arguments$rep) || arguments$rep == "") { rep
} else {arguments$rep} %>% 
  as.numeric()

serie <- if(is.null(arguments$serie) || is.na(arguments$serie) || arguments$serie == "") { serie
} else {arguments$serie} %>% 
  as.numeric()

seed <- if(is.null(arguments$seed) || is.na(arguments$seed) || arguments$seed == "") { seed
} else {arguments$seed} %>% 
  as.numeric()

barcode <- if(is.null(arguments$barcode) || is.na(arguments$barcode) || arguments$barcode == "") { barcode
} else {arguments$barcode} %>% 
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
  as.list() %>%
  purrr::map(base::unique) %>% 
  purrr::map(stats::na.omit) %>% 
  purrr::map(as.vector)
  

# n_factor = 1 ------------------------------------------------------------
# -------------------------------------------------------------------------

    fb <- if (nfactors == 1) {
            
            factors <- factor_levels[[1]]
            
            if (type == "crd") {
              
              design <- agricolae::design.crd(
                trt = factors
                , r = rep
                , serie = serie
                , seed = seed
                )

              fb_info <- design$book %>%
                dplyr::rename( {{factor_names}} := "factors")
                
            }

            if (type == "rcbd") {
              
              design <- agricolae::design.rcbd(
                trt = factors
                , r = rep
                , serie = serie
                , seed = seed
                )

              fb_info <- design$book %>%
                dplyr::rename( {{factor_names}} := "factors")
              
            }

            if (type == "lsd") {

              design <- agricolae::design.lsd(
                trt = factors
                , r = rep
                , serie = serie
                , seed = seed
              )
              
              fb_info <- design$book %>%
                dplyr::rename( {{factor_names}} := "factors")
              
            }

            if (type == "lattice") { # fix rename column?

              if( rep > 3 ) { rep <- 3 }

              design <- agricolae::design.lattice(
                trt = factors
                , r = rep
                , serie = serie
                , seed = seed
                )

              fb_info <- design$book %>%
                dplyr::rename( {{factor_names}} := "trt")

            }

          }

# n_factor >= 2 -----------------------------------------------------------
# -------------------------------------------------------------------------

        if( nfactors == 2 & startsWith(type, "split") ) {
          
          model <- type %>% gsub("split-", "", .)
            
            factor1 <- factor_levels[[1]]
            factor2 <- factor_levels[[2]]

            design <- agricolae::design.split(
              trt1 = factor1,
              trt2 = factor2,
              r = rep,
              design = model,
              serie = serie,
              seed = seed
            )
            
            fb_info <- design$book %>%
              dplyr::rename_with(~ {{ factor_names }}, tail(names(.), 2))
            
        }

# factorial ---------------------------------------------------------------
# -------------------------------------------------------------------------

        if ( nfactors >= 2 & ( type == "crd" | type == "rcbd" | type == "lsd" ) ) {
          
          factors <- lengths(factor_levels)

          design <- agricolae::design.ab(
            trt = factors
            , r = rep
            , serie = serie
            , design = type
            , seed = seed
          )
          
          
          fb <- design$book %>%
            dplyr::rename_with(~ {{ factor_names }}, tail(names(.), nfactors)) 
          
          fb_factors <- fb %>% 
            dplyr::select(!{{factor_names}}) %>% 
            names()
          
          fb_info <- 1:length(factor_names) %>% purrr::map( function(x) {
            
            factor <- names(factors)[x]
            levels <- factor_levels[[factor]] 
            
            org_names <- fb %>% 
              dplyr::select({{factor}}) %>% 
              unique() %>%  
              tibble::deframe() %>% 
              as.vector()
            
            names <- structure(as.character(levels), names = as.character(org_names))
            
            fb %>% 
              dplyr::select(fb_factors, {{factor}}) %>% 
              dplyr::mutate(across({{factor}}, ~dplyr::recode(.x = ., !!!names)))
            
          }) %>% 
            Reduce(function(...) merge(..., all=T), .)
          
        }
          
# include qr --------------------------------------------------------------
# -------------------------------------------------------------------------

fb <- fb_info %>% 
  dplyr::mutate(exp = barcode, .before = 1) %>% 
  tidyr::unite(col = "barcode", everything(), remove = F, sep = "_") %>% 
  dplyr::select(!exp)

list(fieldbook = fb)
  
}

