#' Experimental design without replications
#'
#' Function to deploy field-book experiment without replications
#'
#' @param factors Lists with names and factor vector [list].
#' @param type Randomized [logic: FALSE, TRUE ]
#' @param zigzag Experiment layout in zigzag [logic: FALSE].
#' @param serie Number to start the plot id [numeric: 1000].
#' @param nrows Experimental design dimension [numeric: value]
#' @param seed Replicability from randomization [numeric: NULL].
#' @param fbname Bar code prefix for data collection [string: "inkaverse"].
#'
#' @return A list with the field-book design and parameters
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#'
#' library(inti)
#' 
#' factores <- list("geno" = c(1:200))
#' 
#' fb <- design_noreps(factors = factores
#'                      , type = F
#'                      , zigzag = T
#'                      , nrows = 9
#'                      )
#'                      
#' dsg <- fb$fieldbook
#' 
#' fb %>%   
#'   tarpuy_plotdesign(fill = "plots") + theme(legend.position = "none")
#' 
#' fb$parameters
#' 
#' }

design_noreps <- function(factors
                            , type = FALSE
                            , zigzag = FALSE
                            , nrows = NA
                            , serie = 1000
                            , seed = NULL
                            , fbname = "inkaverse"
                            ) {
  
  # factors <- factores
  
  set.seed(seed)
  
  nfactors <- 1
  rep <- 1
  
  dfactors <- factors %>%
    purrr::map(~ gsub("NA|NULL", NA, .)) %>% 
    purrr::map(base::unique) %>% 
    purrr::map(stats::na.omit) %>% 
    purrr::map(~gsub("[[:space:]]", ".", .)) %>% 
    purrr::set_names(gsub("[[:space:]]", "." , names(.))) %>% 
    .[1:nfactors]
  
  name.factors <- names(dfactors)
  
  nrows <- if(anyNA(nrows)) {rep} else {nrows}
  
  ncols <- dfactors %>% 
    unlist() %>% 
    length()/nrows; ncols <- ceiling(ncols)
  
  fb <- dfactors %>% 
    expand.grid() %>% 
    dplyr::mutate(ntreat = as.numeric(row.names(.))) %>% 
    {
      if(isTRUE(type)) { 
        dplyr::mutate(.data = ., sort = sample.int(n()))
      } else if (isFALSE(type)) { 
        dplyr::mutate(.data = ., sort = ntreat)
        }
    } %>% 
    dplyr::arrange(.data = ., sort) %>% 
    dplyr::mutate(rows = rep(1:nrows,  each = {{ncols}})[1:lengths(dfactors)] ) %>% 
    dplyr::mutate(cols = rep(1:ncols, times = {{nrows}})[1:lengths(dfactors)] ) %>% 
    dplyr::mutate(icols = (ncols - cols) + 1) %>% 
    { 
      if(isTRUE(zigzag))
        dplyr::mutate(.data = .
               , cols = case_when(
                 rows %% 2 == 0 ~ as.character(icols)
                 , rows %% 2 == 1 ~ as.character(cols)
               )) else {.}
    } %>% 
    dplyr::mutate(across(c(.data$cols, .data$rows), as.numeric)) %>% 
    dplyr::mutate(.data = ., plots = serie*.data$rows + .data$cols) %>% 
    dplyr::select(plots, ntreat, {{name.factors}}, sort, everything()) %>% 
    dplyr::mutate(fbname = fbname) %>% 
    tidyr::unite("barcode", fbname, plots, {{name.factors}}, rows, cols
                 , sep = "_", remove = F) %>% 
    dplyr::select(!c(icols, fbname)) 
  
  result <- list(
    fieldbook = fb
    , parameters = list(
      nfactors = nfactors
      , factors = dfactors
      , type = type
      , rep = rep 
      , zigzag = zigzag
      , dim = c(nrows, ncols)
      , seed = seed
      , factornames = name.factors
    )
  )
  
}

