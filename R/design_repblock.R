#' Experimental design in CRD and RCBD
#'
#' Function to deploy field-book experiment for CRD and RCBD
#'
#' @param nfactors Number of factor in the experiment [numeric: 1].
#' @param factors Lists with names and factor vector [list].
#' @param type Type of experimental arrange [string: "crd" "rcbd"]
#' @param rep  Number of replications in the experiment [numeric: 3].
#' @param zigzag Experiment layout in zigzag [logic: F].
#' @param serie Digits in the plots number [numeric: 2].
#' @param dim Experimental design dimension in row and columns [numeric vector]
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
#' factores <- list("geno" = c("a1", "b2", "c3", "d4", "d4")
#'                  , "salt stress" = c(0, 50, 200, 200)
#'                  , time = c(30, 60, 90)
#'                  )
#' 
#' fb <-design_repblock(nfactors = 2
#'                      , factors = factores
#'                      , type = "crd"
#'                      , rep = 4
#'                      , zigzag = T
#'                      )
#' 
#' fb %>%   
#'   tarpuy_plotdesign(fill = "plots")
#' 
#' fb$parameters
#' 
#' }

design_repblock <- function(nfactors = 1
                            , factors
                            , type = "crd"
                            , rep = 3
                            , zigzag = FALSE
                            , dim = NA
                            , serie = 2
                            , seed = 0
                            , fbname = "inkaverse"
                            ) {
  
  dfactors <- factors %>% 
    purrr::map(base::unique) %>% 
    purrr::map(stats::na.omit) %>% 
    purrr::map(~gsub("[[:space:]]", ".", .)) %>% 
    purrr::set_names(gsub("[[:space:]]", "." , names(.))) %>% 
    .[1:nfactors]
  
  block.factor <- if(type %in% "rcbd") {"block"} else {"rep"}
  
  name.factors <- names(dfactors)
  
  nrows <- if(anyNA(dim)) {rep} else {dim[1]}
  
  ncols <- if(anyNA(dim)) {
    
    dfactors %>% 
      lengths() %>% 
      prod()*rep/nrows
    
  } else {dim[2]}
  
  fb <- dfactors %>% 
    expand.grid() %>% 
    dplyr::mutate(ntreat = as.numeric(row.names(.))) %>% 
    tidyr::uncount(rep, .id = {{block.factor}}) %>% 
    dplyr::arrange(.data[[block.factor]], .data$ntreat) %>% 
    {
      if(type %in% "rcbd") {
        dplyr::group_by(.data = ., .data[[block.factor]]) %>% 
          dplyr::mutate(.data = ., order = sample.int(n())) %>% 
          dplyr::ungroup({{block.factor}}) %>%
          dplyr::arrange(.data = ., .data[[block.factor]], .data$order) %>% 
          dplyr::mutate(.data = ., plots = 100*.data[[block.factor]] + .data$order)
      } else if (type %in% "crd") {
        dplyr::mutate(.data = ., order = sample.int(n())) %>%
          dplyr::arrange(.data = ., .data$order) %>% 
          dplyr::mutate(plots = 100 + .data$order)
      }
    } %>% 
    dplyr::mutate(rows = rep(1:nrows,  each = nrow(.)/nrows )) %>% 
    dplyr::mutate(cols = rep(1:ncols, times = nrow(.)/ncols )) %>%
    dplyr::mutate(icols = (ncols - .data$cols) + 1) %>% 
    { 
      if(isTRUE(zigzag))
        dplyr::mutate(.data = .
               , cols = case_when(
                 rows %% 2 == 0 ~ as.character(.data$icols)
                 , rows %% 2 == 1 ~ as.character(.data$cols)
               )) else {.}
    } %>% 
    dplyr::select(.data$plots, {{name.factors}}, everything()) %>% 
    dplyr::mutate(across(.data$cols, as.numeric)) %>% 
    dplyr::mutate(fbname = fbname) %>% 
    tidyr::unite("barcode", .data$fbname, .data$plots, {{name.factors}}, .data$cols, .data$rows
                 , sep = "-", remove = F) %>% 
    dplyr::select(!c(.data$icols, .data$fbname)) 
  
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

