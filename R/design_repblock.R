#' Experimental design in CRD and RCBD
#'
#' Function to deploy field-book experiment for CRD and RCBD
#'
#' @param nfactors Number of factor in the experiment [numeric: 1].
#' @param factors Lists with names and factor vector [list].
#' @param type Type of experimental arrange [string: "crd" "rcbd" "lsd"]
#' @param rep  Number of replications in the experiment [numeric: 3].
#' @param zigzag Experiment layout in zigzag [logic: F].
#' @param serie Number to start the plot id [numeric: 100].
#' @param nrows Experimental design dimension by rows [numeric: value]
#' @param seed Replicability from randomization [numeric: NULL].
#' @param fbname Bar code prefix for data collection [string: "inkaverse"].
#' @param qrcode [string: "\{fbname\}\{plots\}\{factors\}"] String to concatenate the qr code.
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
#' factores <- list("geno" = c("A", "B", "C", "D", "D", 1, NA, NA, NULL, "NA")
#'                  , "salt stress" = c(0, 50, 200, 200, "T0", NA, NULL, "NULL")
#'                  , time = c(30, 60, 90)
#'                  )
#' 
#' fb <-design_repblock(nfactors = 2
#'                      , factors = factores
#'                      , type = "rcbd"
#'                      , rep = 5
#'                      , zigzag = T
#'                      , seed = 0
#'                      , nrows = 20
#'                      , qrcode = "{fbname}{plots}{factors}"
#'                      )
#'                      
#' dsg <- fb$fieldbook
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
                            , nrows = NA
                            , serie = 100
                            , seed = NULL
                            , fbname = "inkaverse"
                            , qrcode = "{fbname}{plots}{factors}"
                            ) {
  
  # nfactors = 2; factors = factores; type = "crd"; rep = 3
  # zigzag = FALSE; nrows = NA; serie = 100; seed = NULL
  # fbname = "inkaverse"; qrcode = "{fbname}{plot}{treat}"
  
  set.seed(seed)
  
  dfactors <- factors %>%
    purrr::map(~ gsub("NA|NULL", NA, .)) %>% 
    purrr::map(base::unique) %>% 
    purrr::map(stats::na.omit) %>% 
    purrr::map(~gsub("[[:space:]]", ".", .)) %>% 
    purrr::set_names(gsub("[[:space:]]", "_" , names(.))) %>% 
    .[1:nfactors]
  
  nrowsfb <- dfactors %>% lengths() %>% prod()*rep

  block.factor <- if(type %in% "rcbd") {"block"} else {"rep"}
  
  name.factors <- names(dfactors)
  
  nrows <- if(anyNA(nrows)) {rep} else {nrows}
  
  ncols <- nrowsfb/nrows; ncols <- ceiling(ncols)
  
  # qr-code name
  
  qrcolumns <- qrcode %>% 
    gsub("factors", paste0(name.factors, collapse = "\\}\\{"), .) %>% 
    strsplit(., split = "\\}\\{") %>% 
    unlist() %>% 
    gsub("\\{|\\}", "", .) %>% 
    trimws()
  
  # design
  
  if(type == "lsd") {
    
    rep <- dfactors[[1]] %>% length()
    
    nrows <- rep
    
    ncols <- rep
  }
  
  fb <- dfactors %>% 
    expand.grid() %>% 
    dplyr::mutate(ntreat = as.numeric(row.names(.))) %>% 
    tidyr::uncount(rep, .id = {{block.factor}}) %>% 
    dplyr::arrange(.data[[block.factor]], .data$ntreat) %>% 
    {
      if(type %in% "rcbd") {
        dplyr::group_by(.data = ., .data[[block.factor]]) %>% 
          dplyr::mutate(.data = ., sort = sample.int(n())) %>% 
          dplyr::ungroup() %>%
          dplyr::arrange(.data = ., .data[[block.factor]], .data$sort) %>% 
          dplyr::mutate(.data = ., plots = serie*.data[[block.factor]] + .data$sort) %>% 
          dplyr::mutate(rows = rep(1:nrows,  each = {{ncols}})[1:nrowsfb] ) %>% 
          dplyr::mutate(cols = rep(1:ncols, times = {{nrows}})[1:nrowsfb] ) %>%
          dplyr::mutate(icols = (ncols - .data$cols) + 1)
      } else if (type %in% "crd") {
        dplyr::mutate(.data = ., sort = sample.int(n())) %>%
          dplyr::arrange(.data = ., .data$sort) %>% 
          dplyr::mutate(plots = serie + .data$sort) %>% 
          dplyr::mutate(rows = rep(1:nrows,  each = {{ncols}})[1:nrowsfb] ) %>% 
          dplyr::mutate(cols = rep(1:ncols, times = {{nrows}})[1:nrowsfb] ) %>%
          dplyr::mutate(icols = (ncols - .data$cols) + 1)
      } else if (type %in% "lsd") {
          dplyr::mutate(.data = ., plots = serie*.data[[block.factor]] + .data$ntreat) %>% 
          dplyr::mutate(rows = rep(1:nrows,  each = {{ncols}})[1:lengths(dfactors)]) %>% 
          dplyr::mutate(cols = rep(1:ncols, times = nrow(.)/ncols )) %>%
          dplyr::mutate(icols = rep(seq(rep), rep) + rep(seq(rep),each=rep) - 1) 
      }
    } %>% 
    { 
      if(isTRUE(zigzag))
        dplyr::mutate(.data = .
               , cols = case_when(
                 rows %% 2 == 0 ~ as.character(.data$icols)
                 , rows %% 2 == 1 ~ as.character(.data$cols)
               )) else {.}
    } %>% 
    dplyr::select(.data$plots, .data$ntreat, {{name.factors}}, .data$sort, everything()) %>% 
    dplyr::mutate(across(.data$cols, as.numeric)) %>% 
    dplyr::mutate(fbname = fbname) %>% 
    tidyr::unite("qrcode", any_of({{qrcolumns}}), sep = "_", remove = F) %>% 
    dplyr::select(.data$qrcode, dplyr::everything()) %>% 
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

