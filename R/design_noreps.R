#' Experimental design without replications
#'
#' Function to deploy field-book experiment without replications
#'
#' @param factors Lists with names and factor vector `[list]`.
#' @param type Randomization in the list `[character: "sorted", "unsorted"]`
#' @param zigzag Experiment layout in zigzag `[logic: FALSE]`.
#' @param serie Number to start the plot id `[numeric: 1000]`.
#' @param nrows Experimental design dimension by rows `[numeric: value]`
#' @param seed Replicability from randomization `[numeric: NULL]`.
#' @param fbname Bar code prefix for data collection `[character: "inkaverse"]`.
#' @param qrcode Concatenate the QR code `[character: "{fbname}{plots}{factors}"]`
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
#' factores <- list("geno" = c(1:99))
#' 
#' fb <- design_noreps(factors = factores
#'                      , type = "sorted"
#'                      , zigzag = F
#'                      , nrows = 10
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

design_noreps <- function(factors
                          , type = "sorted"
                          , zigzag = FALSE
                          , nrows = NA
                          , serie = 100
                          , seed = NULL
                          , fbname = "inkaverse"
                          , qrcode = "{fbname}{plots}{factors}"
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
  
  nrows <- if(is.na(nrows) | nrows == 1) {x <- 10} else {nrows}
  
  ncols <- dfactors %>% 
    unlist() %>% 
    length()/nrows; ncols <- ceiling(ncols)
  
  # qr-code name
  
  qrcolumns <- qrcode %>% 
    gsub("factors", paste0(name.factors, collapse = "\\}\\{"), .) %>% 
    strsplit(., split = "\\}\\{") %>% 
    unlist() %>% 
    gsub("\\{|\\}", "", .) %>% 
    trimws()
  
  # design
  
  fb <- dfactors %>% 
    expand.grid() %>% 
    dplyr::mutate(ntreat = as.numeric(row.names(.))) %>% 
    {
      if(type == "unsorted") { 
        dplyr::mutate(.data = ., sort = sample.int(n()))
      } else if (type == "sorted") { 
        dplyr::mutate(.data = ., sort = .data$ntreat)
        }
    } %>% 
    dplyr::arrange(.data = ., sort) %>% 
    dplyr::mutate(rows = rep(1:nrows,  each = {{ncols}})[1:lengths(dfactors)] ) %>% 
    dplyr::mutate(cols = rep(1:ncols, times = {{nrows}})[1:lengths(dfactors)] ) %>% 
    dplyr::mutate(icols = (ncols - .data$cols) + 1) %>% 
    dplyr::mutate(plots = serie + .data$sort) %>% 
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

