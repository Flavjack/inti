#' Field book traits
#'
#' Function to export field book and traits for be used in field book app.
#'
#' @param fieldbook Experiment field book [dataframe].
#' @param last_factor Last factor in the field book [string: colnames]
#' @param traits Traits information [dataframe or list].
#'
#' @details For the traits parameters you can used shown in the Field Book app
#'
#' @return list
#' 
#' @export
#' 
#' @examples
#' 
#' library(inti)
#' 
#' fieldbook <- inti::potato
#' 
#' traits <- list(
#'   list(variable = "altura de planta"
#'        , abbreviation = "altp"
#'        , format = "numeric"
#'        , when = "30, 40, 50"
#'        , samples = 3
#'        , units = "cm"
#'        , details = NA
#'        , minimum = 0
#'        , maximum = 100
#'        )
#'   , list(variable = "severidad"
#'          , abbreviation = "svr"
#'          , format = "categorical"
#'          , when = "30, 40, 50"
#'          , samples = 1
#'          , units = "scale"
#'          , details = NA
#'          , categories = "1, 3, 5, 7, 9"
#'   )
#'   ,  list(variable = "foto"
#'           , abbreviation = "foto"
#'           , format = "photo"
#'           , when = "hrv, pshrv"
#'           , samples = 1
#'           , units = "image"
#'           , details = NA
#'   )
#'   ,  list(variable = "germinacion"
#'           , abbreviation = "ger"
#'           , format = "boolean"
#'           , when = "30, 40, 50"
#'           , samples = 1
#'           , units = "logical"
#'           , details = NA
#'   )
#' ) 
#' 
#' fbapp <- tarpuy_traits(fieldbook, last_factor = "bloque", traits)
#' 
#' \dontrun{ 
#' 
#' library(inti)
#' library(gsheet)
#' 
#' url_fb <- paste0("https://docs.google.com/spreadsheets/d/"
#'        , "1kIoI__uHQpZ8qXMPFoZpimBhywU8J0Rw49KgjJcovMY/edit#gid=2128359606")
#'        
#' fb <- gsheet2tbl(url_fb) 
#' 
#' url_ds <- paste0("https://docs.google.com/spreadsheets/d/"
#'        , "1kIoI__uHQpZ8qXMPFoZpimBhywU8J0Rw49KgjJcovMY/edit#gid=1559599083")
#'        
#' ds <- gsheet2tbl(url_ds) 
#' 
#' fb <- ds %>% tarpuy_design()
#' 
#' url_trt <- paste0("https://docs.google.com/spreadsheets/d/"
#'        , "1kIoI__uHQpZ8qXMPFoZpimBhywU8J0Rw49KgjJcovMY/edit#gid=1056776892")
#'        
#' traits <- gsheet2tbl(url_trt) 
#' 
#' fbapp <- tarpuy_traits(fb, last_factor = "cols", traits)
#' 
#' dsg <- fbapp[[1]]
#' 
#' }
#' 

tarpuy_traits <- function(fieldbook = NULL
                         , last_factor = NULL
                         , traits = NULL
                         ) {
  
# fieldbook <- fb; last_factor <- "cols"; traits <- traits

# conditions --------------------------------------------------------------
  
  if(is.null(fieldbook)) stop("Select your field book")
  
  if(is.null(traits)) return(list(fieldbook = fb, traits = NA, fb = fb))

# -------------------------------------------------------------------------

  fb <- fieldbook %>% 
    {
      if(!is.null(last_factor)) {
        dplyr::select(.data = ., 1:{{last_factor}})
      } else .
    }
    
# -------------------------------------------------------------------------
  
  cols <- c("trait" = NA_real_,	"format" = NA_real_,	"default" = NA_real_
            ,	"minimum" = NA_real_,	"maximum" = NA_real_,	"details" = NA_real_
            , "categories" = NA_real_)
  
# -------------------------------------------------------------------------

  traitstb <- {
    if(tibble::is_tibble(traits)) {
      traits %>% 
        dplyr::mutate(across(everything(), as.character)) %>% 
        dplyr::rename_with(~ gsub("\\{|\\}", "", .)) %>% 
        tidyr::drop_na(c("abbreviation")) %>% 
        tibble::rownames_to_column() %>% 
        tidyr::pivot_longer(!"rowname") %>% 
        dplyr::group_split(.data$rowname, .keep = FALSE)  %>% 
        purrr::map(~.x %>% deframe) 
      } else if (purrr::is_list(traits)) { traits } 
    } %>% 
    dplyr::bind_rows() %>% 
    tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    dplyr::filter(!grepl("X", .data$abbreviation))
  
  traitsnames <- traitstb %>% 
    dplyr::select(any_of(c("abbreviation", "when", "samples"))) %>% 
    purrr::discard(~all(is.na(.))) %>% 
    names()
  
# -------------------------------------------------------------------------

  fbapp <- traitstb %>% 
    {
      if( "when" %in% traitsnames ) tidyr::separate_rows(data = ., .data$when) else .
    } %>% 
    {
      if( "samples" %in% traitsnames ) {
        dplyr::mutate(.data = ., across(.data$samples, as.numeric)) %>% 
        dplyr::mutate(.data = ., across(.data$samples, ~replace_na(., 1))) %>% 
        tidyr::uncount(.data$samples, .id = "samples") 
      } else .
    } %>% 
    dplyr::rowwise() %>%
    dplyr::mutate("trait" := paste(across(all_of(traitsnames))
                                   , collapse = "_")) %>% 
    dplyr::group_by(across(traitsnames)) %>% 
    {
      if( "when" %in% traitsnames ) dplyr::arrange(.data = ., as.numeric(.data$when)) else .
    } %>% 
    dplyr::ungroup() %>%  
    tibble::rownames_to_column("realPosition") %>% 
    dplyr::mutate(isVisible = "true") %>% 
    dplyr::mutate(defaultValue = dplyr::case_when(
      .data$format %in% "boolean" ~ "false"
      , TRUE ~ .data$defaultValue
    )) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(categories = case_when(
      .data$format %in% "categorical" ~ .data$categories %>% 
        gsub("[[:blank:]]", "", .) %>% 
        strsplit(",|;") %>% 
        unlist() %>% 
        purrr::map_chr(\(x) paste0('{"label":"', x, '","value":"', x, '"}')) %>% 
        paste0(collapse = ", ") %>% 
        paste0("[", ., "]")
      , TRUE ~ as.character("[]")
    )) %>% 
    dplyr::select(all_of(c("trait"
                  , "format"
                  , "defaultValue"
                  , "minimum"
                  , "maximum"
                  , "details"
                  , "categories"
                  , "isVisible"
                  , "realPosition"
                  )))
  
  # fbapp %>% write_delim(file = "traitsx.trt", delim = ",", quote = "all", na = '""')
  
# -------------------------------------------------------------------------
  
  traitsnames <- fbapp %>% 
    dplyr::select(.data$trait) %>% 
    dplyr::mutate("blank" := NA) %>% 
    tidyr::pivot_wider(names_from = .data$trait, values_from = .data$blank)

  fbtraits <- fb %>% 
    merge(.
          , traitsnames
          , by = c("row.names")
          , all.x = T
    ) %>%
    dplyr::select(!.data$Row.names) %>% 
    {
      if ("plots" %in% names(fb)) dplyr::arrange(.data = ., .data$plots)  else .
    }
  
# -------------------------------------------------------------------------


  return(list(fieldbook = fbtraits
              , traits = fbapp
              , fb = fb
              ))
  
}
