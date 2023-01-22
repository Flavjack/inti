#' Field book app connection
#'
#' Function to export field book and traits for be used in field book app.
#'
#' @param fieldbook Experiment field book [dataframe].
#' @param last_factor Last factor in the field book [string: colnames]
#' @param traits Traits information [dataframe or list].
#'
#' @details For the traits parameters you can used shown in the field Book app
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
#' fbapp <- tarpuy_fbapp(fieldbook, last_factor = "bloque", traits)
#' 
#' \dontrun{ 
#' 
#' library(inti)
#' library(gsheet)
#' 
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'        , "13DVdFht6sp4X5MSe7-4S8Mf-qCJFaEOOU0KbLzc0HjE/edit#gid=550178001")
#'        
#' traits <- gsheet2tbl(url) 
#' 
#' }
#' 

tarpuy_fbapp <- function(fieldbook = NULL
                         , last_factor = NULL
                         , traits = NULL
                         ) {
  

# conditions --------------------------------------------------------------
  
  if(is.null(fieldbook)) stop("Select your field book")
  if(is.null(last_factor)) stop("Include the last factor in your field book")
  if(is.null(traits)) stop("Include the traits information")

# -------------------------------------------------------------------------

  fb <- fieldbook %>% 
    select(1: {{last_factor}})
  
# -------------------------------------------------------------------------

  traits <- if(is_tibble(traits)) {
    
    traits %>% 
      dplyr::mutate(across(everything(), as.character)) %>% 
      dplyr::rename_with(~ gsub("\\{|\\}", "", .)) %>% 
      tidyr::drop_na(c(.data$abbreviation, .data$when, .data$format)) %>% 
      dplyr::mutate(across(.data$samples, ~ replace_na(.x, "1"))) %>% 
      tibble::rownames_to_column() %>% 
      tidyr::pivot_longer(!.data$rowname) %>% 
      dplyr::group_split(.data$rowname, .keep = FALSE)  %>% 
      purrr::map(~.x %>% deframe) 
    
  } else if (purrr::is_list(traits)) { traits }

  cols <- c("trait" = NA_real_,	"format" = NA_real_,	"default" = NA_real_
            ,	"minimum" = NA_real_,	"maximum" = NA_real_,	"details" = NA_real_
            , "categories" = NA_real_)
  
  table <- traits %>% 
    dplyr::bind_rows() %>% 
    tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    tidyr::separate_rows(when) %>% 
    tibble::rownames_to_column() %>% 
    dplyr::mutate(across(samples, as.numeric)) %>% 
    tidyr::uncount(samples, .id = "samples") %>% 
    dplyr::rowwise() %>%
    dplyr::mutate("trait" := paste(c(abbreviation, when, samples)
                                   , collapse = "_")) %>% 
    dplyr::group_by(abbreviation, when, samples) %>% 
    dplyr::arrange(when) %>% 
    dplyr::ungroup() %>% 
    tibble::rownames_to_column("realPosition") %>% 
    dplyr::mutate(isVisible = "true") %>% 
    dplyr::mutate(defaultValue = dplyr::case_when(
      format %in% "boolean" ~ "false"
      , TRUE ~ ""
    )) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(categories = case_when(
      format %in% "categorical" ~ .data$categories %>% 
        gsub("[[:blank:]]", "", .) %>% 
        strsplit(",|;") %>% 
        unlist() %>% 
        purrr::map_chr(\(x) paste0('{"label":"', x, '","value":"', x, '"}')) %>% 
        paste0(collapse = ", ") %>% 
        paste0("[", ., "]")
      , TRUE ~ as.character("[]")
    )) %>% 
    dplyr::select(trait
                  , format
                  , defaultValue
                  , minimum
                  , maximum
                  , details
                  , categories
                  , isVisible
                  , realPosition
                  ) 
  
  # table %>% write_delim(file = "traitsx.trt", delim = ",", quote = "all", na = '""')

  return(list(fieldbook = fb, traits = table))
  
}
