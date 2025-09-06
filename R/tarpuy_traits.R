#' Field book traits
#'
#' Function to export field book and traits for be used in field book app.
#'
#' @param fieldbook Experiment field book `[dataframe]`.
#' @param last_factor Last factor in the field book `[character: colname]`
#' @param traits Traits information `[dataframe or list]`.
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
#'        , trait = "altp"
#'        , format = "numeric"
#'        , when = "30, 40, 50"
#'        , samples = 3
#'        , units = "cm"
#'        , details = NA
#'        , minimum = 0
#'        , maximum = 100
#'        )
#'   , list(variable = "severidad"
#'          , trait = "svr"
#'          , format = "scategorical"
#'          , when = "30, 40, 50"
#'          , samples = 1
#'          , units = "scale"
#'          , details = NA
#'          , categories = "1, 3, 5, 7, 9"
#'   )
#'   ,  list(variable = "foto"
#'           , trait = "foto"
#'           , format = "photo"
#'           , when = "hrv, pshrv"
#'           , samples = 1
#'           , units = "image"
#'           , details = NA
#'   )
#'   ,  list(variable = "germinacion"
#'           , trait = "ger"
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
#'        , "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1607116093#gid=1607116093")
#'        
#' fb <- gsheet2tbl(url_fb) 
#' 
#' url_ds <- paste0("https://docs.google.com/spreadsheets/d/"
#'        , "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1278145622#gid=1278145622")
#'        
#' ds <- gsheet2tbl(url_ds) 
#' 
#' fb <- ds %>% tarpuy_design()
#' 
#' url_trt <- paste0("https://docs.google.com/spreadsheets/d/"
#'        , "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1665653985#gid=1665653985")
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
  
  cols <- c("trait" = NA_real_, "format" = NA_real_, "default" = NA_real_
            , "minimum" = NA_real_,	"maximum" = NA_real_, "details" = NA_real_
            , "categories" = NA_real_)
  
# -------------------------------------------------------------------------

  traitstb <- traits %>%
    dplyr::bind_rows() %>% 
    tibble::rowid_to_column() %>% 
    tibble::rownames_to_column() %>% 
    dplyr::mutate(across(everything(), ~ as.character(.))) %>% 
    dplyr::rename_with(~ gsub("\\{|\\}", "", .)) %>% 
    tidyr::drop_na(c("trait")) %>% 
    tidyr::pivot_longer(!"rowname") %>% 
    dplyr::group_split(.data$rowname, .keep = FALSE)  %>% 
    purrr::map(~.x %>% deframe) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(across(.data$rowid, ~ as.numeric(.))) %>% 
    dplyr::arrange(.data$rowid) %>% 
    tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    dplyr::rename("defaultValue" = "default") %>% 
    dplyr::filter(!grepl("X", .data$trait)) %>% 
    dplyr::mutate(across("trait", ~ iconv(., to="ASCII//TRANSLIT"))) %>% 
    dplyr::mutate(across("trait", ~gsub("[[:space:]]", ".", .))) 
  
  traitsnames <- traitstb %>% 
    dplyr::select(any_of(c("trait", "when", "samples"))) %>% 
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
      , TRUE ~ as.character(.data$defaultValue)
    )) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(categories = case_when(
      .data$format %in% c("scategorical", "mcategorical") ~ .data$categories %>% 
        # gsub("[[:blank:]]", "", .) %>% 
        strsplit(",|;") %>% 
        unlist() %>% 
        purrr::map_chr(\(x) paste0('{"label":"', trimws(x) ,'","value":"', trimws(x),'"}')) %>% 
        paste0(collapse = ",") %>% 
        paste0("[", ., "]")
      , TRUE ~ as.character("[]")
    )) %>% 
    dplyr::mutate(format = case_when(
      .data$format %in% c("scategorical") ~ "categorical"
      , .data$format %in% c("mcategorical") ~ "multicat"
      , TRUE ~ .data$format
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
  
  # fbapp %>% readr::write_delim(file = "traitsx.trt", delim = ",", quote = "all", na = '""')
  
# -------------------------------------------------------------------------
  
  traitsnames <- fbapp %>% 
    dplyr::select(.data$trait) %>% 
    dplyr::mutate("blank" := NA) %>% 
    tidyr::pivot_wider(names_from = "trait", values_from = "blank")

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
