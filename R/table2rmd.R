#' Table to Rmarkdown
#'
#' Use Articul8 Add-ons from Google docs to build Rticles
#'
#' @param text Markdown text with table information (string)
#'
#' @return string mutated
#' 
#' @export
#' 

table2rmd <- function(text) {
  
  # text <- ": Potato genotypes (Solanum tuberosum L.) used for water deficit experiment with two commercial varieties and 13 genotypes from an advanced breeding population developed by the International Potato Center (CIP). Source: CIP {#tbl:id.oxthgr87o9kv}"
  # text <- "Two commercial varieties and thirteen potato genotypes from the advanced breeding population at the International Potato Center (CIP) were used in this study ([Table  @tbl:id.b16ekvy5zs9n]:). The commercial varieties were UNICA (CIP392797.22) with a good yield in warm and dry environments [(Demirel et al., 2020; Gutiérrez-Rosales et al., 2007; Rolando et al., 2015)](https://www.zotero.org/google-docs/?LcYrXv); and Achirana INTA (CIP720088) known for its earliness and drought tolerance [(Schafleitner et al., 2007)](https://www.zotero.org/google-docs/?LkdMBJ). The plants were grown in a controlled greenhouse at 28/15°C average day/night temperature with 70±5% average relative humidity, monitored by a weather station ‘HOBO U12 Outdoor/Industrial model’ (Onset Computer Corporation, Bourne, MA, USA)."
  
  # text %>% gsub("\\: (.*) \\{\\#(.+)", "\\1", .)
  
  result <- if(FALSE) { # grepl("\\: (.*) \\{\\#(.+)", text)
    
    opt <- text %>% 
      tibble::enframe(name = "num") %>% 
      dplyr::mutate(id = gsub(".+?\\{\\#(.*)\\}", "\\1", .data$value) %>% gsub(":", "-", .)) %>% 
      dplyr::mutate(title = gsub("\\: (.*) \\{\\#(.+)", "\\1", .data$value)) %>% 
      dplyr::select(!c(.data$value)) %>% 
      tidyr::pivot_longer(!.data$num) %>% 
      dplyr::mutate(opt = dplyr::case_when(
        .data$name %in% "id" ~ paste0("#| label: ", .data$value)
        , .data$name %in% "title" ~ paste0("#| tbl-cap: '", .data$value, "'")
      )) %>% 
      dplyr::select(.data$opt) %>% 
      purrr::as_vector() %>% 
      paste0(collapse = "\n")
      
    chunk <- paste0(
      "```{r}\n"
      , opt
      , "\n\nknitr::kable(NA)"
      , "\n```"
    ) 
    
    # chunk %>% cat()
    
  } else if(isTRUE(grepl("tbl\\:", text))) {
    
    cite <- text %>% 
      gsub("tbl\\:", "tbl-", .)
    
  } else {
    
    text
    
  }
  
  return(result)
  
}

