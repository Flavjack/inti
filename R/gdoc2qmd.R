#' Google docs to Rmarkdown
#'
#' Use Articul8 Add-ons from Google docs to build Rticles
#'
#' @param file Zip file path from Articul8 exported in md format [path]
#' @param export Path to export the files [path: NA (file directory)]
#' @param format Output format [string: "qmd" "rmd"]
#' @param type output file type [strig: "asis" "list", "listfull", "full"]
#'
#' @return path
#' 
#' @details 
#' 
#' If you add "|# END" will replace by "knitr::knit_exit()"
#' 
#' @export
#' 

gdoc2qmd <- function(file
                     , export = NA
                     , format = "qmd"
                     , type = "asis"
                     ){
  
  # file <- choose.files() ; format = "qmd"; export = NA
  # type <- "listfull"

  export <- if(is.na(export)) {
    file %>% gsub(".zip", "", .) %>% file.path()
  } else {export}
  
  zip <- file %>% 
    utils::unzip(overwrite = T, exdir = export)
  
# -------------------------------------------------------------------------
  
  txt <-  zip %>% 
    .[grep('.md', .)] %>%
    readLines() %>% 
    tibble::enframe() %>%
    dplyr::filter(!.data$value %in% "# ") %>% 
    dplyr::mutate(value = gsub("```Unknown element type at this position: UNSUPPORTED```"
                               , "\n\n", .data$value)) %>% 
    utils::head(which(startsWith(.$value, '#| END')), -1) |> 
    dplyr::rowwise()
  
  txtonly <- txt %>% 
    dplyr::filter(!grepl("\\|", .data$value)) %>% 
    dplyr::filter(!grepl("#tbl", .data$value)) %>% 
    dplyr::filter(!grepl("#fig", .data$value))
  
  tt <- txtonly %>% 
    dplyr::filter(if_all(everything(), ~ . != "")) %>% 
    utils::head(1) %>% 
    tibble::deframe() %>% 
    as.vector()
  
  fig <- txt %>% 
    dplyr::filter(grepl("#fig", .data$value)) %>% 
    tibble::as_tibble() %>% 
    dplyr::group_split(.data$value) %>% 
    rev() %>% 
    purrr::map_dfr(~ add_row(.x, .before = grepl("#fig", .x))) %>% 
    dplyr::mutate(across(.data$value, ~ ifelse(is.na(.), "\\newpage", .))) 
  
  figx <- fig %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(across(.data$value, ~gsub("\\{#fig:(.*)\\}", paste0("{#fig:", .data$name ,"}"), .))) 
  
  figlist <- fig %>% 
    dplyr::mutate(value = case_when(
      dplyr::row_number() == 1 ~ .data$value
      , grepl("#fig", .data$value) ~ .data$value
      , TRUE  ~ "\n\n"
    )) %>% 
    dplyr::mutate(across(.data$value, ~gsub("\\]\\((.*)\\)\\{", "](){", .))) 
  
  tab <- txt %>% 
    dplyr::filter(grepl("^\\|", .data$value) | grepl("#tbl", .data$value)) %>% 
    dplyr::mutate(group = case_when(
      grepl("^:", .data$value) ~ as.character(.data$name)
      , TRUE ~ NA
    )) %>% 
    tibble::as_tibble() %>% 
    tidyr::fill("group", .direction = "up") %>% 
    tidyr::drop_na(.data$group) %>% 
    dplyr::group_by(.data$group) %>% 
    dplyr::slice(n(), 1:(n() - 1)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_split(.data$group) %>%
    purrr::map_dfr(~ add_row(.x, .after = grepl("#tbl", .x))) %>% 
    dplyr::mutate(across(.data$value, ~ ifelse(is.na(.), "\\newpage", .))) %>% 
    dplyr::select(!.data$group) %>% 
    dplyr::mutate(across(.data$value, ~gsub("}", "}", .))) 
  
  tabx <- tab %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(across(.data$value, ~gsub("\\{#tbl:(.*)\\}", paste0("{#tbl:", .data$name ,"}"), .)))
  
  tablist <- tab %>% 
    dplyr::filter(!grepl("\\|", .data$value)) %>% 
    dplyr::mutate(value = case_when(
      dplyr::row_number() == 1 ~ .data$value
      , grepl("#tbl", .data$value) ~ .data$value
      , TRUE  ~ "\n\n"
    )) 

# -------------------------------------------------------------------------

  manuscript <- if(type == "full") {
    
    dplyr::bind_rows(txtonly, tablist, figlist, tabx, figx)
    
  } else if (type == "listfull") {
    
    dplyr::bind_rows(txtonly, tab, fig)
    
  } else if (type == "list") {
    
    dplyr::bind_rows(txtonly, tablist, figlist)
    
  } else if (type == "asis") { txt } 
   
    
  doc <- manuscript %>% 
    {
      if(format == "qmd") {
        
        dplyr::mutate(.data = ., value = inti::figure2qmd(text = .data$value, path = export)) %>% 
        dplyr::mutate(.data = ., value = inti::table2qmd(text = .data$value, type))
        
      } else if (format == "rmd") {
        
        dplyr::mutate(.data = ., value = inti::figure2rmd(text = .data$value, path = export)) %>% 
        dplyr::mutate(.data = ., value = inti::table2rmd(text = .data$value)) 
        
      }
    } %>% 
    tibble::add_row(value = "\\newpage", .before = which(grepl("# abstract", .$value, ignore.case = TRUE))) %>% 
    tibble::add_row(value = "\\newpage", .before = which(grepl("# reference", .$value, ignore.case = TRUE))) %>% 
    tibble::add_row(value = "\\newpage", .before = which(grepl("# result", .$value, ignore.case = TRUE)))  %>% 
    tibble::add_row(value = "\\newpage", .before = which(grepl("# discussion", .$value, ignore.case = TRUE))) %>% 
    tibble::add_row(value = paste(tt, "\n\n"), .before = which(grepl("# abstract", .$value, ignore.case = TRUE))) %>%
     dplyr::select(.data$value) %>% 
    tibble::add_row(value = "\n```{r}\nknitr::knit_exit() \n```", ) %>%
    tibble::deframe() %>% 
    writeLines(con = file.path(export, "_doc.Rmd") %>% gsub("\\\\", "\\/", .))

# -------------------------------------------------------------------------

  doc <- list.files(path = export, pattern = "_doc", full.names = T)
  
}

