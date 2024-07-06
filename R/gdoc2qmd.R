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
#' Document rendering until certain point: "#| end"
#' Include for next page: "#| newpage"
#' You can include the cover page params using "#|" in a Google docs table
#' 
#' @export
#' 

gdoc2qmd <- function(file
                     , export = NA
                     , format = "qmd"
                     , type = "asis"
                     ){
  
  # file <- choose.files() ; format = "qmd"; export = NA; type <- "listfull"

  export <- if(is.na(export)) {
    file %>% gsub(".zip", "", .) %>% file.path()
  } else {export}
  
  zip <- file %>% 
    utils::unzip(overwrite = T, exdir = export)
  
# -------------------------------------------------------------------------
  
  text <-  zip %>% 
    .[grep('.md', .)] %>%
    readLines() %>% 
    tibble::enframe() %>%
    dplyr::filter(!grepl("^#+ $", .data$value)) %>%  
    dplyr::filter(!grepl("^#$", .data$value)) %>%  
    dplyr::mutate(value = gsub("```Unknown element type at this position: UNSUPPORTED```"
                               , "\n\n", .data$value)) %>% 
    {
      if (any(grepl(pattern = '#\\| end', x = .$value))) {
        dplyr::slice(.data = ., 1:(which(x = .$value == '#| end')-1))
      } else { . }
    } %>% 
    dplyr::mutate(across(.data$value, ~ gsub("#\\| newpage", "\\\\newpage", .))) %>%
    dplyr::mutate(across(.data$value, ~gsub("!\\[null", "||||![null", .))) %>% 
    tidyr::separate_longer_delim(.data$value, delim = "||") %>% 
    dplyr::rowwise() 
  
  params <- text %>% 
    dplyr::filter(stringr::str_detect(.data$value, "\\| \\#\\|")) %>% 
    dplyr::mutate(across(everything(), ~gsub("\\| \\#\\|", "", .))) %>% 
    dplyr::mutate(across(.data$value, ~gsub("\\|$", "", .))) %>% 
    tidyr::separate(.data$value, c("name", "value"), sep = "\\|") %>% 
    dplyr::mutate(across(everything(), ~trimws(.))) %>% 
    tibble::deframe() %>% 
    as.list() 
  
  txt <- text %>% 
      {
        if(length(params) >0) {
          .[-(1:(max(which(grepl("\\| \\#\\|", .$value))))), ]
        } else {.}
      }

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
    split(1:nrow(.)) %>% 
    purrr::map_dfr(~ add_row(.x, .before = grepl("#fig", .x))) %>% 
    {
      if(length(.) != 0) {
        dplyr::mutate(.data = ., across(.data$value, ~ ifelse(is.na(.), "\\newpage", .)))
      }  else {.}
    }
    
  figx <- fig %>% 
    dplyr::rowwise() %>% 
    {
      if(length(.) != 0) {
        dplyr::mutate(.data = ., across(.data$value, ~gsub("\\{#fig:(.*)\\}", paste0("{#fig:", .data$name ,"}"), .)))
      }  else {.}
    }
    
  figlist <- fig %>% 
    {
      if(length(.) != 0) {
        
        dplyr::mutate(.data = ., value = case_when(
          dplyr::row_number() == 1 ~ .data$value
          , grepl("#fig", .data$value) ~ .data$value
          , TRUE  ~ "\n\n"
        )) %>% 
          dplyr::mutate(.data = ., across(.data$value, ~gsub("\\]\\((.*)\\)\\{", "](){", .))) 
        
      }  else {.}
    }
  
  tab <- txt %>% 
    dplyr::filter(grepl("^\\|", .data$value) | grepl("#tbl", .data$value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(group = case_when(
      grepl("#tbl", .data$value) ~ .data$name
    )) %>% 
    tidyr::fill(., .data$group, .direction = "up") %>% 
    split(.$group) %>% 
    purrr::map_dfr(~ slice(.data = ., c(n(),  1:(n()-1)))) %>% 
    split(.$group) %>% 
    purrr::map_dfr(~ bind_rows(tibble(name = NA, value = NA), .x)) %>% 
    dplyr::mutate(.data = ., across(.data$value, ~ ifelse(is.na(.), "\\newpage", .)))

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
    
    dplyr::bind_rows(txtonly, figlist, tablist, figx, tabx)
    
  } else if (type == "listfull") {
    
    dplyr::bind_rows(txtonly, fig, tab)
    
  } else if (type == "list") {
    
    dplyr::bind_rows(txtonly, figlist, tablist)
    
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
    tibble::add_row(value = "\\newpage", .before = which(grepl("statements and declarations$", .$value, ignore.case = TRUE))) %>% 
    tibble::add_row(value = "\\newpage", .before = which(grepl("# abstract$", .$value, ignore.case = TRUE))) %>% 
    tibble::add_row(value = "\\newpage", .before = which(grepl("# introduction$", .$value, ignore.case = TRUE))) %>% 
    tibble::add_row(value = "\\newpage", .before = which(grepl("# materials and methods$", .$value, ignore.case = TRUE))) %>%
    tibble::add_row(value = "\\newpage", .before = which(grepl("# results$", .$value, ignore.case = TRUE)))  %>% 
    tibble::add_row(value = "\\newpage", .before = which(grepl("# discussion$", .$value, ignore.case = TRUE))) %>% 
    tibble::add_row(value = "\\newpage", .before = which(grepl("# references$", .$value, ignore.case = TRUE))) %>% 
    {
      if (any(grepl(pattern = '# abstract', x = .$value, ignore.case = TRUE))) {
        tibble::add_row(.data = ., value = paste(tt, "\n\n"),
                        .before = which(x = grepl(pattern = "# abstract", x = .$value
                                                  , ignore.case = TRUE)))
      } else {.}
    } %>%
    dplyr::select(.data$value) %>% 
    # tibble::add_row(value = "\n```{r}\nknitr::knit_exit() \n```", ) %>%
    tibble::deframe() %>% 
    writeLines(con = file.path(export, "_doc.Rmd") %>% gsub("\\\\", "\\/", .))


  # -------------------------------------------------------------------------

  doc <- list.files(path = export, pattern = "_doc", full.names = T)
  
  
  return(list(path = doc
              , params = params))
  
}

