#' Figure to Quarto format
#'
#' Use Articul8 Add-ons from Google docs to build Rticles
#'
#' @param text Markdown text with figure information [string]
#' @param path Image path for figures [path: "." (base directory)]
#' @param opts chunk options in brackets [string: NA]
#' 
#' @details
#' 
#' Quarto option can be included in the title using "\{\{\}\}" separated by commas
#'
#' @return string mutated
#' 
#' @export
#' 

figure2qmd <- function(text
                       , path = "."
                       , opts = NA
) {
  
  # path <- tempdir()
  # text <- "The experiment was carried out in a complete randomized block design with two irrigation treatments with five replications of each genotype per treatment. In well-watered (WW) treatment, plants were irrigated according to their transpiration demand ([Figure  @fig:id.z21kyltlev9z]:A) and in water deficit (WD) treatment, the water supply was gradually reduced until the wilting point [(Ray & Sinclair, 1998)](https://www.zotero.org/google-docs/?y2XuVg). At 35 dap, before the stress initiation, the pots were watered to soaking and then allowed to drain overnight [(Bhatnagar-Mathur et al., 2007)](https://www.zotero.org/google-docs/?aBxXpi). The next morning, the pots were sealed in a plastic bag secured with a twist tie to prevent water loss except by transpiration and arranged in the greenhouse according to the experimental design. Thereafter, all the pots were weighed and this weight was defined as the initial pot weight. The inter-daily weight of the pots was measured for ten days to calculate the initial dry down parameters for treatment application ([Figure  @fig:id.z21kyltlev9z]:B). The WD treatment started at 45 dap which coincides with the beginning of tuber initiation."
  # text <- "![(A) Fraction of transpirable soil water (FTSW). (B) Daily transpiration in 15 potato genotypes under well-watered (WW) and water deficit (WD) conditions.](img_0.png){#fig-id.olts8je85fk5}"
  # text <- "![Choose the images. {{fig-width: “50%”, echo: true}}](img_1.png){#fig:id.bzfoh3m13vt1}"
  # text <- "![null|null](img_3.jpg)"
  
  # text %>% gsub(".+\\{\\{(.+)\\}\\}(.+)", "\\1", .)
  # text <- "![null|null](img_5.jpg)  \t\n\r\v\f"
  
  # result %>% cat()
  # 'manuscript/img_0.jpg​' %>% gsub('\u200B', '', .)
  
  result <- if(isTRUE(grepl("^\\!\\[", text))) { 
    
    opt <- text %>% 
      tibble::enframe(name = "num") %>% 
      dplyr::mutate(img = gsub(".+](.*)", "\\1", .data$value)) %>% 
      dplyr::mutate(img = gsub("[[:space:]]", "", .data$img)) %>% 
      dplyr::mutate(img = gsub("\u200B", "", .data$img)) %>% 
      dplyr::mutate(img = gsub("\\(|\\)", "", .data$img)) %>% 
      dplyr::select(!c(.data$value)) %>% 
      tidyr::pivot_longer(!.data$num) %>% 
    
      dplyr::mutate(opt = dplyr::case_when(
        .data$name %in% "id" ~ paste0("#| label: ", .data$value)
        , .data$name %in% "title" ~ paste0("#| fig-cap: '", .data$value, "'")
        , .data$name %in% "img" ~ paste0("\nknitr::include_graphics('", file.path(path, .data$value) %>% gsub("\\\\", "\\/", .),"')")
        , .data$name %in% "opts" ~ .data$value %>% gsub(",", "\n#|", .) %>% paste("#|", .)
      )) %>% 
      tidyr::drop_na(.data$opt) %>% 
      dplyr::select(.data$opt) %>% 
      dplyr::mutate(opt = gsub("[[:space:]]", "", .data$opt)) %>% 
      purrr::as_vector() %>% 
      paste0(collapse = "\n")
      
    chunk <- paste0(
      "```{r}\n"
      , opt
      , "\n```"
    ) 
    
    # chunk %>% cat()
    
  } else if(isTRUE(grepl("fig\\:", text))) {
    
    cite <- text %>%
      gsub("fig\\:", "fig-", .) %>% 
      gsub("]:", "]", .) %>% 
      gsub("Figure  \\@", "\\@", .)
    
  } else {
    
    text
    
  }
  
  return(result)
  
}

