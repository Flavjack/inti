#' Convert a Markdown manuscript into a Quarto document
#'
#' Reads a Markdown file exported from Google Docs and converts it into a
#' Quarto-compatible manuscript. The function detects and separates text,
#' figures, and tables, removes formatting artifacts, restores equations
#' embedded as image placeholders, and inserts format-specific section breaks.
#' The resulting document can be rendered directly using Quarto for HTML,
#' Word, or PDF outputs.
#'
#' @param file Character string indicating the path to the Markdown file.
#' Default is "draft.md".
#' @param export Character string specifying the output directory where the
#' generated .qmd file will be saved. If NULL, the directory name is
#' derived from the input file name.
#' @param type Character string indicating how the manuscript should be
#' organized. "asis" preserves the original structure, whereas "list"
#' rearranges the content into text, figures, and tables.
#'
#' @return A character vector containing the full path of the generated
#' Quarto (.qmd) file.
#'
#' @export

rticle <- function(file = "draft.md",
                   export = "files",
                   type = c("asis", "list")) {
  
  # library(tidyverse)
  # file = "draft.md" ; export = NULL ; type = "list"
  
  type <- match.arg(type)
  
  if (is.null(export)) {
    export <- sub("\\.md$", "", file)
  }
  
  dir.create(export, recursive = T, showWarnings = F)
  

# page break --------------------------------------------------------------

  fmt <- tryCatch(
    knitr::pandoc_to(),
    error = function(e)
      NULL
  )
  
  section_break <- if (identical(fmt, "html")) {
    
    c(
      "",
      "<div style='margin-top: 3em;'></div>",
      ""
    )
    
  } else {
    
    c(
      "",
      "```{=openxml}",
      "<w:p>",
      "  <w:pPr>",
      "    <w:sectPr/>",
      "  </w:pPr>",
      "</w:p>",
      "```",
      ""
    )
    
  }
  
  header_clean <- c(
    "abstract",
    "resumen",
    "keywords",
    "declaration[s]?",
    "statement[s]?",
    "statment[s]?",
    "declaration[s]?\\s+statement[s]?",
    "statement[s]?\\s+and\\s+declaration[s]?",
    "declaration[s]?\\s+and\\s+statement[s]?"
  ) %>%
    paste(collapse = "|") %>%
    paste0("^#*\\s*\\*{0,2}(", . , ")\\*{0,2}\\s*:?[[:space:]]*$")
  
  header_break <- c(
    "abstract",
    "introduction",
    "declaration[s]?",
    "statement[s]?",
    "statment[s]?",
    "declaration[s]?\\s+statement[s]?",
    "statement[s]?\\s+and\\s+declaration[s]?",
    "declaration[s]?\\s+and\\s+statement[s]?"
  ) %>%
    paste(., collapse = "|") %>%
    paste0("^#*\\s*\\*{0,2}(", . , ")\\*{0,2}\\s*:?[[:space:]]*$")
  
  # cross references --------------------------------------------------------
  
  crossrefs <- function(x) {
    
    # ---------------------------
    # 1. Títulos de tablas
    # ---------------------------
    x <- gsub(
      "^\\[Table\\]\\(#tab_([^\\)]+)\\):\\s*(.+)$",
      ": \\2 {#tbl-\\1}",
      x,
      perl = TRUE
    )
    
    # ---------------------------
    # 2. Títulos de figuras
    # ---------------------------
    x <- gsub(
      "^\\[Figure\\]\\(#fig_([^\\)]+)\\):\\s*(.+)$",
      "![\\2]() {#fig-\\1}",
      x,
      perl = TRUE
    )
    
    # ---------------------------
    # 3. Referencias a tablas
    # ---------------------------
    x <- gsub(
      "\\[Table\\]\\(#tab_([^\\)]+)\\)",
      "@tbl-\\1",
      x,
      perl = TRUE
    )
    
    # ---------------------------
    # 4. Referencias a figuras
    # ---------------------------
    x <- gsub(
      "\\[Figure\\]\\(#fig_([^\\)]+)\\)",
      "@fig-\\1",
      x,
      perl = TRUE
    )
    
    x
  }
  
# Fix figure --------------------------------------------------------------
  
  fix_figures <- function(gdoc){
    
    image_pattern <- "^\\s*!\\[\\]\\[(image[0-9]+)\\]\\s*$"
    
    caption_pattern <- "^!\\[(.*)\\]\\(\\)\\s*\\{#(fig-[^}]+)\\}$"
    
    img_rows <- which(
      grepl(
        image_pattern,
        gdoc$value,
        perl = TRUE
      )
    )
    
    for(i in rev(img_rows)){
      
      j <- i + 1
      
      while(
        j <= nrow(gdoc) &&
        trimws(gdoc$value[j]) == ""
      ){
        j <- j + 1
      }
      
      if(j > nrow(gdoc))
        next
      
      if(!grepl(
        caption_pattern,
        gdoc$value[j],
        perl = TRUE
      ))
        next
      
      img_ref <- sub(
        image_pattern,
        "\\1",
        gdoc$value[i],
        perl = TRUE
      )
      
      cap <- stringr::str_match(
        gdoc$value[j],
        caption_pattern
      )
      
      caption <- cap[,2]
      fig_id  <- cap[,3]
      
      gdoc$value[i] <- paste0(
        "![",
        caption,
        "][",
        img_ref,
        "]{#",
        fig_id,
        "}"
      )
      
      gdoc <- gdoc[-j, ]
    }
    
    gdoc
  }
  
  
# Google Docs -------------------------------------------------------------

  gdoc <- file %>%
    readLines(warn = F) %>%
    tibble::enframe() %>%
    dplyr::filter(!grepl("^#+ $", .data$value)) %>%
    dplyr::filter(!grepl("^#$", .data$value)) %>%
    dplyr::mutate(
      value = gsub(
        "!\\[(\\$\\$[^]]*\\$\\$|\\$[^]]*\\$)\\]\\[image[0-9]+\\]",
        "\\1",
        .data$value
      ),
      value = gsub("\\\\\\\\", "\\\\", .data$value),
      value = gsub("\\\\_", "_", .data$value)
    ) %>%
    filter(!grepl("^\\|\\s*\\|$", .data$value)) %>%
    filter(!grepl("^\\|\\s*:?-+:?\\s*\\|$", .data$value)) %>%
    dplyr::mutate(value = dplyr::if_else(
      dplyr::row_number() == 1,
      stringr::str_remove(.data$value, "^#\\s*"),
      .data$value
    )) %>%
    dplyr::mutate(value = if_else(
      grepl(header_clean, .data$value, ignore.case = TRUE),
      gsub("^#+\\s*", "", .data$value),
      .data$value
    )) %>% {
      purrr::reduce(rev(which(
        grepl(header_break, .$value, ignore.case = TRUE)
      )),
      .init = .,
      ~ tibble::add_row(.x, value = section_break, .before = .y))
    } %>% 
    mutate(across(.data$value, ~ crossrefs(.))) %>% 
    fix_figures()
    

# Figures -----------------------------------------------------------------
  
  figure_pattern <- "^\\s*!\\[.*\\]\\[image[0-9]+\\]\\{#fig-[^}]+\\}\\s*$"
  
  figs <- gdoc %>%
    
    mutate(
      
      fig_start = grepl(
        figure_pattern,
        .data$value,
        perl = TRUE
      ),
      
      fig_id = cumsum(.data$fig_start)
      
    ) %>%
    
    filter(.data$fig_id > 0) %>%
    
    group_by(.data$fig_id) %>%
    
    mutate(
      
      first_non_fig = match(
        FALSE,
        
        .data$fig_start |
          trimws(.data$value) == ""
        
      )
      
    ) %>%
    
    filter(
      row_number() < .data$first_non_fig |
        is.na(.data$first_non_fig)
    ) %>%
    
    group_modify(~{
      .x %>%
        add_row(
          value = section_break
        )
    }) %>%
    
    ungroup()

# Tables ------------------------------------------------------------------
  
  table_pattern <- paste0(
    "^\\s*(",
    
    # Formato clásico
    "Table\\s*[0-9]+\\s*[:.]",
    
    "|",
    
    # Cross-reference pandoc
    "\\[Table\\]\\(#tab[_-][^)]+\\)\\s*:",
    
    "|",
    
    # Caption Quarto
    ":\\s*.*\\{#tbl-[^}]+\\}\\s*$",
    
    ")"
  )
  
  tabs <- gdoc %>%
    
    mutate(
      
      table_start = grepl(
        table_pattern,
        .data$value,
        ignore.case = TRUE,
        perl = TRUE
      ),
      
      table_id = cumsum(.data$table_start)
      
    ) %>%
    
    filter(.data$table_id > 0) %>%
    
    group_by(.data$table_id) %>%
    
    mutate(
      
      first_non_table = match(
        FALSE,
        
        .data$table_start |
          
          # filas markdown de la tabla
          grepl(
            "^\\s*\\|",
            .data$value
          ) |
          
          # líneas vacías
          trimws(.data$value) == ""
        
      )
      
    ) %>%
    
    filter(
      
      row_number() < .data$first_non_table |
        
        is.na(.data$first_non_table)
      
    ) %>%
    
    group_modify(~{
      
      .x %>%
        
        add_row(
          value = section_break
        )
      
    }) %>%
    
    ungroup() %>%
    
    filter(
      !.data$name %in%
        stats::na.omit(figs$name)
    )
  

# Text --------------------------------------------------------------------

  txt <- gdoc %>%
    dplyr::filter(
      !.data$name %in% stats::na.omit(tabs$name)
      ,
      !.data$name %in% stats::na.omit(figs$name)
    ) %>%
    add_row(name = max(.$name, na.rm = TRUE) + 1, value = section_break)
  

# manuscript --------------------------------------------------------------

  manuscript <- if (type == "asis") {
    gdoc
    
  } else if (type == "list") {
    docx <- list(txt, figs, tabs) %>%
      bind_rows() %>%
      slice(1:(nrow(.) - length(section_break)))
    
  }

# export ------------------------------------------------------------------

  qmd <- manuscript %>%
    tibble::deframe() %>%
    writeLines(con = file.path(export, gsub(
      pattern = ".md",
      replacement = ".qmd",
      x = file
    )))
  

# result ------------------------------------------------------------------

  list.files(path = export
             ,
             pattern = ".qmd"
             ,
             full.names = T)
  
}