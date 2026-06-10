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
  # file = "draft.md" ; export = NULL ; type = "list"
  
  type <- match.arg(type)
  
  if (is.null(export)) {
    export <- sub("\\.md$", "", file)
  }
  
  dir.create(export, recursive = T, showWarnings = F)
  
  
  fmt <- tryCatch(
    knitr::pandoc_to(),
    error = function(e)
      NULL
  )
  
  section_break <- if (identical(fmt, "html")) {
    "<div style='margin-top: 3em;'></div>"
    
  } else {
    c("```{=openxml}",
      "<w:p>",
      "  <w:pPr>",
      "    <w:sectPr/>",
      "  </w:pPr>",
      "</w:p>",
      "```")
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
    }
  

  figs <- gdoc %>%
    mutate(
      fig_start = grepl("!\\[[^]]*\\]\\[image[0-9]+\\]", .data$value),
      
      fig_id = cumsum(.data$fig_start)
      
    ) %>%
    
    filter(.data$fig_id > 0) %>%
    
    group_by(.data$fig_id) %>%
    
    mutate(caption_row = match(
      TRUE,
      grepl("Figure\\s+[0-9]+\\s*:", .data$value, ignore.case = TRUE)
    )) %>%
    
    filter(row_number() <= .data$caption_row |
             is.na(.data$caption_row)) %>%
    
    filter(trimws(.data$value) != "") %>%
    
    group_modify( ~ {
      .x %>%
        dplyr::add_row(value = section_break)
      
    }) %>%
    
    ungroup()
  
  tabs <- gdoc %>%
    mutate(
      table_start = grepl(
        "Table\\s+[0-9]+\\s*:",
        .data$value,
        ignore.case = TRUE
      ),
      
      table_id = cumsum(.data$table_start)
      
    ) %>%
    
    filter(.data$table_id > 0) %>%
    
    group_by(.data$table_id) %>%
    
    mutate(
      first_non_table = match(
        FALSE,
        (
          grepl("^\\s*\\|", .data$value) &
            !grepl("!\\[.*\\]\\(.*\\)", .data$value)
        ) |
          grepl("^\\s*$", .data$value) |
          grepl(
            "Table\\s+[0-9]+\\s*:",
            .data$value,
            ignore.case = TRUE
          )
      )
    ) %>%
    
    filter(
      row_number() < .data$first_non_table |
        is.na(.data$first_non_table)
    ) %>%
    
    group_modify(~{
      .x %>%
        dplyr::add_row(value = section_break)
    }) %>%
    
    ungroup() %>% 
    dplyr::filter(!.data$name %in% stats::na.omit(figs$name))
  
  
  txt <- gdoc %>%
    dplyr::filter(
      !.data$name %in% stats::na.omit(tabs$name)
      ,
      !.data$name %in% stats::na.omit(figs$name)
    ) %>%
    add_row(name = max(.$name, na.rm = TRUE) + 1, value = section_break)
  
  manuscript <- if (type == "asis") {
    gdoc
    
  } else if (type == "list") {
    docx <- list(txt, figs, tabs) %>%
      bind_rows() %>%
      slice(1:(nrow(.) - length(section_break)))
    
  }
  
  qmd <- manuscript %>%
    tibble::deframe() %>%
    writeLines(con = file.path(export, gsub(
      pattern = ".md",
      replacement = ".qmd",
      x = file
    )))
  
  list.files(path = export
             ,
             pattern = ".qmd"
             ,
             full.names = T)
  
}