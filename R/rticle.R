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
                   export = NULL,
                   type = c("asis", "list")) {
  
  # file = "inst/extdata/_extensions/scihub/draft.md" ; export = NULL ; type = "asis"
  
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
    
    c(
      "```{=openxml}",
      "<w:p>",
      "  <w:pPr>",
      "    <w:sectPr/>",
      "  </w:pPr>",
      "</w:p>",
      "```"
    )
    
  } 
  
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
    dplyr::mutate(value = dplyr::if_else(
      grepl(
        "^#+\\s*\\*{0,2}(abstract|resumen|keywords|declararions)\\*{0,2}\\s*:?[[:space:]]*$",
        .data$value,
        ignore.case = TRUE
      ),
      sub("^#+\\s*", "", .data$value),
      .data$value
    )) %>% {
      purrr::reduce(rev(which(
        grepl(
          "^#*\\s*\\*{0,2}(abstract|introduction|declarations|statments)\\*{0,2}\\s*$",
          .$value,
          ignore.case = TRUE
        )
      )),
      .init = .,
      ~ tibble::add_row(.x, value = section_break, .before = .y))
    }
  
  tabs <- gdoc %>%
    mutate(
      table_start = grepl("^\\|\\s*Table\\s+[0-9]+\\s*:", .data$value),
      table_id = cumsum(.data$table_start)
    ) %>%
    filter(.data$table_id > 0) %>%
    group_by(.data$table_id) %>%
    mutate(
      is_table_line = grepl("^\\s*\\||^\\s*$", .data$value),
      end_table = match(FALSE, .data$is_table_line)
    ) %>%
    filter(row_number() < .data$end_table | is.na(.data$end_table)) %>%
    group_modify( ~ {
      .x %>%
        # add_row(value = "") %>%
        dplyr::add_row(value = section_break)
    }) %>%
    ungroup()
  
  figs <- gdoc %>%
    mutate(
      fig_start = grepl("^\\|\\s*!\\[\\]\\[image[0-9]+\\]\\s*\\|$", .data$value),
      fig_id = cumsum(.data$fig_start)
    ) %>%
    filter(.data$fig_id > 0) %>%
    group_by(.data$fig_id) %>%
    mutate(
      is_fig_line = grepl("^\\s*\\|", .data$value),
      end_fig = match(FALSE, .data$is_fig_line)
    ) %>%
    filter(row_number() < .data$end_fig | is.na(.data$end_fig)) %>%
    group_modify( ~ {
      .x %>%
        # add_row(value = "") %>%
        dplyr::add_row(value = section_break)
    }) %>%
    ungroup()
  
  txt <- gdoc %>%
    dplyr::filter(!.data$name %in% na.omit(tabs$name)
                  , !.data$name %in% na.omit(figs$name)
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