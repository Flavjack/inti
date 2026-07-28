#' Plot Splitplot-RCBD fieldbook design
#'
#' Plot fieldbook sketches for Splitplot-RCBD experimental designs generated
#' by `design_split_rcbd()`.
#'
#' @param data Fieldbook data frame from `design_split_rcbd()`.
#' @param factor Character scalar. Column used to color experimental units.
#'   If missing, `"wp_sp"` is used when available; otherwise, `"ntreat"` is
#'   used.
#' @param fill Character vector. Column or columns used as labels inside each
#'   experimental unit. Default is `"plots"`.
#' @param xlab Character scalar. Optional x axis title.
#' @param ylab Character scalar. Optional y axis title.
#' @param glab Character scalar. Optional legend title.
#' @param text_size Optional positive numeric scalar indicating the plot-label
#'   font size in typographic points (`pt`). If `NULL` or `NA`, the function
#'   calculates an automatic size: 10 pt for one label column, 8 pt for two
#'   columns and 7 pt for three or more columns.
#' @param wrap_width Optional positive integer indicating the approximate
#'   maximum number of characters per line. If `NULL` or `NA`, labels are not
#'   wrapped. Underscores are displayed as spaces only in the sketch.
#' @param font_family Font family used in the sketch. Defaults to
#'   `"Open Sans"`. If it or the optional `systemfonts` package is unavailable,
#'   the function silently uses `"sans"`.
#' @param font_face Font face used in the sketch. Defaults to `"plain"`,
#'   equivalent to regular/normal text.
#'
#' @details
#' Each block is displayed in a separate panel. Within each block:
#'
#' \itemize{
#'   \item `cols` identifies the whole-plot position on the x axis.
#'   \item The ranked `rows` values identify subplot positions on the y axis.
#'   \item A black external border identifies each whole plot.
#' }
#'
#' Label wrapping changes only the displayed text. It does not modify
#' `plots`, `ntreat`, treatment factors, QR codes or any other fieldbook value.
#'
#' `text_size` is expressed in points, like common word processors. Internally
#' it is converted to the size unit expected by `ggplot2::geom_text()`.
#'
#' The visible design name is Splitplot-RCBD, while the stable internal
#' identifier remains `"split-rcbd"`.
#'
#' @return A `ggplot` object.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' plot_split_rcbd_design(
#'   data = fieldbook,
#'   factor = "wp_sp",
#'   fill = c("plots", "ntreat"),
#'   text_size = 9,
#'   wrap_width = 14,
#'   font_family = "Open Sans",
#'   font_face = "plain"
#' )
#'
#' }

plot_split_rcbd_design <- function(
    data,
    factor = NA,
    fill = "plots",
    xlab = NULL,
    ylab = NULL,
    glab = NULL,
    text_size = NULL,
    wrap_width = NULL,
    font_family = "Open Sans",
    font_face = "plain"
) {
  
  # -------------------------------------------------------------------------
  # Helpers -----------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  is_missing_scalar <- function(x) {
    
    is.null(x) ||
      length(x) == 0L ||
      (
        length(x) == 1L &&
          (
            is.na(x) ||
              (
                is.character(x) &&
                  trimws(x) == ""
              )
          )
      )
  }
  
  resolve_font_family <- function(font_family) {
    
    if(
      is.null(font_family) ||
      length(font_family) != 1L ||
      is.na(font_family) ||
      trimws(as.character(font_family)) == ""
    ) {
      return("sans")
    }
    
    font_family <- trimws(
      as.character(font_family)
    )
    
    if(tolower(font_family) == "sans") {
      return("sans")
    }
    
    if(!requireNamespace("systemfonts", quietly = TRUE)) {
      return("sans")
    }
    
    fonts <- tryCatch(
      systemfonts::system_fonts(),
      error = function(e) NULL
    )
    
    if(
      is.null(fonts) ||
      !"family" %in% names(fonts)
    ) {
      return("sans")
    }
    
    available <- any(
      tolower(trimws(fonts$family)) ==
        tolower(font_family),
      na.rm = TRUE
    )
    
    if(available) {
      font_family
    } else {
      "sans"
    }
  }
  
  split_long_word <- function(word, width) {
    
    if(
      word == "" ||
      nchar(word, type = "width") <= width
    ) {
      return(word)
    }
    
    starts <- seq.int(
      from = 1L,
      to = nchar(word),
      by = width
    )
    
    substring(
      word,
      first = starts,
      last = pmin(
        starts + width - 1L,
        nchar(word)
      )
    )
  }
  
  wrap_one_label <- function(value, width) {
    
    if(
      is.na(value) ||
      value == ""
    ) {
      return("")
    }
    
    # Only the plotted label changes; the fieldbook remains untouched.
    value <- gsub(
      "_",
      " ",
      value,
      fixed = TRUE
    )
    
    words <- strsplit(
      trimws(value),
      "[[:space:]]+"
    )[[1L]]
    
    # Explicitly split identifiers with no natural wrapping point.
    words <- unlist(
      lapply(
        words,
        split_long_word,
        width = width
      ),
      use.names = FALSE
    )
    
    paste(
      strwrap(
        paste(words, collapse = " "),
        width = width,
        simplify = TRUE
      ),
      collapse = "\n"
    )
  }
  
  wrap_plot_label <- function(x, width = NULL) {
    
    x <- as.character(x)
    x[is.na(x)] <- ""
    
    if(is.null(width)) {
      return(x)
    }
    
    vapply(
      x,
      wrap_one_label,
      width = width,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )
  }
  
  make_label <- function(
    data,
    fill,
    wrap_width = NULL
  ) {
    
    labels <- lapply(
      fill,
      function(column) {
        
        values <- as.character(
          data[[column]]
        )
        
        values[is.na(values)] <- ""
        
        if(column == "ntreat") {
          values <- ifelse(
            values == "",
            "",
            paste0("T", values)
          )
        }
        
        wrap_plot_label(
          values,
          width = wrap_width
        )
      }
    )
    
    output <- do.call(
      paste,
      c(labels, sep = "\n")
    )
    
    output <- gsub(
      "^\n+|\n+$",
      "",
      output
    )
    
    output <- gsub(
      "\n{3,}",
      "\n\n",
      output
    )
    
    output
  }
  
  # -------------------------------------------------------------------------
  # Checks ------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  
  if(nrow(data) == 0L) {
    stop("'data' must contain at least one experimental unit.")
  }
  
  required_cols <- c(
    "plots",
    "ntreat",
    "block",
    "rows",
    "cols",
    "design"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(data)
  )
  
  if(length(missing_cols) > 0L) {
    stop(
      "Missing required columns for Splitplot-RCBD plot: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }
  
  if(
    anyNA(data$block) ||
    anyNA(data$rows) ||
    anyNA(data$cols)
  ) {
    stop(
      "Columns 'block', 'rows' and 'cols' must not contain missing values."
    )
  }
  
  if(
    !is.numeric(data$rows) ||
    !is.numeric(data$cols) ||
    any(!is.finite(data$rows)) ||
    any(!is.finite(data$cols))
  ) {
    stop(
      "Columns 'rows' and 'cols' must contain finite numeric values."
    )
  }
  
  design_values <- unique(
    tolower(
      trimws(
        as.character(data$design)
      )
    )
  )
  
  design_values <- design_values[
    !is.na(design_values) &
      design_values != ""
  ]
  
  accepted_designs <- c(
    "split-rcbd",
    "split_rcbd",
    "split rcbd",
    "splitplot-rcbd",
    "splitplot_rcbd",
    "splitplot rcbd",
    "split-plot-rcbd",
    "split-plot rcbd"
  )
  
  if(
    length(design_values) > 0L &&
    !design_values[1L] %in% accepted_designs
  ) {
    stop(
      "The fieldbook is not identified as a Splitplot-RCBD design. ",
      "Found: ",
      design_values[1L],
      "."
    )
  }
  
  if(is_missing_scalar(factor)) {
    
    if("wp_sp" %in% names(data)) {
      
      factor <- "wp_sp"
      
    } else {
      
      factor <- "ntreat"
    }
  }
  
  if(
    length(factor) != 1L ||
    !is.character(factor)
  ) {
    stop(
      "'factor' must be the name of one column."
    )
  }
  
  factor <- trimws(factor)
  
  if(!factor %in% names(data)) {
    stop(
      "Column selected in 'factor' was not found in data. ",
      "Available columns: ",
      paste(names(data), collapse = ", "),
      "."
    )
  }
  
  if(
    is.null(fill) ||
    length(fill) == 0L ||
    all(is.na(fill)) ||
    all(trimws(as.character(fill)) == "")
  ) {
    fill <- "plots"
  }
  
  fill <- trimws(
    as.character(fill)
  )
  
  fill <- fill[
    !is.na(fill) &
      fill != ""
  ]
  
  fill <- unique(fill)
  
  missing_fill <- setdiff(
    fill,
    names(data)
  )
  
  if(length(missing_fill) > 0L) {
    stop(
      "Columns selected in 'fill' were not found: ",
      paste(missing_fill, collapse = ", "),
      ". Available columns: ",
      paste(names(data), collapse = ", "),
      "."
    )
  }
  
  if(!is_missing_scalar(text_size)) {
    
    if(
      length(text_size) != 1L ||
      !is.numeric(text_size) ||
      !is.finite(text_size) ||
      text_size <= 0
    ) {
      stop(
        "'text_size' must be a positive numeric value in points, ",
        "NA, or NULL."
      )
    }
    
    text_size <- as.numeric(
      text_size
    )
    
  } else {
    
    text_size <- NULL
  }
  
  if(!is_missing_scalar(wrap_width)) {
    
    if(
      length(wrap_width) != 1L ||
      !is.numeric(wrap_width) ||
      !is.finite(wrap_width) ||
      wrap_width < 1 ||
      wrap_width != floor(wrap_width)
    ) {
      stop(
        "'wrap_width' must be a positive integer, NA, or NULL."
      )
    }
    
    wrap_width <- as.integer(
      wrap_width
    )
    
  } else {
    
    wrap_width <- NULL
  }
  
  allowed_faces <- c(
    "plain",
    "bold",
    "italic",
    "bold.italic"
  )
  
  if(
    length(font_face) != 1L ||
    is.na(font_face) ||
    !font_face %in% allowed_faces
  ) {
    stop(
      "'font_face' must be one of: ",
      paste(allowed_faces, collapse = ", "),
      "."
    )
  }
  
  font_family <- resolve_font_family(
    font_family
  )
  
  # -------------------------------------------------------------------------
  # Data preparation ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  data_plot <- data %>%
    dplyr::group_by(
      .data$block
    ) %>%
    dplyr::mutate(
      .row_block = dplyr::dense_rank(
        .data$rows
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .plot_factor = as.factor(
        .data[[factor]]
      ),
      .plot_label = make_label(
        data = .,
        fill = fill,
        wrap_width = wrap_width
      )
    )
  
  factor_levels <- levels(
    data_plot$.plot_factor
  )
  
  n_factor_levels <- max(
    length(factor_levels),
    1L
  )
  
  color_grps <- grDevices::colorRampPalette(
    c(
      "#86CD80",
      "#F4CB8C",
      "#F3BB00",
      "#0198CD",
      "#FE6673"
    )
  )(n_factor_levels)
  
  if(is.null(xlab)) {
    xlab <- "whole plots"
  }
  
  if(is.null(ylab)) {
    ylab <- "subplots"
  }
  
  if(is.null(glab)) {
    glab <- factor
  }
  
  # Automatic sizes are expressed in typographic points.
  if(is.null(text_size)) {
    
    text_size <- dplyr::case_when(
      length(fill) == 1L ~ 10,
      length(fill) == 2L ~ 8,
      TRUE ~ 7
    )
  }
  
  # ggplot2::geom_text() uses millimetres internally.
  geom_text_size <- text_size / ggplot2::.pt
  
  line_height <- dplyr::case_when(
    length(fill) == 1L ~ 1.05,
    length(fill) == 2L ~ 1.00,
    TRUE ~ 0.95
  )
  
  # -------------------------------------------------------------------------
  # Whole-plot boxes ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  whole_boxes <- data_plot %>%
    dplyr::group_by(
      .data$block,
      .data$cols
    ) %>%
    dplyr::summarise(
      xmin = min(
        .data$cols,
        na.rm = TRUE
      ) - 0.5,
      xmax = max(
        .data$cols,
        na.rm = TRUE
      ) + 0.5,
      ymin = min(
        .data$.row_block,
        na.rm = TRUE
      ) - 0.5,
      ymax = max(
        .data$.row_block,
        na.rm = TRUE
      ) + 0.5,
      .groups = "drop"
    )
  
  # -------------------------------------------------------------------------
  # Plot --------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  plot <- data_plot %>%
    dplyr::arrange(
      .data$block,
      .data$cols,
      .data$.row_block
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$cols,
        y = .data$.row_block,
        fill = .data$.plot_factor
      )
    ) +
    ggplot2::geom_tile(
      color = "grey35",
      linewidth = 0.25
    ) +
    ggplot2::geom_rect(
      data = whole_boxes,
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax
      ),
      inherit.aes = FALSE,
      fill = NA,
      color = "black",
      linewidth = 0.65
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = .data$.plot_label
      ),
      size = geom_text_size,
      family = font_family,
      fontface = font_face,
      lineheight = line_height,
      color = "black",
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(
      ~ block,
      nrow = 1,
      labeller = ggplot2::label_both
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      trans = "reverse",
      breaks = sort(
        unique(data_plot$.row_block)
      )
    ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      breaks = sort(
        unique(data_plot$cols)
      )
    ) +
    ggplot2::scale_fill_manual(
      values = color_grps,
      na.value = "grey90"
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      fill = glab
    ) +
    ggplot2::theme_minimal(
      base_size = 12,
      base_family = font_family
    ) +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_text(
        family = font_family,
        face = font_face
      ),
      legend.text = ggplot2::element_text(
        family = font_family,
        face = font_face,
        size = 9
      ),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = "grey90",
        color = "grey70"
      ),
      strip.text = ggplot2::element_text(
        family = font_family,
        face = font_face
      ),
      axis.title = ggplot2::element_text(
        family = font_family,
        face = font_face
      ),
      axis.text = ggplot2::element_text(
        family = font_family,
        face = font_face,
        color = "grey25"
      ),
      plot.margin = ggplot2::margin(
        6,
        6,
        6,
        6
      )
    )
  
  return(plot)
}