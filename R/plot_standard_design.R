#' Plot standard fieldbook experimental designs
#'
#' Plot standard fieldbook sketches for simple experimental designs generated
#' in Tarpuy. This function is intended for designs with a regular fieldbook
#' layout, such as completely randomized designs, randomized complete block
#' designs, sorted designs and unsorted designs.
#'
#' The function does not calculate the experimental design. It only plots an
#' existing fieldbook. Therefore, if the fieldbook was generated with
#' `zigzag = TRUE`, the zigzag layout is respected because the function uses
#' the existing layout columns.
#'
#' @param data A fieldbook data frame. It must contain at least `rows` and
#'   `cols`. For RCBD/DBCA designs, it should also contain `block`.
#' @param factor Character scalar. Name of the column used to color the
#'   experimental units. If missing, `"block"` is used when available;
#'   otherwise, the third column of `data` is used.
#' @param fill Character vector. Names of one or more columns used as labels
#'   inside each experimental unit. When `ntreat` is used, it is displayed as
#'   `T1`, `T2`, etc.
#' @param xlab Character scalar. Title for the x axis. If `NULL`,
#'   `"columns"` is used.
#' @param ylab Character scalar. Title for the y axis. If `NULL`, `"row"` is
#'   used for non-RCBD designs. For RCBD/DBCA designs, `"blocks"` is used.
#' @param glab Character scalar. Legend title. If `NULL`, `factor` is used.
#' @param text_size Optional positive numeric scalar. Text size passed to
#'   `ggplot2::geom_text()`. If `NULL` or `NA`, it is calculated automatically
#'   from the number of columns selected in `fill`.
#' @param wrap_width Optional positive integer. Maximum approximate number of
#'   characters per line in plot labels. If `NULL` or `NA`, labels are not
#'   wrapped. Underscores are shown as spaces only in the plotted label; the
#'   original fieldbook values are not modified.
#' @param font_family Character scalar. Font family used by the sketch.
#'   Defaults to `"Open Sans"`. If the font cannot be found through
#'   `systemfonts`, `"sans"` is used as a fallback.
#' @param font_face Character scalar. Font face used in labels, axes and
#'   legends. Defaults to `"plain"` (Open Sans Regular).
#'
#' @details
#' Non-blocked standard designs are plotted using `cols` on the x axis and
#' `rows` on the y axis. RCBD/DBCA designs retain their existing Tarpuy
#' representation: `cols` on the x axis and `block` on the y axis.
#'
#' Label formatting affects only the sketch. It does not change `entry`,
#' `ntreat`, QR codes or any other fieldbook value.
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
#' plot_standard_design(
#'   data = fieldbook,
#'   factor = "geno",
#'   fill = c("plots", "entry"),
#'   text_size = 2.5,
#'   wrap_width = 14,
#'   font_family = "Open Sans",
#'   font_face = "plain"
#' )
#'
#' }

plot_standard_design <- function(
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
              (is.character(x) && !nzchar(trimws(x)))
          )
      )
  }
  
  validate_optional_positive_number <- function(x, name) {
    
    if(is_missing_scalar(x)) {
      return(NULL)
    }
    
    if(
      length(x) != 1L ||
      !is.numeric(x) ||
      !is.finite(x) ||
      x <= 0
    ) {
      stop("'", name, "' must be a positive numeric scalar, NA, or NULL.")
    }
    
    as.numeric(x)
  }
  
  validate_optional_positive_integer <- function(x, name) {
    
    value <- validate_optional_positive_number(x, name)
    
    if(is.null(value)) {
      return(NULL)
    }
    
    if(value != floor(value)) {
      stop("'", name, "' must be a positive integer, NA, or NULL.")
    }
    
    as.integer(value)
  }
  
  resolve_font_family <- function(font_family) {
    
    if(
      is.null(font_family) ||
      length(font_family) != 1L ||
      is.na(font_family) ||
      !nzchar(trimws(font_family))
    ) {
      return("sans")
    }
    
    font_family <- trimws(font_family)
    
    if(identical(tolower(font_family), "sans")) {
      return("sans")
    }
    
    if(!requireNamespace("systemfonts", quietly = TRUE)) {
      warning(
        "Package 'systemfonts' is not available, so font '",
        font_family,
        "' cannot be verified. Using 'sans' instead.",
        call. = FALSE
      )
      return("sans")
    }
    
    available_fonts <- tryCatch(
      systemfonts::system_fonts(),
      error = function(e) NULL
    )
    
    font_available <- !is.null(available_fonts) &&
      "family" %in% names(available_fonts) &&
      any(
        tolower(trimws(available_fonts$family)) ==
          tolower(font_family),
        na.rm = TRUE
      )
    
    if(!font_available) {
      warning(
        "Font '", font_family,
        "' was not found. Using 'sans' instead.",
        call. = FALSE
      )
      return("sans")
    }
    
    font_family
  }
  
  split_long_word <- function(word, width) {
    
    if(!nzchar(word) || nchar(word, type = "width") <= width) {
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
      last = pmin(starts + width - 1L, nchar(word))
    )
  }
  
  wrap_one_label <- function(value, width) {
    
    if(is.na(value) || !nzchar(value)) {
      return("")
    }
    
    # Only the displayed label is changed. The source data remains untouched.
    value <- gsub("_", " ", value, fixed = TRUE)
    words <- strsplit(trimws(value), "[[:space:]]+")[[1]]
    
    words <- unlist(
      lapply(words, split_long_word, width = width),
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
  
  format_label_column <- function(values, column, wrap_width) {
    
    values <- as.character(values)
    values[is.na(values)] <- ""
    
    if(identical(column, "ntreat")) {
      values <- ifelse(
        nzchar(values),
        paste0("T", values),
        ""
      )
    }
    
    if(!is.null(wrap_width)) {
      values <- vapply(
        values,
        wrap_one_label,
        width = wrap_width,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      )
    }
    
    values
  }
  
  make_label <- function(data, fill, wrap_width) {
    
    labels <- lapply(
      fill,
      function(column) {
        format_label_column(
          values = data[[column]],
          column = column,
          wrap_width = wrap_width
        )
      }
    )
    
    output <- do.call(
      paste,
      c(labels, sep = "\n")
    )
    
    # Remove leading, trailing and repeated blank lines caused by missing
    # optional label values.
    output <- gsub("^\n+|\n+$", "", output)
    output <- gsub("\n{3,}", "\n\n", output)
    
    output
  }
  
  # -------------------------------------------------------------------------
  # Input validation ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  
  if(nrow(data) == 0L) {
    stop("'data' must contain at least one experimental unit.")
  }
  
  required_layout <- c("rows", "cols")
  missing_layout <- setdiff(required_layout, names(data))
  
  if(length(missing_layout) > 0L) {
    stop(
      "Missing required layout columns: ",
      paste(missing_layout, collapse = ", "),
      "."
    )
  }
  
  if(anyNA(data$rows) || anyNA(data$cols)) {
    stop("Columns 'rows' and 'cols' must not contain missing values.")
  }
  
  if(
    !is.numeric(data$rows) ||
    !is.numeric(data$cols) ||
    any(!is.finite(data$rows)) ||
    any(!is.finite(data$cols))
  ) {
    stop("Columns 'rows' and 'cols' must contain finite numeric values.")
  }
  
  factor_missing <- is_missing_scalar(factor)
  
  if(factor_missing) {
    
    if("block" %in% names(data)) {
      factor <- "block"
    } else if(ncol(data) >= 3L) {
      factor <- names(data)[3L]
    } else {
      stop(
        "'factor' was not provided and no default color column is available."
      )
    }
  }
  
  if(length(factor) != 1L || !is.character(factor)) {
    stop("'factor' must be the name of one column.")
  }
  
  factor <- trimws(factor)
  
  if(!factor %in% names(data)) {
    stop(
      "Column selected in 'factor' was not found. Available columns: ",
      paste(names(data), collapse = ", "),
      "."
    )
  }
  
  if(
    is.null(fill) ||
    length(fill) == 0L ||
    all(is.na(fill)) ||
    all(!nzchar(trimws(as.character(fill))))
  ) {
    fill <- "plots"
  }
  
  fill <- trimws(as.character(fill))
  fill <- fill[!is.na(fill) & nzchar(fill)]
  fill <- unique(fill)
  
  missing_fill <- setdiff(fill, names(data))
  
  if(length(missing_fill) > 0L) {
    stop(
      "Columns selected in 'fill' were not found: ",
      paste(missing_fill, collapse = ", "),
      ". Available columns: ",
      paste(names(data), collapse = ", "),
      "."
    )
  }
  
  text_size <- validate_optional_positive_number(
    text_size,
    "text_size"
  )
  
  wrap_width <- validate_optional_positive_integer(
    wrap_width,
    "wrap_width"
  )
  
  allowed_faces <- c(
    "plain",
    "bold",
    "italic",
    "bold.italic"
  )
  
  if(
    is.null(font_face) ||
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
  
  font_family <- resolve_font_family(font_family)
  
  # -------------------------------------------------------------------------
  # Data preparation ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  data_plot <- data %>%
    dplyr::mutate(
      .plot_factor = as.factor(.data[[factor]]),
      .plot_label = make_label(
        data = .,
        fill = fill,
        wrap_width = wrap_width
      )
    )
  
  design_values <- if("design" %in% names(data_plot)) {
    unique(stats::na.omit(as.character(data_plot$design)))
  } else {
    character(0)
  }
  
  design_type <- if(length(design_values) > 0L) {
    tolower(design_values[1L])
  } else {
    NA_character_
  }
  
  is_rcbd <- !is.na(design_type) &&
    design_type %in% c("rcbd", "dbca") &&
    "block" %in% names(data_plot)
  
  if(is_rcbd && anyNA(data_plot$block)) {
    stop("Column 'block' must not contain missing values for RCBD/DBCA.")
  }
  
  factor_levels <- levels(data_plot$.plot_factor)
  n_factor_levels <- max(length(factor_levels), 1L)
  
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
    xlab <- "columns"
  }
  
  if(is.null(ylab)) {
    ylab <- "row"
  }
  
  if(is.null(glab)) {
    glab <- factor
  }
  
  legend <- "top"
  
  if(is.null(text_size)) {
    text_size <- dplyr::case_when(
      length(fill) == 1L ~ 3.5,
      length(fill) == 2L ~ 3.0,
      TRUE ~ 2.5
    )
  }
  
  line_height <- dplyr::case_when(
    length(fill) == 1L ~ 1.05,
    length(fill) == 2L ~ 1.00,
    TRUE ~ 0.95
  )
  
  common_theme <- ggplot2::theme_minimal(
    base_size = 12,
    base_family = font_family
  ) +
    ggplot2::theme(
      legend.position = legend,
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
      axis.title = ggplot2::element_text(
        family = font_family,
        face = font_face
      ),
      axis.text = ggplot2::element_text(
        family = font_family,
        face = font_face,
        color = "grey25"
      ),
      strip.text = ggplot2::element_text(
        family = font_family,
        face = font_face
      ),
      plot.margin = ggplot2::margin(6, 6, 6, 6)
    )
  
  # -------------------------------------------------------------------------
  # RCBD / DBCA: blocks as rows ---------------------------------------------
  # -------------------------------------------------------------------------
  
  if(is_rcbd) {
    
    plot <- data_plot %>%
      dplyr::arrange(.data$block, .data$cols) %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data$cols,
          y = .data$block,
          fill = .data$.plot_factor
        )
      ) +
      ggplot2::geom_tile(
        color = "grey25",
        linewidth = 0.35
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$.plot_label),
        size = text_size,
        family = font_family,
        fontface = font_face,
        lineheight = line_height,
        color = "black",
        na.rm = TRUE
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        trans = "reverse",
        breaks = sort(unique(data_plot$block))
      ) +
      ggplot2::scale_x_continuous(
        expand = c(0, 0),
        breaks = sort(unique(data_plot$cols))
      ) +
      ggplot2::scale_fill_manual(
        values = color_grps,
        na.value = "grey90"
      ) +
      ggplot2::labs(
        x = xlab,
        y = "blocks",
        fill = glab
      ) +
      common_theme
    
    return(plot)
  }
  
  # -------------------------------------------------------------------------
  # Standard layout: CRD, sorted, unsorted, LSD ------------------------------
  # -------------------------------------------------------------------------
  
  plot <- data_plot %>%
    dplyr::arrange(.data$rows, .data$cols) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$cols,
        y = .data$rows,
        fill = .data$.plot_factor
      )
    ) +
    ggplot2::geom_tile(
      color = "grey25",
      linewidth = 0.35
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$.plot_label),
      size = text_size,
      family = font_family,
      fontface = font_face,
      lineheight = line_height,
      color = "black",
      na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      trans = "reverse",
      breaks = sort(unique(data_plot$rows))
    ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      breaks = sort(unique(data_plot$cols))
    ) +
    ggplot2::scale_fill_manual(
      values = color_grps,
      na.value = "grey90"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      fill = glab
    ) +
    common_theme
  
  plot
}