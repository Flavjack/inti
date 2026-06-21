#' Plot split-plot RCBD fieldbook design
#'
#' Plot fieldbook sketches for split-plot designs under RCBD structure.
#'
#' @param data Fieldbook data frame from `design_split_rcbd()`.
#' @param factor Character. Column used to color experimental units.
#' @param fill Character vector. Column or columns used as labels inside each
#'   experimental unit.
#' @param xlab Character. Optional x axis title.
#' @param ylab Character. Optional y axis title.
#' @param glab Character. Optional legend title.
#'
#' @return A `ggplot` object.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export

plot_split_rcbd_design <- function(data,
                                   factor = NA,
                                   fill = "plots",
                                   xlab = NULL,
                                   ylab = NULL,
                                   glab = NULL) {
  
  # -------------------------------------------------------------------------
  # Checks ------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  
  required_cols <- c("plots", "ntreat", "block", "rows", "cols", "design")
  
  missing_cols <- setdiff(required_cols, names(data))
  
  if(length(missing_cols) > 0) {
    stop(
      "Missing required columns for split-plot RCBD plot: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if(is.na(factor) || is.null(factor) || factor == "") {
    
    factor <- if("wp_sp" %in% names(data)) {
      "wp_sp"
    } else if("ntreat" %in% names(data)) {
      "ntreat"
    } else {
      names(data)[3]
    }
    
  }
  
  if(!factor %in% names(data)) {
    stop(
      "Column selected in 'factor' was not found in data. Available columns: ",
      paste(names(data), collapse = ", ")
    )
  }
  
  if(is.null(fill) || length(fill) == 0 || any(is.na(fill)) || fill[1] == "") {
    fill <- "plots"
  }
  
  if(!all(fill %in% names(data))) {
    stop(
      "Column selected in 'fill' was not found in data. Available columns: ",
      paste(names(data), collapse = ", ")
    )
  }
  
  # -------------------------------------------------------------------------
  # Helper: internal labels --------------------------------------------------
  # -------------------------------------------------------------------------
  
  make_label <- function(data, fill) {
    
    labels <- lapply(fill, function(x) {
      
      if(x == "ntreat") {
        paste0("T", data[[x]])
      } else {
        as.character(data[[x]])
      }
      
    })
    
    do.call(paste, c(labels, sep = "\n"))
    
  }
  
  # -------------------------------------------------------------------------
  # Data preparation ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  data_plot <- data %>%
    dplyr::group_by(.data$block) %>%
    dplyr::mutate(
      .row_block = dplyr::dense_rank(.data$rows)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .plot_factor = as.factor(.data[[factor]]),
      .plot_label = make_label(data = ., fill = fill)
    )
  
  lengthfactor <- data_plot[[factor]] %>%
    as.factor() %>%
    nlevels()
  
  color_grps <- grDevices::colorRampPalette(
    c(
      "#86CD80",
      "#F4CB8C",
      "#F3BB00",
      "#0198CD",
      "#FE6673"
    )
  )(lengthfactor)
  
  if(is.null(xlab)) {
    xlab <- "whole plots"
  }
  
  if(is.null(ylab)) {
    ylab <- "subplots"
  }
  
  if(is.null(glab)) {
    glab <- factor
  }
  
  text_size <- dplyr::case_when(
    length(fill) == 1 ~ 4.0,
    length(fill) == 2 ~ 3.2,
    TRUE ~ 2.7
  )
  
  line_height <- dplyr::case_when(
    length(fill) == 1 ~ 1.05,
    length(fill) == 2 ~ 1.00,
    TRUE ~ 0.95
  )
  
  # -------------------------------------------------------------------------
  # Whole plot boxes ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  whole_boxes <- data_plot %>%
    dplyr::group_by(.data$block, .data$cols) %>%
    dplyr::summarise(
      xmin = min(.data$cols, na.rm = TRUE) - 0.5,
      xmax = max(.data$cols, na.rm = TRUE) + 0.5,
      ymin = min(.data$.row_block, na.rm = TRUE) - 0.5,
      ymax = max(.data$.row_block, na.rm = TRUE) + 0.5,
      .groups = "drop"
    )
  
  # -------------------------------------------------------------------------
  # Plot
  # -------------------------------------------------------------------------
  
  plot <- data_plot %>%
    dplyr::arrange(.data$block, .data$cols, .data$.row_block) %>%
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
      ggplot2::aes(label = .data$.plot_label),
      size = text_size,
      lineheight = line_height,
      fontface = "plain",
      color = "black"
    ) +
    ggplot2::facet_wrap(
      ~ block,
      nrow = 1,
      labeller = ggplot2::label_both
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      trans = "reverse",
      breaks = sort(unique(data_plot$.row_block))
    ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      breaks = sort(unique(data_plot$cols))
    ) +
    ggplot2::scale_fill_manual(values = color_grps) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      fill = glab
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_text(face = "bold"),
      legend.text = ggplot2::element_text(size = 9),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = "grey90",
        color = "grey70"
      ),
      strip.text = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(color = "grey25"),
      plot.margin = ggplot2::margin(6, 6, 6, 6)
    )
  
  return(plot)
  
}