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
#' the existing `rows`, `cols` and `block` columns.
#'
#' @param data A fieldbook data frame. It must contain at least `rows` and
#'   `cols`. For RCBD/DBCA designs, it should also contain `block`.
#' @param factor Character. Name of the column used to color the experimental
#'   units. For example: `"geno"`, `"acc"`, `"nacl"`, `"block"` or `"ntreat"`.
#'   If `NA`, the function uses `"block"` when available; otherwise, it uses
#'   the third column of `data`.
#' @param fill Character vector. Name of one or more columns used as labels
#'   inside each experimental unit. For example: `"plots"`,
#'   `c("plots", "ntreat")` or `c("plots", "acc", "nacl")`. When `ntreat`
#'   is used, it is shown as `T1`, `T2`, etc.
#' @param xlab Character. Title for the x axis. If `NULL`, `"columns"` is used.
#' @param ylab Character. Title for the y axis. If `NULL`, `"row"` is used for
#'   non-RCBD designs. For RCBD/DBCA designs, the y axis is shown as
#'   `"blocks"`.
#' @param glab Character. Title for the legend. If `NULL`, the value of
#'   `factor` is used.
#'
#' @details
#' For non-blocked standard designs, such as CRD/DCA, sorted and unsorted
#' designs, the sketch is plotted using:
#'
#' \itemize{
#'   \item `cols` as the x axis.
#'   \item `rows` as the y axis.
#' }
#'
#' For RCBD/DBCA designs, the sketch is plotted using:
#'
#' \itemize{
#'   \item `cols` as the x axis.
#'   \item `block` as the y axis.
#' }
#'
#' In this way, each row represents one block, which makes the DBCA sketch
#' easier to interpret in field layout previews.
#'
#' The argument `factor` controls the fill color, while `fill` controls the
#' text printed inside each plot. For example, `factor = "nacl"` and
#' `fill = c("plots", "acc", "nacl")` colors plots by NaCl level and writes
#' plot ID, accession and NaCl level inside each experimental unit.
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
#' library(dplyr)
#' library(ggplot2)
#'
#' # Example 1: sorted design without replications
#' factores <- list(
#'   geno = paste0("G", 1:12)
#' )
#'
#' fb <- design_noreps(
#'   factors = factores,
#'   type = "sorted",
#'   zigzag = FALSE,
#'   nrows = 3,
#'   serie = 1000,
#'   seed = 123,
#'   project = "TEST",
#'   qrcode = "{project}{plots}"
#' )
#'
#' dsg <- fb$fieldbook
#'
#' plot_standard_design(
#'   data = dsg,
#'   factor = "geno",
#'   fill = c("plots", "ntreat")
#' )
#'
#' # Example 2: DCA / CRD
#' factores_dca <- list(
#'   geno = paste0("G", 1:6)
#' )
#'
#' fb_dca <- design_repblock(
#'   nfactors = 1,
#'   factors = factores_dca,
#'   type = "crd",
#'   rep = 4,
#'   zigzag = TRUE,
#'   nrows = 4,
#'   serie = 1000,
#'   seed = 123,
#'   project = "DCA",
#'   qrcode = "{project}{plots}"
#' )
#'
#' plot_standard_design(
#'   data = fb_dca$fieldbook,
#'   factor = "geno",
#'   fill = c("plots", "ntreat")
#' )
#'
#' # Example 3: DBCA / RCBD
#' factores_dbca <- list(
#'   acc = paste0("acc", 1:6),
#'   nacl = c(0, 100, 200, 300)
#' )
#'
#' fb_dbca <- design_repblock(
#'   nfactors = 2,
#'   factors = factores_dbca,
#'   type = "rcbd",
#'   rep = 4,
#'   zigzag = TRUE,
#'   serie = 1000,
#'   seed = 123,
#'   project = "DBCA",
#'   qrcode = "{project}{plots}"
#' )
#'
#' plot_standard_design(
#'   data = fb_dbca$fieldbook,
#'   factor = "nacl",
#'   fill = c("plots", "acc", "nacl"),
#'   glab = "NaCl"
#' )
#'
#' }

plot_standard_design <- function(data,
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
  
  if(!all(c("rows", "cols") %in% names(data))) {
    stop("Columns 'rows' and 'cols' are required.")
  }
  
  if(is.na(factor) || is.null(factor) || factor == "") {
    factor <- if("block" %in% names(data)) {
      "block"
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
    dplyr::mutate(
      .plot_factor = as.factor(.data[[factor]]),
      .plot_label = make_label(data = ., fill = fill)
    )
  
  design_type <- if("design" %in% names(data_plot)) {
    unique(data_plot$design)[1]
  } else {
    NA_character_
  }
  
  lengthfactor <- data_plot[[factor]] %>%
    as.factor() %>%
    nlevels()
  
  color_grps <- grDevices::colorRampPalette(
    c(
      "#86CD80", # green
      "#F4CB8C", # orange
      "#F3BB00", # yellow
      "#0198CD", # blue
      "#FE6673"  # red
    )
  )(lengthfactor)
  
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
  
  text_size <- dplyr::case_when(
    length(fill) == 1 ~ 3.5,
    length(fill) == 2 ~ 3.0,
    TRUE ~ 2.5
  )
  
  line_height <- dplyr::case_when(
    length(fill) == 1 ~ 1.05,
    length(fill) == 2 ~ 1.00,
    TRUE ~ 0.95
  )
  
  # -------------------------------------------------------------------------
  # RCBD / DBCA: blocks as rows ---------------------------------------------
  # -------------------------------------------------------------------------
  
  if(!is.na(design_type) &&
     design_type == "rcbd" &&
     "block" %in% names(data_plot)) {
    
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
        lineheight = line_height,
        color = "black"
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
      ggplot2::scale_fill_manual(values = color_grps) +
      ggplot2::labs(
        x = xlab,
        y = "blocks",
        fill = glab
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = legend,
        legend.title = ggplot2::element_text(face = "bold"),
        legend.text = ggplot2::element_text(size = 9),
        panel.grid = ggplot2::element_blank(),
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text = ggplot2::element_text(color = "grey25"),
        plot.margin = ggplot2::margin(6, 6, 6, 6)
      )
    
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
      lineheight = line_height,
      color = "black"
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
    ggplot2::scale_fill_manual(values = color_grps) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      fill = glab
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = legend,
      legend.title = ggplot2::element_text(face = "bold"),
      legend.text = ggplot2::element_text(size = 9),
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(color = "grey25"),
      plot.margin = ggplot2::margin(6, 6, 6, 6)
    )
  
  return(plot)
  
}
