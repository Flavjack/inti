#' Split-plot RCBD experimental design
#'
#' Generate a split-plot design under a randomized complete block design (RCBD)
#' structure for Tarpuy.
#'
#' The first factor is interpreted as the whole-plot factor and the second factor
#' as the subplot factor. Factor column names are preserved in the final
#' fieldbook, while their experimental role is stored in `parameters`.
#'
#' @param nfactors Number of factors in the experiment. For split-plot RCBD it
#'   must be 2.
#' @param factors List with exactly two named factors. The first factor is the
#'   whole-plot factor and the second factor is the subplot factor.
#' @param type Design type. Default is `"split-rcbd"`.
#' @param rep Number of replications or blocks.
#' @param zigzag Field layout in vertical zigzag order. If `TRUE`, subplot row
#'   order is reversed in even whole-plot columns.
#' @param nrows Experimental design dimension by rows. If `NA`, it is calculated
#'   automatically as `rep * number_of_subplot_levels`.
#' @param serie Number used as base for plot numbering.
#' @param seed Seed for reproducible randomization.
#' @param project Barcode or QR code prefix.
#' @param qrcode String used to concatenate QR code fields.
#'
#' @return A list with the fieldbook design and parameters.
#'
#' @export

design_split_rcbd <- function(nfactors = 2,
                              factors,
                              type = "split-rcbd",
                              rep = 3,
                              zigzag = FALSE,
                              nrows = NA,
                              serie = 1000,
                              seed = NULL,
                              project = "inkaverse",
                              qrcode = "{project}{plots}{factors}") {
  
  # -------------------------------------------------------------------------
  # Initial settings 
  # -------------------------------------------------------------------------
  
  set.seed(seed)
  
  if(nfactors != 2) {
    stop("split-rcbd requires exactly 2 factors.")
  }
  
  # -------------------------------------------------------------------------
  # Factor cleaning
  # -------------------------------------------------------------------------
  
  dfactors <- factors %>%
    purrr::map(~ gsub("NA|NULL", NA, .)) %>%
    purrr::map(base::unique) %>%
    purrr::map(stats::na.omit) %>%
    purrr::map(~ gsub("[[:space:]]", ".", .)) %>%
    purrr::set_names(gsub("[[:space:]]", "_" , names(.))) %>%
    .[1:nfactors]
  
  name.factors <- names(dfactors)
  
  whole_plot <- name.factors[1]
  sub_plot   <- name.factors[2]
  
  wp_levels <- dfactors[[whole_plot]]
  sp_levels <- dfactors[[sub_plot]]
  
  n_wp <- length(wp_levels)
  n_sp <- length(sp_levels)
  
  if(n_wp == 0) stop("Whole-plot factor has no valid levels.")
  if(n_sp == 0) stop("Subplot factor has no valid levels.")
  
  # -------------------------------------------------------------------------
  # Design dimensions
  # -------------------------------------------------------------------------
  
  nrows <- if(anyNA(nrows)) n_sp * rep else nrows
  ncols <- n_wp
  
  # -------------------------------------------------------------------------
  # QR code fields
  # -------------------------------------------------------------------------
  
  qrcolumns <- qrcode %>%
    gsub("factors", paste0(name.factors, collapse = "\\}\\{"), .) %>%
    strsplit(split = "\\}\\{") %>%
    unlist() %>%
    gsub("\\{|\\}", "", .) %>%
    trimws()
  
  # -------------------------------------------------------------------------
  # Treatment catalog
  # -------------------------------------------------------------------------
  
  trt_catalog <- dfactors %>%
    expand.grid() %>%
    dplyr::mutate(ntreat = as.numeric(row.names(.))) %>%
    dplyr::mutate(
      wp_sp = paste(.data[[whole_plot]], .data[[sub_plot]], sep = "_")
    )
  
  key_map <- stats::setNames(
    trt_catalog$ntreat,
    paste(trt_catalog[[whole_plot]], trt_catalog[[sub_plot]], sep = "___")
  )
  
  # -------------------------------------------------------------------------
  # Split-plot RCBD randomization
  # -------------------------------------------------------------------------
  
  fb <- purrr::map_dfr(seq_len(rep), function(b) {
    
    wp_rand <- sample(wp_levels, size = n_wp, replace = FALSE)
    
    purrr::map_dfr(seq_len(n_wp), function(wp_i) {
      
      sp_rand <- sample(sp_levels, size = n_sp, replace = FALSE)
      
      tibble::tibble(
        block = b,
        whole_plot_order = wp_i,
        sub_plot_order = seq_len(n_sp),
        !!whole_plot := wp_rand[wp_i],
        !!sub_plot := sp_rand
      )
      
    })
    
  }) %>%
    dplyr::mutate(
      key = paste(.data[[whole_plot]], .data[[sub_plot]], sep = "___"),
      ntreat = unname(key_map[.data$key]),
      wp_sp = paste(.data[[whole_plot]], .data[[sub_plot]], sep = "_")
    ) %>%
    dplyr::select(!"key")
  
  # -------------------------------------------------------------------------
  # Field layout
  # -------------------------------------------------------------------------
  
  fb <- fb %>%
    dplyr::mutate(
      cols = .data$whole_plot_order,
      
      block_start_down = dplyr::case_when(
        isTRUE(zigzag) & n_wp %% 2 == 1 & .data$block %% 2 == 0 ~ TRUE,
        TRUE ~ FALSE
      ),
      
      walk = dplyr::case_when(
        !isTRUE(zigzag) ~ .data$sub_plot_order,
        
        !.data$block_start_down & .data$cols %% 2 == 1 ~ .data$sub_plot_order,
        !.data$block_start_down & .data$cols %% 2 == 0 ~ (n_sp - .data$sub_plot_order) + 1,
        
        .data$block_start_down & .data$cols %% 2 == 1 ~ (n_sp - .data$sub_plot_order) + 1,
        .data$block_start_down & .data$cols %% 2 == 0 ~ .data$sub_plot_order
      ),
      
      rows_local = .data$sub_plot_order,
      rows = ((.data$block - 1) * n_sp) + .data$rows_local
    ) %>%
    dplyr::arrange(.data$block, .data$cols, .data$walk) %>%
    dplyr::group_by(.data$block) %>%
    dplyr::mutate(
      sort = dplyr::row_number(),
      plots = serie * .data$block + .data$sort
    ) %>%
    dplyr::ungroup()
  
  # -------------------------------------------------------------------------
  # QR code and design label
  # -------------------------------------------------------------------------
  
  fb <- fb %>%
    dplyr::mutate(project = project) %>%
    tidyr::unite("qrcode", any_of(qrcolumns), sep = "_", remove = FALSE) %>%
    dplyr::mutate(design = type)
  
  # -------------------------------------------------------------------------
  # Output fieldbook
  # -------------------------------------------------------------------------
  
  fieldbook <- fb %>%
    dplyr::select(
      "qrcode",
      "plots",
      "ntreat",
      all_of(name.factors),
      "wp_sp",
      "block",
      "sort",
      "rows",
      "cols",
      "design"
    )
  
  # -------------------------------------------------------------------------
  # Result 
  # -------------------------------------------------------------------------
  
  result <- list(
    fieldbook = fieldbook,
    parameters = list(
      nfactors = nfactors,
      factors = dfactors,
      type = type,
      rep = rep,
      zigzag = zigzag,
      dim = c(nrows, ncols),
      seed = seed,
      factornames = name.factors,
      whole_plot = whole_plot,
      sub_plot = sub_plot
    )
  )
  
}



#' # Example
#' factors <- list(
#'   Soil = c("S1", "S2", "S3", "S4"),
#'   Fertilizer = c("N1", "N2", "N3", "N4", "N5", "N6")
#' )
#'
#' design_split_rcbd(
#'   factors = factors,
#'   rep = 3,
#'   zigzag = TRUE,
#'   seed = 123
#' )$fieldbook
#' 
#' 



# library(dplyr)
# 
# factors <- list(
#   Soil = c("S1", "S2", "S3"),
#   Fertilizer = c("N1", "N2", "N3", "N4", "N5")
# )
# 
# sp <- design_split_rcbd(
#   factors = factors,
#   rep = 3,
#   zigzag = TRUE,
#   seed = 123
# )
# 
# fb <- sp$fieldbook
# 
# head(fb)
# 
# 
# 
# 
# plot_split_rcbd <- function(data,
#                             factor,
#                             fill = "plots",
#                             xlab = "Whole plots",
#                             ylab = "Subplots",
#                             glab = NULL) {
# 
#   if(is.null(glab)) {
#     glab <- factor
#   }
# 
#   data_plot <- data %>%
#     dplyr::group_by(.data$block) %>%
#     dplyr::mutate(
#       row_block = dplyr::dense_rank(.data$rows)
#     ) %>%
#     dplyr::ungroup()
# 
#   ggplot2::ggplot(
#     data_plot,
#     ggplot2::aes(
#       x = .data$cols,
#       y = .data$row_block,
#       fill = as.factor(.data[[factor]])
#     )
#   ) +
#     ggplot2::geom_tile(
#       color = "black",
#       linewidth = 0.5
#     ) +
#     ggplot2::geom_text(
#       ggplot2::aes(label = .data[[fill]])
#     ) +
#     ggplot2::facet_wrap(
#       ~ block,
#       nrow = 1,
#       labeller = ggplot2::label_both
#     ) +
#     ggplot2::scale_y_reverse(
#       breaks = sort(unique(data_plot$row_block))
#     ) +
#     ggplot2::scale_x_continuous(
#       breaks = sort(unique(data_plot$cols))
#     ) +
#     ggplot2::labs(
#       x = xlab,
#       y = ylab,
#       fill = glab
#     ) +
#     ggplot2::theme_bw() +
#     ggplot2::theme(
#       legend.position = "top",
#       strip.background = ggplot2::element_rect(fill = "grey90"),
#       strip.text = ggplot2::element_text(face = "bold")
#     )
# }
# 
# 
# library(ggplot2)
# 
# w <- plot_split_rcbd(
#   data = fb,
#   factor = "Soil",
#   fill = "plots",
#   glab = "Whole plot"
# )
# 
# w
# 
