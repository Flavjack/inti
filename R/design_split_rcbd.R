#' Split-plot RCBD experimental design
#'
#' Generate a split-plot design under a randomized complete block design (RCBD)
#' structure for TARPUY.
#'
#' The first factor is the whole-plot factor and the second factor is the
#' subplot factor. Whole plots are randomized within each block and subplot
#' levels are randomized independently within every whole plot.
#'
#' @param nfactors Number of factors in the experiment. Splitplot-RCBD requires
#'   exactly two factors.
#' @param factors Named list with the factor levels. The first factor is the
#'   whole-plot factor and the second factor is the subplot factor.
#' @param type Design type. The canonical value is `"split-rcbd"`; accepted
#'   aliases are normalized by `normalize_tarpuy_design_type()`.
#' @param rep Number of replications or blocks.
#' @param zigzag Logical. If `TRUE`, plot numbering follows a continuous
#'   vertical serpentine path through the whole plots and blocks.
#' @param nrows Number of rows in the complete physical layout. The valid
#'   Splitplot-RCBD geometry is `rep * number_of_subplot_levels`; when missing,
#'   it is calculated automatically.
#' @param serie Base number used to generate plot identifiers. For example,
#'   `serie = 1000` generates plots 1001, 1002, ... in block 1 and 2001,
#'   2002, ... in block 2.
#' @param seed Seed used for reproducible randomization. `NA` or `NULL` leaves
#'   the current random-number state unchanged.
#' @param project Barcode or QR-code prefix.
#' @param qrcode Template used to concatenate QR-code fields. The placeholder
#'   `{factors}` expands to both experimental factors.
#'
#' @return A list with `fieldbook` and `parameters`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
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
#' }

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
  # Helpers
  # -------------------------------------------------------------------------
  
  is_missing_scalar <- function(x) {
    is.null(x) ||
      length(x) == 0L ||
      (length(x) == 1L && is.na(x)) ||
      (length(x) == 1L && is.character(x) && !nzchar(trimws(x)))
  }
  
  as_positive_integer <- function(x, name) {
    value <- suppressWarnings(as.numeric(as.character(x)))
    
    if(length(value) != 1L ||
       is.na(value) ||
       !is.finite(value) ||
       value < 1 ||
       value != floor(value) ||
       value > .Machine$integer.max) {
      stop("'", name, "' must be a positive integer.", call. = FALSE)
    }
    
    as.integer(value)
  }
  
  clean_factor <- function(x) {
    if(is.list(x)) {
      x <- unlist(x, recursive = TRUE, use.names = FALSE)
    }
    
    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "NULL")] <- NA_character_
    x <- x[!is.na(x)]
    
    # Preserve the labels used by the experiment. Only repeated internal
    # whitespace is normalized; treatment values are not converted to codes.
    x <- gsub("[[:space:]]+", " ", x)
    
    unique(x)
  }
  
  build_qrcode <- function(data, template, factor_names) {
    if(length(template) != 1L ||
       is.na(template) ||
       !nzchar(trimws(as.character(template)))) {
      stop(
        "'qrcode' must be a non-empty character template.",
        call. = FALSE
      )
    }
    
    template <- trimws(as.character(template))
    
    factor_template <- paste0(
      "{",
      factor_names,
      "}",
      collapse = ""
    )
    
    template <- gsub(
      "{factors}",
      factor_template,
      template,
      fixed = TRUE
    )
    
    tokens <- regmatches(
      template,
      gregexpr("\\{[^{}]+\\}", template)
    )[[1L]]
    
    if(length(tokens) == 0L || identical(tokens, character(0))) {
      stop(
        "'qrcode' must contain at least one field inside braces, for ",
        "example '{project}{plots}{factors}'.",
        call. = FALSE
      )
    }
    
    qrcolumns <- gsub("^\\{|\\}$", "", tokens)
    missing_columns <- setdiff(qrcolumns, names(data))
    
    if(length(missing_columns) > 0L) {
      stop(
        "Unknown QR-code columns: ",
        paste(missing_columns, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    
    qr_data <- data[, qrcolumns, drop = FALSE]
    qr_data[] <- lapply(qr_data, function(x) {
      x <- as.character(x)
      x[is.na(x)] <- ""
      x <- trimws(x)
      
      # Clean only the QR representation. Factor labels in the fieldbook are
      # preserved exactly as experimental information.
      gsub("[[:space:]]+", "_", x)
    })
    
    qrcode_values <- apply(
      qr_data,
      1L,
      function(x) paste(x[nzchar(x)], collapse = "_")
    )
    
    if(any(!nzchar(qrcode_values))) {
      stop(
        "The QR-code template generated one or more empty identifiers.",
        call. = FALSE
      )
    }
    
    qrcode_values
  }
  
  # -------------------------------------------------------------------------
  # Argument validation
  # -------------------------------------------------------------------------
  
  nfactors <- as_positive_integer(nfactors, "nfactors")
  rep <- as_positive_integer(rep, "rep")
  serie <- as_positive_integer(serie, "serie")
  
  if(nfactors != 2L) {
    stop(
      "Splitplot-RCBD requires exactly 2 factors.",
      call. = FALSE
    )
  }
  
  if(length(zigzag) != 1L || is.na(zigzag) || !is.logical(zigzag)) {
    stop("'zigzag' must be TRUE or FALSE.", call. = FALSE)
  }
  
  if(!is.list(factors) || length(factors) < nfactors) {
    stop(
      "'factors' must be a named list containing the whole-plot and ",
      "subplot factors.",
      call. = FALSE
    )
  }
  
  factors <- factors[seq_len(nfactors)]
  original_names <- names(factors)
  
  if(is.null(original_names) ||
     length(original_names) != nfactors ||
     any(is.na(original_names)) ||
     any(!nzchar(trimws(original_names)))) {
    stop(
      "Both factors must have non-empty names.",
      call. = FALSE
    )
  }
  
  factor_names <- gsub(
    "[[:space:]]+",
    "_",
    trimws(original_names)
  )
  
  if(anyDuplicated(factor_names)) {
    duplicated_names <- unique(
      factor_names[duplicated(factor_names)]
    )
    
    stop(
      "Factor names are duplicated after replacing spaces with underscores: ",
      paste(duplicated_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  reserved_columns <- c(
    "qrcode",
    "plots",
    "ntreat",
    "wp_sp",
    "block",
    "sort",
    "rows",
    "cols",
    "design",
    "project",
    "whole_plot_order",
    "sub_plot_order",
    "rows_local",
    "walk"
  )
  
  conflicting_names <- intersect(factor_names, reserved_columns)
  
  if(length(conflicting_names) > 0L) {
    stop(
      "Factor names conflict with reserved fieldbook columns: ",
      paste(conflicting_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  names(factors) <- factor_names
  dfactors <- lapply(factors, clean_factor)
  names(dfactors) <- factor_names
  
  empty_factors <- factor_names[lengths(dfactors) == 0L]
  
  if(length(empty_factors) > 0L) {
    stop(
      "Factors without valid levels: ",
      paste(empty_factors, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if(is_missing_scalar(type)) {
    stop("'type' must be 'split-rcbd'.", call. = FALSE)
  }
  
  if(length(type) != 1L) {
    stop("'type' must contain one value.", call. = FALSE)
  }
  
  type <- normalize_tarpuy_design_type(type)
  
  if(!identical(type, "split-rcbd")) {
    stop(
      "Unsupported Splitplot-RCBD identifier: '",
      type,
      "'.",
      call. = FALSE
    )
  }
  
  if(!is_missing_scalar(seed)) {
    seed_value <- suppressWarnings(as.numeric(as.character(seed)))
    
    if(length(seed_value) != 1L ||
       is.na(seed_value) ||
       !is.finite(seed_value) ||
       seed_value < 0 ||
       seed_value != floor(seed_value) ||
       seed_value > .Machine$integer.max) {
      stop(
        "'seed' must be a non-negative integer, NA, or NULL.",
        call. = FALSE
      )
    }
    
    seed <- as.integer(seed_value)
    set.seed(seed)
  } else {
    seed <- NULL
  }
  
  if(length(project) != 1L) {
    stop("'project' must contain one value.", call. = FALSE)
  }
  
  project <- as.character(project)
  
  if(is.na(project)) {
    project <- ""
  }
  
  # -------------------------------------------------------------------------
  # Factor roles and dimensions
  # -------------------------------------------------------------------------
  
  whole_plot <- factor_names[1L]
  sub_plot <- factor_names[2L]
  
  wp_levels <- dfactors[[whole_plot]]
  sp_levels <- dfactors[[sub_plot]]
  
  n_wp <- length(wp_levels)
  n_sp <- length(sp_levels)
  units_per_block <- n_wp * n_sp
  total_units <- rep * units_per_block
  
  if(!is.finite(units_per_block) ||
     !is.finite(total_units) ||
     total_units > .Machine$integer.max) {
    stop(
      "The requested Splitplot-RCBD contains too many experimental units.",
      call. = FALSE
    )
  }
  
  units_per_block <- as.integer(units_per_block)
  total_units <- as.integer(total_units)
  
  if(units_per_block > serie) {
    stop(
      "'serie' must be greater than or equal to the number of experimental ",
      "units per block to avoid duplicated plot identifiers. Units per block: ",
      units_per_block,
      "; serie: ",
      serie,
      ".",
      call. = FALSE
    )
  }
  
  expected_nrows <- as.integer(rep * n_sp)
  
  if(is_missing_scalar(nrows)) {
    nrows <- expected_nrows
  } else {
    nrows <- as_positive_integer(nrows, "nrows")
    
    if(nrows != expected_nrows) {
      stop(
        "For Splitplot-RCBD, 'nrows' must equal replications multiplied by ",
        "subplot levels so that every whole plot remains contiguous. Expected ",
        "nrows: ",
        expected_nrows,
        ".",
        call. = FALSE
      )
    }
  }
  
  ncols <- as.integer(n_wp)
  
  # -------------------------------------------------------------------------
  # Treatment catalogue
  # -------------------------------------------------------------------------
  
  treatment_catalog <- do.call(
    base::expand.grid,
    c(
      dfactors,
      list(
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    )
  )
  
  names(treatment_catalog) <- factor_names
  treatment_catalog$ntreat <- seq_len(nrow(treatment_catalog))
  treatment_catalog$wp_sp <- paste(
    treatment_catalog[[whole_plot]],
    treatment_catalog[[sub_plot]],
    sep = "_"
  )
  
  # -------------------------------------------------------------------------
  # Hierarchical randomization
  # -------------------------------------------------------------------------
  
  block_list <- lapply(seq_len(rep), function(block_id) {
    randomized_wp <- sample(
      wp_levels,
      size = n_wp,
      replace = FALSE
    )
    
    whole_plot_list <- lapply(seq_len(n_wp), function(wp_position) {
      randomized_sp <- sample(
        sp_levels,
        size = n_sp,
        replace = FALSE
      )
      
      out <- data.frame(
        block = rep.int(block_id, n_sp),
        whole_plot_order = rep.int(wp_position, n_sp),
        sub_plot_order = seq_len(n_sp),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      out[[whole_plot]] <- rep.int(randomized_wp[wp_position], n_sp)
      out[[sub_plot]] <- randomized_sp
      out
    })
    
    block_data <- do.call(rbind, whole_plot_list)
    rownames(block_data) <- NULL
    block_data
  })
  
  fb <- do.call(rbind, block_list)
  rownames(fb) <- NULL
  
  wp_index <- match(fb[[whole_plot]], wp_levels)
  sp_index <- match(fb[[sub_plot]], sp_levels)
  
  fb$ntreat <- wp_index + (sp_index - 1L) * n_wp
  fb$wp_sp <- paste(
    fb[[whole_plot]],
    fb[[sub_plot]],
    sep = "_"
  )
  
  if(anyNA(fb$ntreat)) {
    stop(
      "Internal error: one or more Splitplot-RCBD treatments could not be ",
      "matched to the treatment catalogue.",
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------------
  # Physical layout and plot numbering
  # -------------------------------------------------------------------------
  
  fb$cols <- as.integer(fb$whole_plot_order)
  
  block_start_down <- isTRUE(zigzag) &
    (n_wp %% 2L == 1L) &
    (fb$block %% 2L == 0L)
  
  fb$walk <- if(!isTRUE(zigzag)) {
    fb$sub_plot_order
  } else {
    reverse_in_column <- xor(
      fb$cols %% 2L == 0L,
      block_start_down
    )
    
    ifelse(
      reverse_in_column,
      (n_sp - fb$sub_plot_order) + 1L,
      fb$sub_plot_order
    )
  }
  
  fb$rows_local <- as.integer(fb$sub_plot_order)
  fb$rows <- as.integer(
    ((fb$block - 1L) * n_sp) + fb$rows_local
  )
  
  fb <- fb[
    order(fb$block, fb$cols, fb$walk),
    ,
    drop = FALSE
  ]
  
  fb$sort <- ave(
    seq_len(nrow(fb)),
    fb$block,
    FUN = seq_along
  )
  
  fb$sort <- as.integer(fb$sort)
  fb$plots <- as.numeric(serie) * fb$block + fb$sort
  
  # -------------------------------------------------------------------------
  # QR code and output
  # -------------------------------------------------------------------------
  
  fb$project <- project
  fb$design <- type
  fb$qrcode <- build_qrcode(
    data = fb,
    template = qrcode,
    factor_names = factor_names
  )
  
  if(anyDuplicated(fb$plots)) {
    duplicate_plots <- unique(
      fb$plots[duplicated(fb$plots)]
    )
    
    stop(
      "Duplicated plot identifiers were generated: ",
      paste(utils::head(duplicate_plots, 10L), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if(anyDuplicated(fb$qrcode)) {
    duplicate_qr <- unique(
      fb$qrcode[duplicated(fb$qrcode)]
    )
    
    stop(
      "Duplicated QR-code identifiers were generated: ",
      paste(utils::head(duplicate_qr, 10L), collapse = ", "),
      ". Modify the QR-code template so that every experimental unit is unique.",
      call. = FALSE
    )
  }
  
  coordinates <- paste(fb$rows, fb$cols, sep = ":")
  
  if(anyDuplicated(coordinates)) {
    stop(
      "Internal error: duplicated row/column coordinates were generated.",
      call. = FALSE
    )
  }
  
  expected_treatments <- seq_len(nrow(treatment_catalog))
  
  treatment_check <- split(fb$ntreat, fb$block)
  valid_treatments <- all(vapply(
    treatment_check,
    function(x) identical(sort(as.integer(x)), expected_treatments),
    logical(1)
  ))
  
  if(!valid_treatments) {
    stop(
      "Internal error: each block must contain every whole-plot/subplot ",
      "treatment combination exactly once.",
      call. = FALSE
    )
  }
  
  whole_plot_check <- split(
    fb[[whole_plot]],
    interaction(fb$block, fb$whole_plot_order, drop = TRUE)
  )
  
  if(any(vapply(whole_plot_check, function(x) length(unique(x)) != 1L, logical(1)))) {
    stop(
      "Internal error: a whole plot contains more than one whole-plot level.",
      call. = FALSE
    )
  }
  
  subplot_check <- split(
    fb[[sub_plot]],
    interaction(fb$block, fb$whole_plot_order, drop = TRUE)
  )
  
  valid_subplots <- all(vapply(
    subplot_check,
    function(x) identical(sort(unique(as.character(x))), sort(sp_levels)),
    logical(1)
  ))
  
  if(!valid_subplots) {
    stop(
      "Internal error: every whole plot must contain each subplot level ",
      "exactly once.",
      call. = FALSE
    )
  }
  
  output_columns <- c(
    "qrcode",
    "plots",
    "ntreat",
    factor_names,
    "wp_sp",
    "block",
    "sort",
    "rows",
    "cols",
    "design"
  )
  
  fieldbook <- tibble::as_tibble(
    fb[, output_columns, drop = FALSE]
  )
  
  result <- list(
    fieldbook = fieldbook,
    parameters = list(
      nfactors = nfactors,
      factors = dfactors,
      type = type,
      rep = rep,
      zigzag = zigzag,
      dim = c(nrows, ncols),
      block_dim = c(n_sp, n_wp),
      seed = seed,
      serie = serie,
      project = project,
      qrcode = qrcode,
      factornames = factor_names,
      whole_plot = whole_plot,
      sub_plot = sub_plot,
      factor_roles = c(
        whole_plot = whole_plot,
        subplot = sub_plot
      ),
      whole_plots_per_block = n_wp,
      subplots_per_whole_plot = n_sp,
      units_per_block = units_per_block,
      total_units = total_units
    )
  )
  
  result
}