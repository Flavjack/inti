#' Experimental design in CRD and RCBD
#'
#' Function to deploy field-book experiments for completely randomized designs
#' (CRD/DCA) and randomized complete block designs (RCBD/DBCA).
#'
#' @param nfactors Number of factors in the experiment `[numeric: 1]`.
#' @param factors Named list with the levels of each factor `[list]`.
#' @param type Type of experimental arrangement `[character: "crd", "rcbd"]`.
#'   The aliases `"dca"` and `"dbca"` are also accepted. The former `"lsd"`
#'   branch is intentionally disabled because it did not generate a valid Latin
#'   square design.
#' @param rep Number of replications or blocks in the experiment
#'   `[numeric: 3]`.
#' @param zigzag Arrange the physical layout in zigzag order `[logical: FALSE]`.
#' @param nrows Number of rows in the physical field layout. When missing, the
#'   number of replications or blocks is used.
#' @param serie Base number used to generate plot identifiers `[numeric: 1000]`.
#' @param seed Seed used for reproducible randomization `[numeric: NULL]`.
#' @param project Barcode prefix for data collection
#'   `[character: "inkaverse"]`.
#' @param qrcode Template used to concatenate QR-code fields
#'   `[character: "{project}{plots}"]`. The placeholder `{factors}` expands to
#'   all factor columns.
#'
#' @return A list with the field-book design and parameters.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#'
#' factores <- list(
#'   "geno" = c("A", "B", "C", "D", "D", 1, NA, NULL, "NA"),
#'   "salt stress" = c(0, 50, 200, 200, "T0", NA, NULL, "NULL"),
#'   "time" = c(30, 60, 90)
#' )
#'
#' fb <- design_repblock(
#'   nfactors = 3,
#'   factors = factores,
#'   type = "rcbd",
#'   rep = 5,
#'   zigzag = TRUE,
#'   seed = 123,
#'   nrows = 5,
#'   qrcode = "{project}{plots}"
#' )
#'
#' dsg <- fb$fieldbook
#'
#' fb %>%
#'   tarpuy_plotdesign(fill = "plots")
#'
#' fb$parameters
#'
#' }

design_repblock <- function(nfactors = 1,
                            factors,
                            type = "crd",
                            rep = 3,
                            zigzag = FALSE,
                            nrows = NA,
                            serie = 1000,
                            seed = NULL,
                            project = "inkaverse",
                            qrcode = "{project}{plots}") {
  
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
       value != floor(value)) {
      stop("'", name, "' must be a positive integer.", call. = FALSE)
    }
    
    as.integer(value)
  }
  
  clean_factor <- function(x) {
    if(is.list(x)) {
      x <- unlist(x, recursive = TRUE, use.names = FALSE)
    }
    
    x <- trimws(as.character(x))
    
    reserved <- x %in% c("", "NA", "NULL")
    x[reserved] <- NA_character_
    x <- x[!is.na(x)]
    
    unique(x)
  }
  
  build_qrcode <- function(data, template, factor_names) {
    if(length(template) != 1L ||
       is.na(template) ||
       !nzchar(trimws(template))) {
      stop("'qrcode' must be a non-empty character template.", call. = FALSE)
    }
    
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
        "'qrcode' must contain at least one field inside braces, ",
        "for example '{project}{plots}'.",
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
      trimws(x)
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
  
  if(length(zigzag) != 1L || is.na(zigzag) || !is.logical(zigzag)) {
    stop("'zigzag' must be TRUE or FALSE.", call. = FALSE)
  }
  
  if(!is.list(factors) || length(factors) == 0L) {
    stop("'factors' must be a non-empty named list.", call. = FALSE)
  }
  
  if(nfactors > length(factors)) {
    stop(
      "'nfactors' exceeds the number of supplied factors.",
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
      "Every factor must have a non-empty name.",
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
    "sort",
    "rep",
    "block",
    "rows",
    "cols",
    "design",
    "project",
    "icols"
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
    stop("'type' must be one of: 'crd' or 'rcbd'.", call. = FALSE)
  }
  
  if(length(type) != 1L) {
    stop("'type' must contain one value.", call. = FALSE)
  }
  
  type <- tolower(trimws(as.character(type)))
  
  type_aliases <- c(
    "dca" = "crd",
    "dbca" = "rcbd"
  )
  
  if(type %in% names(type_aliases)) {
    type <- unname(type_aliases[[type]])
  }
  
  if(identical(type, "lsd")) {
    stop(
      "The LSD branch is temporarily disabled because the previous ",
      "implementation did not generate a valid Latin square. Use CRD or RCBD ",
      "until LSD is reimplemented as an independent validated design.",
      call. = FALSE
    )
  }
  
  if(!type %in% c("crd", "rcbd")) {
    stop(
      "Unsupported design type: '",
      type,
      "'. Use 'crd' or 'rcbd'.",
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
  # Treatment catalog
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
  
  n_treatments <- nrow(treatment_catalog)
  n_plots <- n_treatments * rep
  
  if(!is.finite(n_plots) || n_plots > .Machine$integer.max) {
    stop(
      "The requested design contains too many experimental units.",
      call. = FALSE
    )
  }
  
  n_plots <- as.integer(n_plots)
  
  if(type == "rcbd" && n_treatments > serie) {
    stop(
      "'serie' must be greater than or equal to the number of treatments ",
      "per block to avoid duplicated plot identifiers. Treatments per block: ",
      n_treatments,
      "; serie: ",
      serie,
      ".",
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------------
  # Randomization
  # -------------------------------------------------------------------------
  
  if(type == "crd") {
    # Build the same pre-randomization order used by the original function:
    # replication 1 with all treatments, then replication 2, and so on.
    units <- treatment_catalog[
      base::rep(seq_len(n_treatments), times = rep),
      ,
      drop = FALSE
    ]
    
    units$rep <- base::rep(seq_len(rep), each = n_treatments)
    
    random_sort <- sample.int(n_plots)
    units$sort <- random_sort
    
    fb <- units[
      order(units$sort),
      ,
      drop = FALSE
    ]
    
    fb$plots <- serie + fb$sort
    
  } else {
    block_list <- lapply(seq_len(rep), function(block_id) {
      block_data <- treatment_catalog
      block_data$block <- block_id
      block_data$sort <- sample.int(n_treatments)
      
      block_data <- block_data[
        order(block_data$sort),
        ,
        drop = FALSE
      ]
      
      block_data$plots <- serie * block_id + block_data$sort
      block_data
    })
    
    fb <- do.call(rbind, block_list)
    rownames(fb) <- NULL
  }
  
  # -------------------------------------------------------------------------
  # Physical layout
  # -------------------------------------------------------------------------
  
  if(is_missing_scalar(nrows)) {
    nrows <- rep
  } else {
    nrows <- as_positive_integer(nrows, "nrows")
  }
  
  if(nrows > n_plots) {
    stop(
      "'nrows' cannot exceed the number of experimental units (",
      n_plots,
      ").",
      call. = FALSE
    )
  }
  
  ncols <- as.integer(ceiling(n_plots / nrows))
  
  fb$rows <- base::rep(
    seq_len(nrows),
    each = ncols
  )[seq_len(n_plots)]
  
  fb$cols <- base::rep(
    seq_len(ncols),
    times = nrows
  )[seq_len(n_plots)]
  
  if(isTRUE(zigzag)) {
    reverse_cols <- (ncols - fb$cols) + 1L
    
    fb$cols <- ifelse(
      fb$rows %% 2L == 0L,
      reverse_cols,
      fb$cols
    )
  }
  
  fb$rows <- as.integer(fb$rows)
  fb$cols <- as.integer(fb$cols)
  
  # -------------------------------------------------------------------------
  # QR code and output columns
  # -------------------------------------------------------------------------
  
  fb$project <- project
  fb$design <- type
  
  fb$qrcode <- build_qrcode(
    data = fb,
    template = qrcode,
    factor_names = factor_names
  )
  
  if(anyDuplicated(fb$plots)) {
    duplicate_plots <- unique(fb$plots[duplicated(fb$plots)])
    
    stop(
      "Duplicated plot identifiers were generated: ",
      paste(utils::head(duplicate_plots, 10L), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if(anyDuplicated(fb$qrcode)) {
    duplicate_qr <- unique(fb$qrcode[duplicated(fb$qrcode)])
    
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
  
  block_column <- if(type == "rcbd") "block" else "rep"
  
  output_columns <- c(
    "qrcode",
    "plots",
    "ntreat",
    factor_names,
    "sort",
    block_column,
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
      seed = seed,
      factornames = factor_names
    )
  )
  
  result
}