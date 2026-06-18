#' Experimental design: Augmented
#'
#' Fieldbook generator for Augmented Designs.
#'
#' @param checks Vector of check treatments.
#' @param entries Vector of new entries.
#' @param blocks Number of blocks.
#' @param block_size Number of plots per block.
#' @param random Randomize entries allocation.
#' @param zigzag Zigzag field layout.
#' @param dim Optional layout dimensions c(nrows,ncols).
#' @param serie Plot series number.
#' @param seed Random seed.
#' @param project Barcode prefix.
#' @param qrcode QR code template.
#'
#' @return List with fieldbook and parameters.
#' @export

design_augmented <- function(
    checks,
    entries,
    blocks = NULL,
    block_size = NULL,
    random = TRUE,
    zigzag = FALSE,
    dim = NA,
    serie = 1000,
    seed = NULL,
    project = "inkaverse",
    qrcode = "{project}{plots}{entry}"
) {
  
  if (!is.null(seed) && length(seed) == 1 && !is.na(seed) && seed != 0) {
    set.seed(seed)
  }
  
  # -------------------------------------------------------------------------
  # helpers
  # -------------------------------------------------------------------------
  
  clean_vec <- function(x) {
    
    if(is.list(x))
      x <- unlist(x, recursive = TRUE, use.names = FALSE)
    
    x <- as.character(x)
    
    x <- trimws(x)
    
    x[x %in% c("", "NA", "NULL")] <- NA_character_
    
    x <- stats::na.omit(x)
    
    x <- gsub("[[:space:]]+", "_", x)
    
    x <- gsub("[^[:alnum:]_]", "", x)
    
    unique(x)
    
  }
  
  balanced_sizes <- function(n_items, n_groups) {
    
    base_n <- n_items %/% n_groups
    
    extra_n <- n_items %% n_groups
    
    out <- rep(base_n, n_groups)
    
    if(extra_n > 0)
      out[seq_len(extra_n)] <- out[seq_len(extra_n)] + 1
    
    out
    
  }
  
  # -------------------------------------------------------------------------
  # cleaning
  # -------------------------------------------------------------------------
  
  checks <- clean_vec(checks)
  
  entries <- clean_vec(entries)
  
  if(length(checks) == 0)
    stop("At least one check is required.")
  
  if(length(entries) == 0)
    stop("At least one entry is required.")
  
  if(any(checks %in% entries))
    stop("Checks and entries must not overlap.")
  
  n_checks <- length(checks)
  
  n_entries <- length(entries)
  
  # -------------------------------------------------------------------------
  # block definition
  # -------------------------------------------------------------------------
  
  if(!is.null(blocks) && blocks < 1) {
    stop("'blocks' must be >= 1.")
  }
  
  if(!is.null(block_size) && block_size < 1) {
    stop("'block_size' must be >= 1.")
  } 
  
  
  if(!is.null(blocks) && !is.null(block_size)) {
    
    if(block_size <= n_checks)
      stop("'block_size' must be greater than number of checks.")
    
    entries_per_block <- balanced_sizes(n_entries, blocks)
    
    min_needed <- n_checks + max(entries_per_block)
    
    if(block_size < min_needed)
      stop(
        paste0(
          "'block_size' too small. Minimum required = ",
          min_needed
        )
      )
    
  }
  
  else if(!is.null(blocks) && is.null(block_size)) {
    
    entries_per_block <- balanced_sizes(n_entries, blocks)
    
    block_size <- n_checks + max(entries_per_block)
    
  }
  
  else if(is.null(blocks) && !is.null(block_size)) {
    
    if(block_size <= n_checks)
      stop("'block_size' must be greater than number of checks.")
    
    usable_slots <- block_size - n_checks
    
    blocks <- ceiling(n_entries / usable_slots)
    
    entries_per_block <- balanced_sizes(n_entries, blocks)
    
  }
  
  else {
    
    target_block_size <- max(n_checks + 6, 10)
    
    usable_slots <- target_block_size - n_checks
    
    blocks <- ceiling(n_entries / usable_slots)
    
    entries_per_block <- balanced_sizes(n_entries, blocks)
    
    block_size <- n_checks + max(entries_per_block)
    
  }
  
  # -------------------------------------------------------------------------
  # entries allocation
  # -------------------------------------------------------------------------
  
  if(isTRUE(random))
    entries <- sample(entries)
  
  split_entries <- vector("list", blocks)
  
  start_idx <- 1
  
  for(b in seq_len(blocks)) {
    
    n_b <- entries_per_block[b]
    
    end_idx <- start_idx + n_b - 1
    
    split_entries[[b]] <- if(n_b > 0)
      entries[start_idx:end_idx]
    else
      character(0)
    
    start_idx <- end_idx + 1
    
  }
  
  # -------------------------------------------------------------------------
  # fieldbook
  # -------------------------------------------------------------------------
  
  fb_list <- vector("list", blocks)
  
  treatment_levels <- c(checks, entries)
  
  for(b in seq_len(blocks)) {
    
    checks_df <- data.frame(
      entry = checks,
      type = "check",
      stringsAsFactors = FALSE
    )
    
    tests_df <- data.frame(
      entry = split_entries[[b]],
      type = "test",
      stringsAsFactors = FALSE
    )
    
    block_df <- rbind(checks_df, tests_df)
    
    if(isTRUE(random))
      block_df <- block_df[sample(nrow(block_df)), ]
    
    n_fill <- block_size - nrow(block_df)
    
    if(n_fill > 0) {
      
      filler <- data.frame(
        entry = rep(NA_character_, n_fill),
        type = rep(NA_character_, n_fill),
        stringsAsFactors = FALSE
      )
      
      block_df <- rbind(block_df, filler)
      
    }
    
    block_df$block <- b
    
    block_df$sort <- seq_len(nrow(block_df))
    
    block_df$plots <- serie * b + block_df$sort
    
    block_df$ntreat <- ifelse(
      is.na(block_df$entry),
      NA,
      match(block_df$entry, treatment_levels)
    )
    
    fb_list[[b]] <- block_df
    
  }
  
  fb <- do.call(rbind, fb_list)
  
  rownames(fb) <- NULL
  
  # -------------------------------------------------------------------------
  # layout
  # -------------------------------------------------------------------------
  
  total_plots <- nrow(fb)
  
  if(length(dim) == 1 && all(is.na(dim))) {
    
    nrows <- blocks
    
    ncols <- block_size
    
  } else {
    
    nrows <- dim[1]
    
    ncols <- dim[2]
    
    if((nrows * ncols) < total_plots)
      stop("dim is smaller than total plots.")
    
  }
  
  fb$rows <- rep(seq_len(nrows), each = ncols)[seq_len(nrow(fb))]
  
  fb$cols <- rep(seq_len(ncols), times = nrows)[seq_len(nrow(fb))]
  
  fb$icols <- (ncols - fb$cols) + 1
  
  if(isTRUE(zigzag)) {
    
    fb$cols <- ifelse(
      fb$rows %% 2 == 0,
      fb$icols,
      fb$cols
    )
    
  }
  
  # -------------------------------------------------------------------------
  # qrcode
  # -------------------------------------------------------------------------
  
  fb$project <- project
  
  qrcolumns <- qrcode %>%
    strsplit("\\}\\{") %>%
    unlist() %>%
    gsub("\\{|\\}", "", .) %>%
    trimws()
  
  fb$qrcode <- apply(
    fb[, intersect(qrcolumns, names(fb)), drop = FALSE],
    1,
    function(x) paste(x, collapse = "_")
  )
  
  # column checks
  
  fb$checks <- dplyr::case_when(
    fb$type == "check" ~ 1,
    fb$type == "test" ~ 0,
    TRUE ~ NA_real_
  )
  
  # column design
  fb$design <- "augmented"
  
  # -------------------------------------------------------------------------
  # output
  # -------------------------------------------------------------------------
  
  fieldbook <- fb %>%
    dplyr::select(
      qrcode,
      plots,
      ntreat,
      entry,
      type,
      checks,
      block,
      sort,
      rows,
      cols,
      design
    )
  
  parameters <- list(
    design = "augmented",
    checks = checks,
    entries = entries,
    blocks = blocks,
    block_size = block_size,
    entries_per_block = entries_per_block,
    random = random,
    zigzag = zigzag,
    dim = c(nrows, ncols),
    seed = seed
  )
  
  list(
    fieldbook = fieldbook,
    parameters = parameters
  )
  
}

