#' Experimental design: Augmented
#'
#' Fieldbook generator for Augmented Designs.
#'
#' @param checks Vector of check treatments.
#' @param entries Vector of new entries.
#' @param blocks Optional number of blocks. If `NULL`, it is calculated from
#'   `entries`, `checks` and `eu_block`.
#' @param eu_block Number of experimental units per block.
#' @param random Randomize entries allocation.
#' @param zigzag Zigzag field layout.
#' @param dim Optional layout dimensions c(nrows, ncols).
#' @param serie Plot series number.
#' @param seed Random seed.
#' @param project Barcode prefix.
#' @param qrcode QR code template.
#'
#' @return List with fieldbook and parameters.
#'
#' @export

design_augmented <- function(
    checks,
    entries,
    blocks = NULL,
    eu_block = NULL,
    random = TRUE,
    zigzag = FALSE,
    dim = NA,
    serie = 1000,
    seed = NULL,
    project = "inkaverse",
    qrcode = "{project}{plots}{entry}"
) {
  
  # -------------------------------------------------------------------------
  # Initial settings ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(!is.null(seed) && length(seed) == 1 && !is.na(seed) && seed != 0) {
    set.seed(seed)
  }
  
  # -------------------------------------------------------------------------
  # Helpers -----------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  clean_vec <- function(x) {
    
    if(is.list(x)) {
      x <- unlist(x, recursive = TRUE, use.names = FALSE)
    }
    
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
    
    if(extra_n > 0) {
      out[seq_len(extra_n)] <- out[seq_len(extra_n)] + 1
    }
    
    out
    
  }
  
  sequential_sizes <- function(n_items, n_groups, capacity) {
    
    out <- integer(n_groups)
    
    remaining <- n_items
    
    for(i in seq_len(n_groups)) {
      
      out[i] <- min(capacity, remaining)
      
      remaining <- remaining - out[i]
      
    }
    
    out
    
  }
  
  # -------------------------------------------------------------------------
  # Cleaning ----------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  checks <- clean_vec(checks)
  
  entries <- clean_vec(entries)
  
  if(length(checks) == 0) {
    stop("At least one check is required.")
  }
  
  if(length(entries) == 0) {
    stop("At least one entry is required.")
  }
  
  if(any(checks %in% entries)) {
    stop("Checks and entries must not overlap.")
  }
  
  n_checks <- length(checks)
  
  n_entries <- length(entries)
  
  # -------------------------------------------------------------------------
  # Block definition ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(!is.null(blocks) && blocks < 1) {
    stop("'blocks' must be >= 1.")
  }
  
  if(!is.null(eu_block) && eu_block < 1) {
    stop("'eu_block' must be >= 1.")
  }
  
  if(!is.null(blocks) && !is.null(eu_block)) {
    
    if(eu_block <= n_checks) {
      stop("'eu_block' must be greater than number of checks.")
    }
    
    entries_per_block <- balanced_sizes(n_entries, blocks)
    
    min_needed <- n_checks + max(entries_per_block)
    
    if(eu_block < min_needed) {
      stop(
        paste0(
          "'eu_block' too small. Minimum required = ",
          min_needed
        )
      )
    }
    
  } else if(!is.null(blocks) && is.null(eu_block)) {
    
    entries_per_block <- balanced_sizes(n_entries, blocks)
    
    eu_block <- n_checks + max(entries_per_block)
    
  } else if(is.null(blocks) && !is.null(eu_block)) {
    
    if(eu_block <= n_checks) {
      stop("'eu_block' must be greater than number of checks.")
    }
    
    usable_slots <- eu_block - n_checks
    
    blocks <- ceiling(n_entries / usable_slots)
    
    entries_per_block <- sequential_sizes(
      n_items = n_entries,
      n_groups = blocks,
      capacity = usable_slots
    )
    
  } else {
    
    eu_block <- max(n_checks + 6, 10)
    
    usable_slots <- eu_block - n_checks
    
    blocks <- ceiling(n_entries / usable_slots)
    
    entries_per_block <- sequential_sizes(
      n_items = n_entries,
      n_groups = blocks,
      capacity = usable_slots
    )
    
  }
  
  # -------------------------------------------------------------------------
  # Entries allocation -------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(isTRUE(random)) {
    entries <- sample(entries)
  }
  
  split_entries <- vector("list", blocks)
  
  start_idx <- 1
  
  for(b in seq_len(blocks)) {
    
    n_b <- entries_per_block[b]
    
    end_idx <- start_idx + n_b - 1
    
    split_entries[[b]] <- if(n_b > 0) {
      entries[start_idx:end_idx]
    } else {
      character(0)
    }
    
    start_idx <- end_idx + 1
    
  }
  
  # -------------------------------------------------------------------------
  # Fieldbook ---------------------------------------------------------------
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
    
    if(isTRUE(random)) {
      block_df <- block_df[sample(nrow(block_df)), ]
    }
    
    n_fill <- eu_block - nrow(block_df)
    
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
  # Layout ------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  total_plots <- nrow(fb)
  
  if(length(dim) == 1 && all(is.na(dim))) {
    
    nrows <- blocks
    
    ncols <- eu_block
    
  } else {
    
    nrows <- dim[1]
    
    ncols <- dim[2]
    
    if((nrows * ncols) < total_plots) {
      stop("dim is smaller than total plots.")
    }
    
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
  # QR code -----------------------------------------------------------------
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
  
  # -------------------------------------------------------------------------
  # Checks column ------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  fb$checks <- dplyr::case_when(
    fb$type == "check" ~ 1,
    fb$type == "test" ~ 0,
    TRUE ~ NA_real_
  )
  
  # -------------------------------------------------------------------------
  # Design label -------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  fb$design <- "augmented"
  
  # -------------------------------------------------------------------------
  # Output ------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  fieldbook <- fb %>%
    dplyr::select(
      dplyr::all_of(c(
        "qrcode",
        "plots",
        "ntreat",
        "entry",
        "type",
        "checks",
        "block",
        "sort",
        "rows",
        "cols",
        "design"
      ))
    )
  
  parameters <- list(
    design = "augmented",
    checks = checks,
    entries = entries,
    blocks = blocks,
    eu_block = eu_block,
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
