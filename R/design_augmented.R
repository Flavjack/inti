#' Experimental design: Augmented
#'
#' Fieldbook generator for Augmented Designs.
#'
#' @param checks Vector of check treatments.
#' @param entries Vector of new entries.
#' @param blocks Optional number of blocks. If `NULL`, it is calculated from
#'   `entries`, `checks` and `eu_block`.
#' @param eu_block Number of experimental units per block.
#' @param random Randomize entries allocation and positions inside each block.
#' @param zigzag Zigzag field layout.
#' @param dim Optional layout dimensions c(nrows, ncols).
#' @param serie Plot series number.
#' @param seed Random seed. `0` or `NULL` means no fixed seed.
#' @param project Barcode prefix.
#' @param qrcode QR code column template.
#' @param separate_checks Logical. When possible, prevent adjacent checks
#'   inside each block using constrained randomization.
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
    qrcode = "{project}{plots}{entry}",
    separate_checks = TRUE
) {
  
  # -------------------------------------------------------------------------
  # Helpers -----------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  clean_vec <- function(x) {
    
    if(is.list(x)) {
      x <- unlist(x, recursive = TRUE, use.names = FALSE)
    }
    
    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "NULL")] <- NA_character_
    x <- x[!is.na(x)]
    x <- gsub("[[:space:]]+", "_", x)
    x <- gsub("[^[:alnum:]_]", "", x)
    x <- x[nzchar(x)]
    
    unique(x)
  }
  
  as_positive_integer <- function(x, name, allow_null = TRUE) {
    
    is_missing <- is.null(x) ||
      length(x) == 0L ||
      (
        length(x) == 1L &&
          (
            is.na(x) ||
              (is.character(x) && !nzchar(trimws(x)))
          )
      )
    
    if(isTRUE(allow_null) && is_missing) {
      return(NULL)
    }
    
    if(
      length(x) != 1L ||
      is.na(x) ||
      !is.numeric(x) ||
      !is.finite(x) ||
      x < 1 ||
      x != floor(x)
    ) {
      stop("'", name, "' must be a positive integer.")
    }
    
    as.integer(x)
  }
  
  balanced_sizes <- function(n_items, n_groups) {
    
    base_n <- n_items %/% n_groups
    extra_n <- n_items %% n_groups
    
    out <- rep.int(base_n, n_groups)
    
    if(extra_n > 0L) {
      out[seq_len(extra_n)] <- out[seq_len(extra_n)] + 1L
    }
    
    out
  }
  
  arrange_block <- function(block_df, random, separate_checks) {
    
    n_total <- nrow(block_df)
    
    if(n_total <= 1L) {
      return(block_df)
    }
    
    check_rows <- which(block_df$type == "check")
    other_rows <- setdiff(seq_len(n_total), check_rows)
    n_checks_block <- length(check_rows)
    
    # Constrained placement: checks are non-adjacent whenever the block has
    # enough non-check positions. Tests and empty positions fill the rest.
    can_separate <- isTRUE(separate_checks) &&
      n_checks_block > 0L &&
      n_total >= (2L * n_checks_block - 1L)
    
    if(can_separate) {
      
      checks_part <- block_df[check_rows, , drop = FALSE]
      others_part <- block_df[other_rows, , drop = FALSE]
      
      if(isTRUE(random)) {
        checks_part <- checks_part[
          sample.int(nrow(checks_part)),
          ,
          drop = FALSE
        ]
        
        if(nrow(others_part) > 1L) {
          others_part <- others_part[
            sample.int(nrow(others_part)),
            ,
            drop = FALSE
          ]
        }
        
        # Bijection that samples positions with no adjacent checks.
        base_positions <- sort(
          sample.int(
            n = n_total - n_checks_block + 1L,
            size = n_checks_block,
            replace = FALSE
          )
        )
        
        check_positions <- base_positions + seq_len(n_checks_block) - 1L
        
      } else {
        
        check_positions <- round(
          seq(1L, n_total, length.out = n_checks_block)
        )
      }
      
      out <- block_df
      
      out[check_positions, ] <- checks_part
      out[-check_positions, ] <- others_part
      rownames(out) <- NULL
      
      return(out)
    }
    
    if(isTRUE(random)) {
      block_df <- block_df[
        sample.int(n_total),
        ,
        drop = FALSE
      ]
    }
    
    rownames(block_df) <- NULL
    block_df
  }
  
  dim_missing <- is.null(dim) ||
    (length(dim) == 1L && is.na(dim))
  
  # -------------------------------------------------------------------------
  # Initial settings ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  seed_missing <- is.null(seed) ||
    length(seed) == 0L ||
    (length(seed) == 1L && is.na(seed))
  
  if(!seed_missing) {
    
    if(
      length(seed) != 1L ||
      !is.numeric(seed) ||
      !is.finite(seed)
    ) {
      stop("'seed' must be a finite numeric scalar, 0, NA, or NULL.")
    }
    
    if(seed != 0) {
      set.seed(as.integer(seed))
    }
  }
  
  if(
    length(random) != 1L ||
    is.na(random) ||
    !is.logical(random)
  ) {
    stop("'random' must be TRUE or FALSE.")
  }
  
  if(
    length(zigzag) != 1L ||
    is.na(zigzag) ||
    !is.logical(zigzag)
  ) {
    stop("'zigzag' must be TRUE or FALSE.")
  }
  
  if(
    length(separate_checks) != 1L ||
    is.na(separate_checks) ||
    !is.logical(separate_checks)
  ) {
    stop("'separate_checks' must be TRUE or FALSE.")
  }
  
  blocks <- as_positive_integer(blocks, "blocks", allow_null = TRUE)
  eu_block <- as_positive_integer(eu_block, "eu_block", allow_null = TRUE)
  serie <- as_positive_integer(serie, "serie", allow_null = FALSE)
  
  # -------------------------------------------------------------------------
  # Cleaning ----------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  checks <- clean_vec(checks)
  entries <- clean_vec(entries)
  
  if(length(checks) == 0L) {
    stop("At least one check is required.")
  }
  
  if(length(entries) == 0L) {
    stop("At least one entry is required.")
  }
  
  if(any(checks %in% entries)) {
    stop("Checks and entries must not overlap.")
  }
  
  n_checks <- length(checks)
  n_entries <- length(entries)
  
  # Preserve the original treatment order. It must not depend on seed.
  entry_levels <- entries
  treatment_levels <- c(checks, entry_levels)
  
  # -------------------------------------------------------------------------
  # Block definition ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  # Automatic behavior remains available when both values are absent.
  if(is.null(blocks) && is.null(eu_block)) {
    eu_block <- max(n_checks + 6L, 10L)
  }
  
  if(!is.null(eu_block) && eu_block <= n_checks) {
    stop("'eu_block' must be greater than number of checks.")
  }
  
  # Calculate the minimum number of blocks from the available test slots.
  if(is.null(blocks)) {
    
    usable_slots <- eu_block - n_checks
    blocks <- as.integer(ceiling(n_entries / usable_slots))
  }
  
  # Entries are balanced in every branch. This prevents all empty positions
  # from accumulating in the final block.
  entries_per_block <- balanced_sizes(
    n_items = n_entries,
    n_groups = blocks
  )
  
  # If block capacity is omitted, calculate the smallest equal block size.
  if(is.null(eu_block)) {
    eu_block <- n_checks + max(entries_per_block)
  }
  
  usable_slots <- eu_block - n_checks
  min_needed <- n_checks + max(entries_per_block)
  
  if(max(entries_per_block) > usable_slots) {
    stop(
      "'eu_block' too small. Minimum required = ",
      min_needed
    )
  }
  
  if(blocks > 1L && serie < eu_block) {
    stop(
      "'serie' must be greater than or equal to 'eu_block' ",
      "to avoid duplicated plot IDs."
    )
  }
  
  # -------------------------------------------------------------------------
  # Entries allocation -------------------------------------------------------
  # -------------------------------------------------------------------------
  
  allocation_entries <- if(isTRUE(random)) {
    sample(entry_levels)
  } else {
    entry_levels
  }
  
  split_entries <- vector("list", blocks)
  start_idx <- 1L
  
  for(b in seq_len(blocks)) {
    
    n_b <- entries_per_block[b]
    
    if(n_b > 0L) {
      end_idx <- start_idx + n_b - 1L
      split_entries[[b]] <- allocation_entries[start_idx:end_idx]
      start_idx <- end_idx + 1L
    } else {
      split_entries[[b]] <- character(0)
    }
  }
  
  # -------------------------------------------------------------------------
  # Fieldbook ----------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  fb_list <- vector("list", blocks)
  
  for(b in seq_len(blocks)) {
    
    checks_df <- data.frame(
      entry = checks,
      type = rep("check", n_checks),
      stringsAsFactors = FALSE
    )
    
    tests_df <- data.frame(
      entry = split_entries[[b]],
      type = rep("test", length(split_entries[[b]])),
      stringsAsFactors = FALSE
    )
    
    n_fill <- eu_block - nrow(checks_df) - nrow(tests_df)
    
    filler <- data.frame(
      entry = rep(NA_character_, n_fill),
      type = rep(NA_character_, n_fill),
      stringsAsFactors = FALSE
    )
    
    # Empty positions must exist before the within-block arrangement.
    block_df <- rbind(
      checks_df,
      tests_df,
      filler
    )
    
    block_df <- arrange_block(
      block_df = block_df,
      random = random,
      separate_checks = separate_checks
    )
    
    block_df$block <- b
    block_df$sort <- seq_len(nrow(block_df))
    block_df$plots <- serie * b + block_df$sort
    
    block_df$ntreat <- ifelse(
      is.na(block_df$entry),
      NA_integer_,
      match(block_df$entry, treatment_levels)
    )
    
    fb_list[[b]] <- block_df
  }
  
  fb <- do.call(rbind, fb_list)
  rownames(fb) <- NULL
  
  # -------------------------------------------------------------------------
  # Layout -------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  total_plots <- nrow(fb)
  
  if(dim_missing) {
    
    # Default augmented layout: one complete statistical block per field row.
    nrows <- blocks
    ncols <- eu_block
    
  } else {
    
    if(
      length(dim) != 2L ||
      anyNA(dim) ||
      !is.numeric(dim) ||
      any(!is.finite(dim)) ||
      any(dim < 1) ||
      any(dim != floor(dim))
    ) {
      stop("'dim' must contain two positive integers: c(nrows, ncols).")
    }
    
    nrows <- as.integer(dim[1])
    ncols <- as.integer(dim[2])
    
    # A larger rectangle would create unrepresented field cells.
    if((nrows * ncols) != total_plots) {
      stop(
        "'dim' must contain exactly ",
        total_plots,
        " positions for this design."
      )
    }
  }
  
  fb$rows <- rep(
    seq_len(nrows),
    each = ncols
  )[seq_len(total_plots)]
  
  fb$cols <- rep(
    seq_len(ncols),
    times = nrows
  )[seq_len(total_plots)]
  
  if(isTRUE(zigzag)) {
    
    reverse_cols <- (ncols - fb$cols) + 1L
    
    fb$cols <- ifelse(
      fb$rows %% 2L == 0L,
      reverse_cols,
      fb$cols
    )
  }
  
  # -------------------------------------------------------------------------
  # QR code ------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  fb$project <- project
  
  qr_matches <- regmatches(
    qrcode,
    gregexpr("\\{[^{}]+\\}", qrcode)
  )[[1]]
  
  qrcolumns <- gsub("^\\{|\\}$", "", qr_matches)
  
  if(length(qrcolumns) == 0L) {
    stop("'qrcode' must contain at least one column inside braces.")
  }
  
  missing_qrcolumns <- setdiff(qrcolumns, names(fb))
  
  if(length(missing_qrcolumns) > 0L) {
    stop(
      "Unknown QR code columns: ",
      paste(missing_qrcolumns, collapse = ", ")
    )
  }
  
  qr_data <- fb[, qrcolumns, drop = FALSE]
  
  qr_data[] <- lapply(qr_data, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })
  
  fb$qrcode <- do.call(
    paste,
    c(qr_data, sep = "_")
  )
  
  fb$qrcode <- gsub("_+$", "", fb$qrcode)
  
  # -------------------------------------------------------------------------
  # Checks column ------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  fb$checks <- dplyr::case_when(
    fb$type == "check" ~ 1L,
    fb$type == "test" ~ 0L,
    TRUE ~ NA_integer_
  )
  
  # -------------------------------------------------------------------------
  # Design label and integrity checks ---------------------------------------
  # -------------------------------------------------------------------------
  
  fb$design <- "augmented"
  
  block_sizes <- table(fb$block)
  
  if(any(block_sizes != eu_block)) {
    stop("Internal error: blocks do not have equal size.")
  }
  
  check_counts <- table(
    factor(
      fb$entry[which(fb$type == "check")],
      levels = checks
    )
  )
  
  if(any(check_counts != blocks)) {
    stop("Internal error: each check must occur once in every block.")
  }
  
  test_counts <- table(
    factor(
      fb$entry[which(fb$type == "test")],
      levels = entry_levels
    )
  )
  
  if(any(test_counts != 1L)) {
    stop("Internal error: every entry must occur exactly once.")
  }
  
  tests_by_block <- table(
    factor(
      fb$block[which(fb$type == "test")],
      levels = seq_len(blocks)
    )
  )
  
  if((max(tests_by_block) - min(tests_by_block)) > 1L) {
    stop("Internal error: entries are not balanced among blocks.")
  }
  
  empty_by_block <- table(
    factor(
      fb$block[which(is.na(fb$type))],
      levels = seq_len(blocks)
    )
  )
  
  if((max(empty_by_block) - min(empty_by_block)) > 1L) {
    stop("Internal error: empty positions are not balanced among blocks.")
  }
  
  if(isTRUE(separate_checks)) {
    
    separation_possible <- eu_block >= (2L * n_checks - 1L)
    
    if(separation_possible) {
      
      adjacent_checks <- vapply(
        split(fb, fb$block),
        function(block_data) {
          check_positions <- which(block_data$type == "check")
          length(check_positions) > 1L &&
            any(diff(check_positions) == 1L)
        },
        logical(1)
      )
      
      if(any(adjacent_checks)) {
        stop("Internal error: adjacent checks found inside a block.")
      }
    }
  }
  
  coordinates <- paste(fb$rows, fb$cols, sep = ":")
  
  if(anyDuplicated(coordinates)) {
    stop("Internal error: duplicated rows/cols coordinates.")
  }
  
  if(anyDuplicated(fb$plots)) {
    stop("Internal error: duplicated plot identifiers.")
  }
  
  # -------------------------------------------------------------------------
  # Output -------------------------------------------------------------------
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
    entries = entry_levels,
    blocks = blocks,
    eu_block = eu_block,
    entries_per_block = entries_per_block,
    random = random,
    separate_checks = separate_checks,
    zigzag = zigzag,
    dim = c(nrows, ncols),
    seed = seed
  )
  
  list(
    fieldbook = fieldbook,
    parameters = parameters
  )
}
