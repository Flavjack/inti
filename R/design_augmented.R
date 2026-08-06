#' Experimental design: Augmented
#'
#' Fieldbook generator for augmented experimental designs. Every check occurs
#' once in each block and every test entry occurs once in the complete design.
#'
#' @param checks Vector of check treatments.
#' @param entries Vector of new or test entries.
#' @param blocks Optional number of blocks. If `NULL`, it is calculated from
#'   `entries`, `checks` and `eu_block`.
#' @param eu_block Number of experimental units per block. It must be greater
#'   than the number of checks.
#' @param random Logical. Randomize test-entry allocation and positions inside
#'   each block.
#' @param zigzag Logical. Arrange the physical field layout in zigzag order.
#' @param dim Optional physical layout dimensions `c(nrows, ncols)`. The
#'   product must equal the total number of experimental units.
#' @param serie Base number used to generate plot identifiers.
#' @param seed Random seed. `0`, `NA` or `NULL` means that no fixed seed is set
#'   inside this function. TARPUY stores an effective seed in the design sheet
#'   before calling the design generator.
#' @param project Barcode prefix.
#' @param qrcode QR-code column template. The default is
#'   `"{project}{plots}{entry}"`.
#' @param separate_checks Logical. When enough positions are available, place
#'   checks in non-adjacent positions inside each block. With
#'   `random = FALSE`, this placement is deterministic.
#'
#' @return A list with `fieldbook` and `parameters`.
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
  # Helpers
  # -------------------------------------------------------------------------

  is_missing_scalar <- function(x) {
    is.null(x) ||
      length(x) == 0L ||
      (length(x) == 1L && is.na(x)) ||
      (length(x) == 1L && is.character(x) && !nzchar(trimws(x)))
  }

  as_positive_integer <- function(x, name, allow_null = TRUE) {
    if(is_missing_scalar(x)) {
      if(isTRUE(allow_null)) {
        return(NULL)
      }

      stop("'", name, "' must be a positive integer.", call. = FALSE)
    }

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

  clean_codes <- function(x) {
    if(is.list(x)) {
      x <- unlist(x, recursive = TRUE, use.names = FALSE)
    }

    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "NULL")] <- NA_character_
    x <- x[!is.na(x)]

    # Preserve the original labels. Spaces and punctuation are valid in
    # treatment names and must not silently collapse distinct entries.
    unique(x)
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

    can_separate <- isTRUE(separate_checks) &&
      n_checks_block > 0L &&
      n_total >= (2L * n_checks_block - 1L)

    if(can_separate) {
      checks_part <- block_df[check_rows, , drop = FALSE]
      others_part <- block_df[other_rows, , drop = FALSE]

      if(isTRUE(random)) {
        if(nrow(checks_part) > 1L) {
          checks_part <- checks_part[
            sample.int(nrow(checks_part)),
            ,
            drop = FALSE
          ]
        }

        if(nrow(others_part) > 1L) {
          others_part <- others_part[
            sample.int(nrow(others_part)),
            ,
            drop = FALSE
          ]
        }

        # Sampling these base positions and adding the sequence offset creates
        # a one-to-one set of positions with at least one non-check between
        # consecutive checks.
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

  build_qrcode <- function(data, template) {
    if(length(template) != 1L ||
       is.na(template) ||
       !nzchar(trimws(as.character(template)))) {
      stop(
        "'qrcode' must be a non-empty character template.",
        call. = FALSE
      )
    }

    template <- trimws(as.character(template))

    tokens <- regmatches(
      template,
      gregexpr("\\{[^{}]+\\}", template)
    )[[1L]]

    if(length(tokens) == 0L || identical(tokens, character(0))) {
      stop(
        "'qrcode' must contain at least one field inside braces, for ",
        "example '{project}{plots}{entry}'.",
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

      # Clean only the QR-code representation. The original treatment labels
      # in the fieldbook remain unchanged.
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

  if(length(random) != 1L || is.na(random) || !is.logical(random)) {
    stop("'random' must be TRUE or FALSE.", call. = FALSE)
  }

  if(length(zigzag) != 1L || is.na(zigzag) || !is.logical(zigzag)) {
    stop("'zigzag' must be TRUE or FALSE.", call. = FALSE)
  }

  if(length(separate_checks) != 1L ||
     is.na(separate_checks) ||
     !is.logical(separate_checks)) {
    stop("'separate_checks' must be TRUE or FALSE.", call. = FALSE)
  }

  blocks <- as_positive_integer(blocks, "blocks", allow_null = TRUE)
  eu_block <- as_positive_integer(eu_block, "eu_block", allow_null = TRUE)
  serie <- as_positive_integer(serie, "serie", allow_null = FALSE)

  seed_parameter <- NULL

  if(!is_missing_scalar(seed)) {
    seed_value <- suppressWarnings(as.numeric(as.character(seed)))

    if(length(seed_value) != 1L ||
       is.na(seed_value) ||
       !is.finite(seed_value) ||
       seed_value < 0 ||
       seed_value != floor(seed_value) ||
       seed_value > .Machine$integer.max) {
      stop(
        "'seed' must be a non-negative integer, 0, NA, or NULL.",
        call. = FALSE
      )
    }

    seed_parameter <- as.integer(seed_value)

    if(seed_parameter != 0L) {
      set.seed(seed_parameter)
    }
  }

  if(length(project) != 1L) {
    stop("'project' must contain one value.", call. = FALSE)
  }

  project <- as.character(project)

  if(is.na(project)) {
    project <- ""
  }

  checks <- clean_codes(checks)
  entries <- clean_codes(entries)

  if(length(checks) == 0L) {
    stop("At least one check is required.", call. = FALSE)
  }

  if(length(entries) == 0L) {
    stop("At least one test entry is required.", call. = FALSE)
  }

  overlap <- intersect(checks, entries)

  if(length(overlap) > 0L) {
    stop(
      "Checks and test entries must not overlap: ",
      paste(utils::head(overlap, 10L), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  n_checks <- length(checks)
  n_entries <- length(entries)

  # Preserve the supplied order. Treatment numbering must not depend on seed.
  entry_levels <- entries
  treatment_levels <- c(checks, entry_levels)

  # -------------------------------------------------------------------------
  # Block definition
  # -------------------------------------------------------------------------

  # Preserve the existing automatic behavior when neither value is supplied.
  if(is.null(blocks) && is.null(eu_block)) {
    eu_block <- max(n_checks + 6L, 10L)
  }

  if(!is.null(eu_block) && eu_block <= n_checks) {
    stop(
      "'eu_block' must be greater than the number of checks (",
      n_checks,
      ").",
      call. = FALSE
    )
  }

  if(is.null(blocks)) {
    usable_slots <- eu_block - n_checks
    blocks <- as.integer(ceiling(n_entries / usable_slots))
  }

  entries_per_block <- balanced_sizes(
    n_items = n_entries,
    n_groups = blocks
  )

  if(is.null(eu_block)) {
    eu_block <- n_checks + max(entries_per_block)
  }

  usable_slots <- eu_block - n_checks
  min_needed <- n_checks + max(entries_per_block)

  if(max(entries_per_block) > usable_slots) {
    stop(
      "'eu_block' is too small. Minimum required for ",
      blocks,
      " blocks is ",
      min_needed,
      ".",
      call. = FALSE
    )
  }

  if(blocks > n_entries) {
    warning(
      "The requested design contains ",
      blocks - n_entries,
      " block(s) without test entries. Those blocks will contain checks and ",
      "empty plots only.",
      call. = FALSE
    )
  }

  if(isTRUE(separate_checks) &&
     eu_block < (2L * n_checks - 1L)) {
    warning(
      "Checks cannot all be separated inside each block because 'eu_block' ",
      "does not provide enough non-check positions. Randomization will still ",
      "be applied, but adjacent checks may occur.",
      call. = FALSE
    )
  }

  if(blocks > 1L && serie < eu_block) {
    stop(
      "'serie' must be greater than or equal to 'eu_block' to avoid ",
      "duplicated plot identifiers. eu_block = ",
      eu_block,
      "; serie = ",
      serie,
      ".",
      call. = FALSE
    )
  }

  total_plots_double <- as.double(blocks) * as.double(eu_block)
  max_plot_id <- as.double(serie) * as.double(blocks) + as.double(eu_block)

  if(!is.finite(total_plots_double) ||
     total_plots_double > .Machine$integer.max) {
    stop(
      "The requested augmented design contains too many experimental units.",
      call. = FALSE
    )
  }

  if(!is.finite(max_plot_id) || max_plot_id > .Machine$integer.max) {
    stop(
      "The generated plot identifiers would exceed the supported integer ",
      "range. Reduce 'serie', 'blocks', or 'eu_block'.",
      call. = FALSE
    )
  }

  total_plots <- as.integer(total_plots_double)

  # -------------------------------------------------------------------------
  # Test-entry allocation
  # -------------------------------------------------------------------------

  allocation_entries <- if(isTRUE(random)) {
    sample(entry_levels)
  } else {
    entry_levels
  }

  split_entries <- vector("list", blocks)
  start_idx <- 1L

  for(block_id in seq_len(blocks)) {
    n_block <- entries_per_block[block_id]

    if(n_block > 0L) {
      end_idx <- start_idx + n_block - 1L
      split_entries[[block_id]] <- allocation_entries[start_idx:end_idx]
      start_idx <- end_idx + 1L
    } else {
      split_entries[[block_id]] <- character(0)
    }
  }

  # -------------------------------------------------------------------------
  # Fieldbook
  # -------------------------------------------------------------------------

  fb_list <- vector("list", blocks)

  for(block_id in seq_len(blocks)) {
    checks_df <- data.frame(
      entry = checks,
      type = rep("check", n_checks),
      stringsAsFactors = FALSE
    )

    tests_df <- data.frame(
      entry = split_entries[[block_id]],
      type = rep("test", length(split_entries[[block_id]])),
      stringsAsFactors = FALSE
    )

    n_fill <- eu_block - nrow(checks_df) - nrow(tests_df)

    filler_df <- data.frame(
      entry = rep(NA_character_, n_fill),
      type = rep(NA_character_, n_fill),
      stringsAsFactors = FALSE
    )

    block_df <- rbind(
      checks_df,
      tests_df,
      filler_df
    )

    block_df <- arrange_block(
      block_df = block_df,
      random = random,
      separate_checks = separate_checks
    )

    block_df$block <- block_id
    block_df$sort <- seq_len(nrow(block_df))
    block_df$plots <- serie * block_id + block_df$sort

    block_df$ntreat <- ifelse(
      is.na(block_df$entry),
      NA_integer_,
      match(block_df$entry, treatment_levels)
    )

    fb_list[[block_id]] <- block_df
  }

  fb <- do.call(rbind, fb_list)
  rownames(fb) <- NULL

  # -------------------------------------------------------------------------
  # Physical layout
  # -------------------------------------------------------------------------

  dim_missing <- is.null(dim) ||
    length(dim) == 0L ||
    (length(dim) == 1L && is.na(dim))

  if(dim_missing) {
    # One complete statistical block per physical row.
    nrows <- blocks
    ncols <- eu_block
  } else {
    dim_value <- suppressWarnings(as.numeric(as.character(dim)))

    if(length(dim_value) != 2L ||
       anyNA(dim_value) ||
       any(!is.finite(dim_value)) ||
       any(dim_value < 1) ||
       any(dim_value != floor(dim_value)) ||
       any(dim_value > .Machine$integer.max)) {
      stop(
        "'dim' must contain two positive integers: c(nrows, ncols).",
        call. = FALSE
      )
    }

    nrows <- as.integer(dim_value[1L])
    ncols <- as.integer(dim_value[2L])

    if(as.double(nrows) * as.double(ncols) != total_plots) {
      stop(
        "'dim' must contain exactly ",
        total_plots,
        " positions for this design.",
        call. = FALSE
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

  fb$rows <- as.integer(fb$rows)
  fb$cols <- as.integer(fb$cols)

  # -------------------------------------------------------------------------
  # QR code and design metadata
  # -------------------------------------------------------------------------

  fb$project <- project
  fb$qrcode <- build_qrcode(fb, qrcode)

  fb$checks <- ifelse(
    fb$type == "check",
    1L,
    ifelse(fb$type == "test", 0L, NA_integer_)
  )

  fb$design <- "augmented"

  # -------------------------------------------------------------------------
  # Integrity checks
  # -------------------------------------------------------------------------

  block_sizes <- table(
    factor(fb$block, levels = seq_len(blocks))
  )

  if(any(block_sizes != eu_block)) {
    stop(
      "Internal error: blocks do not have equal size.",
      call. = FALSE
    )
  }

  check_counts <- table(
    factor(
      fb$entry[fb$type == "check"],
      levels = checks
    )
  )

  if(any(check_counts != blocks)) {
    stop(
      "Internal error: each check must occur once in every block.",
      call. = FALSE
    )
  }

  test_counts <- table(
    factor(
      fb$entry[fb$type == "test"],
      levels = entry_levels
    )
  )

  if(any(test_counts != 1L)) {
    stop(
      "Internal error: every test entry must occur exactly once.",
      call. = FALSE
    )
  }

  tests_by_block <- table(
    factor(
      fb$block[fb$type == "test"],
      levels = seq_len(blocks)
    )
  )

  if((max(tests_by_block) - min(tests_by_block)) > 1L) {
    stop(
      "Internal error: test entries are not balanced among blocks.",
      call. = FALSE
    )
  }

  empty_by_block <- table(
    factor(
      fb$block[is.na(fb$type)],
      levels = seq_len(blocks)
    )
  )

  if((max(empty_by_block) - min(empty_by_block)) > 1L) {
    stop(
      "Internal error: empty positions are not balanced among blocks.",
      call. = FALSE
    )
  }

  if(isTRUE(separate_checks) &&
     eu_block >= (2L * n_checks - 1L)) {
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
      stop(
        "Internal error: adjacent checks were generated although separation ",
        "was feasible.",
        call. = FALSE
      )
    }
  }

  coordinates <- paste(fb$rows, fb$cols, sep = ":")

  if(anyDuplicated(coordinates)) {
    stop(
      "Internal error: duplicated row/column coordinates were generated.",
      call. = FALSE
    )
  }

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
      "Duplicated QR-code identifiers were generated after cleaning spaces: ",
      paste(utils::head(duplicate_qr, 10L), collapse = ", "),
      ". Modify the QR-code template so every experimental unit is unique.",
      call. = FALSE
    )
  }

  if(nrow(fb) != total_plots) {
    stop(
      "Internal error: the final number of experimental units is incorrect.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------------
  # Output
  # -------------------------------------------------------------------------

  output_columns <- c(
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
  )

  fieldbook <- fb[, output_columns, drop = FALSE]
  rownames(fieldbook) <- NULL

  parameters <- list(
    design = "augmented",
    checks = checks,
    entries = entry_levels,
    blocks = blocks,
    eu_block = eu_block,
    checks_per_block = n_checks,
    entries_per_block = entries_per_block,
    empty_plots_per_block = eu_block - n_checks - entries_per_block,
    random = random,
    separate_checks = separate_checks,
    zigzag = zigzag,
    dim = c(nrows, ncols),
    serie = serie,
    seed = seed_parameter,
    project = project,
    qrcode = qrcode
  )

  list(
    fieldbook = fieldbook,
    parameters = parameters
  )
}
