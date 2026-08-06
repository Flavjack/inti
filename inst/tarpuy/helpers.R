# -------------------------------------------------------------------------
# TARPUY app helpers -------------------------------------------------------
# -------------------------------------------------------------------------
# Application-only helper functions used by inst/tarpuy/server.R.
#
# These functions intentionally live inside inst/tarpuy instead of R/utils.R
# so they do not alter the public API of the inti package or affect YUPANA and
# the other applications distributed with the package.
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Internal utilities -------------------------------------------------------
# -------------------------------------------------------------------------

.tarpuy_is_data_frame <- function(x) {
  is.data.frame(x)
}


.tarpuy_nonempty_character <- function(x) {
  if(is.null(x)) {
    return(character(0))
  }

  x <- as.character(x)
  x <- trimws(x)
  x[!is.na(x) & nzchar(x)]
}


.tarpuy_normalize_design_type <- function(x) {
  x <- .tarpuy_nonempty_character(x)

  if(length(x) == 0L) {
    return(NA_character_)
  }

  value <- tolower(x[[1L]])
  value <- gsub("[[:space:]_]+", "-", value)
  value <- gsub("-+", "-", value)

  aliases <- c(
    "dca" = "crd",
    "crd" = "crd",
    "dbca" = "rcbd",
    "rcbd" = "rcbd",
    "aug" = "augmented",
    "aumentado" = "augmented",
    "augmented" = "augmented",
    "splitplot-rcbd" = "split-rcbd",
    "split-plot-rcbd" = "split-rcbd",
    "splitplotrcbd" = "split-rcbd",
    "split-rcbd" = "split-rcbd"
  )

  if(value %in% names(aliases)) {
    return(unname(aliases[[value]]))
  }

  value
}


.tarpuy_design_display_name <- function(x) {
  value <- .tarpuy_normalize_design_type(x)

  switch(
    value,
    "crd" = "CRD",
    "rcbd" = "RCBD",
    "augmented" = "Augmented",
    "split-rcbd" = "Splitplot-RCBD",
    if(is.na(value)) "—" else as.character(x[[1L]])
  )
}


.tarpuy_clean_column_names <- function(x) {
  x <- as.character(x)
  x <- gsub("[{}]", "", x)
  tolower(trimws(x))
}


.tarpuy_nonempty_values <- function(x) {
  if(length(x) == 0L) {
    return(logical(0))
  }

  if(is.list(x) && !is.data.frame(x)) {
    return(vapply(
      x,
      function(value) {
        if(is.null(value) || length(value) == 0L) {
          return(FALSE)
        }

        value <- as.character(value)
        any(!is.na(value) & nzchar(trimws(value)))
      },
      logical(1L),
      USE.NAMES = FALSE
    ))
  }

  value <- as.character(x)
  !is.na(value) & nzchar(trimws(value))
}


.tarpuy_unique_count <- function(x) {
  x <- x[.tarpuy_nonempty_values(x)]

  if(length(x) == 0L) {
    return(0L)
  }

  as.integer(length(unique(as.character(x))))
}


.tarpuy_balanced_count <- function(values) {
  values <- as.integer(values)
  values <- values[!is.na(values)]

  if(length(values) == 0L) {
    return("—")
  }

  minimum <- min(values)
  maximum <- max(values)

  if(identical(minimum, maximum)) {
    return(as.character(minimum))
  }

  paste0(minimum, "–", maximum)
}


.tarpuy_key_column <- function(existing, new) {
  candidates <- c("qrcode", "plots")

  for(candidate in candidates) {
    if(!candidate %in% names(existing) || !candidate %in% names(new)) {
      next
    }

    existing_key <- as.character(existing[[candidate]])
    new_key <- as.character(new[[candidate]])

    valid_existing <- !is.na(existing_key) & nzchar(trimws(existing_key))
    valid_new <- !is.na(new_key) & nzchar(trimws(new_key))

    if(
      all(valid_existing) &&
      all(valid_new) &&
      !anyDuplicated(existing_key) &&
      !anyDuplicated(new_key)
    ) {
      return(candidate)
    }
  }

  NULL
}


.tarpuy_canonical_column <- function(x, column_name = NULL) {
  value <- as.character(x)
  value <- trimws(value)
  value[is.na(value) | !nzchar(value)] <- "<NA>"

  if(identical(column_name, "design")) {
    value <- vapply(
      value,
      function(item) {
        if(identical(item, "<NA>")) {
          return(item)
        }

        .tarpuy_normalize_design_type(item)
      },
      character(1L),
      USE.NAMES = FALSE
    )
  }

  value
}


.tarpuy_canonical_structure <- function(data, columns, key) {
  out <- data[, columns, drop = FALSE]

  for(column_name in columns) {
    out[[column_name]] <- .tarpuy_canonical_column(
      out[[column_name]],
      column_name = column_name
    )
  }

  ordering <- order(out[[key]], na.last = TRUE, method = "radix")
  out <- out[ordering, , drop = FALSE]
  rownames(out) <- NULL

  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}


# -------------------------------------------------------------------------
# Google Sheets names ------------------------------------------------------
# -------------------------------------------------------------------------

#' Sanitize a Google Sheets worksheet name used by TARPUY.
#'
#' Spaces are converted to underscores, unsupported characters are replaced,
#' repeated underscores are collapsed, and the result is limited to 100
#' characters. The function returns one safe worksheet name.
sanitize_sheet_name <- function(x, fallback = "fb", max_length = 100L) {
  if(length(max_length) != 1L || is.na(max_length) || max_length < 1L) {
    stop("'max_length' must be one positive integer.", call. = FALSE)
  }

  max_length <- as.integer(max_length)

  value <- .tarpuy_nonempty_character(x)
  value <- if(length(value) == 0L) fallback else value[[1L]]

  value <- gsub("[[:cntrl:]]+", "", value)
  value <- gsub("[[:space:]]+", "_", value)
  value <- gsub("[:\\\\/\\?\\*\\[\\]]", "_", value)
  value <- gsub("_+", "_", value)
  value <- gsub("^['_]+|['_]+$", "", value)
  value <- substr(value, 1L, max_length)
  value <- gsub("['_]+$", "", value)

  if(!nzchar(value)) {
    value <- .tarpuy_nonempty_character(fallback)
    value <- if(length(value) == 0L) "fb" else value[[1L]]
    value <- gsub("[[:space:]]+", "_", value)
    value <- gsub("[:\\\\/\\?\\*\\[\\]]", "_", value)
    value <- substr(value, 1L, max_length)
  }

  value
}


# -------------------------------------------------------------------------
# Sheet validation ---------------------------------------------------------
# -------------------------------------------------------------------------

#' Determine whether an object has the minimum structure of a TARPUY fieldbook.
is_valid_fieldbook_sheet <- function(data) {
  if(!.tarpuy_is_data_frame(data) || nrow(data) < 1L) {
    return(FALSE)
  }

  column_names <- names(data)

  if(is.null(column_names) || anyNA(column_names) || anyDuplicated(column_names)) {
    return(FALSE)
  }

  required <- c("qrcode", "plots", "rows", "cols", "design")

  if(!all(required %in% column_names)) {
    return(FALSE)
  }

  if(any(!.tarpuy_nonempty_values(data$qrcode))) {
    return(FALSE)
  }

  if(anyDuplicated(as.character(data$qrcode))) {
    return(FALSE)
  }

  design_values <- unique(.tarpuy_nonempty_character(data$design))

  length(design_values) == 1L
}


#' Determine whether an object has the minimum structure of a TARPUY Traits sheet.
is_valid_traits_sheet <- function(data) {
  if(!.tarpuy_is_data_frame(data) || nrow(data) < 1L) {
    return(FALSE)
  }

  column_names <- names(data)

  if(is.null(column_names) || anyNA(column_names) || anyDuplicated(column_names)) {
    return(FALSE)
  }

  normalized_names <- .tarpuy_clean_column_names(column_names)

  if(anyDuplicated(normalized_names)) {
    return(FALSE)
  }

  required <- c("trait", "when", "samples", "format")

  all(required %in% normalized_names)
}


# -------------------------------------------------------------------------
# Structural and factor columns -------------------------------------------
# -------------------------------------------------------------------------

#' Detect experimental factor columns from a TARPUY fieldbook.
#'
#' TARPUY generators place factors after `ntreat` and before the first
#' statistical/spatial metadata column. Trait and manually added columns are
#' appended after the design structure, so they are not returned here.
detect_factor_columns <- function(fieldbook) {
  if(!.tarpuy_is_data_frame(fieldbook) || ncol(fieldbook) == 0L) {
    return(character(0))
  }

  column_names <- names(fieldbook)

  if(is.null(column_names) || anyNA(column_names) || anyDuplicated(column_names)) {
    return(character(0))
  }

  start <- match("ntreat", column_names)

  if(is.na(start) || start >= length(column_names)) {
    return(character(0))
  }

  boundary_names <- c(
    "entry",
    "type",
    "wp_sp",
    "sort",
    "rep",
    "block",
    "rows",
    "cols",
    "design"
  )

  boundary_positions <- match(boundary_names, column_names, nomatch = 0L)
  boundary_positions <- boundary_positions[boundary_positions > start]

  if(length(boundary_positions) == 0L) {
    return(character(0))
  }

  stop_position <- min(boundary_positions)

  if(stop_position <= (start + 1L)) {
    return(character(0))
  }

  candidates <- column_names[seq.int(start + 1L, stop_position - 1L)]

  reserved <- c(
    "qrcode",
    "plots",
    "ntreat",
    boundary_names,
    "checks",
    "whole_plot_order",
    "sub_plot_order",
    "rows_local",
    "walk"
  )

  candidates[!candidates %in% reserved]
}


#' Detect all columns that define the experimental design structure.
detect_structural_columns <- function(fieldbook) {
  if(!.tarpuy_is_data_frame(fieldbook) || ncol(fieldbook) == 0L) {
    return(character(0))
  }

  fixed_columns <- c(
    "qrcode",
    "plots",
    "ntreat",
    "entry",
    "type",
    "checks",
    "wp_sp",
    "sort",
    "rep",
    "block",
    "rows",
    "cols",
    "design"
  )

  structural <- unique(c(
    fixed_columns,
    detect_factor_columns(fieldbook)
  ))

  # Preserve the fieldbook's actual column order.
  names(fieldbook)[names(fieldbook) %in% structural]
}


# -------------------------------------------------------------------------
# Design comparison and recorded data -------------------------------------
# -------------------------------------------------------------------------

#' Compare the structural part of two TARPUY fieldbooks.
#'
#' Trait columns and manually created columns are ignored. The function
#' returns FALSE when factors, levels, randomization, replications, blocks,
#' plot identifiers, coordinates, design type, or the number of experimental
#' units differ.
same_tarpuy_design <- function(existing, new) {
  if(!.tarpuy_is_data_frame(existing) || !.tarpuy_is_data_frame(new)) {
    return(FALSE)
  }

  if(nrow(existing) != nrow(new) || nrow(new) < 1L) {
    return(FALSE)
  }

  existing_columns <- detect_structural_columns(existing)
  new_columns <- detect_structural_columns(new)

  if(
    length(existing_columns) == 0L ||
    length(new_columns) == 0L ||
    !setequal(existing_columns, new_columns)
  ) {
    return(FALSE)
  }

  # The new design defines the canonical comparison order.
  comparison_columns <- new_columns

  if(
    !all(comparison_columns %in% names(existing)) ||
    !all(comparison_columns %in% names(new))
  ) {
    return(FALSE)
  }

  key <- .tarpuy_key_column(existing, new)

  if(is.null(key) || !key %in% comparison_columns) {
    return(FALSE)
  }

  existing_structure <- .tarpuy_canonical_structure(
    existing,
    columns = comparison_columns,
    key = key
  )

  new_structure <- .tarpuy_canonical_structure(
    new,
    columns = comparison_columns,
    key = key
  )

  identical(existing_structure, new_structure)
}


#' Check whether trait or manually created columns contain recorded values.
#'
#' Zero, FALSE, dates, and text are treated as data. Only NA and empty or
#' whitespace-only strings are considered empty.
has_recorded_data <- function(data,
                              columns = NULL,
                              structural_columns = NULL) {
  if(!.tarpuy_is_data_frame(data) || nrow(data) == 0L) {
    return(FALSE)
  }

  if(is.null(columns)) {
    if(is.null(structural_columns)) {
      structural_columns <- detect_structural_columns(data)
    }

    columns <- setdiff(names(data), structural_columns)
  }

  columns <- unique(as.character(columns))
  columns <- columns[columns %in% names(data)]

  if(length(columns) == 0L) {
    return(FALSE)
  }

  any(vapply(
    data[, columns, drop = FALSE],
    function(x) any(.tarpuy_nonempty_values(x)),
    logical(1L),
    USE.NAMES = FALSE
  ))
}


# -------------------------------------------------------------------------
# Safe fieldbook synchronization ------------------------------------------
# -------------------------------------------------------------------------

#' Merge an existing fieldbook with a regenerated fieldbook of the same design.
#'
#' The new design structure is retained. Existing trait values, historical
#' trait columns, and manually created columns are matched by `qrcode`
#' (or by unique `plots` as a fallback). New trait columns are appended empty.
#' The function refuses to merge fieldbooks whose design structures differ.
merge_existing_fieldbook <- function(existing, new) {
  if(!.tarpuy_is_data_frame(existing) || !.tarpuy_is_data_frame(new)) {
    stop("'existing' and 'new' must be data frames.", call. = FALSE)
  }

  if(!same_tarpuy_design(existing, new)) {
    stop(
      "The existing and regenerated fieldbooks do not have the same design structure.",
      call. = FALSE
    )
  }

  key <- .tarpuy_key_column(existing, new)

  if(is.null(key)) {
    stop(
      "The fieldbooks require a unique, non-empty 'qrcode' or 'plots' column for synchronization.",
      call. = FALSE
    )
  }

  existing_key <- as.character(existing[[key]])
  new_key <- as.character(new[[key]])
  index <- match(new_key, existing_key)

  if(anyNA(index)) {
    stop(
      "Some regenerated experimental units could not be matched to the existing fieldbook.",
      call. = FALSE
    )
  }

  structural_columns <- detect_structural_columns(new)
  existing_extra <- setdiff(names(existing), detect_structural_columns(existing))
  new_extra <- setdiff(names(new), structural_columns)

  output <- new[, structural_columns, drop = FALSE]

  # Preserve all historical Traits and manually created columns in their
  # current order, including columns no longer present in the Traits sheet.
  for(column_name in existing_extra) {
    output[[column_name]] <- existing[[column_name]][index]
  }

  # Append only genuinely new Traits. Existing values always take precedence.
  new_only <- setdiff(new_extra, existing_extra)

  for(column_name in new_only) {
    output[[column_name]] <- new[[column_name]]
  }

  rownames(output) <- NULL
  output
}


# -------------------------------------------------------------------------
# Dynamic Layout Summary --------------------------------------------------
# -------------------------------------------------------------------------

#' Build the one-row, design-specific Layout Summary used by TARPUY.
#'
#' The returned data frame is intentionally wider for augmented and
#' split-plot designs. The frontend will place it inside a local horizontal
#' scroll container instead of compressing the headings.
build_layout_summary <- function(fieldbook) {
  if(!is_valid_fieldbook_sheet(fieldbook)) {
    return(data.frame())
  }

  design_type <- .tarpuy_normalize_design_type(fieldbook$design)
  design_name <- .tarpuy_design_display_name(fieldbook$design)
  plots <- .tarpuy_unique_count(fieldbook$plots)
  rows <- .tarpuy_unique_count(fieldbook$rows)
  cols <- .tarpuy_unique_count(fieldbook$cols)

  if(identical(design_type, "crd")) {
    replications <- if("rep" %in% names(fieldbook)) {
      .tarpuy_unique_count(fieldbook$rep)
    } else {
      "—"
    }

    return(data.frame(
      "Design" = design_name,
      "Plots" = plots,
      "Replications" = replications,
      "Rows" = rows,
      "Cols" = cols,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  if(identical(design_type, "rcbd")) {
    blocks <- if("block" %in% names(fieldbook)) {
      .tarpuy_unique_count(fieldbook$block)
    } else {
      "—"
    }

    plots_per_block <- "—"

    if("block" %in% names(fieldbook)) {
      counts <- vapply(
        split(fieldbook$plots, as.character(fieldbook$block)),
        .tarpuy_unique_count,
        integer(1L)
      )
      plots_per_block <- .tarpuy_balanced_count(counts)
    }

    return(data.frame(
      "Design" = design_name,
      "Plots" = plots,
      "Blocks" = blocks,
      "Plots per block" = plots_per_block,
      "Rows" = rows,
      "Cols" = cols,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  if(identical(design_type, "augmented")) {
    blocks <- if("block" %in% names(fieldbook)) {
      .tarpuy_unique_count(fieldbook$block)
    } else {
      "—"
    }

    type_values <- if("type" %in% names(fieldbook)) {
      tolower(trimws(as.character(fieldbook$type)))
    } else {
      rep(NA_character_, nrow(fieldbook))
    }

    entry_values <- if("entry" %in% names(fieldbook)) {
      fieldbook$entry
    } else {
      rep(NA_character_, nrow(fieldbook))
    }

    checks <- .tarpuy_unique_count(entry_values[type_values == "check"])
    test_entries <- .tarpuy_unique_count(entry_values[type_values == "test"])
    empty_plots <- sum(is.na(type_values) | !nzchar(type_values))

    return(data.frame(
      "Design" = design_name,
      "Plots" = plots,
      "Blocks" = blocks,
      "Checks" = checks,
      "Test entries" = test_entries,
      "Empty plots" = as.integer(empty_plots),
      "Cols" = cols,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  if(identical(design_type, "split-rcbd")) {
    blocks <- if("block" %in% names(fieldbook)) {
      .tarpuy_unique_count(fieldbook$block)
    } else {
      "—"
    }

    whole_plot_counts <- integer(0)
    subplot_counts <- integer(0)

    if(all(c("block", "cols") %in% names(fieldbook))) {
      whole_plot_counts <- vapply(
        split(fieldbook$cols, as.character(fieldbook$block)),
        .tarpuy_unique_count,
        integer(1L)
      )

      group <- interaction(
        as.character(fieldbook$block),
        as.character(fieldbook$cols),
        drop = TRUE,
        lex.order = TRUE
      )

      subplot_counts <- vapply(
        split(fieldbook$plots, group),
        .tarpuy_unique_count,
        integer(1L)
      )
    }

    return(data.frame(
      "Design" = design_name,
      "Plots" = plots,
      "Blocks" = blocks,
      "Whole plots/block" = .tarpuy_balanced_count(whole_plot_counts),
      "Subplots/whole plot" = .tarpuy_balanced_count(subplot_counts),
      "Rows" = rows,
      "Cols" = cols,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    "Design" = design_name,
    "Plots" = plots,
    "Rows" = rows,
    "Cols" = cols,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}


# -------------------------------------------------------------------------
# Mobile export ------------------------------------------------------------
# -------------------------------------------------------------------------

#' Select stable Field Book CSV columns without relying on column position.
#'
#' Trait columns are intentionally excluded because they are supplied to the
#' Field Book mobile application through the separate .trt file. Extra
#' informational columns can be appended explicitly with `additional`.
build_mobile_columns <- function(fieldbook, additional = NULL) {
  if(!is_valid_fieldbook_sheet(fieldbook)) {
    return(character(0))
  }

  factor_columns <- detect_factor_columns(fieldbook)

  required_order <- c(
    "qrcode",
    "plots",
    "ntreat",
    factor_columns,
    "entry",
    "type",
    "wp_sp",
    "block",
    "rep",
    "sort",
    "rows",
    "cols",
    "design"
  )

  required_order <- unique(required_order)
  required_order <- required_order[required_order %in% names(fieldbook)]

  additional <- .tarpuy_nonempty_character(additional)
  additional <- unique(additional[additional %in% names(fieldbook)])
  additional <- setdiff(additional, required_order)

  c(required_order, additional)
}


# -------------------------------------------------------------------------
# Sketch defaults ----------------------------------------------------------
# -------------------------------------------------------------------------

#' Choose the default Color by column for the TARPUY sketch.
default_sketch_color <- function(fieldbook) {
  if(!is_valid_fieldbook_sheet(fieldbook)) {
    return(NULL)
  }

  design_type <- .tarpuy_normalize_design_type(fieldbook$design)
  factor_columns <- detect_factor_columns(fieldbook)

  if(identical(design_type, "augmented") && "type" %in% names(fieldbook)) {
    return("type")
  }

  if(
    design_type %in% c("crd", "rcbd", "split-rcbd") &&
    length(factor_columns) > 0L
  ) {
    return(factor_columns[[1L]])
  }

  if("ntreat" %in% names(fieldbook)) {
    return("ntreat")
  }

  candidates <- setdiff(
    names(fieldbook),
    c("qrcode", "plots", "sort", "rows", "cols", "design")
  )

  if(length(candidates) == 0L) {
    return(NULL)
  }

  candidates[[1L]]
}
