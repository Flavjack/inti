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



# -------------------------------------------------------------------------
# Sketch dimensions --------------------------------------------------------
# -------------------------------------------------------------------------

#' Describe the physical grid used by a TARPUY sketch.
#'
#' The returned geometry is used only to recommend export dimensions and to
#' create a responsive preview. It never changes the experimental design.
sketch_layout_geometry <- function(fieldbook) {
  if(!is_valid_fieldbook_sheet(fieldbook)) {
    return(list(
      design = NA_character_,
      blocks = 1L,
      columns = 1L,
      rows = 1L,
      effective_columns = 1L,
      effective_rows = 1L
    ))
  }

  design_type <- .tarpuy_normalize_design_type(fieldbook$design)
  count_unique <- function(x) {
    values <- x[.tarpuy_nonempty_values(x)]
    max(1L, as.integer(length(unique(values))))
  }

  if(
    identical(design_type, "split-rcbd") &&
    "block" %in% names(fieldbook)
  ) {
    blocks <- unique(as.character(fieldbook$block))
    blocks <- blocks[!is.na(blocks) & nzchar(trimws(blocks))]

    if(length(blocks) == 0L) {
      blocks <- "1"
    }

    columns_by_block <- vapply(
      blocks,
      function(block_value) {
        count_unique(fieldbook$cols[as.character(fieldbook$block) == block_value])
      },
      integer(1L),
      USE.NAMES = FALSE
    )

    rows_by_block <- vapply(
      blocks,
      function(block_value) {
        count_unique(fieldbook$rows[as.character(fieldbook$block) == block_value])
      },
      integer(1L),
      USE.NAMES = FALSE
    )

    columns <- max(columns_by_block, na.rm = TRUE)
    rows <- max(rows_by_block, na.rm = TRUE)
    number_blocks <- max(1L, length(blocks))

    return(list(
      design = design_type,
      blocks = as.integer(number_blocks),
      columns = as.integer(columns),
      rows = as.integer(rows),
      effective_columns = as.integer(number_blocks * columns),
      effective_rows = as.integer(rows)
    ))
  }

  columns <- count_unique(fieldbook$cols)
  rows <- count_unique(fieldbook$rows)
  blocks <- if("block" %in% names(fieldbook)) {
    count_unique(fieldbook$block)
  } else {
    1L
  }

  list(
    design = design_type,
    blocks = as.integer(blocks),
    columns = as.integer(columns),
    rows = as.integer(rows),
    effective_columns = as.integer(columns),
    effective_rows = as.integer(rows)
  )
}


#' Recommend practical download dimensions for a TARPUY sketch.
#'
#' Dimensions are expressed in centimetres. They are intentionally moderate:
#' the preview is generated independently, while users may still override the
#' suggested values for a specific publication or printing requirement.
recommended_sketch_dimensions <- function(fieldbook) {
  geometry <- sketch_layout_geometry(fieldbook)

  width_cm <- geometry$effective_columns * 2.0 + 5.0
  height_cm <- geometry$effective_rows * 1.65 + 5.0

  if(identical(geometry$design, "split-rcbd")) {
    width_cm <- width_cm + geometry$blocks * 0.8
    height_cm <- height_cm + 1.0
  }

  width_cm <- min(120, max(18, width_cm))
  height_cm <- min(80, max(9, height_cm))

  list(
    width_cm = round(width_cm, 1),
    height_cm = round(height_cm, 1),
    geometry = geometry
  )
}


#' Calculate pixel dimensions for the browser preview.
#'
#' Width and height follow the editable Sketch controls, so changing either
#' value immediately changes the preview geometry. Preview DPI is intentionally
#' fixed by the server and remains independent from the Resolution control used
#' for downloaded PNG files.
sketch_preview_dimensions <- function(
    fieldbook,
    width_cm = NULL,
    height_cm = NULL,
    dpi = 100L
) {
  recommended <- recommended_sketch_dimensions(fieldbook)

  width_cm <- suppressWarnings(as.numeric(width_cm))
  height_cm <- suppressWarnings(as.numeric(height_cm))
  dpi <- suppressWarnings(as.numeric(dpi))

  if(length(width_cm) == 0L || is.na(width_cm) || !is.finite(width_cm)) {
    width_cm <- recommended$width_cm
  }
  if(length(height_cm) == 0L || is.na(height_cm) || !is.finite(height_cm)) {
    height_cm <- recommended$height_cm
  }
  if(length(dpi) == 0L || is.na(dpi) || !is.finite(dpi) || dpi < 72) {
    dpi <- 100
  }

  width_cm <- min(200, max(5, width_cm))
  height_cm <- min(200, max(5, height_cm))
  dpi <- as.integer(round(dpi))

  list(
    width_px = as.integer(round(width_cm / 2.54 * dpi)),
    height_px = as.integer(round(height_cm / 2.54 * dpi)),
    dpi = dpi,
    width_cm = width_cm,
    height_cm = height_cm,
    geometry = recommended$geometry
  )
}


# -------------------------------------------------------------------------
# Trait identity and metadata ---------------------------------------------
# -------------------------------------------------------------------------

#' Return an empty TARPUY Trait metadata table.
#'
#' This table is stored in the internal `_tarpuy_traits_meta` worksheet. It is
#' never appended to the fieldbook or exported to Field Book mobile files.
tarpuy_empty_trait_metadata <- function() {
  data.frame(
    fieldbook_sheet = character(),
    traits_sheet = character(),
    trait_id = character(),
    generated_column = character(),
    generated_index = integer(),
    status = character(),
    updated_at = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


.tarpuy_new_trait_id <- function(existing = character(0)) {
  existing <- as.character(existing)
  existing <- existing[!is.na(existing) & nzchar(trimws(existing))]

  stamp <- gsub(
    "[^0-9]",
    "",
    format(Sys.time(), "%Y%m%d%H%M%OS6")
  )
  process_id <- as.integer(Sys.getpid())
  counter <- length(existing) + 1L

  repeat {
    candidate <- paste0(
      "T",
      stamp,
      "_",
      process_id,
      "_",
      sprintf("%06d", counter)
    )

    if(!candidate %in% existing) {
      return(candidate)
    }

    counter <- counter + 1L
  }
}



#' Ensure that active rows in a Traits worksheet have stable internal IDs.
#'
#' Existing IDs are retained. Duplicated IDs are repaired after the first
#' occurrence, and active rows without an ID receive a new one. IDs already
#' present on temporarily blank rows are retained so the same row can be
#' restored without being treated as a completely new Trait.
tarpuy_prepare_trait_ids <- function(data) {
  if(!.tarpuy_is_data_frame(data)) {
    stop("'data' must be a Traits data frame.", call. = FALSE)
  }

  out <- as.data.frame(
    data,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  normalized_names <- .tarpuy_clean_column_names(names(out))
  trait_position <- match("trait", normalized_names)

  if(is.na(trait_position)) {
    stop("The Traits sheet must contain column {trait}.", call. = FALSE)
  }

  id_position <- match("_trait_id", normalized_names)
  changed <- FALSE

  if(is.na(id_position)) {
    out[["_trait_id"]] <- NA_character_
    id_position <- ncol(out)
    changed <- TRUE
  } else if(!identical(names(out)[[id_position]], "_trait_id")) {
    names(out)[[id_position]] <- "_trait_id"
    changed <- TRUE
  }

  ids <- trimws(as.character(out[[id_position]]))
  ids[is.na(ids) | !nzchar(ids)] <- NA_character_

  trait_values <- trimws(as.character(out[[trait_position]]))
  active <- !is.na(trait_values) &
    nzchar(trait_values) &
    toupper(trait_values) != "X"

  seen <- character(0)

  for(i in seq_len(nrow(out))) {
    current <- ids[[i]]

    if(!is.na(current) && current %in% seen) {
      current <- NA_character_
      ids[[i]] <- NA_character_
      changed <- TRUE
    }

    if(is.na(current) && active[[i]]) {
      current <- .tarpuy_new_trait_id(c(seen, ids))
      ids[[i]] <- current
      changed <- TRUE
    }

    if(!is.na(current)) {
      seen <- c(seen, current)
    }
  }

  out[[id_position]] <- ids

  list(
    data = out,
    changed = changed,
    column_index = as.integer(id_position)
  )
}


#' Normalize a TARPUY Trait metadata table.
tarpuy_normalize_trait_metadata <- function(metadata) {
  template <- tarpuy_empty_trait_metadata()

  if(is.null(metadata) || !.tarpuy_is_data_frame(metadata) || nrow(metadata) == 0L) {
    return(template)
  }

  out <- as.data.frame(
    metadata,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for(column_name in names(template)) {
    if(!column_name %in% names(out)) {
      out[[column_name]] <- template[[column_name]]
    }
  }

  out <- out[, names(template), drop = FALSE]

  character_columns <- setdiff(names(template), "generated_index")
  for(column_name in character_columns) {
    out[[column_name]] <- trimws(as.character(out[[column_name]]))
    out[[column_name]][is.na(out[[column_name]])] <- ""
  }

  out$generated_index <- suppressWarnings(as.integer(out$generated_index))
  out$status[!out$status %in% c("active", "historical")] <- "historical"

  valid <- nzchar(out$trait_id) &
    nzchar(out$generated_column) &
    !is.na(out$generated_index) &
    out$generated_index >= 1L

  out <- out[valid, , drop = FALSE]
  rownames(out) <- NULL
  out
}


.tarpuy_column_value_count <- function(data, column_name) {
  if(
    !.tarpuy_is_data_frame(data) ||
    !column_name %in% names(data)
  ) {
    return(0L)
  }

  as.integer(sum(.tarpuy_nonempty_values(data[[column_name]])))
}


#' Compare active Trait metadata before and after editing the Traits sheet.
#'
#' The returned plan identifies renames by stable `trait_id` and
#' `generated_index`, and identifies obsolete columns produced by deleted
#' Traits or by reducing the number of generated samples/moments.
tarpuy_trait_change_plan <- function(old_metadata, new_metadata, existing) {
  old <- tarpuy_normalize_trait_metadata(old_metadata)
  new <- tarpuy_normalize_trait_metadata(new_metadata)

  old_active <- old[old$status == "active", , drop = FALSE]
  new_active <- new[new$status == "active", , drop = FALSE]

  empty_renames <- data.frame(
    trait_id = character(),
    generated_index = integer(),
    old_column = character(),
    new_column = character(),
    value_count = integer(),
    conflict = logical(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  empty_obsolete <- data.frame(
    trait_id = character(),
    generated_index = integer(),
    old_column = character(),
    value_count = integer(),
    reason = character(),
    conflict = logical(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if(nrow(old_active) == 0L) {
    return(list(
      renames = empty_renames,
      obsolete = empty_obsolete,
      has_changes = FALSE,
      metadata_initialized = FALSE
    ))
  }

  rename_rows <- list()
  obsolete_rows <- list()
  rename_count <- 0L
  obsolete_count <- 0L

  trait_ids <- unique(c(old_active$trait_id, new_active$trait_id))

  for(trait_id in trait_ids) {
    old_rows <- old_active[
      old_active$trait_id == trait_id,
      ,
      drop = FALSE
    ]
    new_rows <- new_active[
      new_active$trait_id == trait_id,
      ,
      drop = FALSE
    ]

    old_rows <- old_rows[
      order(old_rows$generated_index, method = "radix"),
      ,
      drop = FALSE
    ]
    new_rows <- new_rows[
      order(new_rows$generated_index, method = "radix"),
      ,
      drop = FALSE
    ]

    if(nrow(old_rows) == 0L) {
      next
    }

    if(nrow(new_rows) == 0L) {
      for(i in seq_len(nrow(old_rows))) {
        obsolete_count <- obsolete_count + 1L
        old_column <- old_rows$generated_column[[i]]
        obsolete_rows[[obsolete_count]] <- data.frame(
          trait_id = trait_id,
          generated_index = old_rows$generated_index[[i]],
          old_column = old_column,
          value_count = .tarpuy_column_value_count(existing, old_column),
          reason = "deleted_trait",
          conflict = old_column %in% new_active$generated_column,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
      next
    }

    # First preserve exact generated names, even if the user changed their
    # order in {when} or {samples}. This prevents a harmless reorder from being
    # interpreted as a chain of renames.
    exact_names <- intersect(
      old_rows$generated_column,
      new_rows$generated_column
    )

    old_remaining <- old_rows[
      !old_rows$generated_column %in% exact_names,
      ,
      drop = FALSE
    ]
    new_remaining <- new_rows[
      !new_rows$generated_column %in% exact_names,
      ,
      drop = FALSE
    ]

    pair_count <- min(nrow(old_remaining), nrow(new_remaining))

    if(pair_count > 0L) {
      for(i in seq_len(pair_count)) {
        old_column <- old_remaining$generated_column[[i]]
        new_column <- new_remaining$generated_column[[i]]
        rename_count <- rename_count + 1L
        rename_rows[[rename_count]] <- data.frame(
          trait_id = trait_id,
          generated_index = old_remaining$generated_index[[i]],
          old_column = old_column,
          new_column = new_column,
          value_count = .tarpuy_column_value_count(existing, old_column),
          conflict = new_column %in% names(existing) &&
            !identical(old_column, new_column),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }

    if(nrow(old_remaining) > pair_count) {
      extra_positions <- seq.int(pair_count + 1L, nrow(old_remaining))

      for(i in extra_positions) {
        obsolete_count <- obsolete_count + 1L
        old_column <- old_remaining$generated_column[[i]]
        obsolete_rows[[obsolete_count]] <- data.frame(
          trait_id = trait_id,
          generated_index = old_remaining$generated_index[[i]],
          old_column = old_column,
          value_count = .tarpuy_column_value_count(existing, old_column),
          reason = "reduced_generation",
          conflict = old_column %in% new_active$generated_column,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }
  }

  renames <- if(rename_count == 0L) {
    empty_renames
  } else {
    do.call(rbind, rename_rows)
  }

  obsolete <- if(obsolete_count == 0L) {
    empty_obsolete
  } else {
    do.call(rbind, obsolete_rows)
  }

  list(
    renames = renames,
    obsolete = obsolete,
    has_changes = nrow(renames) > 0L || nrow(obsolete) > 0L,
    metadata_initialized = TRUE
  )
}


#' Reconcile a regenerated fieldbook using explicit Trait decisions.
#'
#' Manual columns are always retained. Selected rename operations move existing
#' values to the new generated column name. Selected obsolete columns are
#' deleted; unselected obsolete columns are retained as historical columns.
tarpuy_reconcile_trait_columns <- function(
    existing,
    new,
    rename_map = character(0),
    delete_columns = character(0)
) {
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

  index <- match(as.character(new[[key]]), as.character(existing[[key]]))
  if(anyNA(index)) {
    stop(
      "Some regenerated experimental units could not be matched to the existing fieldbook.",
      call. = FALSE
    )
  }

  # Preserve the source-column names before coercing the target values.
  # `as.character()` / `unname()`-style coercion may remove vector names;
  # losing them silently converts every selected rename into a no-op and causes
  # TARPUY to retain the old columns while appending the newly generated ones.
  rename_sources <- names(rename_map)
  rename_targets <- as.character(unname(rename_map))

  if(length(rename_targets) > 0L && is.null(rename_sources)) {
    stop(
      "'rename_map' must be a named character vector whose names are the existing columns.",
      call. = FALSE
    )
  }

  if(length(rename_sources) != length(rename_targets)) {
    stop("Invalid Trait rename map.", call. = FALSE)
  }

  valid_rename <- !is.na(rename_sources) &
    nzchar(trimws(rename_sources)) &
    !is.na(rename_targets) &
    nzchar(trimws(rename_targets))

  rename_sources <- trimws(rename_sources[valid_rename])
  rename_targets <- trimws(rename_targets[valid_rename])

  if(anyDuplicated(rename_sources)) {
    stop("Trait reconciliation contains duplicated source columns.", call. = FALSE)
  }

  if(anyDuplicated(rename_targets)) {
    stop("Trait reconciliation contains duplicated target columns.", call. = FALSE)
  }

  rename_map <- stats::setNames(rename_targets, rename_sources)

  delete_columns <- unique(.tarpuy_nonempty_character(delete_columns))

  structural_columns <- detect_structural_columns(new)
  existing_extra <- setdiff(names(existing), detect_structural_columns(existing))
  new_extra <- setdiff(names(new), structural_columns)

  output <- new[, structural_columns, drop = FALSE]

  for(column_name in existing_extra) {
    if(column_name %in% delete_columns) {
      next
    }

    output_name <- if(column_name %in% names(rename_map)) {
      unname(rename_map[[column_name]])
    } else {
      column_name
    }

    if(output_name %in% names(output)) {
      stop(
        "Trait reconciliation would create a duplicated column: ",
        output_name,
        ".",
        call. = FALSE
      )
    }

    output[[output_name]] <- existing[[column_name]][index]
  }

  for(column_name in new_extra) {
    if(column_name %in% names(output)) {
      next
    }

    if(column_name %in% names(existing)) {
      output[[column_name]] <- existing[[column_name]][index]
    } else {
      output[[column_name]] <- new[[column_name]]
    }
  }

  rownames(output) <- NULL
  output
}


#' Build the metadata state after applying Trait reconciliation decisions.
tarpuy_finalize_trait_metadata <- function(
    old_metadata,
    new_metadata,
    renamed_sources = character(0),
    deleted_columns = character(0)
) {
  old <- tarpuy_normalize_trait_metadata(old_metadata)
  new <- tarpuy_normalize_trait_metadata(new_metadata)

  renamed_sources <- unique(.tarpuy_nonempty_character(renamed_sources))
  deleted_columns <- unique(.tarpuy_nonempty_character(deleted_columns))

  new$status <- "active"
  active_columns <- unique(new$generated_column)

  retained_old <- old[
    !old$generated_column %in% renamed_sources &
      !old$generated_column %in% deleted_columns &
      !old$generated_column %in% active_columns,
    ,
    drop = FALSE
  ]

  if(nrow(retained_old) > 0L) {
    retained_old$status <- "historical"
  }

  out <- rbind(retained_old, new)

  if(nrow(out) == 0L) {
    return(tarpuy_empty_trait_metadata())
  }

  out$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  duplicate_key <- paste(
    out$fieldbook_sheet,
    out$traits_sheet,
    out$trait_id,
    out$generated_column,
    out$status,
    sep = "\r"
  )
  out <- out[!duplicated(duplicate_key, fromLast = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}
