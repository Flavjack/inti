#' Fieldbook experimental designs
#'
#' Function to deploy the experimental designs currently supported by TARPUY.
#'
#' @param data Experimental design data frame containing factor names and
#'   levels. A design sheet may also include the columns \code{{arguments}} and
#'   \code{{values}} to override the function arguments.
#' @param nfactors Number of factors in the experiment `[default = 1]`.
#' @param type Type of experimental arrangement `[default = "crd"]`.
#'   Supported designs are `"crd"`, `"rcbd"`, `"augmented"`, and
#'   `"split-rcbd"`. The aliases `"dca"` and `"dbca"` are accepted.
#' @param rep Number of replications or blocks in the experiment
#'   `[default = 2]`.
#' @param zigzag Arrange the physical layout in zigzag order
#'   `[logical: FALSE]`.
#' @param nrows Number of rows in the physical field layout. When missing, the
#'   corresponding design function calculates the layout.
#' @param serie Base number used to generate plot identifiers
#'   `[numeric: 100]`.
#' @param seed Seed used for reproducible randomization. `0`, `NA`, and `NULL`
#'   preserve the historical TARPUY behavior of using a random seed.
#' @param project Barcode prefix for data collection.
#' @param qrcode Template used to concatenate QR-code fields
#'   `[character: "{project}{plots}"]`.
#'
#' @details The design sheet can include two optional columns named
#'   \code{{arguments}} and \code{{values}}. Values supplied in those columns
#'   override the corresponding function arguments. Factor columns are the
#'   remaining columns whose names are not enclosed in braces (`{}`) or square
#'   brackets (`[]`).
#'
#'   TARPUY currently dispatches only designs with an implemented and validated
#'   generator: CRD/DCA, RCBD/DBCA, augmented, and split-plot RCBD. Other design
#'   identifiers are rejected explicitly instead of being routed to incomplete
#'   generators.
#'
#' @return A data frame containing the generated fieldbook.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' library(gsheet)
#'
#' url <- paste0(
#'   "https://docs.google.com/spreadsheets/d/",
#'   "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit"
#' )
#'
#' fb <- gsheet2tbl(url)
#'
#' dsg <- fb %>% tarpuy_design()
#'
#' dsg %>% tarpuy_plotdesign()
#'
#' }

tarpuy_design <- function(data,
                          nfactors = 1,
                          type = "crd",
                          rep = 2,
                          zigzag = FALSE,
                          nrows = NA,
                          serie = 100,
                          seed = NULL,
                          project = NA,
                          qrcode = "{project}{plots}") {
  
  # -------------------------------------------------------------------------
  # Internal helpers
  # -------------------------------------------------------------------------
  
  is_blank_scalar <- function(x) {
    is.null(x) ||
      length(x) == 0L ||
      (length(x) == 1L && is.na(x)) ||
      (length(x) == 1L && is.character(x) && !nzchar(trimws(x)))
  }
  
  has_real_value <- function(x) {
    if(is.list(x)) {
      x <- unlist(x, recursive = TRUE, use.names = FALSE)
    }
    
    if(length(x) == 0L) {
      return(FALSE)
    }
    
    x_chr <- trimws(as.character(x))
    any(!is.na(x) & nzchar(x_chr))
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
  
  as_optional_positive_integer <- function(x, name, default = NULL) {
    if(is_blank_scalar(x)) {
      return(default)
    }
    
    as_positive_integer(x, name)
  }
  
  as_logical_flag <- function(x, name, default) {
    if(is_blank_scalar(x)) {
      return(default)
    }
    
    if(length(x) != 1L) {
      stop("'", name, "' must contain one logical value.", call. = FALSE)
    }
    
    if(is.logical(x) && !is.na(x)) {
      return(x)
    }
    
    value <- tolower(trimws(as.character(x)))
    
    true_values <- c("true", "t", "yes", "y", "1")
    false_values <- c("false", "f", "no", "n", "0")
    
    if(value %in% true_values) {
      return(TRUE)
    }
    
    if(value %in% false_values) {
      return(FALSE)
    }
    
    stop(
      "'", name, "' must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  as_seed <- function(x) {
    if(is_blank_scalar(x)) {
      return(NULL)
    }
    
    value <- suppressWarnings(as.numeric(as.character(x)))
    
    if(length(value) != 1L ||
       is.na(value) ||
       !is.finite(value) ||
       value < 0 ||
       value != floor(value) ||
       value > .Machine$integer.max) {
      stop(
        "'seed' must be a non-negative integer, 0, NA, or NULL.",
        call. = FALSE
      )
    }
    
    # Historical TARPUY behavior: seed = 0 means a new randomization.
    if(value == 0) {
      return(NULL)
    }
    
    as.integer(value)
  }
  
  normalize_project <- function(x) {
    if(is_blank_scalar(x)) {
      return("")
    }
    
    if(length(x) != 1L) {
      stop("'project' must contain one value.", call. = FALSE)
    }
    
    value <- trimws(as.character(x))
    value_ascii <- suppressWarnings(iconv(value, to = "ASCII//TRANSLIT"))
    
    if(!is.na(value_ascii)) {
      value <- value_ascii
    }
    
    value <- toupper(value)
    gsub("[[:space:]]+", "-", value)
  }
  
  normalize_qrcode <- function(x, default) {
    if(is_blank_scalar(x)) {
      x <- default
    }
    
    if(length(x) != 1L || is.na(x) || !nzchar(trimws(as.character(x)))) {
      stop("'qrcode' must be a non-empty character template.", call. = FALSE)
    }
    
    trimws(as.character(x))
  }
  
  is_wrapped_name <- function(x, opening, closing) {
    startsWith(x, opening) & endsWith(x, closing)
  }
  
  read_argument_table <- function(x) {
    required <- c("{arguments}", "{values}")
    present <- required %in% names(x)
    
    if(any(present) && !all(present)) {
      stop(
        "The design sheet must contain both '{arguments}' and '{values}', ",
        "or neither of them.",
        call. = FALSE
      )
    }
    
    if(!all(present)) {
      return(list())
    }
    
    argument_names <- trimws(as.character(x[["{arguments}"]]))
    argument_values <- x[["{values}"]]
    
    keep <- !is.na(argument_names) & nzchar(argument_names)
    argument_names <- argument_names[keep]
    argument_values <- argument_values[keep]
    
    if(length(argument_names) == 0L) {
      return(list())
    }
    
    argument_names <- gsub("^\\{|\\}$", "", argument_names)
    argument_names <- tolower(trimws(argument_names))
    
    if(any(!nzchar(argument_names))) {
      stop(
        "The '{arguments}' column contains an empty argument name.",
        call. = FALSE
      )
    }
    
    duplicated_arguments <- unique(argument_names[duplicated(argument_names)])
    
    if(length(duplicated_arguments) > 0L) {
      stop(
        "Duplicated design arguments: ",
        paste(duplicated_arguments, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    
    stats::setNames(as.list(argument_values), argument_names)
  }
  
  argument_or_default <- function(arguments, name, default) {
    if(!name %in% names(arguments) || is_blank_scalar(arguments[[name]])) {
      return(default)
    }
    
    arguments[[name]]
  }
  
  extract_augmented_column <- function(x, requested_name) {
    normalized_names <- tolower(trimws(names(x)))
    matches <- which(normalized_names == requested_name)
    
    if(length(matches) == 0L) {
      stop(
        "Columns 'checks' and 'entries' are required for an augmented design.",
        call. = FALSE
      )
    }
    
    if(length(matches) > 1L) {
      stop(
        "The augmented design contains duplicated '", requested_name,
        "' columns.",
        call. = FALSE
      )
    }
    
    x[[matches]]
  }
  
  # -------------------------------------------------------------------------
  # Input and design-sheet validation
  # -------------------------------------------------------------------------
  
  if(missing(data) || is.null(data) || !is.data.frame(data)) {
    stop("'data' must be a data frame containing the design sheet.", call. = FALSE)
  }
  
  if(ncol(data) == 0L) {
    stop("'data' does not contain any columns.", call. = FALSE)
  }
  
  arguments <- read_argument_table(data)
  
  nfactors <- as_positive_integer(
    argument_or_default(arguments, "nfactors", nfactors),
    "nfactors"
  )
  
  type <- argument_or_default(arguments, "type", type)
  type <- normalize_tarpuy_design_type(type)
  
  if(is_blank_scalar(type)) {
    stop("'type' must identify an experimental design.", call. = FALSE)
  }
  
  type <- tolower(trimws(as.character(type[1L])))
  
  supported_designs <- c("crd", "rcbd", "augmented", "split-rcbd")
  
  if(!type %in% supported_designs) {
    stop(
      "Design '", type,
      "' is not implemented in TARPUY. Available designs: ",
      paste(supported_designs, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  rep <- as_positive_integer(
    argument_or_default(arguments, "rep", rep),
    "rep"
  )
  
  zigzag <- as_logical_flag(
    argument_or_default(arguments, "zigzag", zigzag),
    "zigzag",
    default = FALSE
  )
  
  nrows <- as_optional_positive_integer(
    argument_or_default(arguments, "nrows", nrows),
    "nrows",
    default = NA_integer_
  )
  
  serie <- as_positive_integer(
    argument_or_default(arguments, "serie", serie),
    "serie"
  )
  
  seed <- as_seed(argument_or_default(arguments, "seed", seed))
  
  project <- normalize_project(
    argument_or_default(arguments, "project", project)
  )
  
  qrcode <- normalize_qrcode(
    argument_or_default(arguments, "qrcode", qrcode),
    default = qrcode
  )
  
  blocks <- as_optional_positive_integer(
    argument_or_default(arguments, "blocks", NULL),
    "blocks",
    default = NULL
  )
  
  eu_block <- as_optional_positive_integer(
    argument_or_default(arguments, "eu_block", NULL),
    "eu_block",
    default = NULL
  )
  
  random <- as_logical_flag(
    argument_or_default(arguments, "random", TRUE),
    "random",
    default = TRUE
  )
  
  separate_checks <- as_logical_flag(
    argument_or_default(arguments, "separate_checks", TRUE),
    "separate_checks",
    default = TRUE
  )
  
  # Factor columns are all non-metadata columns with at least one value.
  column_names <- names(data)
  metadata_columns <-
    is_wrapped_name(column_names, "{", "}") |
    is_wrapped_name(column_names, "[", "]")
  
  factor_data <- data[, !metadata_columns, drop = FALSE]
  
  if(ncol(factor_data) > 0L) {
    populated <- vapply(factor_data, has_real_value, logical(1))
    factor_data <- factor_data[, populated, drop = FALSE]
  }
  
  # -------------------------------------------------------------------------
  # Closed design dispatcher
  # -------------------------------------------------------------------------
  
  design_registry <- list(
    crd = design_repblock,
    rcbd = design_repblock,
    augmented = design_augmented,
    `split-rcbd` = design_split_rcbd
  )
  
  if(type == "augmented") {
    checks <- extract_augmented_column(factor_data, "checks")
    entries <- extract_augmented_column(factor_data, "entries")
    
    if(identical(qrcode, "{project}{plots}")) {
      qrcode <- "{project}{plots}{entry}"
    }
    
    result <- design_registry[[type]](
      checks = checks,
      entries = entries,
      blocks = blocks,
      eu_block = eu_block,
      random = random,
      zigzag = zigzag,
      serie = serie,
      seed = seed,
      project = project,
      qrcode = qrcode,
      separate_checks = separate_checks
    )
    
  } else {
    if(ncol(factor_data) < nfactors) {
      stop(
        "The design requires ", nfactors,
        " factor column(s), but only ", ncol(factor_data),
        " populated factor column(s) were found.",
        call. = FALSE
      )
    }
    
    factor_names <- names(factor_data)[seq_len(nfactors)]
    
    if(any(is.na(factor_names)) || any(!nzchar(trimws(factor_names)))) {
      stop("Every factor column must have a non-empty name.", call. = FALSE)
    }
    
    factor_levels <- as.list(factor_data[, factor_names, drop = FALSE])
    
    if(type == "split-rcbd") {
      if(nfactors != 2L) {
        stop(
          "A split-plot RCBD requires exactly two factors: whole plot and subplot.",
          call. = FALSE
        )
      }
      
      result <- design_registry[[type]](
        nfactors = nfactors,
        factors = factor_levels,
        type = type,
        rep = rep,
        zigzag = zigzag,
        nrows = nrows,
        serie = serie,
        seed = seed,
        project = project,
        qrcode = qrcode
      )
      
    } else {
      # design_repblock() supports rep = 1, so the defective design_noreps()
      # branch is no longer required.
      result <- design_registry[[type]](
        nfactors = nfactors,
        factors = factor_levels,
        type = type,
        rep = rep,
        zigzag = zigzag,
        nrows = nrows,
        serie = serie,
        seed = seed,
        project = project,
        qrcode = qrcode
      )
    }
  }
  
  if(!is.list(result) || is.null(result$fieldbook)) {
    stop(
      "The selected design generator did not return a valid fieldbook.",
      call. = FALSE
    )
  }
  
  result$fieldbook
}
