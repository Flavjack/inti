# Internal helpers ---------------------------------------------------------

.tarpuy_empty_to_na <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[is.na(x) | x == "" | toupper(x) %in% c("NA", "NULL")] <- NA_character_
  x
}


.tarpuy_normalize_trait_token <- function(x) {
  x <- .tarpuy_empty_to_na(x)
  valid <- !is.na(x)
  
  if (!any(valid)) {
    return(x)
  }
  
  normalized <- iconv(x[valid], from = "", to = "ASCII//TRANSLIT")
  failed <- is.na(normalized)
  normalized[failed] <- x[valid][failed]
  
  normalized <- gsub("[[:space:]]+", "", normalized)
  normalized <- gsub("[^[:alnum:]_]", "_", normalized)
  normalized <- gsub("_+", "_", normalized)
  normalized <- gsub("^_+|_+$", "", normalized)
  normalized[normalized == ""] <- NA_character_
  
  x[valid] <- normalized
  x
}


.tarpuy_parse_samples <- function(x, source_row = NA_integer_) {
  token <- .tarpuy_normalize_trait_token(x)
  
  if (length(token) == 0L || is.na(token) || !nzchar(token)) {
    return(list(prefix = "", count = NA_integer_))
  }
  
  valid_pattern <- "^([[:alpha:]_][[:alnum:]_]*)?[0-9]+$"
  
  if (!grepl(valid_pattern, token)) {
    row_text <- if (!is.na(source_row)) paste0(" en la fila ", source_row) else ""
    stop(
      "{samples}", row_text,
      " debe ser un entero positivo o un identificador seguido de un ",
      "entero positivo; por ejemplo: 4, q4 o plant10.",
      call. = FALSE
    )
  }
  
  number_match <- regexpr("[0-9]+$", token)
  number_text <- regmatches(token, number_match)
  count <- suppressWarnings(as.integer(number_text))
  
  prefix <- if (number_match > 1L) {
    substr(token, 1L, number_match - 1L)
  } else {
    ""
  }
  
  if (is.na(count) || count < 1L) {
    row_text <- if (!is.na(source_row)) paste0(" en la fila ", source_row) else ""
    stop(
      "{samples}", row_text,
      " debe contener un entero mayor que cero.",
      call. = FALSE
    )
  }
  
  list(prefix = prefix, count = count)
}


.tarpuy_split_when <- function(x) {
  x <- .tarpuy_empty_to_na(x)
  
  if (length(x) == 0L || is.na(x)) {
    return(NA_character_)
  }
  
  moments <- strsplit(x, "\\s*[,;]\\s*", perl = TRUE)[[1L]]
  moments <- .tarpuy_normalize_trait_token(moments)
  moments <- moments[!is.na(moments) & nzchar(moments)]
  
  if (length(moments) == 0L) NA_character_ else moments
}


.tarpuy_categories_json <- function(categories, format) {
  format <- tolower(trimws(as.character(format)))
  
  if (!format %in% c("scategorical", "mcategorical", "categorical", "multicat")) {
    return("[]")
  }
  
  categories <- .tarpuy_empty_to_na(categories)
  
  if (length(categories) == 0L || is.na(categories)) {
    return("[]")
  }
  
  values <- strsplit(categories, "\\s*[,;]\\s*", perl = TRUE)[[1L]]
  values <- trimws(values)
  values <- values[nzchar(values)]
  
  if (length(values) == 0L) {
    return("[]")
  }
  
  items <- vapply(
    values,
    function(value) {
      quoted <- encodeString(value, quote = '"')
      paste0('{"label":', quoted, ',"value":', quoted, "}")
    },
    character(1L),
    USE.NAMES = FALSE
  )
  
  paste0("[", paste(items, collapse = ","), "]")
}


.tarpuy_empty_trait_export <- function() {
  tibble::tibble(
    trait = character(),
    format = character(),
    defaultValue = character(),
    minimum = character(),
    maximum = character(),
    details = character(),
    categories = character(),
    isVisible = character(),
    realPosition = character()
  )
}


.tarpuy_empty_trait_metadata_export <- function() {
  tibble::tibble(
    trait_id = character(),
    generated_column = character(),
    generated_index = integer(),
    source_row = integer()
  )
}


# Main function ------------------------------------------------------------

#' Field book traits
#'
#' Function to export a field book and its trait definitions for use in the
#' Field Book app.
#'
#' @param fieldbook Experimental field book `[data.frame]`.
#' @param last_factor Optional name of the last structural column to include in
#'   the CSV exported to Field Book `[character: colname]`.
#' @param traits Traits information `[data.frame or list]`.
#'
#' @details
#' The trait sheet can contain the columns `variable`, `{trait}`, `{when}`,
#' `{samples}`, `{format}`, `units`, `details`, and `categories`.
#'
#' Spaces inside `{trait}`, `{when}`, and the textual part of `{samples}` are
#' removed. The components are joined with underscores. For example,
#' `{trait} = "G"`, `{when} = "Dia 1"`, and `{samples} = "plant3"` generate
#' `G_Dia1_plant1`, `G_Dia1_plant2`, and `G_Dia1_plant3`.
#'
#' @return A list with four elements:
#' * `fieldbook`: the field book including empty trait columns;
#' * `traits`: the trait definition table used to export the `.trt` file;
#' * `fb`: the base field book used to export the Field Book CSV;
#' * `metadata`: internal mapping between a stable Trait ID and every generated
#'   fieldbook column. This element is used by TARPUY and is not exported to the
#'   fieldbook or to Field Book mobile files.
#'
#' @export
#'
#' @examples
#'
#' library(inti)
#'
#' fieldbook <- inti::potato
#'
#' traits <- list(
#'   list(
#'     variable = "altura de planta",
#'     trait = "altp",
#'     format = "numeric",
#'     when = "Dia 30, Dia 40, Dia 50",
#'     samples = "plant3",
#'     units = "cm",
#'     details = NA,
#'     minimum = 0,
#'     maximum = 100
#'   ),
#'   list(
#'     variable = "severidad",
#'     trait = "svr",
#'     format = "scategorical",
#'     when = "30, 40, 50",
#'     samples = 1,
#'     units = "scale",
#'     details = NA,
#'     categories = "1, 3, 5, 7, 9"
#'   ),
#'   list(
#'     variable = "foto",
#'     trait = "foto",
#'     format = "photo",
#'     when = "hrv, pshrv",
#'     samples = NA,
#'     units = "image",
#'     details = NA
#'   ),
#'   list(
#'     variable = "germinacion",
#'     trait = "G",
#'     format = "boolean",
#'     when = "0, 1, 2",
#'     samples = 1,
#'     units = "logical",
#'     details = NA
#'   )
#' )
#'
#' fbapp <- tarpuy_traits(fieldbook, last_factor = "bloque", traits)
#'
#' \dontrun{
#'
#' library(inti)
#' library(gsheet)
#'
#' url_ds <- paste0(
#'   "https://docs.google.com/spreadsheets/d/",
#'   "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1278145622"
#' )
#'
#' ds <- gsheet2tbl(url_ds)
#' fb <- ds |> tarpuy_design()
#'
#' url_trt <- paste0(
#'   "https://docs.google.com/spreadsheets/d/",
#'   "1510fOKj0g4CDEAFkrpFbr-zNMnle_Hou9O_wuf7Vdo4/edit?gid=1665653985"
#' )
#'
#' traits <- gsheet2tbl(url_trt)
#' fbapp <- tarpuy_traits(fb, last_factor = "cols", traits)
#' }
#'

tarpuy_traits <- function(fieldbook = NULL,
                          last_factor = NULL,
                          traits = NULL) {
  
  # Conditions -------------------------------------------------------------
  
  if (is.null(fieldbook)) {
    stop("Select your field book", call. = FALSE)
  }
  
  fb <- tibble::as_tibble(fieldbook)
  
  if (!is.null(last_factor)) {
    if (length(last_factor) != 1L || is.na(last_factor) || !nzchar(trimws(last_factor))) {
      stop("Select a valid last factor", call. = FALSE)
    }
    
    last_factor <- trimws(last_factor)
    last_position <- match(last_factor, names(fb))
    
    if (is.na(last_position)) {
      stop(
        "The last factor '", last_factor, "' was not found in the field book.",
        call. = FALSE
      )
    }
    
    fb <- fb[, seq_len(last_position), drop = FALSE]
  }
  
  if (is.null(traits)) {
    return(list(
      fieldbook = fb,
      traits = NA,
      fb = fb,
      metadata = .tarpuy_empty_trait_metadata_export()
    ))
  }
  
  # Standardize the traits table ------------------------------------------
  
  traitstb <- dplyr::bind_rows(traits)
  
  if (nrow(traitstb) == 0L) {
    return(list(
      fieldbook = fb,
      traits = .tarpuy_empty_trait_export(),
      fb = fb,
      metadata = .tarpuy_empty_trait_metadata_export()
    ))
  }
  
  names(traitstb) <- gsub("\\{|\\}", "", names(traitstb))
  
  duplicated_columns <- unique(names(traitstb)[duplicated(names(traitstb))])
  
  if (length(duplicated_columns) > 0L) {
    stop(
      "Duplicated columns were found in the traits table after removing braces: ",
      paste(duplicated_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!"trait" %in% names(traitstb)) {
    stop("The traits table must contain the column {trait}.", call. = FALSE)
  }
  
  if ("defaultValue" %in% names(traitstb) && !"default" %in% names(traitstb)) {
    traitstb$default <- traitstb$defaultValue
  }
  
  required_columns <- c(
    "variable", "trait", "when", "samples", "format", "units",
    "details", "categories", "default", "minimum", "maximum"
  )
  
  missing_columns <- setdiff(required_columns, names(traitstb))
  
  for (column in missing_columns) {
    traitstb[[column]] <- NA_character_
  }

  if(!"_trait_id" %in% names(traitstb)) {
    traitstb[["_trait_id"]] <- NA_character_
  }
  
  traitstb <- traitstb |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::mutate(.source_row = dplyr::row_number())
  
  traitstb$variable <- .tarpuy_empty_to_na(traitstb$variable)
  traitstb$trait <- .tarpuy_empty_to_na(traitstb$trait)
  traitstb$details <- .tarpuy_empty_to_na(traitstb$details)
  
  missing_details <- is.na(traitstb$details)
  traitstb$details[missing_details] <- traitstb$variable[missing_details]
  
  # Remove blank rows and the untouched template rows whose trait is "X".
  traitstb <- traitstb |>
    dplyr::filter(!is.na(.data$trait), trimws(.data$trait) != "X")
  
  if (nrow(traitstb) == 0L) {
    return(list(
      fieldbook = fb,
      traits = .tarpuy_empty_trait_export(),
      fb = fb,
      metadata = .tarpuy_empty_trait_metadata_export()
    ))
  }

  traitstb$`_trait_id` <- .tarpuy_empty_to_na(traitstb$`_trait_id`)
  missing_trait_ids <- is.na(traitstb$`_trait_id`)
  traitstb$`_trait_id`[missing_trait_ids] <- paste0(
    "ROW",
    traitstb$.source_row[missing_trait_ids]
  )
  
  # Expand moments and samples row by row ---------------------------------
  
  expanded_rows <- vector("list", 0L)
  expanded_index <- 0L
  
  for (i in seq_len(nrow(traitstb))) {
    source_row <- as.integer(traitstb$.source_row[[i]])
    base_trait <- .tarpuy_normalize_trait_token(traitstb$trait[[i]])
    
    if (is.na(base_trait) || !nzchar(base_trait)) {
      stop(
        "{trait} in row ", source_row,
        " is empty after normalization.",
        call. = FALSE
      )
    }
    
    moments <- .tarpuy_split_when(traitstb$when[[i]])
    sample_info <- .tarpuy_parse_samples(
      traitstb$samples[[i]],
      source_row = source_row
    )
    
    sample_labels <- if (is.na(sample_info$count)) {
      NA_character_
    } else {
      paste0(sample_info$prefix, seq_len(sample_info$count))
    }

    generated_index <- 0L
    
    for (moment in moments) {
      for (sample_label in sample_labels) {
        components <- c(base_trait, moment, sample_label)
        components <- components[!is.na(components) & nzchar(components)]
        generated_name <- paste(components, collapse = "_")
        
        expanded_index <- expanded_index + 1L
        generated_index <- generated_index + 1L
        expanded_rows[[expanded_index]] <- traitstb[i, , drop = FALSE]
        expanded_rows[[expanded_index]]$trait <- generated_name
        expanded_rows[[expanded_index]]$.generated_index <- generated_index
      }
    }
  }
  
  expanded <- dplyr::bind_rows(expanded_rows)
  generated_names <- expanded$trait

  metadata <- tibble::tibble(
    trait_id = as.character(expanded$`_trait_id`),
    generated_column = as.character(generated_names),
    generated_index = as.integer(expanded$.generated_index),
    source_row = as.integer(expanded$.source_row)
  )
  
  duplicated_traits <- unique(generated_names[duplicated(generated_names)])
  
  if (length(duplicated_traits) > 0L) {
    stop(
      "No se puede generar el fieldbook. Existen nombres de variables ",
      "duplicados después de normalizar los espacios: ",
      paste0("`", duplicated_traits, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  fieldbook_collisions <- intersect(generated_names, names(fb))
  
  if (length(fieldbook_collisions) > 0L) {
    stop(
      "No se puede generar el fieldbook. Las siguientes variables de Traits ",
      "coinciden con columnas existentes del fieldbook: ",
      paste0("`", fieldbook_collisions, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  # Build the .trt table ---------------------------------------------------
  
  original_format <- tolower(trimws(expanded$format))
  
  default_value <- .tarpuy_empty_to_na(expanded$default)
  boolean_rows <- which(!is.na(original_format) & original_format == "boolean")
  default_value[boolean_rows] <- "false"
  
  categories_json <- vapply(
    seq_len(nrow(expanded)),
    function(i) {
      .tarpuy_categories_json(
        categories = expanded$categories[[i]],
        format = original_format[[i]]
      )
    },
    character(1L),
    USE.NAMES = FALSE
  )
  
  output_format <- dplyr::case_when(
    original_format == "scategorical" ~ "categorical",
    original_format == "mcategorical" ~ "multicat",
    TRUE ~ original_format
  )
  
  fbapp <- tibble::tibble(
    trait = generated_names,
    format = output_format,
    defaultValue = default_value,
    minimum = .tarpuy_empty_to_na(expanded$minimum),
    maximum = .tarpuy_empty_to_na(expanded$maximum),
    details = .tarpuy_empty_to_na(expanded$details),
    categories = categories_json,
    isVisible = "true",
    realPosition = as.character(seq_len(nrow(expanded)))
  )
  
  # Add empty trait columns without changing the original row relationship --
  
  fbtraits <- fb
  
  for (trait_name in generated_names) {
    fbtraits[[trait_name]] <- NA
  }
  
  if ("plots" %in% names(fbtraits)) {
    fbtraits <- dplyr::arrange(fbtraits, .data$plots)
  }
  
  list(
    fieldbook = fbtraits,
    traits = fbapp,
    fb = fb,
    metadata = metadata
  )
}
