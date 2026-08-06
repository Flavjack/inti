#' Fieldbook plot experimental designs
#'
#' Plot fieldbook sketches according to the experimental design type.
#'
#' This function is the common plotting interface used by TARPUY. It reads the
#' design stored in the fieldbook, chooses the corresponding plotter and sends
#' the same plotting arguments to all supported design types.
#'
#' @param data Fieldbook data frame or a design object containing a fieldbook.
#' @param factor Character scalar. Column used to color experimental units.
#'   When omitted, `NA`, an empty string or `"auto"`, TARPUY selects a design-
#'   appropriate default: the first experimental factor for CRD, RCBD and
#'   Splitplot-RCBD, and `type` for augmented designs.
#' @param fill Character vector. Column or columns used as labels inside the
#'   experimental units. Defaults to `"plots"`.
#' @param xlab Character scalar. Optional x-axis title. When `NULL`, each
#'   plotter determines the title from the design geometry.
#' @param ylab Character scalar. Optional y-axis title. When `NULL`, each
#'   plotter determines whether the physical `rows` represent rows, blocks or
#'   subplot positions.
#' @param glab Character scalar. Optional legend title.
#' @param text_size Optional positive numeric scalar indicating the plot-label
#'   font size in typographic points (`pt`). When `NULL` or `NA`, each plotter
#'   calculates a suitable size automatically.
#' @param wrap_width Optional positive integer retained for backward
#'   compatibility. When `NULL` or `NA`, label wrapping is calculated
#'   automatically by the selected plotter. This argument does not need to be
#'   exposed as a control in the TARPUY interface.
#' @param font_family Character scalar retained for programmatic use. Defaults
#'   to `"Open Sans"`; the plotters fall back to `"sans"` when necessary.
#' @param font_face Character scalar retained for programmatic use. One of
#'   `"plain"`, `"bold"`, `"italic"` or `"bold.italic"`.
#'
#' @details
#' Supported designs and plotters are:
#'
#' \itemize{
#'   \item CRD and RCBD: `plot_standard_design()`.
#'   \item Augmented: `plot_augmented_design()`.
#'   \item Splitplot-RCBD: `plot_split_rcbd_design()`.
#' }
#'
#' The function never recalculates or rearranges the design. All plotters use
#' `cols` as the x coordinate and `rows` as the y coordinate. Columns such as
#' `block` are used only for labels, grouping or faceting.
#'
#' `wrap_width`, `font_family` and `font_face` remain available to avoid
#' breaking existing programmatic calls, but the TARPUY frontend should expose
#' only the general `text_size` control. Automatic label wrapping is used when
#' `wrap_width = NULL`.
#'
#' @return A `ggplot` object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' tarpuy_plotdesign(
#'   data = fieldbook,
#'   factor = "auto",
#'   fill = c("plots", "ntreat"),
#'   text_size = 9
#' )
#'
#' }

tarpuy_plotdesign <- function(
    data,
    factor = NA,
    fill = "plots",
    xlab = NULL,
    ylab = NULL,
    glab = NULL,
    text_size = NULL,
    wrap_width = NULL,
    font_family = "Open Sans",
    font_face = "plain"
) {
  
  # -------------------------------------------------------------------------
  # Helpers -----------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  is_missing_scalar <- function(x) {
    
    if(is.null(x) || length(x) == 0L) {
      return(TRUE)
    }
    
    if(length(x) != 1L) {
      return(FALSE)
    }
    
    if(is.na(x)) {
      return(TRUE)
    }
    
    is.character(x) && !nzchar(trimws(x))
  }
  
  validate_optional_character_scalar <- function(x, name) {
    
    if(is_missing_scalar(x)) {
      return(NULL)
    }
    
    if(length(x) != 1L || !is.character(x) || is.na(x)) {
      stop(
        "'", name, "' must be a character scalar, NA, or NULL.",
        call. = FALSE
      )
    }
    
    value <- trimws(x)
    
    if(!nzchar(value)) {
      return(NULL)
    }
    
    value
  }
  
  validate_optional_positive_number <- function(x, name) {
    
    if(is_missing_scalar(x)) {
      return(NULL)
    }
    
    if(
      length(x) != 1L ||
      !is.numeric(x) ||
      is.na(x) ||
      !is.finite(x) ||
      x <= 0
    ) {
      stop(
        "'", name,
        "' must be a positive numeric scalar, NA, or NULL.",
        call. = FALSE
      )
    }
    
    as.numeric(x)
  }
  
  validate_optional_positive_integer <- function(x, name) {
    
    value <- validate_optional_positive_number(x, name)
    
    if(is.null(value)) {
      return(NULL)
    }
    
    if(value != floor(value)) {
      stop(
        "'", name,
        "' must be a positive integer, NA, or NULL.",
        call. = FALSE
      )
    }
    
    as.integer(value)
  }
  
  extract_fieldbook <- function(x) {
    
    if(is.data.frame(x)) {
      return(x)
    }
    
    if(!is.list(x)) {
      stop(
        "'data' must be a fieldbook data frame or a design object ",
        "containing a fieldbook.",
        call. = FALSE
      )
    }
    
    preferred_names <- c("fieldbook", "fb")
    
    for(name in preferred_names) {
      if(
        name %in% names(x) &&
        is.data.frame(x[[name]])
      ) {
        return(x[[name]])
      }
    }
    
    data_frame_items <- which(
      vapply(x, is.data.frame, logical(1))
    )
    
    if(length(data_frame_items) == 1L) {
      return(x[[data_frame_items]])
    }
    
    if(length(data_frame_items) > 1L) {
      stop(
        "'data' contains more than one unnamed data frame. ",
        "Provide the fieldbook data frame directly or name it 'fieldbook'.",
        call. = FALSE
      )
    }
    
    stop(
      "'data' must be a fieldbook data frame or a design object ",
      "containing a fieldbook.",
      call. = FALSE
    )
  }
  
  detect_experimental_factors <- function(fieldbook, design_type) {
    
    if(design_type == "augmented") {
      return(character(0))
    }
    
    nms <- names(fieldbook)
    ntreat_position <- match("ntreat", nms)
    
    if(is.na(ntreat_position)) {
      return(character(0))
    }
    
    end_markers <- if(design_type == "split-rcbd") {
      c("wp_sp", "sort", "block", "rep", "rows", "cols", "design")
    } else {
      c("sort", "block", "rep", "rows", "cols", "design")
    }
    
    end_positions <- match(end_markers, nms)
    end_positions <- end_positions[
      !is.na(end_positions) & end_positions > ntreat_position
    ]
    
    end_position <- if(length(end_positions) > 0L) {
      min(end_positions)
    } else {
      length(nms) + 1L
    }
    
    if(end_position <= ntreat_position + 1L) {
      return(character(0))
    }
    
    candidates <- nms[
      seq.int(ntreat_position + 1L, end_position - 1L)
    ]
    
    reserved <- c(
      "qrcode", "plots", "ntreat", "entry", "type", "wp_sp",
      "sort", "rep", "block", "rows", "cols", "design"
    )
    
    candidates <- setdiff(candidates, reserved)
    candidates[nzchar(trimws(candidates))]
  }
  
  resolve_default_factor <- function(fieldbook, design_type) {
    
    if(design_type == "augmented") {
      
      if("type" %in% names(fieldbook)) {
        return("type")
      }
      
      if("entry" %in% names(fieldbook)) {
        return("entry")
      }
      
      stop(
        "The augmented fieldbook requires column 'type' or 'entry' ",
        "to determine the default color variable.",
        call. = FALSE
      )
    }
    
    factor_columns <- detect_experimental_factors(
      fieldbook = fieldbook,
      design_type = design_type
    )
    
    if(length(factor_columns) > 0L) {
      return(factor_columns[1L])
    }
    
    if(design_type == "split-rcbd" && "wp_sp" %in% names(fieldbook)) {
      return("wp_sp")
    }
    
    if("ntreat" %in% names(fieldbook)) {
      return("ntreat")
    }
    
    stop(
      "No experimental factor or treatment column is available for ",
      "the default color variable.",
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------------
  # Fieldbook ---------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  fieldbook <- extract_fieldbook(data)
  
  if(nrow(fieldbook) == 0L) {
    stop(
      "'fieldbook' must contain at least one experimental unit.",
      call. = FALSE
    )
  }
  
  if(anyDuplicated(names(fieldbook))) {
    duplicated_names <- unique(
      names(fieldbook)[duplicated(names(fieldbook))]
    )
    
    stop(
      "The fieldbook contains duplicated column names: ",
      paste(duplicated_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  required_coordinates <- c("rows", "cols")
  missing_coordinates <- setdiff(required_coordinates, names(fieldbook))
  
  if(length(missing_coordinates) > 0L) {
    stop(
      "The fieldbook is missing required coordinate columns: ",
      paste(missing_coordinates, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if(!"design" %in% names(fieldbook)) {
    stop(
      "Column 'design' is required to choose the plot method.",
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------------
  # Design type -------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  design_raw <- trimws(as.character(fieldbook$design))
  design_raw <- design_raw[!is.na(design_raw) & nzchar(design_raw)]
  
  if(length(design_raw) == 0L) {
    stop(
      "Column 'design' has no valid design value.",
      call. = FALSE
    )
  }
  
  design_type <- unique(
    vapply(
      design_raw,
      normalize_tarpuy_design_type,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )
  )
  
  design_type <- design_type[
    !is.na(design_type) & nzchar(design_type)
  ]
  
  if(length(design_type) == 0L) {
    stop(
      "Column 'design' has no recognized design value.",
      call. = FALSE
    )
  }
  
  if(length(design_type) > 1L) {
    stop(
      "The fieldbook contains more than one experimental design: ",
      paste(design_type, collapse = ", "),
      ". Plot each design separately.",
      call. = FALSE
    )
  }
  
  design_type <- design_type[1L]
  
  supported_designs <- c(
    "crd",
    "rcbd",
    "augmented",
    "split-rcbd"
  )
  
  if(!design_type %in% supported_designs) {
    stop(
      "Plot method not implemented for design '",
      design_type,
      "'. Supported designs are: ",
      paste(supported_designs, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------------
  # Common interface --------------------------------------------------------
  # -------------------------------------------------------------------------
  
  factor_is_auto <- is_missing_scalar(factor) ||
    (
      length(factor) == 1L &&
        is.character(factor) &&
        tolower(trimws(factor)) == "auto"
    )
  
  if(factor_is_auto) {
    factor <- resolve_default_factor(
      fieldbook = fieldbook,
      design_type = design_type
    )
  } else {
    
    if(length(factor) != 1L || !is.character(factor) || is.na(factor)) {
      stop(
        "'factor' must be one column name, 'auto', NA, or NULL.",
        call. = FALSE
      )
    }
    
    factor <- trimws(factor)
  }
  
  if(!factor %in% names(fieldbook)) {
    stop(
      "Column selected in 'factor' was not found: ",
      factor,
      ". Available columns are: ",
      paste(names(fieldbook), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  if(
    is.null(fill) ||
    length(fill) == 0L ||
    all(is.na(fill)) ||
    all(!nzchar(trimws(as.character(fill))))
  ) {
    fill <- "plots"
  }
  
  fill <- trimws(as.character(fill))
  fill <- unique(fill[!is.na(fill) & nzchar(fill)])
  
  missing_fill <- setdiff(fill, names(fieldbook))
  
  if(length(missing_fill) > 0L) {
    stop(
      "Columns selected in 'fill' were not found: ",
      paste(missing_fill, collapse = ", "),
      ". Available columns are: ",
      paste(names(fieldbook), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  xlab <- validate_optional_character_scalar(xlab, "xlab")
  ylab <- validate_optional_character_scalar(ylab, "ylab")
  glab <- validate_optional_character_scalar(glab, "glab")
  
  text_size <- validate_optional_positive_number(
    text_size,
    "text_size"
  )
  
  wrap_width <- validate_optional_positive_integer(
    wrap_width,
    "wrap_width"
  )
  
  font_family <- validate_optional_character_scalar(
    font_family,
    "font_family"
  )
  
  if(is.null(font_family)) {
    font_family <- "Open Sans"
  }
  
  allowed_faces <- c(
    "plain",
    "bold",
    "italic",
    "bold.italic"
  )
  
  if(
    length(font_face) != 1L ||
    !is.character(font_face) ||
    is.na(font_face) ||
    !font_face %in% allowed_faces
  ) {
    stop(
      "'font_face' must be one of: ",
      paste(allowed_faces, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  
  # -------------------------------------------------------------------------
  # Plotter registry --------------------------------------------------------
  # -------------------------------------------------------------------------
  
  plotter_registry <- c(
    "crd" = "plot_standard_design",
    "rcbd" = "plot_standard_design",
    "augmented" = "plot_augmented_design",
    "split-rcbd" = "plot_split_rcbd_design"
  )
  
  plotter_name <- unname(plotter_registry[[design_type]])
  
  if(
    is.null(plotter_name) ||
    !exists(plotter_name, mode = "function", inherits = TRUE)
  ) {
    stop(
      "The plotting function '",
      plotter_name,
      "' is not available.",
      call. = FALSE
    )
  }
  
  plotter <- get(
    plotter_name,
    mode = "function",
    inherits = TRUE
  )
  
  plot_arguments <- list(
    data = fieldbook,
    factor = factor,
    fill = fill,
    xlab = xlab,
    ylab = ylab,
    glab = glab,
    text_size = text_size,
    wrap_width = wrap_width,
    font_family = font_family,
    font_face = font_face
  )
  
  do.call(plotter, plot_arguments)
}