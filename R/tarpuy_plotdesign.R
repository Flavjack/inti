#' Fieldbook plot experimental designs
#'
#' Plot fieldbook sketches according to the experimental design type.
#'
#' This function works as a dispatcher. It detects the design type from the
#' fieldbook and sends the data to the corresponding plotting function.
#'
#' @param data Fieldbook data frame or design object containing a fieldbook.
#' @param factor Character scalar. Column used to color experimental units.
#' @param fill Character vector. Column or columns used as labels inside
#'   experimental units.
#' @param xlab Character scalar. Optional x axis title.
#' @param ylab Character scalar. Optional y axis title.
#' @param glab Character scalar. Optional legend title.
#' @param text_size Optional positive numeric scalar indicating the plot-label
#'   font size in typographic points (`pt`). If `NULL` or `NA`, the selected
#'   plotting function calculates an automatic size.
#' @param wrap_width Optional positive integer indicating the approximate
#'   maximum number of characters per line. If `NULL` or `NA`, labels are not
#'   wrapped.
#' @param font_family Font family used in the sketch. Defaults to
#'   `"Open Sans"`. Each plotting function silently falls back to `"sans"`
#'   when the requested font is unavailable.
#' @param font_face Font face used in the sketch. Defaults to `"plain"`,
#'   equivalent to regular/normal text.
#'
#' @details
#' The text and font arguments are forwarded to:
#'
#' \itemize{
#'   \item `plot_standard_design()`
#'   \item `plot_augmented_design()`
#'   \item `plot_split_rcbd_design()`
#' }
#'
#' The visible name `Splitplot-RCBD` and accepted spelling variants are
#' normalized to the stable internal identifier `"split-rcbd"` before the
#' plotting method is selected. The fieldbook itself is not modified.
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
#'   factor = "entry",
#'   fill = c("plots", "entry"),
#'   text_size = 9,
#'   wrap_width = 14,
#'   font_family = "Open Sans",
#'   font_face = "plain"
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
  # Helper ------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  is_missing_scalar <- function(x) {
    
    is.null(x) ||
      length(x) == 0L ||
      (
        length(x) == 1L &&
          (
            is.na(x) ||
              (
                is.character(x) &&
                  trimws(x) == ""
              )
          )
      )
  }
  
  # -------------------------------------------------------------------------
  # Get fieldbook ------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(is.data.frame(data)) {
    
    fieldbook <- data
    
  } else if(
    is.list(data) &&
    "fieldbook" %in% names(data) &&
    is.data.frame(data$fieldbook)
  ) {
    
    fieldbook <- data$fieldbook
    
  } else if(
    is.list(data) &&
    length(data) >= 1L &&
    is.data.frame(data[[1L]])
  ) {
    
    fieldbook <- data[[1L]]
    
  } else {
    
    stop(
      "'data' must be a fieldbook data frame or a design object ",
      "containing a fieldbook."
    )
  }
  
  # -------------------------------------------------------------------------
  # Checks ------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(nrow(fieldbook) == 0L) {
    stop(
      "'fieldbook' must contain at least one experimental unit."
    )
  }
  
  if(!"design" %in% names(fieldbook)) {
    stop(
      "Column 'design' is required to choose the plot method."
    )
  }
  
  if(!is_missing_scalar(text_size)) {
    
    if(
      length(text_size) != 1L ||
      !is.numeric(text_size) ||
      !is.finite(text_size) ||
      text_size <= 0
    ) {
      stop(
        "'text_size' must be a positive numeric value in points, ",
        "NA, or NULL."
      )
    }
    
    text_size <- as.numeric(
      text_size
    )
    
  } else {
    
    text_size <- NULL
  }
  
  if(!is_missing_scalar(wrap_width)) {
    
    if(
      length(wrap_width) != 1L ||
      !is.numeric(wrap_width) ||
      !is.finite(wrap_width) ||
      wrap_width < 1 ||
      wrap_width != floor(wrap_width)
    ) {
      stop(
        "'wrap_width' must be a positive integer, NA, or NULL."
      )
    }
    
    wrap_width <- as.integer(
      wrap_width
    )
    
  } else {
    
    wrap_width <- NULL
  }
  
  allowed_faces <- c(
    "plain",
    "bold",
    "italic",
    "bold.italic"
  )
  
  if(
    length(font_face) != 1L ||
    is.na(font_face) ||
    !font_face %in% allowed_faces
  ) {
    stop(
      "'font_face' must be one of: ",
      paste(allowed_faces, collapse = ", "),
      "."
    )
  }
  
  # -------------------------------------------------------------------------
  # Normalize design type ----------------------------------------------------
  # -------------------------------------------------------------------------
  
  design_raw <- trimws(
    as.character(fieldbook$design)
  )
  
  design_raw <- design_raw[
    !is.na(design_raw) &
      design_raw != ""
  ]
  
  if(length(design_raw) == 0L) {
    stop(
      "Column 'design' has no valid design value."
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
    !is.na(design_type) &
      design_type != ""
  ]
  
  if(length(design_type) == 0L) {
    stop(
      "Column 'design' has no recognized design value."
    )
  }
  
  if(length(design_type) > 1L) {
    warning(
      "More than one design value found after normalization. ",
      "Using the first one: ",
      design_type[1L],
      call. = FALSE
    )
  }
  
  design_type <- design_type[1L]
  
  # -------------------------------------------------------------------------
  # Standard designs ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(design_type %in% c(
    "crd",
    "rcbd",
    "sorted",
    "unsorted",
    "lsd"
  )) {
    
    return(
      plot_standard_design(
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
    )
  }
  
  # -------------------------------------------------------------------------
  # Augmented design ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(design_type == "augmented") {
    
    return(
      plot_augmented_design(
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
    )
  }
  
  # -------------------------------------------------------------------------
  # Splitplot-RCBD -----------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(design_type == "split-rcbd") {
    
    return(
      plot_split_rcbd_design(
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
    )
  }
  
  # -------------------------------------------------------------------------
  # Unsupported design -------------------------------------------------------
  # -------------------------------------------------------------------------
  
  stop(
    "Plot method not implemented for design: ",
    design_type
  )
}