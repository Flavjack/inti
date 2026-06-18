#' Fieldbook plot experimental designs
#'
#' Plot fieldbook sketches according to the experimental design type.
#'
#' This function works as a dispatcher. It detects the design type from the
#' fieldbook and sends the data to the corresponding plotting function.
#'
#' @param data Fieldbook data frame or design object containing a fieldbook.
#' @param factor Character. Column used to color experimental units.
#' @param fill Character vector. Column or columns used as labels inside
#'   experimental units.
#' @param xlab Character. Optional x axis title.
#' @param ylab Character. Optional y axis title.
#' @param glab Character. Optional legend title.
#'
#' @return A `ggplot` object.
#'
#' @export

tarpuy_plotdesign <- function(data,
                              factor = NA,
                              fill = "plots",
                              xlab = NULL,
                              ylab = NULL,
                              glab = NULL) {
  
  # -------------------------------------------------------------------------
  # Get fieldbook ------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(is.data.frame(data)) {
    
    fieldbook <- data
    
  } else if(is.list(data) && "fieldbook" %in% names(data)) {
    
    fieldbook <- data$fieldbook
    
  } else if(is.list(data) && length(data) >= 1 && is.data.frame(data[[1]])) {
    
    fieldbook <- data[[1]]
    
  } else {
    
    stop("'data' must be a fieldbook data frame or a design object with fieldbook.")
    
  }
  
  # -------------------------------------------------------------------------
  # Checks ------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(!"design" %in% names(fieldbook)) {
    stop("Column 'design' is required to choose the plot method.")
  }
  
  design_type <- unique(fieldbook$design)
  design_type <- design_type[!is.na(design_type)]
  
  if(length(design_type) == 0) {
    stop("Column 'design' has no valid design value.")
  }
  
  if(length(design_type) > 1) {
    warning(
      "More than one design value found. Using the first one: ",
      design_type[1]
    )
  }
  
  design_type <- tolower(trimws(as.character(design_type[1])))
  
  # -------------------------------------------------------------------------
  # Standard designs ---------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(design_type %in% c("crd", "rcbd", "sorted", "unsorted", "lsd")) {
    
    return(
      plot_standard_design(
        data = fieldbook,
        factor = factor,
        fill = fill,
        xlab = xlab,
        ylab = ylab,
        glab = glab
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
        glab = glab
      )
    )
    
  }
  
  # -------------------------------------------------------------------------
  # Split-plot RCBD ----------------------------------------------------------
  # -------------------------------------------------------------------------
  
  if(design_type == "split-rcbd") {
    
    return(
      plot_split_rcbd_design(
        data = fieldbook,
        factor = factor,
        fill = fill,
        xlab = xlab,
        ylab = ylab,
        glab = glab
      )
    )
    
  }
  
  # -------------------------------------------------------------------------
  # Unsupported design -------------------------------------------------------
  # -------------------------------------------------------------------------
  
  stop("Plot method not implemented for design: ", design_type)
  
}
