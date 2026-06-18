#' Split-plot experimental designs
#'
#' Dispatch split-plot experimental designs.
#'
#' @param nfactors Number of factors in the experiment.
#' @param factors List with factor levels.
#' @param type Split-plot design type.
#' @param rep Number of replications.
#' @param zigzag Field layout in zigzag.
#' @param nrows Experimental design dimension by rows.
#' @param serie Number to start the plot id.
#' @param seed Seed for randomization.
#' @param project Barcode prefix.
#' @param qrcode String to concatenate the QR code.
#'
#' @return A list with the fieldbook design and parameters.
#'
#' @export

design_split <- function(nfactors = 2,
                         factors,
                         type = "split-rcbd",
                         rep = 3,
                         zigzag = FALSE,
                         nrows = NA,
                         serie = 1000,
                         seed = NULL,
                         project = "inkaverse",
                         qrcode = "{project}{plots}{factors}") {
  
  if(type == "split-rcbd") {
    
    return(
      design_split_rcbd(
        nfactors = nfactors,
        factors = factors,
        type = type,
        rep = rep,
        zigzag = zigzag,
        nrows = nrows,
        serie = serie,
        seed = seed,
        project = project,
        qrcode = qrcode
      )
    )
    
  }
  
  # if(type == "split-crd") {
  #   
  #   return(
  #     design_split_crd(...)
  #   )
  #   
  # }
  
  stop("Split design type not implemented.")
  
}