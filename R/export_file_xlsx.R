#' Export xlsx
#'
#' @description export fieldbook in xlsx
#' @param data exported data
#' @param fname file name
#' @importFrom openxlsx write.xlsx

export_file_xlsx <- function(data, fname){

  openxlsx::write.xlsx(x = data, file = fname)

}
