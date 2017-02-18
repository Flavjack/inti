#' Import google spreadsheet or xlsx file
#'
#' @description function to import information from google spreadsheet or xlsx file.
#' @param dir local file directory for xlsx document or url from google spreadsheet
#' @param sheet if is a xlsx file, you can choose the sheet number
#' @return data frame
#' @importFrom gsheet gsheet2tbl
#' @importFrom readxl read_excel
#' @importFrom dplyr  '%>%'
#' @export

getData <- function(dir, sheet = 1) {


  if (file.exists(dir) == TRUE) {

    readxl::read_excel(path = dir, sheet = sheet) %>% as.data.frame()

  } else{

    gsheet::gsheet2tbl(url = dir) %>% as.data.frame()

  }


}


