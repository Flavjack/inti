utils::globalVariables(c(".", ":=", "%>%"))

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`


GoogleAuth <- function() {
  
  googlesheets4::gs4_auth(email = TRUE)
  
}


RenewAuth <- function() {
  
  googlesheets4::gs4_token()
  
}


yupana_full <- function() {
  
  inti::yupana(dependencies = TRUE)
  
}


tarpuy_full <- function() {
  
  inti::tarpuy(dependencies = TRUE)
  
}

