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

anova_table <- function(model) {
  
  model %>%
    rownames_to_column("Factor") %>% 
    mutate(Sig = case_when(
      `Pr(>F)` <= 0.001  ~ "***"
      , `Pr(>F)` <= 0.01  ~ "**"
      , `Pr(>F)` <= 0.05  ~ "*"
      , `Pr(>F)` > 0.05 ~ "ns"
    )) %>% 
    mutate(across(everything(), as.factor)) %>%
    tibble() %>% 
    tibble::add_row(Factor = "---") %>% 
    tibble::add_row(Factor = "Significance:"
                    , `Sum Sq` = "0.001 ***"
                    , `Mean Sq` = "0.01 **"
                    , `F value` = "0.05 *"
    )
}