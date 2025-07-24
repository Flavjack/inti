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

anova_table <- function(model, digits = 4) {
  
  df <- as.data.frame(model) %>%
    tibble::rownames_to_column("Factor")
  
  # Detectar nombre de la columna de p-valor
  p_col <- dplyr::case_when(
    "Pr(>F)" %in% names(df)      ~ "Pr(>F)",
    "Pr(>Chisq)" %in% names(df)  ~ "Pr(>Chisq)",
    TRUE ~ NA_character_
  )
  
  if (is.na(p_col)) stop("No p-value column found: expected 'Pr(>F)' or 'Pr(>Chisq)'.")
  
  # Formato de p-valores: notación científica si son muy pequeños
  df[[p_col]] <- ifelse(
    df[[p_col]] < 0.0001,
    formatC(df[[p_col]], format = "e", digits = digits),
    formatC(df[[p_col]], format = "f", digits = digits)
  )
  
  # Añadir columna de significancia
  df <- df %>%
    dplyr::mutate(
      Sig = dplyr::case_when(
        as.numeric(.data[[p_col]]) <= 0.001 ~ "***",
        as.numeric(.data[[p_col]]) <= 0.01  ~ "**",
        as.numeric(.data[[p_col]]) <= 0.05  ~ "*",
        TRUE ~ "ns"
      )
    )
  
  # Convertir columnas a texto
  df <- df %>% dplyr::mutate(across(everything(), as.character))
  
  # Agregar leyenda
  df <- df %>%
    tibble::add_row(Factor = "---") %>%
    tibble::add_row(
      Factor = "Significance:",
      !!names(df)[2] := "0.001 ***",
      !!names(df)[3] := "0.01 **",
      !!p_col := "0.05 *"
    )
  
  return(df)
}