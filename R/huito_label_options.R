#' Label options
#'
#' Get environment variables for label options
#'
#' @param layers value to match the lists in the environment
#'
#' @return data frame
#'
#' @importFrom stringr str_detect
#' @export
#'

huito_label_options <- function(layers) {

  pattern <- paste0(layers, collapse = "|")
  match <- ls(envir = .GlobalEnv) %>% str_detect(pattern = pattern)
  layer_lis <- ls(envir = .GlobalEnv)[match == TRUE]
  opts <- list(value = NA, size = NA, font = NA, position = NA, angle = NA)

  all <- mget(layer_lis, envir= globalenv())
  all[["opt"]] <- opts

  label_opts <- do.call(bind_rows, all) %>%
    mutate(layer = case_when(
      element %in% "label" ~ 0
    )) %>%
    arrange(layer)
}
