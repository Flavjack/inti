#' Remove outliers
#'
#' Function to remove outliers according the lowest or highest values
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param trait Name of the trait.
#' @param n_low Number of point to remove with lowest values
#' @param n_high Number of point to remove with highest values
#'
#' @details Could be use applying pipes. Remove n lowest or highest point.
#'
#' @return Vector with the selected values as NA's
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @importFrom dplyr mutate
#'
#' @export

rm_outliers <- function(data
                        , trait
                        , n_low
                        , n_high
){

  if(missing(n_low)) {n_low <- 0}

  if(missing(n_high)) {n_high <- 0}

  data %>%
    mutate({{trait}} := replace({{trait}},
                                order({{trait}},
                                      decreasing = F)[0:n_low], NA)) %>%
    mutate({{trait}} := replace({{trait}},
                                order({{trait}},
                                      decreasing = T)[0:n_high], NA))
}
