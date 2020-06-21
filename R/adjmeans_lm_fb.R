#' Adjusted means using linear models
#'
#' Function to calculate the adjusted means using Linear models (lm or aov).
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param trait Name of the trait.
#' @param lm.model The effects in the model. See examples.
#' @param comparison Factor for the comparisons.
#' @param test Test comparison (default = "SNK"). Others: "TUKEY" & "DUNCAN".
#' @param anova ANOVA table (default = FALSE).
#' @param tab_vars Specific the variables in the summary table (Default = NULL).
#' @param sep Separator between the variables when use tab_vars. See details
#' @param digits Number of digits in the table (Default = 3).
#' @param plot_treat Treat of comparison in dot plot (Default = NULL). See example.
#' @param plot_groups Group of comparison in dot plot (Default = NULL). See example.
#' @param plot_diag Show diagnostic plots (default = FALSE).
#'
#' @details
#'
#' Using \code{tab_vars} select the variables shown the adjusted mean table with the \code{sep}.
#'
#' @return A list with two objects
#'
#'  1. Summary statistics.
#'
#'  2. Adjusted means with the mean comparisons under HSD test (p<0.05).
#'
#' @author Flavio Lozano-Isla
#'
#' @importFrom dplyr bind_rows
#'
#' @export

adjmeans_lm_fb <- function(data
                     , trait
                     , lm.model
                     , comparison
                     , test = "SNK"
                     , anova = FALSE
                     , plot_diag = FALSE
                     , plot_treat = NULL
                     , plot_groups = NULL
                     , tab_vars = NULL
                     , sep = NULL
                     , digits = 3
                     ){

  col_read <- if (is.numeric(trait)) {

    var <- colnames(data)[trait:ncol(data)]
    ft <- trait:ncol(data)
    list(vars = var, cols_n = ft)

  } else if (is.character(trait)) {

    var <- trait
    ft <- match(trait, colnames(data))
    list(vars = var, cols_n = ft)
  }

  if(is.null(tab_vars)) { tab_vars <- "mean" }

  adjm <- lapply(col_read$cols_n, function(x){

    trt <- data %>%
      inti::adjmeans_lm(data = .
                        , trait = colnames(.)[x]
                        , lm.model = lm.model
                        , comparison = comparison
                        , test = test
                        , anova = anova
                        , digits = digits
                        , tab_vars = tab_vars
                        , sep = sep
                        , plot_treat = plot_treat
                        , plot_groups = plot_groups
                        , plot_diag = plot_diag
                        )
    })

  stats <- do.call(bind_rows
                   , lapply(1:length(adjm)
                            , function(i) adjm[[i]][[1]]))

  tab_mean <- Reduce(function(...) base::merge(..., by = comparison, all = TRUE)
                  , lapply(1:length(adjm)
                           , function(i) adjm[[i]][[2]]))

  # Result

  list(
    statistics = stats
    , means = tab_mean
    )

}
