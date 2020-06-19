#' Adjusted means using linear models
#'
#' Function to calculate the adjusted means using Linear models (lm or aov).
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param trait Name of the trait.
#' @param lm.model The effects in the model. See examples.
#' @param comparison Factor for the comparisons.
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
#'  1. Summary statistics
#'
#'  2. Adjusted means with the mean comparisons under HSD test (p<0.05)
#'
#' @author Flavio Lozano-Isla
#'
#' @importFrom stats aov
#' @importFrom dplyr across desc arrange
#' @importFrom tidyr unite
#'
#' @export

adjmeans <- function(data
                     , trait
                     , lm.model
                     , comparison
                     , digits = 3
                     , tab_vars = NULL
                     , sep = NULL
                     , plot_treat = NULL
                     , plot_groups = NULL
                     , plot_diag = FALSE
                     ){

  treat <- NULL

  # arguments

  trait <- as.name(trait)
  plot_treat <- as.name(plot_treat)
  plot_groups <- as.name(plot_groups)

  # Model

  model <- as.formula(paste(trait, lm.model, sep = " ~ "))

  lm <- data %>%
    aov(model, data = .)

  anova(lm)

  # Dot plot

  if (!is.null(plot_treat) && !is.null(plot_groups)) {

    p_dots <- data %>%
      ggplot(aes( {{ trait }}, {{ plot_treat }} )) +
      geom_point(aes(color = {{ plot_groups }} )) +
      theme_minimal()

    p_dots
  }

  # Diagnostic plot

  if (plot_diag == TRUE) {

    par(mfrow=c(2,2))
    hist(resid(lm), main = trait)
    qqnorm(resid(lm), main = trait); qqline(resid(lm))
    plot(fitted(lm), resid(lm, type = "pearson"), main = trait); abline(h=0)
    plot(resid(lm), main = trait)

  }

  # Mean comparison

  mc <- HSD.test(
    y = lm
    , trt = comparison
  )

  tb_smr <- merge(
    mc %>% pluck("means") %>% rownames_to_column("treat")
    ,  mc %>% pluck("groups") %>% rownames_to_column("treat")
    , all = TRUE) %>%
    separate(treat, comparison, sep = ":") %>%
    arrange(desc({{trait}}))


  if (!is.null(tab_vars)) {

    if(is.null(sep)){sep = " + "} else (sep = sep)

    tb_smr <- tb_smr %>%
      select(
        comparison
        , {{trait}}
        , tab_vars
      ) %>%
      mutate(across(is.numeric, ~round(., digits = digits))) %>%
      arrange(desc({{trait}})) %>%
      unite(
        {{trait}}
        , {{trait}}
        , {{tab_vars}}
        , sep = sep
      )
  }

  # Results

  smr <- list(
    statistics = mc %>% pluck("statistics")
    , means = tb_smr
  )

}
