#' Adjusted means using linear models
#'
#' Function to calculate the adjusted means using Linear models (lm or aov)
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param trait Name of the trait.
#' @param lm.model The effects in the model. See examples.
#' @param comparison Factor for the comparisons.
#' @param test Test comparison (default = "SNK"). Others: "TUKEY" & "DUNCAN".
#' @param anova ANOVA table (default = FALSE).
#' @param tab_vars Specific the variables in the summary table (default = NULL).
#' @param sep Separator between the variables when use \code{tab_vars}. See
#'   details.
#' @param digits Number of digits in the table (default = 3).
#' @param plot_treat Treat of comparison in dot plot (default = NULL). See
#'   example.
#' @param plot_groups Group of comparison in dot plot (default = NULL). See
#'   example.
#' @param plot_diag Show diagnostic plots (default = FALSE).
#'
#' @details
#'
#' Using \code{tab_vars} select the variables shown the adjusted mean table with
#' the \code{sep}. Mean comparison at p < 0.05.
#'
#' @return A list with two objects:
#'
#'   1. Summary statistics.
#'
#'   2. Means with the mean comparisons test.
#'
#' @author Flavio Lozano Isla
#'
#' @import ggplot2
#' @import agricolae
#' @import dplyr
#' @importFrom stats aov
#' @importFrom tidyr unite
#' @importFrom graphics plot
#' 
#' @export
#' 

adjmeans_lm <- function(data
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

  treat <- NULL

  # arguments

  trait <- as.name(trait)
  if (!is.null(plot_treat)) { plot_treat <- as.name(plot_treat) }
  if (!is.null(plot_groups)) { plot_groups <- as.name(plot_groups) }
  if(is.null(sep)){sep = " + "}

  # Varible

  if (anova == TRUE || plot_diag == TRUE) {

    print(paste("##>-----------------------------------------"))
    print(paste("##>", trait))
    print(paste("##>-----------------------------------------"))

  }

  # Model

  model <- as.formula(paste(trait, lm.model, sep = " ~ "))

  lm <- data %>%
    aov(model, data = .)

  if (anova == TRUE) {

    anova(lm) %>% print()

  }

  # Diagnostic plot

  if (plot_diag == TRUE) {

    par(mfrow=c(2,2))
    hist(resid(lm), main = trait)
    qqnorm(resid(lm), main = trait); qqline(resid(lm))
    plot(fitted(lm), resid(lm, type = "pearson"), main = trait); abline(h=0)
    plot(resid(lm), main = trait)

  }

  # Dot plot

  if (!is.null(plot_treat) && !is.null(plot_groups)) {

    p_dots <- data %>%
      ggplot(aes( {{ trait }}, {{ plot_treat }} )) +
      geom_point(aes(color = {{ plot_groups }} )) +
      theme_minimal()

    print(p_dots)
  }

  # Mean comparison

  if (test == "SNK"){

    mc <- SNK.test(
      y = lm
      , trt = comparison
    )

  } else if (test == "TUKEY") {

    mc <- HSD.test(
      y = lm
      , trt = comparison
    )

  } else if (test == "DUNCAN") {

    mc <- duncan.test(
      y = lm
      , trt = comparison
    )

  }

  tb_smr <- merge(
    mc %>% pluck("means") %>% rownames_to_column("treat")
    ,  mc %>% pluck("groups") %>% rownames_to_column("treat")
    , all = TRUE) %>%
    separate(treat, comparison, sep = ":") %>%
    arrange(desc({{trait}}))

  if (is.null(tab_vars)) {

    tb_smr

  } else if (tab_vars == "mean") {

    tb_smr <- tb_smr %>%
      select(
        comparison
        , {{trait}}
      ) %>%
      arrange(desc({{trait}}))

  } else if ( tab_vars != trait ) {

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

  smr_stat <- mc %>%
    pluck("statistics") %>%
    dplyr::mutate(trait =  print(as.character(trait))) %>%
    select(trait, dplyr::everything())

  # Results

  smr <- list(
    statistics = smr_stat
    , means = tb_smr
  )

}
