#' Field book report
#'
#' Function to create a complete report of the fieldbook
#'
#' @param data Field book data.
#' @param fb_smr Summary of the variables in the fieldbook.
#' @param variable Model used for the experimental design.
#' @param comp_diag Factors to compare. See details.
#' @param model_facts Comparison test (default = "SNK"). Others: "TUKEY" & "DUNCAN".
#' @param treat_comp Significance level for the analysis (default = 0.05).
#' @param test_comp Significance level for the analysis (default = 0.05).
#' @param sig_level Significance level for the analysis (default = 0.05).
#'
#' @details
#'
#' For compare the factors you should use ":". For example, to compare treatment1
#' and treatment2 => "treatment1:treatment2".
#'
#' @return data frame
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' library(googlesheets4)
#' library(tidyverse)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "15uwCgQRtR01B3FJaZBE8t_0bOC_8Bbey9ccwVlZH0jg/edit#gid=56711214")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (data <- gs %>%
#'     range_read("fb"))
#'
#' (fb_smr <- gs %>%
#'   range_read("fbsmr"))
#'
#' report <- fielbook_report(data
#'                           , fb_smr = fb_smr
#'                           , variable = "aftotal"
#'                           , comp_diag = "bloque"
#'                           )
#' report
#'
#' }
#'
#'
#'
#' @export

fielbook_report <- function(data
                            , fb_smr
                            , variable
                            , comp_diag
                            , model_facts = NULL
                            , treat_comp = NULL
                            , test_comp = "SNK"
                            , sig_level = 0.05
                            ) {

# fieldbook structure -----------------------------------------------------
# -------------------------------------------------------------------------

  factor_list <- fb_smr %>%
    filter(type %in% "factor") %>%
    select(where(~!all(is.na(.))))

  vars_num <- fb_smr %>%
    filter(type %in% "numeric")

  vars_cat <- fb_smr %>%
    filter(type %in% "character")

  fb <- data %>%
    select(where(~!all(is.na(.)))) %>%
    mutate(across( factor_list[["variables"]], as.character)) %>%
    mutate(across( factor_list[["variables"]], as.factor)) %>%
    mutate(across( vars_num[["variables"]], as.numeric)) %>%
    mutate(across( vars_cat[["variables"]], as.character))

# fieldbook arguments -----------------------------------------------------
# -------------------------------------------------------------------------

  test_comp <- match.arg(test_comp, c("snk" , "SNK"
                                      , "duncan", "DUNCAN"
                                      , "hsd", "tukey", "TUKEY"))

  if( test_comp %in% c("tukey", "snk", "tukey", "TUKEY") ) {

    test_comp <- switch(test_comp,
                        snk = "SNK"
                        , tukey = "HSD"
                        , TUKEY = "HSD"
                        , DUNCAN = "duncan")
  }

# extract arguments -------------------------------------------------------
# -------------------------------------------------------------------------

  arguments <- fb_smr %>%
    select(where(~!all(is.na(.)))) %>%
    filter(variables %in% {{variable}})


# -------------------------------------------------------------------------

  model_facts_opt <- c("model_facts", "model")
  model_facts_math <- names(arguments) %in% model_facts_opt
  model_facts_name <- names(arguments)[model_facts_math == TRUE]

  if ( length(model_facts_name)  > 0  )  {

    model_facts <- arguments %>%
      select({{model_facts_name}}) %>%
      pluck(1)

  }

  # -------------------------------------------------------------------------

  treat_comp_opt <- c("treat_comp", "comparison", "compare", "comparar")
  treat_comp_math <- names(arguments) %in% treat_comp_opt
  treat_comp_name <- names(arguments)[treat_comp_math == TRUE]

  if ( length(treat_comp_name)  > 0  )  {

    treat_comp <- arguments %>%
      select({{treat_comp_name}}) %>%
      pluck(1)

  }

  # -------------------------------------------------------------------------

  test_comp_opt <- c("test_comp", "test_media", "test_comparison", "test_compare")
  test_comp_math <- names(arguments) %in% test_comp_opt
  test_comp_name <- names(arguments)[test_comp_math == TRUE]

  if ( length(test_comp_name)  > 0  )  {

    test_comp <- arguments %>%
      select({{test_comp_name}}) %>%
      pluck(1)

  }

  # -------------------------------------------------------------------------

  sig_level_opt <- c("sig_level", "signicance", "alpha")
  sig_level_math <- names(arguments) %in% sig_level_opt
  sig_level_name <- names(arguments)[sig_level_math == TRUE]

  if ( length(sig_level_name)  > 0  )  {

    sig_level <- arguments %>%
      select({{sig_level_name}}) %>%
      pluck(1)
  }

# anova -------------------------------------------------------------------
# -------------------------------------------------------------------------

  model_formula <- function(variable, model_facts){

    model_formula <- as.formula(paste(variable, model_facts, sep = "~"))

    model_formula

  }

  model_aov <- function(fb
                    , variable
                    , model_formula
                    ){

    model_aov <- aov(model_formula, fb)

    anova(model_aov) %>% print()

    par(mfrow=c(2,2))
    hist(resid(model_aov), main = variable)
    qqnorm(resid(model_aov), main = variable); qqline(resid(model_aov))
    plot(fitted(model_aov), resid(model_aov, type = "pearson"), main = variable); abline(h=0)
    plot(resid(model_aov), main = variable)

    model_aov

    }

# diagnostic plot ---------------------------------------------------------
# -------------------------------------------------------------------------

  dotplot <- function(fb
                      , variable
                      , treat_comp
                      , comp_diag
                      ){

    compare <- strsplit(treat_comp,  ":")[[1]][[1]]

    plot <- fb %>%
      ggplot(aes( .data[[variable]], .data[[compare]] )) +
      geom_point( aes(color = .data[[compare]]) ) +
      theme_minimal()

    }

# mean comparison ---------------------------------------------------------
# -------------------------------------------------------------------------

  mean_comparison <- function(model_aov
                              , variable
                              , treat_comp
                              , sig_level
                              ){

   compare <- strsplit(treat_comp,  ":")[[1]][[1]]

   if (test_comp == "SNK"){

     mc <- SNK.test(
       y = model_aov
       , trt = compare
       , alpha = sig_level
     )

   } else if (test_comp == "HSD") {

     mc <- HSD.test(
       y = model_aov
       , trt = compare
       , alpha = sig_level
     )

   } else if (test_comp == "DUNCAN") {

     mc <- duncan.test(
       y = model_aov
       , trt = compare
       , alpha = sig_level
     )
   }

   tb_mc <- merge(
     mc %>% pluck("means") %>% rownames_to_column("treat")
     ,  mc %>% pluck("groups") %>% rownames_to_column("treat")
     , all = TRUE) %>%
     separate(treat, compare, sep = ":") %>%
     arrange(desc( {{variable}} )) %>%
     mutate(ste = std/sqrt(r), .after = r)

   smr_stat <- mc %>%
     pluck("statistics") %>%
     dplyr::mutate(trait =  {{variable}}, .before = "MSerror")


   mean_comparison <- list(
     table = tb_mc
     , stats = smr_stat
   )

  }

# apply functions ---------------------------------------------------------
# -------------------------------------------------------------------------

  model_formula <- model_formula(variable, model_facts)

  model_aov <- model_aov(fb, variable, model_formula)

  dotplot <- dotplot(fb, variable, treat_comp, comp_diag)

  mean_comparison <- mean_comparison(model_aov
                                     , variable
                                     , treat_comp
                                     , sig_level)

# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  fb_report = list(
    formula = model_formula
    , anova = model_aov
    , diag_plot = dotplot
    , table_summary = mean_comparison[["table"]]
    , stat_summary = mean_comparison[["stats"]]
  )

}
