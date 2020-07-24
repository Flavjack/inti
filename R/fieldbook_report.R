#' Field book report
#'
#' Function to create a complete report of the fieldbook
#'
#' @param data Field book data.
#' @param fb_smr Summary of the variables in the fieldbook.
#' @param variable Response variable
#' @param model_facts Model used for the experimental design.
#' @param dotplot_groups Factors to compare. See details.
#' @param model_diag Diagnostic for model (default = FALSE).
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
#' report <- fieldbook_report(data
#'                           , fb_smr = fb_smr
#'                           , variable = "rendexportable"
#'                           , dotplot_groups = "bloque"
#'                           , model_diag = T
#'                           )
#' report
#'
#' }
#'
#' @export

fieldbook_report <- function(data
                            , fb_smr
                            , variable
                            , model_facts
                            , dotplot_groups = NULL
                            , model_diag = FALSE
                            ) {

  variables <- values <- treat <- type <- r <- std <- where <- NULL

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

  arguments <- fb_smr %>%
    select(where(~!all(is.na(.)))) %>%
    filter(variables %in% {{variable}} )

# -------------------------------------------------------------------------

  model_facts_opt <- c("model_facts", "model")
  model_facts_match <- names(arguments) %in% model_facts_opt
  model_facts_name <- names(arguments)[model_facts_match == TRUE]

  if ( length(model_facts_name)  > 0  )  {

    model_facts <- arguments %>%
      select({{model_facts_name}}) %>%
      pluck(1)

  }

# anova -------------------------------------------------------------------
# -------------------------------------------------------------------------

  model_formula <- function(variable, model_facts){

    model_formula <- as.formula(paste( {{variable}}, model_facts, sep = "~"))

    model_formula

  }

  model_aov <- function(fb
                    , variable
                    , model_formula
                    , model_diag
                    ){

    model <- aov(model_formula, fb)

    if (model_diag == TRUE) {

      par(mfrow=c(2,2), no.readonly = FALSE)
      hist(resid(model), main = {{variable}})
      qqnorm(resid(model), main = {{variable}}); qqline(resid(model))
      plot(fitted(model), resid(model, type = "pearson"), main = {{variable}}); abline(h=0)
      plot(resid(model), main = {{variable}})

      }

    model

    }

# diagnostic plot ---------------------------------------------------------
# -------------------------------------------------------------------------

  dotplot <- function(fb
                      , variable
                      , dotplot_groups
                      ){

      plot <- fb %>%
        ggplot(aes( .data[[variable]], .data[[dotplot_groups]] )) +
        geom_point( aes(color = .data[[dotplot_groups]]) ) +
        theme_minimal()

    }

# apply functions ---------------------------------------------------------
# -------------------------------------------------------------------------

  model_formula <- model_formula(variable, model_facts)

  model_aov <- model_aov(fb, variable, model_formula, model_diag)

  if ( !is.null(dotplot_groups) ) {

    dotplot_diag <- dotplot(fb, variable, dotplot_groups)

  } else { dotplot_diag <- NULL }

# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  fb_report = list(
    formula = model_formula
    , anova = model_aov
    , dotplot = dotplot_diag
    )

}
