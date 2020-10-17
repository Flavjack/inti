#' Fieldbook report
#'
#' Function to create a complete report of the fieldbook
#'
#' @param data Field book data.
#' @param fb_smr Summary of the variables in the fieldbook.
#' @param variable Response variable.
#' @param model_facts Model used for the experimental design.
#' @param dotplot_groups Factors to compare. See details.
#' @param model_diag Diagnostic for model (default = FALSE).
#'
#' @details
#'
#' For compare the factors you should use ":". For example, to compare
#' treatment1 and treatment2: \code{treatment1:treatment2}.
#'
#' @return data frame
#'
#' @import dplyr
#' @importFrom stats rstandard
#' 
#' @export
#'
#' @examples
#'
#' library(inti)
#' library(googlesheets4)
#' 
#' if (gs4_has_token()) {
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=172957346")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (data <- gs %>%
#'     range_read("fb"))
#'
#' (fb_smr <- gs %>%
#'   range_read("fbsm"))
#'
#' report <- fieldbook_report(data
#'                           , fb_smr = fb_smr
#'                           , variable = "LA"
#'                           , dotplot_groups = "genotype"
#'                           , model_diag = TRUE
#'                           )
#' report
#'
#' }
#' 

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
    filter(type %in% "numeric") %>% 
    filter(levels > 0)

  vars_cat <- fb_smr %>%
    filter(type %in% "character") %>% 
    filter(levels > 0)

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
      
      prp <- par(no.readonly = TRUE)
      on.exit(par(prp))   
      
      par(mfrow=c(2,2))
      hist(resid(model), main = {{variable}})
      qqnorm(resid(model), main = {{variable}}); qqline(resid(model))
      plot(fitted(model), resid(model, type = "pearson"), main = {{variable}}); abline(h=0)
      plot(resid(model), main = {{variable}})

    }

    model

    }

# diagnostic plot ---------------------------------------------------------
# -------------------------------------------------------------------------

diagplot <- function( model ) {

    p1 <- ggplot(model , aes(rstandard(model))) +
      geom_histogram(bins = 30) +
      xlab("Residuals") +
      ylab("Frequency") +
      ggtitle("Frequency vs Residuals") +
      theme_bw()

    p2 <- ggplot(model, aes(fitted(model), resid(model))) +
      geom_point() +
      stat_smooth(method="loess", formula = 'y ~ x') +
      geom_hline(yintercept=0, col="red", linetype="dashed") +
      xlab("Fitted values") +
      ylab("Residuals") +
      ggtitle("Residual vs Fitted") +
      theme_bw()

    p3 <- ggplot(model, aes(sample = rstandard(model))) +
      geom_qq() +
      stat_qq_line() +
      xlab("Theoretical Quantiles") +
      ylab("Standardized Residuals") +
      ggtitle("Normal Q-Q") +
      theme_bw()

    p4 <- ggplot(model, aes(fitted(model), sqrt(abs(resid(model))))) +
      geom_point(na.rm=TRUE) +
      stat_smooth(method="loess", na.rm = TRUE, formula = 'y ~ x') +
      xlab("Fitted Value") +
      ylab(expression(sqrt("|Standardized residuals|"))) +
      ggtitle("Scale-Location") +
      theme_bw()

    return(list( freq = p1, qqnorm = p2, resid = p3, sresid = p4))

    }

# -------------------------------------------------------------------------

  dotplot <- function(fb
                      , variable
                      , dotplot_groups
                      ){

      plot <- fb %>%
        ggplot(aes( .data[[variable]], .data[[dotplot_groups]] )) +
        geom_point( aes(color = .data[[dotplot_groups]]) ) +
        theme_bw() +
        theme(legend.position = "none")

    }

# apply functions ---------------------------------------------------------
# -------------------------------------------------------------------------

  model_formula <- model_formula(variable, model_facts)

  model_aov <- model_aov(fb, variable, model_formula, model_diag)

  diag_plot <- diagplot(model_aov)

  if ( !is.null(dotplot_groups) ) {

    dotplot_diag <- dotplot(fb, variable, dotplot_groups)

  } else { dotplot_diag <- NULL }

# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  fb_report = list(
    formula = model_formula
    , anova = model_aov
    , diagplot = diag_plot
    , dotplot = dotplot_diag
    )

}
