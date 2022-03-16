#' Remove outliers
#'
#' Function to remove outliers in MET experiments
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param trait Name of the trait.
#' @param model The fixed or random effects in the model.
#'
#' @description
#'
#' Use the method M4 in Bernal Vasquez (2016). Bonferroni Holm test to judge
#' residuals standardized by the re scaled MAD (BH MADR).
#'
#' @return list. 1. Table with date without outliers. 2. The outliers in the
#'   dataset.
#'
#' @references
#'
#' Bernal Vasquez, Angela Maria, et al. “Outlier Detection Methods for
#' Generalized Lattices: A Case Study on the Transition from ANOVA to REML.”
#' Theoretical and Applied Genetics, vol. 129, no. 4, Apr. 2016.
#'
#' @importFrom stats median pnorm residuals p.adjust
#' @importFrom lme4 lmer
#' 
#' @export
#' 
#' @examples
#'
#' library(inti)
#'
#' rmout <- outliers_remove(
#'   data = potato
#'   , trait ="stemdw"
#'   , model = "0 + (1|bloque) + geno"
#'   )
#'   
#' rmout$outliers
#'   

outliers_remove <- function(data
                            , trait
                            , model
                            ) {

  out_flag <- bholm <- NULL

  formula <- as.formula(paste(trait, model, sep = "~"))
  model_fact <- all.vars(formula)
  model <- lme4::lmer(formula, data)
  
  # re-scaled MAD
  resi <- cbind(residuals(model, type = "response"))
  medi <- median(resi, na.rm = TRUE)
  MAD <- median((abs(resi-medi)), na.rm = TRUE)
  re_MAD <- MAD*1.4826
  # end
  
  # MAD standardized residuals
  res_MAD <- resi/re_MAD 
  # end
  
  # Calculate adjusted p-values
  rawp.BHStud <- 2 * (1 - pnorm(abs(res_MAD))) 

  newdt <- data %>%
    select({{model_fact}}) %>%
    relocate({{trait}}, .after = last_col()) %>%
    drop_na() %>% # fix model test bug?
    cbind.data.frame(., resi, res_MAD, rawp.BHStud)

  # Produce a Bonferroni-Holm tests for the adjusted p-values

  test.BHStud <- p.adjust(rawp.BHStud, method = "holm")

  BHStud_test <- tibble(adjp = rawp.BHStud
                        , bholm = test.BHStud
                        ) %>% 
    rownames_to_column("index") %>% 
    mutate(out_flag = ifelse(bholm <0.05, "OUTLIER", "."))

  outliers <- cbind(newdt, BHStud_test) %>%
    dplyr::filter(out_flag %in% "OUTLIER")

  nwdt <- cbind(newdt, BHStud_test) %>%
    dplyr::filter(!out_flag %in% "OUTLIER") %>%
    select({{model_fact}}) %>%
    relocate({{trait}}, .after = last_col())

  list(
    data = nwdt
    , outliers = outliers
    )

}
