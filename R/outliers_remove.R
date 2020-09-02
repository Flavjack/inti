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
#' Use the method M4 in Bernal-Vasquez (2016). Bonferroni–Holm test to judge
#' residuals standardized by the re‑scaled MAD (BH‑MADR).
#'
#' @return list. 1. Table with date without outliers. 2. The outliers in the
#'   dataset.
#'
#' @author Flavio Lozano-Isla
#'
#' @references
#'
#' Bernal-Vasquez, Angela-Maria, et al. “Outlier Detection Methods for
#' Generalized Lattices: A Case Study on the Transition from ANOVA to REML.”
#' Theoretical and Applied Genetics, vol. 129, no. 4, Apr. 2016, pp. 787–804.
#' Springer Link, doi:10.1007/s00122-016-2666-6.
#'
#' @importFrom stats median pnorm residuals
#' @importFrom multtest mt.rawp2adjp
#'

outliers_remove <- function(data
                            , trait
                            , model
                            ) {

  out_flag <- NULL

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
    drop_na() %>%
    cbind(., resi, res_MAD, rawp.BHStud)

  # Produce a Bonferroni-Holm tests for the adjusted p-values

  test.BHStud <- mt.rawp2adjp(rawp.BHStud, proc = c("Holm"))

  adjp <- cbind(test.BHStud[[1]][,1])
  bholm <- cbind(test.BHStud[[1]][,2])
  index <- cbind(test.BHStud[[2]])

  BHStud_test <- tibble(adjp, bholm, index) %>%
    mutate(out_flag = ifelse(bholm <0.05, "OUTLIER", ".")) %>%
    arrange(index)

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
