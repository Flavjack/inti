#' Remove outliers using mixed models
#'
#' Function to remove outliers in MET experiments
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param formula mixed model formula.
#' @param drop_na drop NA values from the data.frame
#' @param plot_diag Diagnostic plot based in the raw and clean data
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
#' rmout <- potato %>%
#'   remove_outliers(data = .
#'   , formula = stemdw ~ 0 + (1|bloque) + treat*geno
#'   , plot_diag = FALSE
#'   , drop_na = FALSE
#'   )
#'
#' rmout
#'   

remove_outliers <- function(data
                            , formula
                            , drop_na = FALSE
                            , plot_diag = FALSE
                            ) {
  
  # data = potato; drop_na = F; plot_diag = T
  # formula = stemdw ~ 0  + (1|bloque) + treat*geno
  
  out_flag <- bholm <- NULL
  
  formula <- as.formula(formula)
  factors <- all.vars(formula)
  trait <- factors[1]
  mdfct <- factors[-1]
  
  rawdt <- data %>% 
    tibble::rownames_to_column("index") %>% 
    tidyr::drop_na({{trait}}) %>% 
    dplyr::select("index", {{factors}}) %>%
    dplyr::relocate({{trait}}, .after = last_col()) 
  
  modeli <- lme4::lmer(formula, rawdt)
  
  # re-scaled MAD
  resi <- cbind(residuals(modeli, type = "response"))
  medi <- median(resi, na.rm = TRUE)
  MAD <- median((abs(resi-medi)), na.rm = TRUE)
  re_MAD <- MAD*1.4826
  # end
  
  # MAD standardized residuals
  res_MAD <- resi/re_MAD 
  # end
  
  # Calculate adjusted p-values
  rawp.BHStud <- 2 * (1 - pnorm(abs(res_MAD))) 

  newdt <- rawdt %>% 
    cbind.data.frame(., resi, res_MAD, rawp.BHStud)

  # Produce a Bonferroni-Holm tests for the adjusted p-values

  test.BHStud <- p.adjust(rawp.BHStud, method = "holm")

  BHStud_test <- tibble(adjp = rawp.BHStud
                        , bholm = test.BHStud
                        ) %>% 
    # rownames_to_column("index") %>% 
    mutate(out_flag = ifelse(bholm <0.05, "OUTLIER", "."))
  
  mrgdt <- cbind(newdt, BHStud_test)
  
  outliers <- mrgdt %>%
    dplyr::filter(out_flag %in% "OUTLIER")

  cleandt <- mrgdt %>%
    dplyr::mutate({{trait}} := case_when(
      !out_flag %in% "OUTLIER" ~ as.character(.data[[trait]])
      , TRUE ~ NA_character_
    )) %>% 
    dplyr::mutate(across({{trait}}, as.numeric)) %>% 
    {if (isTRUE(drop_na)) {drop_na(data = ., any_of({{trait}}))} else {.}} %>% 
    dplyr::select(1:{{trait}}) %>% 
    dplyr::mutate(across(.data$index, ~ as.numeric(.))) %>% 
    dplyr::arrange(.data$index)
  
  modelf <- lme4::lmer(formula, cleandt)
  
  diagplot <- if(isTRUE(plot_diag)) {
    
    raw <- rawdt %>% 
      tidyr::drop_na({{trait}}) %>%
      plot_diagnostic(formula) %>% 
      cowplot::plot_grid(nrow = 1, plotlist = .
                         , labels = paste("Raw data:", {{trait}}))
    
    clean <- cleandt %>% 
      tidyr::drop_na({{trait}}) %>%
      plot_diagnostic(formula) %>% 
      cowplot::plot_grid(nrow = 1, plotlist = .
                         , labels = paste("Clean data:", trait))
    
    list(raw, clean) %>% 
      cowplot::plot_grid(nrow = 2, plotlist = .)
    
    } else { NULL }
  

  list(
    data = list(raw = rawdt, clean = cleandt)
    , outliers = outliers
    , diagplot = diagplot
    , model = list(raw = modeli, clean = modelf)
    )
  
}
