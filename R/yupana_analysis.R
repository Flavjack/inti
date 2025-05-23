#' Fieldbook analysis report
#'
#' Function to create a complete report of the fieldbook
#'
#' @param data Field book data.
#' @param last_factor The last factor in your fieldbook.
#' @param response Response variable.
#' @param comparison Factors to compare
#' @param model_factors Model used for the experimental design.
#' @param test_comp Comprasison test c("SNK", "TUKEY", "DUNCAN")
#' @param sig_level Significal test (default: p = 0.005)
#' @param plot_dist Plot data distribution (default = "boxplot")
#' @param plot_diag Diagnostic plots for model (default = FALSE).
#' @param digits Digits number in the table exported.
#'
#' @return list
#'
#' @import dplyr
#' @importFrom stats aov
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#'
#' library(inti)
#' 
#' fb <- potato
#' 
#' rsl <- yupana_analysis(data = fb
#'                        , last_factor = "bloque"
#'                        , response = "spad_83"
#'                        , model_factors = "geno * treat"
#'                        , comparison = c("geno", "treat")
#'                        )
#' 
#' }
#' 

yupana_analysis <- function(data
                            , last_factor = NULL
                            , response
                            , model_factors
                            , comparison
                            , test_comp = "SNK"
                            , sig_level = 0.05
                            , plot_dist = "boxplot"
                            , plot_diag = FALSE
                            , digits = 2
                           ) {
  
  where <- NULL

if(FALSE) {
  
  data <- fb
  last_factor = NULL
  response <- "spad_29"
  comparison <- c("geno", "treat")
  model_factors <- "bloque*geno*treat"
  test_comp = "SNK"
  sig_level = 0.05
  plot_dist = "boxplot"
  model_diag = FALSE
  digits = 2
  
}
  
# fieldbook structure -----------------------------------------------------
# -------------------------------------------------------------------------

  fb <- data %>%
    {if(!is.null(last_factor)) 
      mutate(.data = ., across(c(1:{{last_factor}}), as.factor)) else 
        mutate(.data = ., across({{comparison}}, as.factor)) } %>% 
    {if(!is.null(last_factor)) 
      mutate(.data = ., across(!c(1:{{last_factor}}), as.numeric)) else .} %>%
    data.frame()
  

# model -------------------------------------------------------------------

  model <- as.formula(paste({{response}}, model_factors, sep = "~"))
  
# anova -------------------------------------------------------------------

  model_aov <- aov(formula = model, data = fb)
  
# diagnostic plots --------------------------------------------------------
# -------------------------------------------------------------------------
  
  if (plot_diag == TRUE) {
    
    prp <- par(no.readonly = TRUE)
    on.exit(par(prp))   
    par(mfrow=c(2,2))
    hist(resid(model_aov), main = {{response}})
    qqnorm(resid(model_aov), main = {{response}}); qqline(resid(model_aov))
    plot(fitted(model_aov)
         , resid(model_aov, type = "pearson")
         , main = {{response}}); abline(h=0)
    plot(resid(model_aov), main = {{response}})
    
  }
  
  plotdiag <- plot_diag(model = model_aov
                        , title = response
                        )
  

# distribution plots ------------------------------------------------------
# -------------------------------------------------------------------------

  plotdist <- plot_raw(fb
                       , type = "boxplot"
                       , y = response
                       , x = comparison[1]
                       , group = if(is.na(comparison[2])) NULL else comparison[2]
                       ) + 
    {if(!is.na(comparison[3])) facet_grid(. ~ .data[[comparison[3]]])}
  
# mean comparison ---------------------------------------------------------
# -------------------------------------------------------------------------
  
  mc <- mean_comparison(fb
                        , response = response
                        , model_factors = model_factors
                        , comparison = comparison
                        , test_comp = test_comp
                        , sig_level = sig_level
                        )
  
  comptab <- mc$comparison %>% 
    mutate(across(where(is.numeric), ~round(., digits = digits))) %>% 
    dplyr::select(!c(.data$se))
  
  info <- comptab %>% 
    select({{response}}:ncol(.)) %>%
    names()
  
  factors <- comptab %>% 
    select(!{{info}}) %>% 
    names()
  
# model as text -----------------------------------------------------------

  model_formula <- paste(deparse(model, width.cutoff = 500), collapse="")

# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  fb_report = list(
    model = model_formula
    , anova = model_aov
    , plotdiag = plotdiag
    , plotdist = plotdist
    , meancomp = comptab
    , stats = mc$stats
    , response = response
    , comparison = comparison
    , factors = factors
    , tabvar = info
    , model_factors = model_factors
    , test_comp = test_comp
    , sig_level = sig_level
    )

}
