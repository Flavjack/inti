#' Fieldbook analysis report
#'
#' Function to create a complete report of the fieldbook
#'
#' @param data Field book data.
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
#' library(gsheet)
#' 
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=946957922")
#' # browseURL(url)
#' 
#' fb <- gsheet2tbl(url)
#' 
#' yrs <- yupana_analysis(data = fb
#'                        , response = "spad_83"
#'                        , model_factors = "geno + treat"
#'                        , comparison = c("geno", "treat")
#'                        )
#'                        
#' yrs$meancomp
#' 
#' }
#' 

yupana_analysis <- function(data
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
  response <- "spad_29"
  comparison <- c("geno", "treat")
  model_factors <- "geno*treat"
  test_comp = "SNK"
  sig_level = 0.05
  plot_dist = "boxplot"
  model_diag = FALSE
  digits = 2
  
}
  
# anova -------------------------------------------------------------------
# -------------------------------------------------------------------------
  
  model <- as.formula(paste( {{response}}, model_factors, sep = "~"))
  
  model_aov <- aov(model, data)
  
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
  
  plotdiag <- plot_diag(model_aov)
  

# distribution plots ------------------------------------------------------
# -------------------------------------------------------------------------

  plotdist <- plot_raw(data
                       , type = "boxplot"
                       , y = response
                       , x = comparison[1]
                       , group = if(is.na(comparison[2])) NULL else comparison[2]
                       ) + 
    {if(!is.na(comparison[3])) facet_grid(. ~ .data[[comparison[3]]])}
  
# mean comparison ---------------------------------------------------------
# -------------------------------------------------------------------------
  
  mc <- mean_comparison(data
                        , response = response
                        , model_factors = model_factors
                        , comparison = comparison
                        , test_comp = test_comp
                        , sig_level = sig_level
                        )
  
  comptab <- mc$comparison %>% 
    mutate(across(where(is.numeric), ~round(., digits = digits)))
  
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
    )

}
