#' Diagnostic plots
#'
#' Function to plot the diagnostic of models
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param formula Mixed model formula
#' @param title Plot title
#'
#' @return plots
#' 
#' @importFrom stats density
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' library(inti)
#' 
#' plot_diagnostic(data = potato
#'                 , formula = stemdw ~ (1|bloque) + geno*treat)
#' 
#' }
#' 

plot_diagnostic <- function(data, formula, title = NA) {
  
  # data <- inti::potato; formula = stemdw ~ (1|bloque) + geno*treat
  # formula <- stemdw ~ bloque + geno*treat; title = NA
  
  formula <- as.formula(formula)
  
  model  <-  if( length(lme4::findbars(formula))>0 ) {
    
    data %>% lme4::lmer(formula, data = .)
    
  } else {  data %>% aov(formula, data = .) }
  
  
  stopifnot(class(model) %in% c("lm", "aov", "lmerMod"))
  
  title <- if(is.null(title) || is.na(title) ) {""} else {title}
  
  dt <- if ( inherits(model, "lm") || inherits(model, "aov") ) {
    
    ggplot2::fortify(model, data)
    
  } else if ( inherits(model, "lmerMod") ) {
    
    lme4::fortify.merMod(model, data)
    
  }
  
  # Histogram
  p1 <- ggplot(dt , aes(.data$.resid)) +
    geom_histogram(aes(y = ggplot2::after_stat(density)), 
                   color = "black", fill = "grey82", bins = 30) + 
    stat_function(fun = stats::dnorm, color = "blue", args = list(mean = 0, sd = sd(dt$.resid))) +
    theme_bw() +
    labs(x = "Residuals"
         , y = "Frequency"
         , title = title
         , subtitle = "Histogram"
         )
  
  # Q-Q plot
  p2 <- ggplot(dt, aes(sample = .data$.resid)) +
    stat_qq() +
    stat_qq_line() +
    theme_bw() +
    labs(x = "Theorical Quantiles"
         , y = "Sample Quantiles"
         , title = title
         , subtitle = "Q-Q plot"
         )
  
  # Residual plot
  p3 <- ggplot(dt, aes(.data$.fitted, .data$.resid)) +
    geom_point() +
    stat_smooth(method="loess", formula = 'y ~ x') +
    geom_hline(yintercept=0, col="red", linetype="dashed") +
    theme_bw() +
    labs(x = "Fitted values"
         , y = "Residuals"
         , title = title
         , subtitle = "Residual plot"
         )
  
  # Scale-location | Homoscedasticity
  p4 <- ggplot(dt, aes(.data$.fitted, sqrt(abs(.data$.resid)))) +
    geom_point(na.rm=TRUE) +
    stat_smooth(method="loess", na.rm = TRUE, formula = 'y ~ x') +
    theme_bw() +
    labs(x = "Fitted values"
         , y = expression(sqrt("|Standardized residuals|"))
         , title = title
         , subtitle = "Homoscedasticity" 
         )
  
  p5 <- ggplot(dt, aes(stats::hatvalues(model), .data$.resid)) + 
    stat_smooth(method="loess", na.rm=TRUE) +
    xlab("Leverage")+ylab("Standardized Residuals") +
    ggtitle("Residual vs Leverage Plot") + 
    scale_size_continuous("Cook's Distance", range=c(1,5)) + 
    theme_bw() + theme(legend.position="bottom")
  
  return(list(histogram = p1
              , qqplot = p2
              , residual = p3
              , homoscedasticity = p4
              ))

}
