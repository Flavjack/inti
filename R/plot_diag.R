#' Diagnostic plots
#'
#' Function to plot the diagnostic of models
#'
#' @param model Statistical model
#' @param title Plot title
#'
#' @return plots
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' dt <- potato
#' 
#' lm <- aov(stemdw ~ bloque + geno*treat, dt)
#' 
#' plot(lm, which = 1)
#' plot_diag(lm)[3]
#' 
#' plot(lm, which = 2)
#' plot_diag(lm)[2]
#' 
#' plot(lm, which = 3)
#' plot_diag(lm)[4]
#' 
#' plot(lm, which = 4)
#' plot_diag(lm)[1]
#' 
#' }
#' 

plot_diag <- function( model, title = NA) {
  
  stopifnot(class(model) %in% c("lm", "aov", "lmerMod"))
  
  title <- if(is.null(title) || is.na(title) ) {""} else {title}
  
  dt <- if ( class(model) == "lm" || class(model) == "aov" ) {
    
    ggplot2::fortify(model)
    
  } else if ( class(model) == "lmerMod" ) {
    
    lme4::fortify.merMod(model)
    
  }
  
  # Histogram
  p1 <- ggplot(dt , aes(.data$.resid)) +
    geom_histogram(aes_string(y = "..density..", fill = "..count.."), 
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
  
  p5 <-ggplot(dt, aes(stats::hatvalues(model), .data$.resid)) + 
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
