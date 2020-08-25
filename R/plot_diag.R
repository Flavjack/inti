#' Diagnostic plots
#'
#' Function to plot the diagnostic of models
#'
#' @param model statistical model
#'
#' @return plots
#'
#' @import dplyr
#' @importFrom stats rstandard
#'
#' @export

plot_diag <- function( model ) {

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
