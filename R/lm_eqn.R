#' Regression line equation
#'
#' @description Construction of the regression line equation
#' @param x variable in the x axis
#' @param y variable in the y axis
#' @param data dataframe with the information
#' @return regression equation
#'
#' @author Flavio Lozano-Isla
#'
#' @importFrom stats as.formula coef
#'
#' @export

lm_eqn <- function(x, y, data) {

  fml <- as.formula(paste(x,y, sep = " ~ "))
  mdl <- lm(fml, data)

  eq <- as.character(as.expression( substitute(italic(y) == a + (b) * italic(x) * "," ~~ italic(R)^2 ~ "=" ~ r2,
                                               list(a = format(coef(mdl)[1], digits=2), b = format(coef(mdl)[2], digits=2),
                                                    r2 = format(summary(mdl)$r.squared, digits=3)))))

  eq

  # eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,
  #                  list(a = format(coef(mdl)[1], digits = 2),
  #                       b = format(coef(mdl)[2], digits = 2),
  #                       r2 = format(summary(mdl)$r.squared, digits = 3)))
  # as.character(as.expression(eq))


}
