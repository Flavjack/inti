#' Adjusted means based in BLUEs
#'
#' Function to calculate the adjusted means using the Best Linear Unbiased Estimators (BLUEs).
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param trait Name of the trait.
#' @param treatment Name of the treat.
#' @param fix.model The fixed effects in the model. See examples.
#' @details The blues calculation is based in the pairwaise comparison and its could takes time according the number of the treatments.
#' @return A data frame with the BLUEs.
#' @author Flavio Lozano-Isla
#' @examples
#'
#' \dontrun{
#' library(tidyverse)
#' library(emmeans)
#' library(lme4)
#' library(lmerTest)
#' library(agridat)
#'
#' adm <- adjmeans(data = john.alpha
#'                 , trait = "yield"
#'                 , treatment = "gen"
#'                 , fix.model = "rep + (1|rep:block) + gen"
#'                 )
#' adm
#' }
#'
#' @importFrom emmeans emmeans
#' @importFrom purrr pluck
#' @importFrom dplyr select all_of
#' @importFrom tibble as_tibble
#' @export

adjmeans <- function(data
                     , trait
                     , treatment
                     , fix.model
                     ){

  print(trait)

  # fixed genotype effect
  f.md <- as.formula(paste(trait, paste(fix.model, collapse = " + "), sep = " ~ "))
  g.fix <- eval(bquote(lmer(.(f.md), data = data)))

  # summary(g.fix)
  plot(g.fix, main = trait)
  qqnorm(resid(g.fix), main = trait); qqline(resid(g.fix))
  hist(resid(g.fix), main = trait)

  ## Best Linear Unbiased Estimators (BLUE)
  BLUE <- g.fix %>%
    emmeans::emmeans(as.formula(paste("pairwise", treatment, sep = " ~ ")), )

  BLUEs <- BLUE %>%
    purrr::pluck("emmeans") %>%
    tibble::as_tibble

  ### results
  blues  <-  BLUEs %>%
    dplyr::select(dplyr::all_of(treatment), !!(trait) := emmean)

}

