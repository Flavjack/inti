#' Heritability in plant breeding on a genotype-difference basis
#'
#' Function to calculate the heritability for multiple traits:
#' 1. The variance components.
#' 2. Heritability under three approaches: Standard, Cullis and Piepho.
#' 3. Best Linear Unbiased Predictors (BLUPs).
#' 4. Best Linear Unbiased Estimators (BLUEs).
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param trait Name of the traits or column index since start the analysis.
#' @param gen.name Name of the genotypes.
#' @param rep.n Number of replications in the experiment.
#' @param loc.name Name of the location (default = NULL). See details.
#' @param loc.n Number of locations (default = 1). See details.
#' @param year.name Name of the years (default = NULL). See details.
#' @param year.n Number of years (default = 1). See details.
#' @param fix.model The fixed effects in the model. See examples.
#' @param ran.model The random effects in the model. See examples.
#' @param blues Calculate the BLUEs (default = FALSE).
#' @param effects Conditional modes of the random effects instead of the BLUPs (default = FALSE).
#' @param plot_diag Show diagnostic plots (default = FALSE).
#' @param plot_dots Show dotplot genotypes vs trait (default = NULL). See examples.
#'
#' @details The function allows to made the calculation for individual or multi-environmental trials (MET) using th fixed and random model.
#'
#' For individual experiments is necessary provide the \code{trait}, \code{gen.name}, \code{rep.n}.
#'
#' For MET experiments you should \code{loc.n} and \code{loc.name} y/o \code{year.n} and \code{year.name} according your experiment.
#'
#' The blues calculation is based in the pairwise comparison and its could takes time according the number of the genotypes.
#'
#' You can specify as \code{blues = FALSE} for calculate the variance components and blups faster.
#'
#' For more information review the references.
#'
#' @return A list with three object:
#'
#' 1. Table with the variance components and heritability.
#'
#' 2. BLUPs.
#'
#' 3. BLUEs.
#'
#' @author
#'
#' Maria Belen Kistner
#'
#' Flavio Lozano-Isla
#'
#' @references
#'
#' Schmidt, P., J. Hartung, J. Bennewitz, and H.-P. Piepho. 2019. Heritability in Plant Breeding on a Genotype-Difference Basis. Genetics 212(4): 991–1008. doi: 10.1534/genetics.119.302134.
#'
#' Schmidt, P., J. Hartung, J. Rath, and H.-P. Piepho. 2019. Estimating Broad-Sense Heritability with Unbalanced Data from Agricultural Cultivar Trials. Crop Science 59(2): 525–536. doi: 10.2135/cropsci2018.06.0376.
#'
#' @source
#'
#' https://github.com/PaulSchmidtGit/Heritability/tree/master/Alternative%20Heritability%20Measures
#'
#' https://stackoverflow.com/questions/38697477/mean-variance-of-a-difference-of-blues-or-blups-in-lme4
#'
#' @export

H2cal_fb <- function(data
                     , trait
                     , gen.name
                     , rep.n
                     , loc.name = NULL
                     , loc.n = 1
                     , year.name = NULL
                     , year.n = 1
                     , fix.model
                     , ran.model
                     , blues = FALSE
                     , effects = FALSE
                     , plot_diag = FALSE
                     , plot_dots = NULL
){

  col_read <- if (is.numeric(trait)) {

    var <- colnames(data)[trait:ncol(data)]
    ft <- trait:ncol(data)
    list(vars = var, cols_n = ft)

  } else if (is.character(trait)) {

    var <- trait
    ft <- match(trait, colnames(data))
    list(vars = var, cols_n = ft)
  }

  h2c <- lapply(col_read$cols_n, function(x){

    trt <- data %>%
      inti::H2cal(data = .
                  , trait = colnames(.)[x]
                  , gen.name = gen.name
                  , rep.n = rep.n
                  , loc.name = loc.name
                  , loc.n = loc.n
                  , year.name = year.name
                  , year.n = year.n
                  , fix.model = fix.model
                  , ran.model = ran.model
                  , blues = blues
                  , effects = effects
                  , plot_diag = plot_diag
                  , plot_dots = plot_dots
                  )
  })

  h2cal <- do.call(rbind
                   , lapply(1:length(h2c)
                            , function(i) h2c[[i]][[1]]))

  blups <- Reduce(function(...) merge(..., by = gen.name, all = TRUE)
                  , lapply(1:length(h2c)
                           , function(i) h2c[[i]][[2]]))

  if (blues == FALSE) {

    blues <-  NULL

  } else if (blues == TRUE) {

    blues <- Reduce(function(...) merge(..., by = gen.name, all = TRUE)
                    , lapply(1:length(h2c)
                             , function(i) h2c[[i]][[3]])) %>%
      select(gen.name, col_read$vars)

  }

  list(tabsmr = h2cal, blups = blups, blues = blues)

}
