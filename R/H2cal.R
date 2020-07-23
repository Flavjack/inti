#' Heritability in plant breeding on a genotype-difference basis
#'
#' Function to calculate:
#' 1. The variance components.
#' 2. Heritability under three approaches: Standard, Cullis and Piepho.
#' 3. Best Linear Unbiased Predictors (BLUPs).
#' 4. Best Linear Unbiased Estimators (BLUEs).
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param trait Name of the trait.
#' @param gen.name Name of the genotypes.
#' @param rep.n Number of replications in the experiment.
#' @param loc.name Name of the location (default = NULL). See details.
#' @param loc.n Number of locations (default = 1). See details.
#' @param year.name Name of the years (default = NULL). See details.
#' @param year.n Number of years (default = 1). See details.
#' @param fix.model The fixed effects in the model. See examples.
#' @param ran.model The random effects in the model. See examples.
#' @param summary Summary of the random model (default = TRUE).
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
#' @importFrom dplyr filter pull rename mutate all_of
#' @importFrom purrr pluck as_vector
#' @importFrom emmeans emmeans
#' @importFrom stringr str_detect
#' @importFrom tibble rownames_to_column as_tibble tibble
#' @importFrom lme4 ranef VarCorr
#' @importFrom graphics abline par
#' @importFrom stats fitted var as.formula
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal
#'
#' @source
#'
#' https://github.com/PaulSchmidtGit/Heritability/tree/master/Alternative%20Heritability%20Measures
#'
#' https://stackoverflow.com/questions/38697477/mean-variance-of-a-difference-of-blues-or-blups-in-lme4
#'
#' @examples
#'
#' \dontrun{
#' library(inti)
#' library(tidyverse)
#' library(emmeans)
#' library(lme4)
#' library(lmerTest)
#' library(agridat)
#'
#'  dt <- john.alpha
#'  hr <- H2cal(data = dt
#'             , trait = "yield"
#'             , gen.name = "gen"
#'             , rep.n = 3
#'             , fix.model = "rep + (1|rep:block) + gen"
#'             , ran.model = "rep + (1|rep:block) + (1|gen)"
#'             , blues = T
#'             , plot_diag = TRUE
#'             , plot_dots = "rep"
#'             )
#'  hr$tabsmr
#'  hr$blups
#'  hr$blues
#' }
#'
#' @export

H2cal <- function(data
                  , trait
                  , gen.name
                  , rep.n
                  , loc.n = 1
                  , year.n = 1
                  , loc.name = NULL
                  , year.name = NULL
                  , fix.model
                  , ran.model
                  , summary = FALSE
                  , blues = FALSE
                  , effects = FALSE
                  , plot_diag = FALSE
                  , plot_dots = NULL
                  ){

  # library(tidyverse)
  # library(emmeans)
  # library(lme4)
  # library(lmerTest)

  grp <- emmean <- SE <- Var <-  NULL # avoid Undefined global functions or variables

  # Varible

  if (summary == TRUE || plot_diag == TRUE || !is.null(plot_dots)) {

    print(paste("##>-----------------------------------------"))
    print(paste("##>", trait))
    print(paste("##>-----------------------------------------"))

  }

  ### fit models

  # random genotype effect
  r.md <- as.formula(paste(trait, paste(ran.model, collapse = " + "), sep = " ~ "))
  g.ran <- eval(bquote(lmer(.(r.md), data = data)))

  if (summary == TRUE) {

    summary(g.ran) %>% print()

  }

  # fixed genotype effect
  f.md <- as.formula(paste(trait, paste(fix.model, collapse = " + "), sep = " ~ "))
  g.fix <- eval(bquote(lmer(.(f.md), data = data)))
  # summary(g.fix)

  ### Plot models

  if (plot_diag == TRUE) {

    par(mfrow=c(2,4))
    hist(resid(g.fix), main = trait)
    qqnorm(resid(g.fix), main = trait); qqline(resid(g.fix))
    plot(fitted(g.fix), resid(g.fix, type = "pearson"), main = trait); abline(h=0)
    plot(resid(g.fix), main = trait)
    hist(resid(g.ran), main = trait)
    qqnorm(resid(g.ran), main = trait); qqline(resid(g.ran))
    plot(fitted(g.ran), resid(g.ran, type = "pearson"), main = trait); abline(h=0)
    plot(resid(g.ran), main = trait)
    par(mfrow=c(1,1))

  }

  ### Dot plots

  if (!is.null(plot_dots)) {

    p_dots <- data %>%
      ggplot(aes(!!as.name(trait), !!as.name(gen.name))) +
      geom_point(aes(color = !!as.name(plot_dots))) +
      theme_minimal()

    print(p_dots)

  }

  ### handle model estimates

  # number of genotypes

  gen.n <- g.ran %>%
    summary() %>%
    purrr::pluck("ngrps") %>%
    .[gen.name]

  # genotypic variance component

  vc.g <- g.ran %>%
    lme4::VarCorr() %>%
    tibble::as_tibble() %>%
    dplyr::filter(grp == gen.name) %>%
    dplyr::pull(vcov)

  # environment variance component

  if(loc.n > 1){

    gxl <- paste(gen.name, loc.name, sep = ":")

    vc.gxl <- g.ran %>%
      lme4::VarCorr() %>%
      tibble::as_tibble() %>%
      dplyr::filter(grp == gxl) %>%
      dplyr::pull(vcov)

    if( length(strsplit(gxl,":")[[1]]) < 2) {

      message("You should include loc.name in the arguments")

      vc.gxl <- 0

    } else if (identical(vc.gxl, numeric(0))) {

      message("You should include (1|genotype:location) interaction")

      vc.gxl <- 0

    }

  } else if (!is.null(loc.name) && loc.n == 1) {

    message("You should include loc.n in the arguments")

    vc.gxl <- 0

    } else if (loc.n == 1){

    vc.gxl <- 0

  }

  # year variance component

  if(year.n > 1){

    gxy <- paste(gen.name, year.name, sep = ":")

    vc.gxy <- g.ran %>%
      lme4::VarCorr() %>%
      tibble::as_tibble() %>%
      dplyr::filter(grp == gxy) %>%
      dplyr::pull(vcov)

    if( length(strsplit(gxy,":")[[1]]) < 2 ) {

      message("You should include year.name in the arguments")

      vc.gxy <- 0

    } else if (identical(vc.gxy, numeric(0))) {

      message("You should include (1|genotype:year) interaction")

      vc.gxy <- 0

    }

  } else if (!is.null(year.name) && year.n == 1) {

    message("You should include year.n in the arguments")

    vc.gxy <- 0

  } else if (year.n == 1){

    vc.gxy <- 0

  }

  # location x year variance component (review in MET)

  if(year.n > 1 && loc.n > 1){

    gxlxy <- paste(gen.name, loc.name, year.name, sep = ":")

    vc.gxlxy <- g.ran %>%
      lme4::VarCorr() %>%
      tibble::as_tibble() %>%
      dplyr::filter(grp == gxlxy) %>%
      dplyr::pull(vcov)

    if( length(strsplit(gxlxy,":")[[1]]) < 3 ) {

      message("You should include location/years arguments")

      vc.gxlxy <- 0

    } else if (identical(vc.gxlxy, numeric(0))) {

      message("You should include (1|genotype:location:year) interaction")

      vc.gxlxy <- 0

    }

  } else if (year.n == 1 && loc.n == 1){

    vc.gxlxy <- 0

  } else {vc.gxlxy <- 0}

  # error variance component

  vc.e <- g.ran %>%
    lme4::VarCorr() %>%
    tibble::as_tibble() %>%
    dplyr::filter(grp == "Residual") %>%
    dplyr::pull(vcov)

  ## Best Linear Unbiased Estimators (BLUE)

  if(blues == TRUE){

    BLUE <- g.fix %>%
      emmeans::emmeans(as.formula(paste("pairwise", gen.name, sep = " ~ ")), )

    BLUEs <- BLUE %>%
      purrr::pluck("emmeans") %>%
      tibble::as_tibble() %>%
      dplyr::rename(!!trait := 'emmean')

    # mean variance of a difference between genotypes (BLUEs)

    vdBLUE.avg <- BLUE %>%
      purrr::pluck("contrasts") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(Var=SE^2) %>%
      dplyr::pull(Var) %>%
      mean(.) # vdBLUE.avg

  } else if (blues == FALSE){

    BLUEs <- NULL

    count <- vcov(g.fix) %>%
      purrr::pluck("Dimnames") %>%
      purrr::pluck(1) %>%
      stringr::str_detect(gen.name) %>%
      summary(.) %>%
      purrr::pluck("FALSE")

    if(is.null(count)){

      vdBLUE.avg <- g.fix %>%
        vcov(.) %>%
        base::as.matrix(.) %>%
        diag(.) %>%
        as.vector() %>%
        mean(.)

    } else if (count > 0) {

      vdBLUE.avg <- g.fix %>%
        vcov(.) %>%
        base::as.matrix(.) %>%
        diag(.) %>%
        as.vector() %>%
        .[-c(1:count)] %>%
        mean(.)
    }
  }

  ## Best Linear Unbiased Predictors (BLUP)

  if(effects == FALSE){

    BLUPs <- g.ran %>%
      stats::coef() %>%
      purrr::pluck(gen.name) %>%
      tibble::rownames_to_column(gen.name) %>%
      dplyr::rename(!!trait := '(Intercept)') %>%
      tibble::as_tibble(.) %>%
      dplyr::select(all_of(gen.name), dplyr::all_of(trait))

  } else {

    BLUPs <- g.ran %>%
      lme4::ranef() %>%
      purrr::pluck(gen.name) %>%
      tibble::rownames_to_column(gen.name) %>%
      dplyr::rename(!!trait := '(Intercept)') %>%
      tibble::as_tibble(.) %>%
      dplyr::select(all_of(gen.name), dplyr::all_of(trait))
  }

  # mean variance of a difference between genotypes (BLUPs)

  vdBLUP.avg <- g.ran %>%
    lme4::ranef(condVar = TRUE) %>%
    purrr::pluck(gen.name) %>%
    attr("postVar") %>%
    purrr::as_vector(.) %>%
    mean(.)*2

  ## Summary table of adjusted means

  if (blues == TRUE) {

    smd <- BLUEs %>%
        dplyr::summarise(
          mean = mean(!!as.name(trait), na.rm = T)
          , std = sqrt(var(!!as.name(trait), na.rm = T))
          , min = min(!!as.name(trait))
          , max = max(!!as.name(trait))
        )

  } else if (blues == FALSE) {

    smd <- BLUPs %>%
      dplyr::summarise(
        mean = mean(!!as.name(trait), na.rm = T)
        , std = sqrt(var(!!as.name(trait), na.rm = T))
        , min = min(!!as.name(trait))
        , max = max(!!as.name(trait))
      )

  }

  ## Heritability

  # H2 Standard
  H2.s <- vc.g/(vc.g + vc.gxl/loc.n + vc.gxy/year.n + vc.gxlxy/(loc.n*year.n) + vc.e/(loc.n*year.n*rep.n))

  # H2 Piepho
  H2.p <- vc.g/(vc.g + vdBLUE.avg/2)

  # H2 Cullis
  H2.c <- 1 - (vdBLUP.avg/2/vc.g)

  ## Summary table VC & H^2

  vrcp <- dplyr::tibble(
    varible = trait
    , rep = rep.n
    , geno = gen.n
    , env = loc.n
    , year = year.n
    , mean = smd$mean
    , std = smd$std
    , min = smd$min
    , max = smd$max
    , V.g = vc.g
    , V.gxl = vc.gxl
    , V.gxy = vc.gxy
    , V.e = vc.e
    , h2.s = H2.s
    , h2.c = H2.c
    , h2.p = H2.p
    )

  ## Results

  rsl <- list(
    tabsmr = vrcp
    , blups = BLUPs
    , blues = BLUEs

    )
}

