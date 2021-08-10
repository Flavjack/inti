#' Heritability in plant breeding
#'
#' Heritability in plant breeding on a genotype difference basis
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param trait Name of the trait.
#' @param gen.name Name of the genotypes.
#' @param rep.n Number of replications in the experiment.
#' @param loc.name Name of the location (default = NULL). See details.
#' @param loc.n Number of locations (default = 1). See details.
#' @param year.name Name of the years (default = NULL). See details.
#' @param year.n Number of years (default = 1). See details.
#' @param ran.model The random effects in the model. See examples.
#' @param fix.model The fixed effects in the model. See examples.
#' @param emmeans Use emmeans for calculate the BLUEs (default = FALSE).
#' @param summary Print summary from random model (default = FALSE).
#' @param plot_diag Show diagnostic plots (default = FALSE).
#' @param outliers.rm Remove outliers (default = FALSE). See references.
#' @param weights an optional vector of ‘prior weights’ to be used in the
#'   fitting process (default = NULL).
#' @param trial Name of the trial in the results (default = NULL). 
#'
#' @details
#'
#' The function allows to made the calculation for individual or
#' multi-environmental trials (MET) using th fixed and random model.
#'
#' 1. The variance components.
#'
#' 2. Heritability under three approaches: Standard, Cullis and Piepho.
#'
#' 3. Best Linear Unbiased Predictors (BLUPs).
#'
#' 4. Best Linear Unbiased Estimators (BLUEs).
#'
#' 5. Outliers remove.
#'
#' For individual experiments is necessary provide the \code{trait},
#' \code{gen.name}, \code{rep.n}.
#'
#' For MET experiments you should \code{loc.n} and \code{loc.name} and/or
#' \code{year.n} and \code{year.name} according your experiment.
#'
#' The blues calculation is based in the pairwise comparison and its could takes
#' time according the number of the genotypes.
#'
#' You can specify as \code{blues = FALSE} for calculate the variance components
#' and blups faster.
#'
#' For more information review the references.
#'
#' @return list
#'
#' @author
#'
#' Maria Belen Kistner
#'
#' Flavio Lozano Isla
#'
#' @references
#'
#' Schmidt, P., J. Hartung, J. Bennewitz, and H.P. Piepho. 2019. Heritability in
#' Plant Breeding on a Genotype Difference Basis. Genetics 212(4).
#'
#' Schmidt, P., J. Hartung, J. Rath, and H.P. Piepho. 2019. Estimating Broad
#' Sense Heritability with Unbalanced Data from Agricultural Cultivar Trials.
#' Crop Science 59(2).
#'
#' Bernal Vasquez, Angela Maria, et al. “Outlier Detection Methods for
#' Generalized Lattices: A Case Study on the Transition from ANOVA to REML.”
#' Theoretical and Applied Genetics, vol. 129, no. 4, Apr. 2016.
#'
#' @importFrom dplyr filter pull rename mutate all_of
#' @importFrom purrr pluck as_vector
#' @importFrom stringr str_detect str_replace
#' @importFrom tibble rownames_to_column as_tibble tibble
#' @importFrom lme4 lmer ranef VarCorr
#' @importFrom graphics abline par
#' @importFrom stats fitted var as.formula
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal
#' @importFrom emmeans emmeans
#'
#' @export
#'
#' @examples
#'
#' library(inti)
#' 
#' dt <- potato
#' 
#' hr <- H2cal(data = dt
#'             , trait = "tubdw"
#'             , gen.name = "geno"
#'             , rep.n = 5
#'             , ran.model = "1 + (1|bloque) + (1|geno)"
#'             , fix.model = "0 + (1|bloque) + geno"
#'             , emmeans = TRUE
#'             , plot_diag = TRUE
#'             , outliers.rm = TRUE
#'             )
#'             
#'  hr$tabsmr
#'  hr$blues
#'  hr$blups
#'  

H2cal <- function(data
                  , trait
                  , gen.name
                  , rep.n
                  , loc.n = 1
                  , year.n = 1
                  , loc.name = NULL
                  , year.name = NULL
                  , ran.model
                  , fix.model
                  , summary = FALSE
                  , emmeans = FALSE
                  , weights = NULL
                  , plot_diag = FALSE
                  , outliers.rm = FALSE
                  , trial = NULL
                  ){

  # avoid Undefined global functions or variables
  grp <- emmean <- SE <- Var <- NULL 
  
  # outliers remove ---------------------------------------------------------

  if ( outliers.rm == TRUE ) {

    out.rm <- data %>% outliers_remove(data = .
                                     , trait = trait
                                     , model = ran.model
                                     )
    dt.rm <- out.rm %>% pluck(1)

    out.fm <- data %>% outliers_remove(data = .
                                       , trait = trait
                                       , model = fix.model
                                       )
    dt.fm <- out.fm %>% pluck(1)

    outliers <- list(random = out.rm$outliers, fixed = out.fm$outliers)

  } else {

    dt.rm <- data

    dt.fm <- data

    outliers <- NULL

  }

# fit models --------------------------------------------------------------
  
  # random genotype effect
  r.md <- as.formula(paste(trait, paste(ran.model, collapse = " + "), sep = " ~ "))
  g.ran <- eval(bquote(lmer(.(r.md), weights = weights, data = dt.rm)))

  # fixed genotype effect
  f.md <- as.formula(paste(trait, paste(fix.model, collapse = " + "), sep = " ~ "))
  g.fix <- eval(bquote(lmer(.(f.md), weights = weights, data = dt.fm)))

  # Print model summary -----------------------------------------------------
  
  if (summary == TRUE) {
    
    summary(g.ran) %>% print()
    
  }

  # Plot models -------------------------------------------------------------

  if (plot_diag == TRUE) {

    prp <- par(no.readonly = TRUE)
    on.exit(par(prp))   
    
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

# -------------------------------------------------------------------------

  ### handle model estimates

  # number of genotypes

  gen.n <- g.ran %>%
    summary() %>%
    purrr::pluck("ngrps") %>%
    .[gen.name]

  # genotypic variance component

  vc.g <- c(VarCorr(g.ran)[[gen.name]])

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
  

## Best Linear Unbiased Estimators (BLUE) :: fixed model
    
    if(emmeans == TRUE){
      
      BLUE <- g.fix %>%
        emmeans::emmeans(as.formula(paste("pairwise", gen.name, sep = " ~ ")))
      
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
        mean(.)
      
    } else if (emmeans == FALSE) {
      
      count <- vcov(g.fix) %>%
        purrr::pluck("Dimnames") %>%
        purrr::pluck(1) %>%
        stringr::str_detect(gen.name) %>%
        summary(.) %>%
        purrr::pluck("FALSE") %>% 
        as.numeric()
      
      if(length(count) == 0) { count <- 0 }
  
      BLUEs <- g.fix %>%
        lme4::fixef() %>%
        data.frame() %>% 
        rownames_to_column({{gen.name}}) %>% 
        dplyr::rename(!!trait := .) %>% 
        mutate(across({{gen.name}}, ~stringr::str_replace(., gen.name, ""))) %>% 
        mutate(across({{trait}}, as.numeric)) %>% 
        mutate(smith.w = diag(solve(vcov(g.fix)))) %>% 
        dplyr::slice((count+1):NROW(.))  
      
      vdBLUE.avg <- g.fix %>%
        vcov(.) %>%
        base::as.matrix(.) %>%
        diag(.) %>%
        enframe() %>%
        dplyr::slice((count+1):NROW(.)) %>% 
        select(!.data$name) %>% 
        deframe() %>% 
        mean()
      
    }


# Best Linear Unbiased Predictors (BLUP) - random model -------------------

      BLUPs <- g.ran %>%
        stats::coef() %>%
        purrr::pluck(gen.name) %>%
        tibble::rownames_to_column(gen.name) %>%
        dplyr::rename(!!trait := '(Intercept)') %>%
        tibble::as_tibble(.) %>%
        dplyr::select({{gen.name}}, {{trait}}) %>% 
        {if (!is.null(trial)) dplyr::mutate(.data = ., trial = trial) else .} %>% 
        {if (!is.null(trial)) select(.data = ., trial, everything()) else .}
      
# mean variance of a difference between genotypes (BLUPs) -----------------

      vdBLUP.avg <- g.ran %>%
        lme4::ranef(condVar = TRUE) %>%
        purrr::pluck(gen.name) %>%
        attr("postVar") %>%
        purrr::as_vector(.) %>%
        mean(.)*2

# Summary table of adjusted means (BLUEs) ---------------------------------

    smd <- BLUEs %>%
        dplyr::summarise(
          mean = mean(!!as.name(trait), na.rm = T)
          , std = sqrt(var(!!as.name(trait), na.rm = T))
          , min = min(!!as.name(trait))
          , max = max(!!as.name(trait))
        ) 
    
# -------------------------------------------------------------------------
    
    BLUEs <- BLUEs %>% 
      {if (!is.null(trial)) dplyr::mutate(.data = ., trial = trial) else .} %>% 
      {if (!is.null(trial)) select(.data = ., trial, everything()) else .}

# -------------------------------------------------------------------------

  ## Heritability

  # H2 Standard
  H2.s <- vc.g/(vc.g + vc.gxl/loc.n + vc.gxy/year.n + vc.gxlxy/(loc.n*year.n) + vc.e/(loc.n*year.n*rep.n))

  # H2 Piepho
  H2.p <- vc.g/(vc.g + vdBLUE.avg/2)

  # H2 Cullis
  H2.c <- 1 - (vdBLUP.avg/2/vc.g)

  ## Summary table VC & H^2

  vrcp <- dplyr::tibble(
    variable = trait
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
    ) %>% 
    {if (!is.null(trial)) dplyr::mutate(.data = ., trial = trial) else .} %>% 
    {if (!is.null(trial)) select(.data = ., trial, everything()) else .}

  ## Results

  rsl <- list(
    tabsmr = vrcp
    , blups = BLUPs
    , blues = BLUEs
    , model = g.ran
    , outliers = outliers
    )
}

