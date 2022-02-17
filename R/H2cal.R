#' Broad-sense heritability in plant breeding
#'
#' Heritability in plant breeding on a genotype difference basis
#'
#' @param data Experimental design data frame with the factors and traits.
#' @param trait Name of the trait.
#' @param gen.name Name of the genotypes.
#' @param rep.n Number of replications in the experiment.
#' @param env.name Name of the environments (default = NULL). See details.
#' @param env.n Number of environments (default = 1). See details.
#' @param year.name Name of the years (default = NULL). See details.
#' @param year.n Number of years (default = 1). See details.
#' @param fixed.model The fixed effects in the model (BLUEs). See examples.
#' @param random.model The random effects in the model (BLUPs). See examples.
#' @param emmeans Use emmeans for calculate the BLUEs (default = FALSE).
#' @param summary Print summary from random model (default = FALSE).
#' @param plot_diag Show diagnostic plots for fixed and random effects (default
#'   = FALSE). Options: "base", "ggplot". .
#' @param outliers.rm Remove outliers (default = FALSE). See references.
#' @param weights an optional vector of ‘prior weights’ to be used in the
#'   fitting process (default = NULL).
#' @param trial Column with the name of the trial in the results (default =
#'   NULL).
#'
#' @details
#'
#' The function allows to made the calculation for individual or
#' multi-environmental trials (MET) using fixed and random model.
#'
#' 1. The variance components based in the random model and the population
#' summary information based in the fixed model (BLUEs).
#'
#' 2. Heritability under three approaches: Standard (ANOVA), Cullis (BLUPs) and
#' Piepho (BLUEs).
#'
#' 3. Best Linear Unbiased Estimators (BLUEs), fixed effect.
#'
#' 4. Best Linear Unbiased Predictors (BLUPs), random effect.
#'
#' 5. Table with the outliers removed for each model.
#'
#' For individual experiments is necessary provide the \code{trait},
#' \code{gen.name}, \code{rep.n}.
#'
#' For MET experiments you should \code{env.n} and \code{env.name} and/or
#' \code{year.n} and \code{year.name} according your experiment.
#'
#' The BLUEs calculation based in the pairwise comparison could be time
#' consuming with the increase of the number of the genotypes. You can specify
#' \code{emmeans = FALSE} and the calculate of the BLUEs will be faster.
#'
#' If \code{emmeans = FALSE} you should change 1 by 0 in the fixed model for
#' exclude the intersect in the analysis and get all the genotypes BLUEs.
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
#' Bernal Vasquez, Angela Maria, et al. “Outlier Detection Methods for
#' Generalized Lattices: A Case Study on the Transition from ANOVA to REML.”
#' Theoretical and Applied Genetics, vol. 129, no. 4, Apr. 2016.
#'
#' Buntaran, H., Piepho, H., Schmidt, P., Ryden, J., Halling, M., and Forkman,
#' J. (2020). Cross validation of stagewise mixed model analysis of Swedish
#' variety trials with winter wheat and spring barley. Crop Science, 60(5).
#'
#' Schmidt, P., J. Hartung, J. Bennewitz, and H.P. Piepho. 2019. Heritability in
#' Plant Breeding on a Genotype Difference Basis. Genetics 212(4).
#'
#' Schmidt, P., J. Hartung, J. Rath, and H.P. Piepho. 2019. Estimating Broad
#' Sense Heritability with Unbalanced Data from Agricultural Cultivar Trials.
#' Crop Science 59(2).
#'
#' Tanaka, E., and Hui, F. K. C. (2019). Symbolic Formulae for Linear Mixed
#' Models. In H. Nguyen (Ed.), Statistics and Data Science. Springer.
#'
#' Zystro, J., Colley, M., and Dawson, J. (2018). Alternative Experimental
#' Designs for Plant Breeding. In Plant Breeding Reviews. John Wiley and Sons,
#' Ltd.
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
#'             , trait = "stemdw"
#'             , gen.name = "geno"
#'             , rep.n = 5
#'             , fixed.model = "0 + (1|bloque) + geno"
#'             , random.model = "1 + (1|bloque) + (1|geno)"
#'             , emmeans = TRUE
#'             , plot_diag = FALSE
#'             , outliers.rm = TRUE
#'             )
#'
#'  hr$tabsmr
#'  hr$blues
#'  hr$blups
#'  hr$outliers
#'  

H2cal <- function(data
                  , trait
                  , gen.name
                  , rep.n
                  , env.n = 1
                  , year.n = 1
                  , env.name = NULL
                  , year.name = NULL
                  , fixed.model
                  , random.model
                  , summary = FALSE
                  , emmeans = FALSE
                  , weights = NULL
                  , plot_diag = FALSE
                  , outliers.rm = FALSE
                  , trial = NULL
                  ){
  
  

# -------------------------------------------------------------------------
  
  if (FALSE) {
    
    data = dt
    trait = "stemdw"
    gen.name = "geno"
    rep.n = 5
    fixed.model = "0 + (1|bloque) + geno"
    random.model = "1 + (1|bloque) + (1|geno)"
    emmeans = TRUE
    plot_diag = TRUE
    outliers.rm = F
    
    weights = NULL
    summary = TRUE
    
  }
  
# -------------------------------------------------------------------------

  grp <- emmean <- SE <- Var <- where <- NULL 
  V.g <- V.gxl <- env <- V.gxy <- year <- NULL
  V.gxlxy <- V.e <- V.p <- vdBLUEs <- vdBLUPs <- NULL
  
# outliers remove ---------------------------------------------------------

  if ( outliers.rm == TRUE ) {

    out.rm <- data %>% outliers_remove(data = .
                                     , trait = trait
                                     , model = random.model
                                     )
    dt.rm <- out.rm %>% purrr::pluck(1) 

    out.fm <- data %>% outliers_remove(data = .
                                       , trait = trait
                                       , model = fixed.model
                                       )
    dt.fm <- out.fm %>% purrr::pluck(1) 

    outliers <- list(fixed = out.fm$outliers, random = out.rm$outliers)

  } else if (outliers.rm == FALSE) {

    dt.rm <- data 

    dt.fm <- data 

    outliers <- NULL

  }

# fit models --------------------------------------------------------------
  
  #> fixed genotype effect
  f.md <- as.formula(paste(trait, paste(fixed.model, collapse = " + "), sep = " ~ "))
  g.fix <- eval(bquote(lme4::lmer(.(f.md), weights = weights, data = dt.fm)))
  
  #> random genotype effect
  r.md <- as.formula(paste(trait, paste(random.model, collapse = " + "), sep = " ~ "))
  g.ran <- eval(bquote(lme4::lmer(.(r.md), weights = weights, data = dt.rm)))

# Print model summary -----------------------------------------------------
  
  if (summary == TRUE) {
    
    summary(g.ran) %>% print()
    
  }

# Plot models -------------------------------------------------------------
  
  fm.title <- paste("Fixed model:", trait)
  rm.title <- paste("Random model:", trait)
  
  if (plot_diag == TRUE || plot_diag == "base") {
    
    diag.plot <- NULL
    
    prp <- par(no.readonly = TRUE)
    on.exit(par(prp))   
    
    par(mfrow=c(2,4))
    hist(resid(g.fix), main = fm.title)
    qqnorm(resid(g.fix), main = fm.title); qqline(resid(g.fix))
    plot(fitted(g.fix), resid(g.fix, type = "pearson"), main = fm.title); abline(h=0)
    plot(resid(g.fix), main = fm.title)
    #>
    hist(resid(g.ran), main = rm.title)
    qqnorm(resid(g.ran), main = rm.title); qqline(resid(g.ran))
    plot(fitted(g.ran), resid(g.ran, type = "pearson"), main = rm.title); abline(h=0)
    plot(resid(g.ran), main = rm.title)
    par(mfrow=c(1,1))
    
  } else if (plot_diag == "ggplot") {
    
    diag.fix <- inti::plot_diag(g.fix, title = fm.title)
    diag.ran <- inti::plot_diag(g.ran, title = rm.title)
    
    diag.plot <- list(fixed = diag.fix, random = diag.ran)
      
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = 2
                                                                 , ncol = 4
                                                                 )))
    
    vplayout <- function(x, y) 
      grid::viewport(layout.pos.row = x, layout.pos.col = y)
    print(diag.fix$histogram, vp = vplayout(1, 1))
    print(diag.fix$qqplot, vp = vplayout(1, 2))
    print(diag.fix$residual, vp = vplayout(1, 3))
    print(diag.fix$homoscedasticity, vp = vplayout(1, 4))
    print(diag.ran$histogram, vp = vplayout(2, 1))
    print(diag.ran$qqplot, vp = vplayout(2, 2))
    print(diag.ran$residual, vp = vplayout(2, 3))
    print(diag.ran$homoscedasticity, vp = vplayout(2, 4))
    
    } else {diag.plot <- NULL}
  
# Model estimates --------------------------------------------------
  
# number of genotypes

  gen.n <- g.ran %>%
    summary() %>%
    purrr::pluck("ngrps") %>%
    .[{{gen.name}}]

# genotypic variance component

  vc.g <- c(lme4::VarCorr(g.ran)[[gen.name]])

# environment variance component

  if(env.n > 1){

    gxl <- paste(gen.name, env.name, sep = ":")

    vc.gxl <- g.ran %>%
      lme4::VarCorr() %>%
      tibble::as_tibble() %>%
      dplyr::filter(grp == gxl) %>%
      dplyr::pull(vcov)

    if( length(strsplit(gxl,":")[[1]]) < 2) {

      message("You should include env.name in the arguments")

      vc.gxl <- 0

    } else if (identical(vc.gxl, numeric(0))) {

      message("You should include (1|genotype:environment) interaction")

      vc.gxl <- 0

    }

  } else if (!is.null(env.name) && env.n == 1) {

    message("You should include env.n in the arguments")

    vc.gxl <- 0

    } else if (env.n == 1){

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

# environment x year variance component (review in MET)

  if(year.n > 1 && env.n > 1){

    gxlxy <- paste(gen.name, env.name, year.name, sep = ":")

    vc.gxlxy <- g.ran %>%
      lme4::VarCorr() %>%
      tibble::as_tibble() %>%
      dplyr::filter(grp == gxlxy) %>%
      dplyr::pull(vcov)

    if( length(strsplit(gxlxy,":")[[1]]) < 3 ) {

      message("You should include environment/years arguments")

      vc.gxlxy <- 0

    } else if (identical(vc.gxlxy, numeric(0))) {

      message("You should include (1|genotype:environment:year) interaction")

      vc.gxlxy <- 0

    }

  } else if (year.n == 1 && env.n == 1){

    vc.gxlxy <- 0

  } else {vc.gxlxy <- 0}

# error variance component

  vc.e <- g.ran %>%
    lme4::VarCorr() %>%
    tibble::as_tibble() %>%
    dplyr::filter(grp == "Residual") %>%
    dplyr::pull(vcov)
  
  
# Best Linear Unbiased Estimators (BLUE) :: fixed model -------------------
  
    if(emmeans == TRUE){
      
      BLUE <- g.fix %>%
        emmeans::emmeans(as.formula(paste("pairwise", gen.name, sep = " ~ ")))
      
      BLUEs <- BLUE %>%
        purrr::pluck("emmeans") %>%
        tibble::as_tibble() %>%
        dplyr::rename(!!trait := 'emmean') 
      
      
# mean variance of a difference between genotypes (BLUEs) -----------------
      
      vdBLUE.avg <- BLUE %>%
        purrr::pluck("contrasts") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(Var=SE^2) %>%
        dplyr::pull(Var) %>%
        mean(.)
      
    } else if (emmeans == FALSE) {
      
      BLUEs <- g.fix %>%
        lme4::fixef() %>%
        data.frame() %>% 
        rownames_to_column({{gen.name}}) %>% 
        dplyr::rename(!!trait := .) %>% 
        mutate(across({{trait}}, as.numeric)) %>% 
        mutate(smith.w = diag(solve(vcov(g.fix)))) %>% 
        dplyr::filter(grepl({{gen.name}}, .data[[gen.name]])) %>% 
        mutate(across({{gen.name}}, ~stringr::str_replace(., gen.name, "")))
      
      vdBLUE.avg <- g.fix %>%
        vcov(.) %>%
        base::as.matrix(.) %>%
        diag(.) %>%
        enframe() %>%
        dplyr::filter(grepl({{gen.name}}, .data$name)) %>% 
        select(!.data$name) %>% 
        deframe() %>% 
        mean()
      
    }
  
# Best Linear Unbiased Predictors (BLUP) :: random model -------------------

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
          mean = mean(.data[[trait]], na.rm = T)
          , std = sqrt(var(.data[[trait]], na.rm = T))
          , min = min(.data[[trait]])
          , max = max(.data[[trait]])
        ) 
    
# -------------------------------------------------------------------------
    
    BLUEs <- BLUEs %>% 
      {if (!is.null(trial)) dplyr::mutate(.data = ., trial = trial) else .} %>% 
      {if (!is.null(trial)) select(.data = ., trial, everything()) else .}

# -------------------------------------------------------------------------

## Summary table VC & Heritability

  vrcp <- dplyr::tibble(
    trait = trait
    , rep = rep.n
    , geno = gen.n
    , env = env.n
    , year = year.n
    ) %>% 
    merge(., smd) %>% 
    mutate(V.g = vc.g) %>% 
    mutate(V.gxl = vc.gxl) %>% 
    mutate(V.gxy = vc.gxy) %>% 
    mutate(V.gxlxy = vc.gxlxy) %>% 
    mutate(V.e = vc.e) %>% 
    mutate(V.p = (V.g + V.gxl/env + V.gxy/year + V.gxlxy/(env*year) + V.e/(env*year*rep))) %>%
    mutate(repeatability = V.g/(V.g + V.e/rep)) %>% 
    mutate(H2.s = V.g/V.p) %>% 
    mutate(vdBLUEs = vdBLUE.avg) %>% 
    mutate(H2.p = V.g/(V.g + vdBLUEs/2)) %>%
    mutate(vdBLUPs = vdBLUP.avg) %>% 
    mutate(H2.c = 1 - (vdBLUPs/2/V.g)) %>% 
    select(!matches("BLUE|BLUP")) %>%
    purrr::discard(~all(is.nan(.))) %>%
    {if (env.n == 1) dplyr::select(.data = ., !V.gxl) else .} %>% 
    {if (year.n == 1) dplyr::select(.data = ., !V.gxy) else .} %>%
    {if (env.n && year.n == 1) dplyr::select(.data = ., !V.gxlxy) else .} %>%
    {if (!is.null(trial)) dplyr::mutate(.data = ., trial = trial) else .} %>% 
    {if (!is.null(trial)) dplyr::select(.data = ., trial, everything()) else .}
    

# result ------------------------------------------------------------------

  rsl <- list(
    tabsmr = vrcp
    , blups = BLUPs
    , blues = BLUEs
    , model = g.ran
    , outliers = outliers
    , diagplot = diag.plot
    )
}

