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
#' @param plot_diag Show diagnostic plots using ggplot2 and cowplot
#'   (default = FALSE).
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
#' For individual experiments is necessary provide the `{trait}`,
#' `{gen.name}`,  `{rep.n}`.
#'
#' For MET experiments you should `{env.n}` and `{env.name}` and/or
#' `{year.n}` and `{year.name}` according your experiment.
#'
#' The BLUEs calculation based in the pairwise comparison could be time
#' consuming with the increase of the number of the genotypes. You can specify
#' `{emmeans = FALSE}` and the calculate of the BLUEs will be faster.
#'
#' If `{emmeans = FALSE}` you should change 1 by 0 in the fixed model for
#' exclude the intersect in the analysis and get all the genotypes BLUEs.
#'
#' Diagnostic plots are produced with ggplot2 and assembled with cowplot.
#' If `{outliers.rm = FALSE}`, fixed and random model diagnostics are
#' displayed in one combined panel. If `{outliers.rm = TRUE}`, fixed and
#' random diagnostics are displayed in separate panels comparing before
#' and after cleaning.
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
#' @importFrom emmeans emmeans
#' @importFrom ggplot2 ggplot aes geom_histogram stat_qq stat_qq_line geom_point geom_hline labs theme_minimal
#' @importFrom cowplot plot_grid ggdraw draw_label
#'
#' @export
#'
#' @examples
#'
#' library(inti)
#'  
#' md <- met %>%
#'   H2cal(trait = "yield",
#'   gen.name = "cultivar",
#'   rep.n = 2,
#'   env.name = "env",
#'   env.n = 18,
#'   fixed.model = ~ 0 + env + (1 | env:rep:alpha) + cultivar,
#'   random.model = ~ 1 + env +
#'     (1 | env:rep) + (1 | env:rep:alpha) +
#'     (1 | cultivar:env) + (1 | cultivar),
#'   summary = TRUE,
#'   plot_diag = TRUE,
#'   outliers.rm = TRUE,
#'   emmeans = FALSE
#' )
#' 
#'  md$tabsmr
#'  md$blues
#'  md$blups 
#'  md$outliers
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
    
    data = potato
    trait = "stemdw"
    gen.name = "geno"
    rep.n = 5
    fixed.model = ~ 0 + (1|bloque) + geno
    random.model = ~ 1 + (1|bloque) + (1|geno)
    emmeans = TRUE
    plot_diag = TRUE
    outliers.rm = TRUE
    
    weights = NULL
    summary = TRUE
    env.name = NULL
    year.name = NULL
    
  }
  
# -------------------------------------------------------------------------

  grp <- emmean <- SE <- Var <- where <- NULL 
  V.g <- V.gxl <- env <- V.gxy <- year <- NULL
  V.gxlxy <- V.e <- V.p <- vdBLUEs <- vdBLUPs <- NULL


# model -------------------------------------------------------------------

  random.model <- as.formula(paste(trait, deparse1(random.model)))
  fixed.model <- as.formula(paste(trait, deparse1(fixed.model)))

# outliers remove ---------------------------------------------------------

  rm.title <- paste("Random model:", trait)
  fm.title <- paste("Fixed model:", trait)

  # Data used for the final models.
  # Outlier detection/cleaning is performed only when requested.
  if (isTRUE(outliers.rm)) {

    out.rm <- data %>%
      remove_outliers(
        data = .
        , formula = random.model
        , drop_na = FALSE
        , plot_diag = FALSE
        , title = rm.title
      )

    out.fm <- data %>%
      remove_outliers(
        data = .
        , formula = fixed.model
        , drop_na = FALSE
        , plot_diag = FALSE
        , title = fm.title
      )

    dt.rm <- out.rm$data$clean
    dt.fm <- out.fm$data$clean

    outliers <- list(
      fixed = out.fm$outliers
      , random = out.rm$outliers
    )

  } else {

    dt.rm <- data
    dt.fm <- data
    outliers <- NULL

  }

# fit models --------------------------------------------------------------

  # Final models: use either the complete data or the cleaned data.
  g.fix <- lme4::lmer(
    formula = fixed.model
    , weights = weights
    , data = dt.fm
  )

  g.ran <- lme4::lmer(
    formula = random.model
    , weights = weights
    , data = dt.rm
  )

# Print model summary -----------------------------------------------------

  # summary(g.ran) is relatively expensive; calculate it once only when
  # requested or when the number of genotypes is needed below.
  g.ran.sum <- summary(g.ran)

  if (isTRUE(summary)) {
    print(g.ran.sum)
  }

# Diagnostics -------------------------------------------------------------

  # Diagnostic plots are built with ggplot2 and assembled with cowplot.
  # If outliers.rm = FALSE, one combined panel is produced containing
  # the fixed and random model diagnostics.
  # If outliers.rm = TRUE, two separate panels are produced: fixed and random.
  # Each panel compares the model BEFORE and AFTER cleaning.

  diag.plot <- NULL

  if (isTRUE(plot_diag)) {

    make_diag <- function(model, title) {

      res <- stats::resid(model)
      fit <- stats::fitted(model)
      res.pearson <- stats::resid(model, type = "pearson")

      plot_data <- data.frame(
        fitted = fit,
        residual = res,
        pearson = res.pearson,
        index = seq_along(res)
      )

      p1 <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$residual)) +
        ggplot2::geom_histogram(bins = 30) +
        ggplot2::labs(
          title = "Residuals",
          x = "Residuals",
          y = "Count"
        ) +
        ggplot2::theme_minimal()

      p2 <- ggplot2::ggplot(plot_data, ggplot2::aes(sample = .data$residual)) +
        ggplot2::stat_qq() +
        ggplot2::stat_qq_line() +
        ggplot2::labs(
          title = "Normal Q-Q",
          x = "Theoretical quantiles",
          y = "Sample quantiles"
        ) +
        ggplot2::theme_minimal()

      p3 <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data$fitted, y = .data$pearson)
      ) +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::labs(
          title = "Pearson residuals",
          x = "Fitted values",
          y = "Pearson residuals"
        ) +
        ggplot2::theme_minimal()

      p4 <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data$index, y = .data$residual)
      ) +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::labs(
          title = "Residual sequence",
          x = "Observation",
          y = "Residuals"
        ) +
        ggplot2::theme_minimal()

      list(
        histogram = p1,
        qqplot = p2,
        pearson = p3,
        sequence = p4
      )
    }

    arrange_diag <- function(diag, labels = NULL) {
      cowplot::plot_grid(
        plotlist = diag,
        ncol = 4,
        labels = labels,
        align = "hv"
      )
    }

    if (isTRUE(outliers.rm)) {

      # Models fitted to the original data for BEFORE cleaning diagnostics.
      g.fix.before <- lme4::lmer(
        formula = fixed.model,
        weights = weights,
        data = data
      )

      g.ran.before <- lme4::lmer(
        formula = random.model,
        weights = weights,
        data = data
      )

      fixed_before <- make_diag(g.fix.before, fm.title)
      fixed_after  <- make_diag(g.fix,        fm.title)
      random_before <- make_diag(g.ran.before, rm.title)
      random_after  <- make_diag(g.ran,        rm.title)

      fixed_title <- cowplot::ggdraw() +
        cowplot::draw_label(
          fm.title,
          fontface = "bold",
          size = 14,
          hjust = 0.5,
          x = 0.5,
          y = 0.5
        )

      fixed_before_title <- cowplot::ggdraw() +
        cowplot::draw_label(
          "Before cleaning",
          fontface = "bold",
          size = 11,
          hjust = 0.5,
          x = 0.5,
          y = 0.5
        )

      fixed_after_title <- cowplot::ggdraw() +
        cowplot::draw_label(
          "After cleaning",
          fontface = "bold",
          size = 11,
          hjust = 0.5,
          x = 0.5,
          y = 0.5
        )

      random_title <- cowplot::ggdraw() +
        cowplot::draw_label(
          rm.title,
          fontface = "bold",
          size = 14,
          hjust = 0.5,
          x = 0.5,
          y = 0.5
        )

      random_before_title <- cowplot::ggdraw() +
        cowplot::draw_label(
          "Before cleaning",
          fontface = "bold",
          size = 11,
          hjust = 0.5,
          x = 0.5,
          y = 0.5
        )

      random_after_title <- cowplot::ggdraw() +
        cowplot::draw_label(
          "After cleaning",
          fontface = "bold",
          size = 11,
          hjust = 0.5,
          x = 0.5,
          y = 0.5
        )

      diag.fixed <- cowplot::plot_grid(
        fixed_title,
        fixed_before_title,
        arrange_diag(fixed_before),
        fixed_after_title,
        arrange_diag(fixed_after),
        ncol = 1,
        rel_heights = c(0.10, 0.08, 1, 0.08, 1)
      )

      diag.random <- cowplot::plot_grid(
        random_title,
        random_before_title,
        arrange_diag(random_before),
        random_after_title,
        arrange_diag(random_after),
        ncol = 1,
        rel_heights = c(0.10, 0.08, 1, 0.08, 1)
      )

      diag.plot <- list(
        fixed = diag.fixed,
        random = diag.random
      )

      print(diag.fixed)
      print(diag.random)

    } else {

      fixed_diag <- make_diag(g.fix, fm.title)
      random_diag <- make_diag(g.ran, rm.title)

      # One combined panel when no outliers are removed.
      fixed_title <- cowplot::ggdraw() +
        cowplot::draw_label(
          fm.title,
          fontface = "bold",
          size = 14,
          hjust = 0.5,
          x = 0.5,
          y = 0.5
        )

      random_title <- cowplot::ggdraw() +
        cowplot::draw_label(
          rm.title,
          fontface = "bold",
          size = 14,
          hjust = 0.5,
          x = 0.5,
          y = 0.5
        )

      fixed_panel <- cowplot::plot_grid(
        fixed_title,
        arrange_diag(fixed_diag),
        ncol = 1,
        rel_heights = c(0.10, 1)
      )

      random_panel <- cowplot::plot_grid(
        random_title,
        arrange_diag(random_diag),
        ncol = 1,
        rel_heights = c(0.10, 1)
      )

      diag.plot <- cowplot::plot_grid(
        fixed_panel,
        random_panel,
        ncol = 1,
        align = "v"
      )

      print(diag.plot)
    }
  }

# Model estimates --------------------------------------------------
  
# Model estimates --------------------------------------------------

# number of genotypes

  gen.n <- g.ran.sum$ngrps[[gen.name]]

# Variance components: calculate VarCorr() only once

  vc <- lme4::VarCorr(g.ran)
  vc.tbl <- tibble::as_tibble(vc)

  get_vc <- function(term) {
    value <- vc.tbl$vcov[vc.tbl$grp == term]
    if (length(value) == 0L) 0 else value[[1]]
  }

  vc.g <- get_vc(gen.name)
  vc.e <- get_vc("Residual")
  vc.gxl <- 0
  vc.gxy <- 0
  vc.gxlxy <- 0

# genotype x environment variance component

  if (env.n > 1) {

    if (is.null(env.name)) {
      message("You should include env.name in the arguments")
    } else {
      gxl <- paste(gen.name, env.name, sep = ":")
      vc.gxl <- get_vc(gxl)

      if (identical(vc.gxl, 0)) {
        message("You should include (1|genotype:environment) interaction")
      }
    }

  } else if (!is.null(env.name)) {

    message("You should include env.n in the arguments")

  }

# genotype x year variance component

  if (year.n > 1) {

    if (is.null(year.name)) {
      message("You should include year.name in the arguments")
    } else {
      gxy <- paste(gen.name, year.name, sep = ":")
      vc.gxy <- get_vc(gxy)

      if (identical(vc.gxy, 0)) {
        message("You should include (1|genotype:year) interaction")
      }
    }

  } else if (!is.null(year.name)) {

    message("You should include year.n in the arguments")

  }

# genotype x environment x year variance component

  if (year.n > 1 && env.n > 1) {

    if (is.null(env.name) || is.null(year.name)) {
      message("You should include environment/years arguments")
    } else {
      gxlxy <- paste(gen.name, env.name, year.name, sep = ":")
      vc.gxlxy <- get_vc(gxlxy)

      if (identical(vc.gxlxy, 0)) {
        message("You should include (1|genotype:environment:year) interaction")
      }
    }

  }
  
  
# Best Linear Unbiased Estimators (BLUE) :: fixed model -------------------
  
    if (isTRUE(emmeans)) {
      
      BLUE <- g.fix %>%
        emmeans::emmeans(
          as.formula(paste("pairwise", gen.name, sep = " ~ "))
        )
      
      BLUEs <- BLUE %>%
        purrr::pluck("emmeans") %>%
        tibble::as_tibble() %>%
        dplyr::rename(!!trait := "emmean") 
      
# mean variance of a difference between genotypes (BLUEs) -----------------
      
      vdBLUE.avg <- BLUE %>%
        purrr::pluck("contrasts") %>%
        tibble::as_tibble() %>%
        dplyr::mutate(Var = SE^2) %>%
        dplyr::pull(Var) %>%
        mean()
      
    } else {
      
      # Convert the sparse Matrix covariance object to a base matrix.
      # This preserves the original calculation and avoids diag() errors
      # such as "long vectors not supported yet: array.c:2290".
      vc.fix <- base::as.matrix(stats::vcov(g.fix))
      fixef.g <- lme4::fixef(g.fix)
      fix.names <- names(fixef.g)
      gen.pattern <- paste0("^", gen.name)
      gen.idx <- grepl(gen.pattern, fix.names)

      BLUEs <- data.frame(
        parameter = fix.names[gen.idx]
        , estimate = unname(fixef.g[gen.idx])
      ) %>%
        tibble::as_tibble() %>%
        dplyr::rename(!!gen.name := .data$parameter, !!trait := .data$estimate) %>%
        dplyr::mutate(
          smith.w = diag(solve(vc.fix))[gen.idx]
        ) %>%
        dplyr::mutate(
          !!gen.name := stringr::str_replace(.data[[gen.name]], gen.name, "")
        )

      vdBLUE.avg <- mean(diag(vc.fix)[gen.idx])
      
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
    , mean = smd$mean
    , std = smd$std
    , min = smd$min
    , max = smd$max
    , V.g = vc.g
    , V.gxl = vc.gxl
    , V.gxy = vc.gxy
    , V.gxlxy = vc.gxlxy
    , V.e = vc.e
    , V.p = vc.g + vc.gxl/env.n + vc.gxy/year.n +
            vc.gxlxy/(env.n * year.n) + vc.e/(env.n * year.n * rep.n)
    , repeatability = vc.g/(vc.g + vc.e/rep.n)
    , H2.s = vc.g/(vc.g + vc.gxl/env.n + vc.gxy/year.n +
                    vc.gxlxy/(env.n * year.n) + vc.e/(env.n * year.n * rep.n))
    , vdBLUEs = vdBLUE.avg
    , H2.p = vc.g/(vc.g + vdBLUE.avg/2)
    , vdBLUPs = vdBLUP.avg
    , H2.c = 1 - (vdBLUP.avg/2/vc.g)
  ) %>%
    dplyr::select(!c(vdBLUEs, vdBLUPs)) %>% 
    purrr::discard(~all(is.nan(.))) %>%
    {if (env.n == 1) dplyr::select(.data = ., -V.gxl) else .} %>%
    {if (year.n == 1) dplyr::select(.data = ., -V.gxy) else .} %>%
    {if (env.n == 1 && year.n == 1) dplyr::select(.data = ., -V.gxlxy) else .} %>%
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

