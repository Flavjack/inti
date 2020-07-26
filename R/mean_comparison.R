#' Mean comparison test
#'
#' Function to compare treatment from lm or aov
#'
#' @param data Field book data.
#' @param fb_smr Summary of the variables in the fieldbook.
#' @param variable Model used for the experimental design.
#' @param model_facts Comparison test (default = "SNK"). Others: "TUKEY" & "DUNCAN".
#' @param treat_comp Significance level for the analysis (default = 0.05).
#' @param test_comp Significance level for the analysis (default = 0.05).
#' @param sig_level Significance level for the analysis (default = 0.05).
#' @param graph_opts Include option in the table for graphs (default = FALSE)
#'
#' @details
#'
#' For compare the factors you should use ":". For example, to compare treatment1
#' and treatment2 => "treatment1:treatment2".
#'
#' @return data frame
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @import dplyr
#' @importFrom tibble enframe deframe
#' @importFrom agricolae SNK.test HSD.test duncan.test
#' @importFrom grDevices gray.colors
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' library(googlesheets4)
#' library(tidyverse)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "15uwCgQRtR01B3FJaZBE8t_0bOC_8Bbey9ccwVlZH0jg/edit#gid=56711214")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (data <- gs %>%
#'     range_read("fb"))
#'
#' (fb_smr <- gs %>%
#'   range_read("fbsmr"))
#'
#' mc <- mean_comparison(data
#'                      , fb_smr = fb_smr
#'                      , variable = "aftotal"
#'                      , graph_opts = T
#'                      )
#'
#' table <- mc$comparison
#'
#' table %>% write_sheet(ss = gs, sheet = "tabgraph")
#'
#' }
#'
#' @export

mean_comparison <- function(data
                            , fb_smr
                            , variable
                            , model_facts
                            , treat_comp
                            , test_comp = "SNK"
                            , sig_level = 0.05
                            , graph_opts = FALSE
                            ) {

  where <- NULL

  # fieldbook structure -----------------------------------------------------
  # -------------------------------------------------------------------------

  factor_list <- fb_smr %>%
    filter(.data$type %in% "factor") %>%
    select(where(~!all(is.na(.))))

  factor_opt <- factor_list %>%
    select(!.data$type) %>%
    deframe()

  vars_num <- fb_smr %>%
    filter(.data$type %in% "numeric")

  vars_cat <- fb_smr %>%
    filter(.data$type %in% "character")

  fb <- data %>%
    select(where(~!all(is.na(.)))) %>%
    mutate(across( factor_list[["variables"]], as.character)) %>%
    mutate(across( factor_list[["variables"]], as.factor)) %>%
    mutate(across( vars_num[["variables"]], as.numeric)) %>%
    mutate(across( vars_cat[["variables"]], as.character))

  # fieldbook arguments -----------------------------------------------------
  # -------------------------------------------------------------------------

  arguments <- fb_smr %>%
    select(where(~!all(is.na(.)))) %>%
    filter(.data$variables %in% {{variable}} )

  # -------------------------------------------------------------------------

  test_comp_opt <- c("test_comp", "comparison", "mean_comparison", "compare")
  test_comp_match <- names(arguments) %in% test_comp_opt
  test_comp_name <- names(arguments)[test_comp_match == TRUE]

  if ( length(test_comp_name)  > 0  )  {

    test_comp <- arguments %>%
      select({{test_comp_name}}) %>%
      pluck(1)

  }

  test_comp <- match.arg(test_comp, c("snk" , "SNK"
                                      , "duncan", "DUNCAN"
                                      , "hsd", "tukey", "TUKEY"))

  if( test_comp %in% c("tukey", "snk", "tukey", "TUKEY", "duncan") ) {

    test_comp <- switch(test_comp,
                        snk = "SNK"
                        , tukey = "HSD"
                        , TUKEY = "HSD"
                        , duncan = "DUNCAN")
    }

  # -------------------------------------------------------------------------

  model_facts_opt <- c("model_facts", "model")
  model_facts_match <- names(arguments) %in% model_facts_opt
  model_facts_name <- names(arguments)[model_facts_match == TRUE]

  if ( length(model_facts_name)  > 0  )  {

    model_facts <- arguments %>%
      select({{model_facts_name}}) %>%
      pluck(1)

  }

  # -------------------------------------------------------------------------

  treat_comp_opt <- c("treat_comp", "comparison", "compare", "comparar")
  treat_comp_match <- names(arguments) %in% treat_comp_opt
  treat_comp_name <- names(arguments)[treat_comp_match == TRUE]

  if ( length(treat_comp_name)  > 0  )  {

    treat_comp <- arguments %>%
      select({{treat_comp_name}}) %>%
      pluck(1)

  }

  # -------------------------------------------------------------------------

  sig_level_opt <- c("sig_level", "signicance", "alpha")
  sig_level_match <- names(arguments) %in% sig_level_opt
  sig_level_name <- names(arguments)[sig_level_match == TRUE]

  if ( length(sig_level_name)  > 0  )  {

    sig_level <- arguments %>%
      select({{sig_level_name}}) %>%
      pluck(1)
  }

  # anova -------------------------------------------------------------------
  # -------------------------------------------------------------------------

  model_aov <- function(fb
                        , variable
                        , model_facts
                        ){

    model_formula <- as.formula(paste( {{variable}}, model_facts, sep = "~"))

    model_aov <- aov(model_formula, fb)

    }

  # test comparison ---------------------------------------------------------
  # -------------------------------------------------------------------------

  treat_comp <- strsplit(treat_comp, ":") %>%
    pluck(1) %>%
    gsub(" ", "", ., fixed = TRUE)

  mean_comparison <- function(model_aov
                              , variable
                              , treat_comp
                              , sig_level
                              ){

    if (test_comp == "SNK"){

      mc <- SNK.test(
        y = model_aov
        , trt = treat_comp
        , alpha = sig_level
      )

    } else if (test_comp == "HSD") {

      mc <- HSD.test(
        y = model_aov
        , trt = treat_comp
        , alpha = sig_level
      )

    } else if (test_comp == "DUNCAN") {

      mc <- duncan.test(
        y = model_aov
        , trt = treat_comp
        , alpha = sig_level
      )
    }

    tb_mc <- merge(
      mc %>% pluck("means") %>% rownames_to_column("treatments")
      ,  mc %>% pluck("groups") %>% rownames_to_column("treatments")
      , all = TRUE) %>%
      rename_with(tolower, !c(.data$treatments, {{variable}})) %>%
      arrange(desc( {{variable}} )) %>%
      mutate(ste = .data$std/sqrt(.data$r), .after = .data$r) %>%
      select(!c(.data$q25, .data$q50, .data$q75)) %>%
      rename("sig" = .data$groups)

    if ( length(treat_comp) <= 2 ) {

      tb_mc <- tb_mc %>%
        separate(.data$treatments, {{treat_comp}}, sep = ":")

    }

    smr_stat <- mc %>%
      pluck("statistics") %>%
      dplyr::mutate(variable =  {{variable}}, .before = "MSerror") %>%
      merge(mc$parameters, .) %>%
      select({{variable}}, everything())

    mean_comparison <- list(
      table = tb_mc
      , stats = smr_stat
    )

  }

  # apply functions ---------------------------------------------------------
  # -------------------------------------------------------------------------

  model_aov <- model_aov(fb, variable, model_facts)

  comparison <- mean_comparison(model_aov
                                , variable
                                , treat_comp
                                , sig_level
                                )

  # graph table -------------------------------------------------------------
  # -------------------------------------------------------------------------

  if ( length(treat_comp) >= 3 ){

    x <- "treatments"
    groups <- "treatments"
    colors <-   gray.colors(n = factor_opt[treat_comp[2]]
                            , start = 0.3
                            , end = 0.9)
      tibble('{colors}' = .)

  }

  if ( length(treat_comp) == 2 ){

    x <- treat_comp[1]
    groups <- treat_comp[2]
    colors <- gray.colors(n = factor_opt[treat_comp[2]]
                          , start = 0.3
                          , end = 0.9) %>%
      tibble('{colors}' = .)
  }

  if ( length(treat_comp) == 1 ){

    x <- treat_comp[1]
    groups <- treat_comp[1]
    colors <- gray.colors(n = factor_opt[treat_comp[1]]
                          , start = 0.3
                          , end = 0.9) %>%
      tibble('{colors}' = .)

  }

  if ( min(comparison$table$min) > 0 &  max(comparison$table$max) > 0 ) {

    limits <- paste(0, round(max(comparison$table$max)*1.25),  sep = ":")
    brakes <- round(abs(max(comparison$table$max)*1.25/5))

  } else if ( min(comparison$table$min) < 0 &  max(comparison$table$max) > 0 ) {

    limits <- paste(round(min(comparison$table$min)*1.25)
                    , round(max(comparison$table$max)*1.25),  sep = ":")
    brakes <- round(abs(max(comparison$table$max)*1.25/5))

  } else if ( min(comparison$table$min) < 0 &  max(comparison$table$max) < 0 ) {

    limits <- paste(round(min(comparison$table$min))*1.25
                    , 0,  sep = ":")
    brakes <- round(abs(min(comparison$table$min)*1.25/5))

  }


  if ( graph_opts == TRUE ) {

    arguments <- c(type = "bar"
                   , x = x
                   , y = {{variable}}
                   , groups = groups
                   , xlab = x
                   , ylab = {{variable}}
                   , glab = groups
                   , legend = "top"
                   , limits = limits
                   , brake = brakes
                   , sig = "sig"
                   , error = TRUE
                   , theme = "minimal"
    )

    opts_table <- enframe(arguments) %>%
      rename('{arguments}' = .data$name, '{values}' = .data$value) %>%
      merge(colors, ., by = 0, all = TRUE) %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(-.data$Row.names)

    comparison[["table"]]  <- merge( comparison[["table"]]
                                     , opts_table
                                     , by = 0
                                     , all.y = T
    )  %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(-.data$Row.names)

  }

  # results -----------------------------------------------------------------
  # -------------------------------------------------------------------------

  mean_comparison = list(
    comparison = comparison[["table"]]
    , stats = comparison[["stats"]]
    )

}
