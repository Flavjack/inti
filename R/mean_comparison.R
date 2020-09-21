#' Mean comparison using data frames 
#'
#' Function to compare treatment from lm or aov using data frames
#'
#' @param data Fieldbook data.
#' @param fb_smr Summary of the variables in the fieldbook.
#' @param variable Model used for the experimental design.
#' @param model_facts Comparison test (default = "SNK"). Others: "TUKEY",
#'   "DUNCAN".
#' @param comp_facts Significance level for the analysis (default = 0.05).
#' @param test_comp Significance level for the analysis (default = 0.05).
#' @param sig_level Significance level for the analysis (default = 0.05).
#' @param graph_opts Include option in the table for graphs (default = FALSE).
#'
#' @details
#'
#' For compare the factors you should use ":". For example, to compare
#' treatment1 and treatment2: \code{treatment1:treatment2}.
#'
#' @return data frame
#'
#' @import dplyr
#' @importFrom tibble enframe deframe
#' @importFrom agricolae SNK.test HSD.test duncan.test
#' @importFrom grDevices gray.colors
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' library(googlesheets4)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=1414357945")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (data <- gs %>%
#'     range_read("fb"))
#'
#' (fb_smr <- gs %>%
#'   range_read("fbsm"))
#'
#' mc <- mean_comparison(data
#'                      , fb_smr = fb_smr
#'                      , variable = "BIOMDW"
#'                      , graph_opts = TRUE
#'                      )
#'
#' table <- mc$comparison
#'
#' table %>% sheet_write(ss = gs, sheet = "plot")
#'
#' }
#' 

mean_comparison <- function(data
                            , fb_smr
                            , variable
                            , model_facts
                            , comp_facts
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

  comp_facts_opt <- c("comp_facts", "treat_comp", "comparison", "compare", "comparar")
  comp_facts_match <- names(arguments) %in% comp_facts_opt
  comp_facts_name <- names(arguments)[comp_facts_match == TRUE]

  if ( length(comp_facts_name)  > 0  )  {

    comp_facts <- arguments %>%
      select({{comp_facts_name}}) %>%
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

  comp_facts <- strsplit(comp_facts, ":") %>%
    pluck(1) %>%
    gsub(" ", "", ., fixed = TRUE)

  mean_comparison <- function(model_aov
                              , variable
                              , comp_facts
                              , sig_level
                              ){

    if (test_comp == "SNK"){

      mc <- SNK.test(
        y = model_aov
        , trt = comp_facts
        , alpha = sig_level
      )

    } else if (test_comp == "HSD") {

      mc <- HSD.test(
        y = model_aov
        , trt = comp_facts
        , alpha = sig_level
      )

    } else if (test_comp == "DUNCAN") {

      mc <- duncan.test(
        y = model_aov
        , trt = comp_facts
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

    if ( length(comp_facts) <= 2 ) {

      tb_mc <- tb_mc %>%
        separate(.data$treatments, {{comp_facts}}, sep = ":")

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
                                , comp_facts
                                , sig_level
                                )

  # graph table -------------------------------------------------------------
  # -------------------------------------------------------------------------

  if ( length(comp_facts) >= 3 ){

    x <- "treatments"
    groups <- "treatments"
    colors <-   gray.colors(n = comparison$stats$ntr
                            , start = 0.8
                            , end = 0.3) %>%
      tibble('{colors}' = .)

  }

  if ( length(comp_facts) == 2 ){

    x <- comp_facts[1]
    groups <- comp_facts[2]
    colors <- gray.colors(n = factor_opt[comp_facts[2]]
                          , start = 0.8
                          , end = 0.3) %>%
      tibble('{colors}' = .)
  }

  if ( length(comp_facts) == 1 ){

    x <- comp_facts[1]
    groups <- comp_facts[1]
    colors <- gray.colors(n = factor_opt[comp_facts[1]]
                          , start = 0.8
                          , end = 0.3) %>%
      tibble('{colors}' = .)

  }

  min_value <- min(comparison$table$min)
  max_value <- max(comparison$table$max)

  if ( min_value >= 0 & max_value > 0 ) {

    limits <- paste(0, round(max_value*1.2, 1),  sep = "x")
    brakes <- abs(round(max_value*1.2, 1))/5

  } else if ( min_value < 0 &  max_value > 0 ) {

    limits <- paste(round(min_value*1.2, 1)
                    , round(max_value*1.2, 1),  sep = "x")
    brakes <- abs(round(max_value*1.2, 1))/5

  } else if ( min_value < 0 &  max_value <= 0 ) {

    limits <- paste( round(min_value*1.2, 1), 0,  sep = "x")
    brakes <- abs(round(min_value*1.2, 1))/5

  }

  if ( graph_opts == TRUE ) {

    graph_opts <- c(type = "bar"
                   , x = x
                   , y = {{variable}}
                   , groups = groups
                   , xlab = x
                   , ylab = {{variable}}
                   , glab = groups
                   , limits = limits
                   , brakes = brakes
                   , sig = "sig"
                   , error = "ste"
                   , legend = "top"
                   )

    opts_table <- enframe(graph_opts) %>%
      rename('{arguments}' = .data$name, '{values}' = .data$value) %>%
      merge(colors, ., by = 0, all = TRUE) %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(-.data$Row.names)

    comparison[["table"]]  <- merge( comparison[["table"]]
                                     , opts_table
                                     , by = 0
                                     , all = TRUE
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
