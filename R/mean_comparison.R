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
#' @param digits Number of digits in the table.
#'
#' @details
#'
#' For compare the factors you should use "*". For example, to compare
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
#' library(gsheet)
#' 
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/"
#'               , "edit#gid=172957346")
#' # browseURL(url)
#' 
#' fb <- gsheet2tbl(url)
#' 
#' model <- fb %>% 
#'   fieldbook_summary(last_factor = "bloque"
#'                     , model_facts = "tratamiento*genotipo"
#'                     , comp_facts = "tratamiento*genotipo"
#'                     )
#' 
#' mc <- fb %>%
#'   mean_comparison(fb_smr = model
#'                   , variable = "hi"
#'                   , graph_opts = T)
#' mc$comparison
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
                            , digits = 2
                            ) {

  where <- NULL
  
  if(FALSE){
    
    data <- fb
    
    fb_smr <- model
    
  }

  # fieldbook structure -----------------------------------------------------
  # -------------------------------------------------------------------------

  factor_list <- fb_smr %>%
    filter(.data$type %in% "factor") %>%
    select(where(~!all(is.na(.))))

  factor_opt <- factor_list %>%
    select(.data$variables, .data$levels) %>%
    deframe()

  vars_num <- fb_smr %>%
    filter(.data$type %in% "numeric") %>% 
    filter(levels > 0)

  vars_cat <- fb_smr %>%
    filter(.data$type %in% "character") %>% 
    filter(levels > 0)
  
  labels <- fb_smr %>% 
    select(.data$variables, .data$plot_label) %>% 
    deframe()

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
      purrr::pluck(1)

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
      purrr::pluck(1)

  }

  # -------------------------------------------------------------------------

  comp_facts_opt <- c("comp_facts", "treat_comp", "comparison", "compare", "comparar")
  comp_facts_match <- names(arguments) %in% comp_facts_opt
  comp_facts_name <- names(arguments)[comp_facts_match == TRUE]

  if ( length(comp_facts_name)  > 0  )  {

    comp_facts <- arguments %>%
      select({{comp_facts_name}}) %>%
      purrr::pluck(1)

  }

  # -------------------------------------------------------------------------

  sig_level_opt <- c("sig_level", "signicance", "alpha")
  sig_level_match <- names(arguments) %in% sig_level_opt
  sig_level_name <- names(arguments)[sig_level_match == TRUE]

  if ( length(sig_level_name)  > 0  )  {

    sig_level <- arguments %>%
      select({{sig_level_name}}) %>%
      purrr::pluck(1)
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

  comp_facts <- strsplit(comp_facts
                         , split = "[-+*/)( ]|[^x][0-9]+|^[0-9]+") %>%
    purrr::pluck(1) %>%
    gsub(" ", "", ., fixed = TRUE)

  mean_comparison <- function(model_aov
                              , variable
                              , comp_facts
                              , sig_level
                              ){

    if (test_comp == "SNK"){

      mc <- agricolae::SNK.test(
        y = model_aov
        , trt = comp_facts
        , alpha = sig_level
      )

    } else if (test_comp == "HSD") {

      mc <- agricolae::HSD.test(
        y = model_aov
        , trt = comp_facts
        , alpha = sig_level
      )

    } else if (test_comp == "DUNCAN") {

      mc <- agricolae::duncan.test(
        y = model_aov
        , trt = comp_facts
        , alpha = sig_level
      )
    }

    tb_mc <- merge(
      mc %>% purrr::pluck("means") %>% rownames_to_column("treatments")
      ,  mc %>% purrr::pluck("groups") %>% rownames_to_column("treatments")
      , all = TRUE) %>%
      rename_with(tolower, !c(.data$treatments, {{variable}})) %>%
      arrange(desc( {{variable}} )) %>%
      mutate(ste = .data$std/sqrt(.data$r), .after = .data$r) %>%
      select(!c(.data$q25, .data$q50, .data$q75)) %>%
      rename("sig" = .data$groups)

    if ( length(comp_facts) <= 3 ) {

      tb_mc <- tb_mc %>%
        separate(.data$treatments, {{comp_facts}}, sep = ":", remove = F)

    }

    smr_stat <- mc %>%
      purrr::pluck("statistics") %>%
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
    group <- "treatments"
    colors <-   gray.colors(n = comparison$stats$ntr
                            , start = 0.8
                            , end = 0.3) %>%
      tibble('{colors}' = .)

  }

  if ( length(comp_facts) == 2 ){

    x <- comp_facts[1]
    group <- comp_facts[2]
    colors <- gray.colors(n = factor_opt[comp_facts[2]]
                          , start = 0.8
                          , end = 0.3) %>%
      tibble('{colors}' = .)
  }

  if ( length(comp_facts) == 1 ){

    x <- comp_facts[1]
    group <- comp_facts[1]
    colors <- gray.colors(n = factor_opt[comp_facts[1]]
                          , start = 0.8
                          , end = 0.3) %>%
      tibble('{colors}' = .)

  }

  min_value <- min(comparison$table$min)
  max_value <- max(comparison$table$max)

  if ( min_value >= 0 & max_value > 0 ) {

    limits <- paste(0, round(max_value*1.2, 1),  sep = "*")
    brakes <- abs(round(max_value*1.2, 1))/5

  } else if ( min_value < 0 &  max_value > 0 ) {

    limits <- paste(round(min_value*1.2, 1)
                    , round(max_value*1.2, 1),  sep = "*")
    brakes <- abs(round(max_value*1.2, 1))/5

  } else if ( min_value < 0 &  max_value <= 0 ) {

    limits <- paste( round(min_value*1.2, 1), 0,  sep = "*")
    brakes <- abs(round(min_value*1.2, 1))/5

  }
  
  ylim <- paste(limits, brakes, sep = "*")

  if ( graph_opts == TRUE ) {
    
    xlab <- labels[ x ] %>% as.vector()
    ylab <- labels[ {{variable}} ] %>% as.vector()
    glab <- labels[ group ] %>% as.vector()
    
    graph_opts <- c(type = "bar"
                   , x = x
                   , y = {{variable}}
                   , group = group
                   , xlab = xlab
                   , ylab = ylab
                   , glab = glab
                   , ylimits = ylim
                   , xrotation = "0*0.5*0.5"
                   , sig = "sig"
                   , error = "ste"
                   , legend = "top"
                   , xtext = NA
                   , gtext = NA
                   , dimensions = NA
                   , opt = NA
                   )

    opts_table <- enframe(graph_opts) %>%
      rename('{arguments}' = .data$name, '{values}' = .data$value) %>%
      merge(colors, ., by = 0, all = TRUE) %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names)

    comparison[["table"]]  <- merge( comparison[["table"]]
                                     , opts_table
                                     , by = 0
                                     , all = TRUE
                                     )  %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names) %>% 
      mutate(across(where(is.numeric), ~round(., digits))) #!

  }

  # results -----------------------------------------------------------------
  # -------------------------------------------------------------------------

  mean_comparison = list(
    comparison = comparison[["table"]]
    , stats = comparison[["stats"]]
    )

}
