source("http://lozanoisla.com/setup.r")
url <- "https://docs.google.com/spreadsheets/d/1ilw0NHT7mihaM-3U48KzkuMt927xe8ukX6rNuIw2fT0/edit#gid=0"
# browseURL(url)
gs <- as_sheets_id(url)

data <- gs %>%
  range_read("design")

fieldbook_design <- function(data
                             , nfactors = NULL
                             , type = c("crd", "rcbd", "lsd")
                             , rep = NULL
                             , series = 2
                             , seed = 0
                             ){

# arguments ---------------------------------------------------------------

  type <- match.arg(type)

  data_fb <- data %>%
    select(!starts_with("...")) %>%
    rename_with(~str_replace_all(.,"\\s+|\\.", "_")) %>%
    mutate(across(.cols = everything(), ~str_replace_all(.,"\\s+|\\.", "_")))

  treatments_names <- data_fb %>%
    select(!starts_with("[") | !ends_with("]")) %>%
    names()

  treatments_levels <- data_fb %>%
    select(treatments_names) %>%
    as.list() %>%
    map(discard, is.na)

  param_values <- data_fb %>%
    select(starts_with("[") | ends_with("]")) %>%
    rename_with(~str_replace_all(.,"\\[|\\]", "")) %>%
    as.list() %>%
    map(discard, is.na)

  if ( "nfactors" %in% names(param_values) ) {

    nfactors <- param_values %>% pluck("nfactors") %>% as.numeric()

  }

  if ( "type" %in%  names(param_values) ) {

    type <- param_values %>% pluck("type")

  }

  if ( "rep" %in%  names(param_values) ) {

    rep <- param_values %>% pluck("rep") %>% as.numeric()

  }

  if ( "series" %in%  names(param_values) ) {

    series <- param_values %>% pluck("series") %>% as.numeric()

  }

  if ( "seed" %in%  names(param_values) ) {

    seed <- param_values %>% pluck("seed") %>% as.numeric()

  }

# Factor = 1 --------------------------------------------------------------
# -------------------------------------------------------------------------

  if ( nfactors == 1 ) {

    treat_name <- names(treatments_levels) %>% pluck(1)
    treatments <- treatments_levels %>% pluck(1)

    if(type == "crd"){

      design  <- agricolae::design.crd(trt = treatments
                                   , r = rep
                                   , serie = series
                                   , seed = seed
                                   , randomization = TRUE
                                   )

     result <-  design %>%
       pluck("book") %>%
       dplyr::rename({{treat_name}} := treatments)

    }

    if(type == "rcbd"){

      design  <- agricolae::design.rcbd(trt = treatments
                                       , r = rep
                                       , serie = series
                                       , seed = seed
                                       , randomization = TRUE
                                       )
      result <- list(

        design = design %>%
          pluck("book") %>%
          dplyr::rename({{treat_name}} := treatments)
        , sketch = design %>% pluck("sketch")

        )

    }

  }

# Factor > 1 --------------------------------------------------------------
# -------------------------------------------------------------------------

  # if ( nfactors > 1 ) {
  #
  #   treat_name <- names(treatments_levels) %>% pluck(1)
  #   treatments <- treatments_levels %>% pluck(1)
  #
  #   if(type == "crd"){
  #
  #     design  <- agricolae::design.crd(trt = treatments
  #                                      , r = rep
  #                                      , serie = series
  #                                      , seed = seed
  #                                      , randomization = TRUE
  #     )
  #
  #     result <-  design %>% pluck("book")
  #
  #   }
  #
  #
  # }
  #

# result ------------------------------------------------------------------
# -------------------------------------------------------------------------

  return(result)

}

data <- gs %>%
  range_read("design")

data %>% fieldbook_design()
