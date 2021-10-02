#' Mean comparison test 
#'
#' Function to compare treatment from lm or aov using data frames
#'
#' @param data Fieldbook data.
#' @param response Model used for the experimental design.
#' @param model_factors Factor in the model.
#' @param comparison Significance level for the analysis (default = 0.05).
#' @param test_comp Comparison test (default = "SNK"). Others: "TUKEY", "DUNCAN".
#' @param sig_level Significance level for the analysis (default = 0.05).
#'
#' @return list
#'
#' @import dplyr
#' @importFrom agricolae SNK.test HSD.test duncan.test
#' @importFrom tidyr separate
#' @importFrom stats aov
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
#' mc <- mean_comparison(data = fb
#'                       , response = "hi"
#'                       , model_factors = "geno*treat"
#'                       , comparison = c("geno", "treat")
#'                       , test_comp = "TUKEY"
#'                       )
#' mc$comparison
#' mc$stat
#' 
#' }
#' 

mean_comparison <- function(data
                            , response
                            , model_factors
                            , comparison
                            , test_comp = "SNK"
                            , sig_level = 0.05
                            ) {
  
if(FALSE) {
  
data <- fb
response  <-  "hi"
model_factors <-  "geno*treat"
comparison <- c("geno", "treat")
test_comp <- "TUKEY"
  
  
}

# arguments ---------------------------------------------------------------
# -------------------------------------------------------------------------

  test_comp <- match.arg(test_comp, c("snk" , "SNK"
                                      , "duncan", "DUNCAN"
                                      , "hsd", "tukey", "TUKEY"))
  
  model_formula <- as.formula(paste({{response}}
                                    , model_factors
                                    , sep = "~"))
  
  model_aov <- aov(model_formula, data)
  


# test comparison ---------------------------------------------------------
# -------------------------------------------------------------------------
  
  if (test_comp == "SNK"){

      mc <- agricolae::SNK.test(
        y = model_aov
        , trt = comparison
        , alpha = sig_level
      )

    } 
  
  if (test_comp == "TUKEY") {
    
      mc <- agricolae::HSD.test(
        y = model_aov
        , trt = comparison
        , alpha = sig_level
      )
      }

  if (test_comp == "DUNCAN") {

      mc <- agricolae::duncan.test(
        y = model_aov
        , trt = comparison
        , alpha = sig_level
      )
    }

  tb_mc <- merge(
    mc %>% purrr::pluck("means") %>% rownames_to_column("treatments")
    ,  mc %>% purrr::pluck("groups") %>% rownames_to_column("treatments")
    , all = TRUE) %>%
    rename_with(tolower, !c(.data$treatments, {{response}})) %>%
    arrange(desc( {{response}} )) %>%
    mutate(ste = .data$std/sqrt(.data$r), .after = .data$r) %>%
    select(!c(.data$q25, .data$q50, .data$q75)) %>%
    rename("sig" = .data$groups)

    if ( length(comparison) <= 3 ) {

      tb_mc <- tb_mc %>%
        separate(.data$treatments, {{comparison}}, sep = ":", remove = F)

    }

  smr_stat <- mc %>%
    purrr::pluck("statistics") %>%
    dplyr::mutate(response =  {{response}}, .before = "MSerror") %>%
    merge(mc$parameters, .) %>%
    select(.data$response, everything()) %>% 
    select(!matches("StudentizedRange|MSD")) %>% 
    rename(ntreats = ntr
           , comparison = name.t ) %>% 
    mutate(comparison = gsub(":", "*", .data$comparison))
    

# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  mean_comparison = list(
    comparison = tb_mc
    , stats = smr_stat
    )

}
