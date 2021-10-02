#' Graph options to export
#'
#' Function to export the graph options and model parameters
#'
#' @param data Result from yupana_analysis function.
#' @param type Plot type
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param glab Title for the legend
#' @param ylimits limits of the y axis
#' @param xrotation Rotation in x axis c(angle, h, v)
#' @param xtext Text labels in x axis
#' @param gtext Text labels in group
#' @param legend the position of legends ("none", "left", "right", "bottom",
#'   "top", or two-element numeric vector)
#' @param sig Column with the significance
#' @param error Show the error bar ("ste" or "std").
#' @param color colored figure (TRUE), otherwise black & white (FALSE)
#' @param opt Add news layer to the plot
#' @param dimension Dimension of graphs
#'
#' @return data frame
#'
#' @import dplyr
#' @importFrom tibble enframe deframe
#' @importFrom agricolae SNK.test HSD.test duncan.test
#' @importFrom grDevices gray.colors
#' @importFrom utils packageVersion
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
#'               , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=172957346")
#' # browseURL(url)
#' 
#' fb <- gsheet2tbl(url)
#' 
#' smr <- yupana_analysis(data = fb
#'                        , last_factor = "bloque"
#'                        , response = "spad_83"
#'                        , model_factors = "block + geno*treat"
#'                        , comparison = c("treat")
#'                        )
#'                        
#' gtab <- yupana_export_smr(smr)
#' 
#' }
#' 

yupana_export_smr <- function(data
                             , type = NA
                             , xlab = NA
                             , ylab = NA
                             , glab = NA
                             , ylimits = NA
                             , xrotation = c(0, 0.5, 0.5)
                             , xtext = NA
                             , gtext = NA
                             , legend = "top"
                             , sig = NA
                             , error = NA
                             , color = TRUE
                             , opt = NA
                             , dimension = c(20, 10, 100)
                            ) {
  
  where <- NULL
  
  if(FALSE) {
    
  data <- smr
  
type = NA
xlab = NA
ylab = NA
glab = NA
ylimits = NA
xrotation = "0*0.5*0.5"
xtext = NA
gtext = NA
legend = "top"
sig = NA
error = NA
color = TRUE
opt = NA
dimension = "20*10*100"

  }
  
  options(scipen = 99)
  
# arguments ---------------------------------------------------------------
# -------------------------------------------------------------------------

  response <- data$response
  model_factors <- data$model_factors
  comparison <- data$comparison
  model <- data$model
  test_comp = data$test_comp
  sig_level = data$sig_level
  
# graph table -------------------------------------------------------------
# -------------------------------------------------------------------------
  
  if ( length(comparison) == 1 ){

    x <- comparison[1]
    group <- comparison[1]
    
  } else if ( length(comparison) <= 3 ){
    
    x <- comparison[1]
    group <- comparison[2]
    
  }

# color pallete -----------------------------------------------------------

  nlevels <- data$meancomp %>%  
    select(.data[[group]]) %>% 
    n_distinct()
  
  if(isTRUE(color)) {
    
    color <- colorRampPalette(
      c("#86CD80"   # green
        , "#F4CB8C" # orange
        , "#F3BB00" # yellow
        , "#0198CD" # blue
        , "#FE6673" # red
      ))(nlevels)
    
  } else if (isFALSE(color)) {
    
    color <-   gray.colors(n = nlevels
                            , start = 0.8
                            , end = 0.3
                           ) 
    
  } else { color <- color }
  
  pallete <- color %>%
    tibble('colors' = .)
  
    graph_opts <- c(type = type
                   , x = x
                   , y = response
                   , group = group
                   , xlab = xlab
                   , ylab = ylab
                   , glab = glab
                   , ylimits = paste(ylimits, collapse = "*")  
                   , xrotation = paste(xrotation, collapse = "*") 
                   , sig = sig
                   , error = error
                   , legend = legend
                   , gtext = paste(gtext, collapse = ",") 
                   , xtext = paste(xtext, collapse = ",")  
                   , opt = opt
                   , dimension = paste(dimension, collapse = "*") 
                   )

    opts_table <- enframe(graph_opts) %>%
      rename('arguments' = .data$name, 'values' = .data$value) %>%
      merge(pallete, ., by = 0, all = TRUE) %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names)
    
    aov_table <- data$anova %>% 
      anova() %>% 
      rownames_to_column("Factor") %>% 
      mutate(across(everything(), as.character)) %>% 
      tibble() 
    
    stat_smr <- data$stats %>% 
      rownames_to_column() %>% 
      mutate(across(everything(), as.character)) %>%
      pivot_longer(!.data$rowname
                   , names_to = "statistics"
                   , values_to = "information"
                   ) %>% 
      select(!.data$rowname) %>% 
      tibble::add_row(
        "statistics" = c("model"
                           , "version"
                           )
        , "information" = c(model
                              , paste("inti", packageVersion('inti'))
                              )
        )
      
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

    graph_table <- data$meancomp %>% 
      merge(.
            , opts_table
            , by = 0
            , all = TRUE
            )  %>%
      add_column("[plot]" = "||", .before = "colors") %>% 
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names) %>%
      add_column("[stats]" = "||") %>%
      merge(.
            , stat_smr
            , by = 0
            , all = TRUE
            ) %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names) %>%
      add_column("[aov]" = "||") %>%
      merge(.
            , aov_table
            , by = 0
            , all = TRUE
      ) %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names) 
      
# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  return(graph_table)

}
