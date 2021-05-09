#' Graph options to export
#'
#' Function to export the graph options and model parameters
#'
#' @param data Fieldbook data.
#' @param response Model used for the experimental design.
#' @param comparison Factor to compare
#' @param model_factors Factor interaction in the model
#' @param test_comp Type of test comparison
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
#' @param sig_level Level of significance for the test
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
#' mc <- mean_comparison(data = fb
#'                       , response = "hi"
#'                       , model_factors = "geno*treat"
#'                       , comparison = c("geno", "treat")
#'                       )
#' mc$comparison
#' 
#' }
#' 

yupana_graph_opt <- function(data
                             , response
                             , comparison
                             , model_factors
                             , test_comp = NA
                             , type = NA
                             , xlab = NA
                             , ylab = NA
                             , glab = NA
                             , ylimits = NA
                             , xrotation = "0*0.5*0.5"
                             , xtext = NA
                             , gtext = NA
                             , legend = "top"
                             , sig = NA
                             , error = NA
                             , color = TRUE
                             , opt = NA
                             , dimension = "20*10*100"
                             , sig_level = NA
                            ) {
  
  where <- NULL
  
  if(FALSE) {
    
  data <- mc$comparison
  response <- "hi"
  model_factors <- "geno + treat + geno*treat"
  comparison <- c("geno", "treat")
  
test_comp = NA
sig_level = NA
type = NA
xlab = NA
ylab = NA
glab = NA
ylimits = NA
xrotation = "0*0.5*0.5"
xtext = NA
gtext = NA
legend = "top"
sig = 0.05
error = NA
color = TRUE
opt = NA
dimension = "20*10*100"
    
    
  }
  
# graph table -------------------------------------------------------------
# -------------------------------------------------------------------------
  
  model <- as.formula(paste({{response}}, model_factors, sep = "~"))
  model <- paste(deparse(model, width.cutoff = 500), collapse="")

  if ( length(comparison) >= 3 ){
    
    x <- "treatments"
    group <- "treatments"
    
    }

  if ( length(comparison) == 2 ){

    x <- comparison[1]
    group <- comparison[2]
    
    }

  if ( length(comparison) == 1 ){

    x <- comparison[1]
    group <- comparison[1]
    
  }
  

# color pallete -----------------------------------------------------------

  nlevels <- data %>% 
    select(.data[[group]]) %>% 
    n_distinct()
  
  if(isTRUE(color)) {
    
    color <- colorRampPalette(
      c("#86CD80"   # green
        , "#F4CB8C" # orange
        , "#F3BB00" # yellow
        , "#0198CD" # blue
        , "#FE6673" # red
      ))(length(data[[group]] %>% unique())) %>% 
      tibble('{colors}' = .)
    
  } 
  
  if(isFALSE(color)) {
    
    color <-   gray.colors(n = nlevels
                            , start = 0.8
                            , end = 0.3) %>%
      tibble('{colors}' = .)
    
  }
  
    graph_opts <- c(type = type
                   , x = x
                   , y = {{response}}
                   , group = group
                   , xlab = xlab
                   , ylab = ylab
                   , glab = glab
                   , ylimits = ylimits
                   , xrotation = xrotation
                   , sig = sig
                   , error = error
                   , legend = legend
                   , gtext = gtext
                   , xtext = xtext
                   , opt = opt
                   , dimension = dimension
                   , model = model
                   , comparison = paste(comparison, collapse = "*")
                   , test_comp = test_comp
                   , sig_level = sig_level
                   )

    opts_table <- enframe(graph_opts) %>%
      rename('{arguments}' = .data$name, '{values}' = .data$value) %>%
      merge(color, ., by = 0, all = TRUE) %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names)

    graph_table <- merge(data
                        , opts_table
                        , by = 0
                        , all = TRUE
                        )  %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names) 

# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  return(graph_table)

}
