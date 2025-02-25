#' Graph options to export
#'
#' Function to export the graph options and model parameters
#'
#' @param data Result from yupana_analysis or yupana_import.
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
#' @importFrom stats anova
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
#'                        , model_factors = "block + geno*riego"
#'                        , comparison = c("geno", "riego")
#'                        )
#'                        
#' gtab <- yupana_export(smr, type = "line", ylimits = c(0, 100, 2))
#' 
#' #> import
#' 
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=1202800640")
#' # browseURL(url)
#' 
#' fb <- gsheet2tbl(url)
#' 
#' info <- yupana_import(fb)
#' 
#' etab <- yupana_export(info)
#' 
#' info2 <- yupana_import(etab)
#' 
#' etab2 <- yupana_export(info2)
#' 
#' }
#' 

yupana_export <- function(data
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
  
# -------------------------------------------------------------------------
# yupana_analysis() -------------------------------------------------------
# -------------------------------------------------------------------------

  if(length(data) > 10) { 
    
# arguments ---------------------------------------------------------------

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
  
  
  ylim <- ifelse(!is.na(ylimits)
                 , paste(ylimits, collapse = "*")
                 , NA) %>% purrr::pluck(1)
  
  gtext <- ifelse(!is.na(gtext)
                  , paste(gtext, collapse = ",")
                  , NA) %>% purrr::pluck(1)
  
  xtext <- ifelse(!is.na(xtext)
                  , paste(xtext, collapse = ",")
                  , NA) %>% purrr::pluck(1)
  
  graph_opts <- c(type = type
                 , x = x
                 , y = response
                 , group = group
                 , xlab = xlab
                 , ylab = ylab
                 , glab = glab
                 , ylimits = ylim
                 , xrotation = paste(xrotation, collapse = "*")
                 , sig = sig
                 , error = error
                 , legend = legend
                 , gtext = gtext
                 , xtext = xtext
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
      anova_table()
    
    stat_smr <- data$stats %>% 
      rownames_to_column() %>% 
      mutate(across(everything(), as.character)) %>%
      pivot_longer(!.data$rowname
                   , names_to = "statistics"
                   , values_to = "information"
                   ) %>% 
      select(!.data$rowname) %>% 
      tibble::add_row(
        "statistics" = c(
          "model"
          , "date"
          , "version"
          )
        , "information" = c(
          model
          , format(Sys.time(), '%Y-%m-%d %H:%M:%S')
          , paste("inti", packageVersion('inti'))
          )
        )
      
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

    graph_table <-  aov_table %>% 
      tibble::add_column("[stats]" = "||") %>% 
      merge(.
            , stat_smr
            , by = 0
            , all = TRUE
      ) %>%
      tibble::add_column("[means]" = "||") %>% 
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names) %>%
      merge(.
            , data$meancomp 
            , by = 0
            , all = TRUE
      ) %>% 
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names) %>% 
      tibble::add_column("[plot]" = "||") %>% 
      merge(.
            , opts_table
            , by = 0
            , all = TRUE
      ) %>% 
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names) %>%
      tibble::add_column("[aov]" = "||", .before = "Factor") %>% 
      tidyr::fill(c(.data$`[stats]`, .data$`[means]`)
                  , .direction = "down") %>% 
      tibble::add_column("[inti]" = "||", .before = "[aov]")
      
# -------------------------------------------------------------------------
#> yupana_import
# -------------------------------------------------------------------------

  } else { 
    
    # data <- info
    
    pallete <- data$plot_args$color %>% 
      enframe(value = "colors") %>% 
      select(!.data$name)
    
    
    ylimits <- if(is.null(ylimits) || is.na(ylimits) || ylimits == "") { 
      NA
    } else if(is.vector(ylimits)) {
      ylimits %>%
        paste(., collapse = "*")
    } else {ylimits}
    
    gtext <- if(is.null(gtext) || is.na(gtext) || gtext == "") { 
      NA
    } else if(is.vector(gtext)) {
      gtext %>%
        paste(., collapse = ",")
    } else {gtext}
    
    xtext <- if(is.null(xtext) || is.na(xtext) || xtext == "") { 
      NA
    } else if(is.vector(xtext)) {
      xtext %>%
        paste(., collapse = ",")
    } else {xtext}

    graph_opts <- c(
      type = type
      , x = data$plot_args$x
      , y = data$plot_args$y
      , group = data$plot_args$group
      #>
      , xlab = if(!is.na(xlab)) xlab else data$plot_args$xlab  
      , ylab = if(!is.na(ylab)) ylab else data$plot_args$ylab
      , glab = if(!is.na(glab)) glab else data$plot_args$glab
      , ylimits = ylimits
      , xrotation = paste(xrotation, collapse = "*") 
      , sig = sig
      , error = error
      , legend = legend
      , gtext = gtext
      , xtext = xtext  
      , opt = opt
      , dimension = paste(dimension, collapse = "*") 
      ) %>% 
      enframe() %>%
      rename('arguments' = .data$name, 'values' = .data$value) %>%
      merge(pallete, ., by = 0, all = TRUE) %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names)  
 
# -------------------------------------------------------------------------
    
stat <- data$stats %>% 
      mutate(information = case_when(
        statistics == "date" ~ format(Sys.time(), '%Y-%m-%d %H:%M:%S')
        , statistics == "version" ~ paste("inti", packageVersion('inti'))
        , TRUE ~ information
      ))

# -------------------------------------------------------------------------

    graph_table <- data$smr %>% 
      merge(.
            , graph_opts
            , by = 0
            , all = TRUE
            ) %>% 
      tibble::add_column("[plot]" = "||", .before = "colors") %>% 
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names) %>%
      tibble::add_column("[stats]" = "||") %>%
      merge(.
            , stat
            , by = 0
            , all = TRUE
      ) %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names) %>%
      tibble::add_column("[aov]" = "||") %>%
      merge(.
            , data$aov
            , by = 0
            , all = TRUE
      ) %>%
      mutate(across(.data$Row.names, as.numeric)) %>%
      arrange(.data$Row.names) %>%
      select(!.data$Row.names) 
      
  }
  
# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  return(as.data.frame(graph_table))

}
