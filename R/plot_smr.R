#' Plot summary data
#'
#' Graph summary data into bar o line plot
#'
#' @param data Output from ger_testcomp function
#' @param type Type of graphic. "bar" or "line"
#' @param x Axis x variable
#' @param y Axis y variable
#' @param group Group variable
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
#'
#' @details
#'
#' If the table is a out put of \code{mean_comparison(graph_opts = TRUE)}
#' function. Its contain all the parameter for the plot.
#'
#' @return plot
#'
#' @import dplyr
#' @importFrom grDevices colorRampPalette colors
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
#' 
#' yrs <- yupana_analysis(data = fb
#'                        , response = "hi"
#'                        , model_factors = "geno*treat"
#'                        , comparison = c("geno", "treat")
#'                        )
#' 
#' yrs$meancomp %>% plot_smr(type = "bar"
#'                           , x = "geno"
#'                           , y = "hi"
#'                           , group = "treat"
#'                           , ylimits = c(0, 1, 0.1)
#'                           , opt = "theme_minimal()"
#'                           , color = c("brown", "blue", "black")
#'                           , sig = "sig"
#'                           ) +
#'                            facet_grid(.~ treat)
#' 
#' }
#' 

plot_smr <- function(data
                     , type = NULL
                     , x = NULL
                     , y = NULL
                     , group = NULL
                     , xlab = NULL
                     , ylab = NULL
                     , glab = NULL
                     , ylimits = NULL
                     , xrotation = c(0, 0.5, 0.5)
                     , xtext = NULL
                     , gtext = NULL
                     , legend = "top"
                     , sig = NULL
                     , error = NULL
                     , color = TRUE
                     , opt = NULL
                     ) {
  
# match args --------------------------------------------------------------

legend <- match.arg(legend, c("top", "left", "right", "bottom", "none"))
type <- match.arg(type, c("barra", "linea"))

if(!c(x %in% colnames(data))) stop("colum no exist")
if(!c(y %in% colnames(data))) stop("colum no exist")

# -------------------------------------------------------------------------

if(is.null(group)) {group <- x}


# graph-color -------------------------------------------------------------

if (isTRUE(color)) {
  
  color <- colorRampPalette(
    c("#86CD80"   # green
      , "#F4CB8C" # orange
      , "#F3BB00" # yellow
      , "#0198CD" # blue
      , "#FE6673" # red
    ))(length(data[[group]] %>% unique()))
  
} else if (isFALSE(color)) {
  
  color <- gray.colors(n =  data[[group]] %>% unique() %>% length()
                       , start = 0.8
                       , end = 0.3) 
  
} else {
  
  color <- color
  
}

# sci-labels --------------------------------------------------------------

if ( !is.null(xlab) ) { 
  
  xlab <- xlab %>%
    gsub(pattern = " ", "~", .)
  xlab <- eval(expression(parse(text = xlab)))
  
}

if ( !is.null(ylab) ) { #
  
  ylab <- ylab %>%
    gsub(pattern = " ", "~", .)
  
  ylab <- eval(expression(parse(text = ylab)))
  
}

if ( !is.null(glab) ) {
  
  glab <- glab %>%
    gsub(pattern = " ", "~", .)
  glab <- eval(expression(parse(text = glab)))
  
} 

# type --------------------------------------------------------------------

plotdt <- data %>% 
  select(!starts_with("{") | !ends_with("}")) %>%
  select_if(~ !all(is.na(.))) %>%
  drop_na(names(.[1])) %>% 
  mutate(across(c({{group}}), as.factor))

# bar plot ----------------------------------------------------------------

if(type == "barra") {
  
  plot <- plotdt %>% 
    ggplot(., aes(x = .data[[x]]
                  , y = .data[[y]]
                  , fill = .data[[group]])
           ) +
    
    geom_col(
      position = position_dodge2()
      , colour="black"
      , size=.4
      , na.rm = T
    ) +
    labs(
      x = if(is.null(xlab)) x else xlab
      , y = if(is.null(ylab)) y else ylab
      , fill = if(is.null(glab)) group else glab
    ) +
    
    {
      if (!is.null(error)) 
        geom_errorbar(
          aes(ymin = .data[[y]] - .data[[error]]
              , ymax = .data[[y]] + .data[[error]] )
          , position = position_dodge(width = 0.9)
          , width = 0.15
          , na.rm = T) 
      
    } +
    {
      if (!is.null(sig) )  
        
        geom_text(
          aes(label = .data[[sig]]
              , y = if(!is.null(error)) .data[[y]] + .data[[error]] else .data[[y]])
          , position = position_dodge(width = 0.9)
          , na.rm = T
          , colour = "black"
          , vjust = -0.5
          , hjust = 0.5
          , angle = 0
          , size = 2.5
          ) 
    } +
    scale_fill_manual(values = color
                      , labels = if(!is.null(gtext)) gtext else waiver()) 
}

# line plot ---------------------------------------------------------------

if (type == "linea") {
  
  plot <- plotdt %>% 
    ggplot( aes(x = .data[[x]]
                , y = .data[[y]]
                , shape = .data[[group]]
                , colour = .data[[group]]
    ) ) +
    
    geom_point( aes(group =  .data[[group]]
                    , shape = .data[[group]]
                    , color = .data[[group]]
    ), size = 2.5 ) +
    
    geom_line( aes( group =  .data[[group]]
                    , color = .data[[group]]
                    , linetype = .data[[group]]
    ) ,  size = 1 ) +
    labs(x = if(is.null(xlab)) x else xlab
         , y = if(is.null(ylab)) y else ylab
         , shape = if(is.null(glab)) group else glab
         , color = if(is.null(glab)) group else glab
         , linetype = if(is.null(glab)) group else glab
    ) +
    
    {
      if (!is.null(error))  
        geom_errorbar(aes(ymin = .data[[y]] - .data[[error]]
                          , ymax = .data[[y]] + .data[[error]])
                      , width = 0.08) 
    } +
    
    {
      if (!is.null(sig))  
        
        geom_text(
          aes(label = .data[[sig]]
              , y = if(!is.null(error)) .data[[y]] + .data[[error]] else .data[[y]])
          , colour = "black"
          , vjust = -0.5
          , hjust = 0.5
          , angle = 0
          , size = 2.5
          ) 
    } +
    
    scale_color_manual(
      labels = if(!is.null(gtext)) gtext else waiver()
      , values = color
    ) + 
    scale_linetype_discrete(labels = if(!is.null(gtext)) gtext else waiver()) +
    scale_shape_discrete(labels = if(!is.null(gtext)) gtext else waiver())
  
}

# layers ------------------------------------------------------------------

graph <- plot + 
  { if(!is.null(xtext)) scale_x_discrete(labels = xtext) } +
  {
    if(!is.null(ylimits))
      scale_y_continuous(
        limits = ylimits[1:2] 
        , breaks = seq(ylimits[1], ylimits[2], by = abs(ylimits[3]))
        , expand = c(0,0)
      )
  }

layers <- 'graph +
  theme_minimal() +
  theme(legend.position = legend
    , panel.border = element_rect(colour = "black", fill=NA)
    , panel.background = element_rect(fill = "transparent")
    , legend.background = element_rect(fill = "transparent")
    , axis.text.x = element_text(angle = xrotation[1]
                                 , hjust= xrotation[2]
                                 , vjust = xrotation[3])
    )'

if(is.null(opt)) {
  eval(parse(text = layers)) 
} else {
  eval(parse(text = paste(layers, opt, sep = " + ")))
}

}
