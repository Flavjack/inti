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
#'               , "edit#gid=883914570")
#' # browseURL(url)
#'
#' fb <- gsheet2tbl(url)
#' fb %>% plot_smr()
#'
#' fb %>%
#'   plot_smr(type = "line"
#'            , x = "genotipo"
#'            , y = "lfa"
#'            , xlab = "test"
#'            , ylab = "gran"
#'            , glab = "legend"
#'            , xrotation = c(45, 0.5, 0.5)
#'            , ylimits = c(0, 15000, 3000)
#'            )
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
                     , xrotation = NULL
                     , xtext = NULL
                     , gtext = NULL
                     , legend = NULL
                     , sig = NULL
                     , error = NULL
                     , color = TRUE
                     , opt = NULL
                     ) {
  
if(FALSE) {
  
  data <- mc$comparison
  
  data <- fb
  
  var <- list(type.e = NULL
              , x.e = NULL
              , y.e = NULL
              , group.e = NULL
              , xlab.e = NULL
              , ylab.e = NULL
              , glab.e = NULL
              , ylimits.e = NULL
              , xrotation.e = NULL
              , xtext.e = NULL
              , gtext.e = NULL
              , legend.e = NULL
              , sig.e = NULL
              , error.e = NULL
              , color.e = TRUE
              , opt.e = NULL
              ) 
  
  list2env(var, environment())
  
}
  
type.e <- x.e <- y.e <- group.e <- xlab.e <- ylab.e <- glab.e <- NULL
ylimits.e <- xrotation.e <- xtext.e <- gtext.e <- legend.e <- NULL
sig.e <- error.e <- color.e <- opt.e  <-  NULL
  
# arguments ---------------------------------------------------------------

arg_dt <- data %>%
  as.data.frame() %>%
  select(starts_with("{") | ends_with("}")) %>%
  rename_with(~ gsub("\\{|\\}", "", .) ) 

# -------------------------------------------------------------------------

  if(length(arg_dt) >= 2) {

  graph_opts <- arg_dt %>%
    select(.data$arguments, .data$values) %>%
    drop_na(.data$arguments) %>% 
    mutate(arguments = paste0(.data$arguments, ".e")) %>% 
    deframe() %>% 
    as.list()
  
  list2env(graph_opts, environment())

  if("colors" %in% names(arg_dt)) {

    plot_color <- arg_dt %>%
      select(colors) %>%
      drop_na() %>% 
      deframe()
    
    color <- TRUE
    
  } 

  }

# -------------------------------------------------------------------------

if(is.null(x)) x <- x.e 
if(is.null(y)) y <- y.e

if(is.null(type) & is.na(type.e)) {
  type <- "bar"
} else {
  type <- type.e 
  }

if(is.null(group) & is.na(group.e)) group <- x else group <- group.e

if(is.null(xlab) & is.na(xlab.e)) xlab <- NULL else xlab <- xlab.e
if(is.null(ylab) & is.na(ylab.e)) ylab <- NULL else ylab <- ylab.e
if(is.null(glab) & is.na(glab.e)) glab <- NULL else glab <- glab.e

if(is.null(legend) & is.na(legend.e)) legend <- "top" else legend <- legend.e
if(is.null(sig) & is.na(sig.e)) sig <- NULL else sig <- sig.e
if(is.null(error) & is.na(error.e)) error <- NULL else error <- error.e

if(is.null(xtext) & is.na(xtext.e)) {
  xtext <- NULL
} else {
  xtext <- xtext.e %>%
    strsplit(split = ",") %>% 
    unlist()
}

if(is.null(gtext) & is.na(gtext.e)) {
  gtext <- NULL
} else {
  gtext <- gtext.e %>%
    strsplit(split = ",") %>% 
    unlist()
} 

if(is.null(xrotation) & is.na(xrotation.e)) {
  xrotation <- c(0, 0.5, 0.5)
  } else {
    xrotation <- xrotation.e %>%
      strsplit(split = "[*]") %>% 
      unlist() %>% 
      as.numeric()
    }

if(is.null(ylimits) & is.na(ylimits.e)) {
  ylimits <- NULL
} else {
  ylimits <- ylimits.e %>% 
    strsplit(split = "[*]") %>% 
    unlist() %>% 
    as.numeric()
  }

if(is.null(opt) & is.na(opt.e)) {
  opt <- NA
} else {
  opt <- opt.e %>% 
    gsub(' +', " ", .) %>%
    gsub("[\r\n]", "", .) 
} 

# match args --------------------------------------------------------------

legend <- match.arg(legend, c("top", "left", "right", "bottom", "none"))
type <- match.arg(type, c("barra", "linea"))

if(!c(x %in% colnames(data))) stop("colum no exist")
if(!c(y %in% colnames(data))) stop("colum no exist")

# graph-color -------------------------------------------------------------

color_full <- colorRampPalette(
  c("#86CD80"   # green
    , "#F4CB8C" # orange
    , "#F3BB00" # yellow
    , "#0198CD" # blue
    , "#FE6673" # red
  ))(length(data[[group]] %>% unique()))

color_gray <- gray.colors(n =  data[[group]] %>% unique() %>% length()
                          , start = 0.8
                          , end = 0.3) 

if(length(plot_color) != 0) {
  
  color_full <- plot_color
  
  color_gray <- plot_color
  
} 

# sci-labels --------------------------------------------------------------

if ( !is.null(xlab) ) { 
  
  xlab <- xlab %>%
    gsub(pattern = " ", "~", .)
  xlab <- eval(expression(parse(text = xlab)))
  
} else { xlab <- x }

if ( !is.null(ylab) ) { #
  
  ylab <- ylab %>%
    gsub(pattern = " ", "~", .)
  
  ylab <- eval(expression(parse(text = ylab)))
  
} else { ylab <- y }

if ( !is.null(glab) ) {
  
  glab <- glab %>%
    gsub(pattern = " ", "~", .)
  glab <- eval(expression(parse(text = glab)))
  
} else { glab <- group }

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
                  , fill = .data[[group]])) +
    
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
          , angle = 0) 
    } +
    scale_fill_manual(values = if(isFALSE(color)) color_gray else color_full
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
          , angle = 0) 
    } +
    
    scale_color_manual(
      labels = if(!is.null(gtext)) gtext else waiver()
      , values = if(isFALSE(color)) color_gray else color_full
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
  theme(
    panel.background = element_rect(fill = "transparent")
    , plot.background = element_rect(fill = "transparent")
    , panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    , legend.background = element_rect(fill = "transparent")
    , legend.box.background = element_rect(fill = "transparent")
    , legend.position = legend
    , axis.text.x = element_text(angle = xrotation[1]
                                 , hjust= xrotation[2]
                                 , vjust = xrotation[3])
  )'



if(is.na(opt)) {
  eval(parse(text = layers)) 
} else {
  eval(parse(text = paste(layers, opt, sep = " + ")))
}

}
