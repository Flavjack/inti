#' Plot raw data
#'
#' @description Function use the raw data for made a boxplot graphic
#' @param data raw data
#' @param type Type of graphic. c("boxplot", "scatterplot")
#' @param x Axis x variable
#' @param y Axis y variable
#' @param group Group variable
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param glab Title for the legend
#' @param legend the position of legends ("none", "left", "right", "bottom",
#'   "top", or two-element numeric vector)
#' @param ylimits Limits and break of the y axis c(init, end, brakes)
#' @param xlimits For scatterplot. Limits and break of the x axis c(init, end, brakes)
#' @param xrotation Rotation in x axis c(angle, h, v)
#' @param xtext Text labels in x axis
#' @param gtext Text labels in groups
#' @param color Colored figure (TRUE), otherwise black & white (FALSE) 
#' @param linetype Line type for regression. Default = 0
#' @param opt Add new layers to the plot
#' @return plot
#' @import dplyr
#' @import ggplot2
#' @importFrom stats lm
#' @export 
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' library(gsheet)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "1D1KYc5FMTHow_PpW6ijravmkF_z9zES8Incfroi-Tc4/edit#gid=1336259432")
#' # browseURL(url)
#'
#' fb <- gsheet2tbl(url)
#'
#' fb %>%
#'   plot_raw(type = "sca"
#'            , x = "elt_test"
#'            , y = "tam_test"
#'            , group = "testiculo"
#'            , color = T
#'            , ylimits = c(0, 1500, 300)
#'            , linetype = 0
#'            )
#'
#' }
#' 

plot_raw <- function(data
                     , type = "boxplot"
                     , x
                     , y
                     , group = NULL
                     , xlab = NULL
                     , ylab = NULL
                     , glab = NULL
                     , ylimits = NULL
                     , xlimits = NULL
                     , xrotation = NULL
                     , legend = "top"
                     , xtext = NULL
                     , gtext = NULL
                     , color = TRUE
                     , linetype = 1
                     , opt = NULL
                     ){

# -------------------------------------------------------------------------

  type <- match.arg(type, c(
    "boxplot", "scatterplot"
    ))
  
# -------------------------------------------------------------------------

if(!c(x %in% colnames(data))) stop("colum no exist")
if(!c(y %in% colnames(data))) stop("colum no exist")

if(is.null(xrotation)) xrotation <- c(0, 0.5, 0.5) 

# graph-color -------------------------------------------------------------

if(type == "boxplot") {
  
  if(is.null(group)) group <- x else group <- group
  
  if(is.null(group)) { ncolors <- length(data[[x]] %>% unique()) }
  
  else { ncolors <- length(data[[group]] %>% unique()) }
  
} else if (type == "scatterplot") {
  
  if(is.null(group)) { ncolors <- 1 } 
  else { ncolors <- length(data[[group]] %>% unique()) }
  
}

  if (isTRUE(color)) {
    
    color <- colorRampPalette(
      c("#86CD80"   # green
        , "#F4CB8C" # orange
        , "#F3BB00" # yellow
        , "#0198CD" # blue
        , "#FE6673" # red
      ))(ncolors)
    
  } else if (isFALSE(color)) {
    
    color <- gray.colors(n = ncolors
                         , start = 0.8
                         , end = 0.3) 
    
  } else {
    
    color <- color
    
  }
  
# -------------------------------------------------------------------------

if(type == "boxplot") {
  
  plotdt <- data %>% 
    mutate(across(c({{x}}, {{group}}), as.factor))
  
  type <- plotdt %>% 
    ggplot(., aes(x = .data[[x]]
                  , y = .data[[y]]
                  , fill = .data[[group]]
    )) +
    geom_boxplot(outlier.colour = "red", outlier.size = 2.5) +
    geom_point(position = position_jitterdodge()) +
    
    {if(!is.null(xtext)) scale_x_discrete(labels = xtext)} 
  
} else if(type == "scatterplot") {

  plotdt <- data %>% 
    mutate(across({{group}}, as.factor))
  
  type <- plotdt %>% {
    
    if(!is.null(group)) {
      
      ggplot(data = ., aes(x = .data[[x]]
                    , y = .data[[y]]
                    , shape = .data[[group]]
                    , color = .data[[group]]
                    )) 
      
    } else { ggplot(data = ., aes(x = .data[[x]], y = .data[[y]])) }
    
    } +
      
    geom_point(size = 2.5) +
    
    geom_smooth(method = lm
                , formula = 'y ~ x'
                , se = FALSE
                , fullrange = TRUE
                , linetype = linetype
                ) +
    
    {if(!is.null(xlimits)) scale_x_continuous(limits = xlimits[1:2] 
                                              , breaks = seq(xlimits[1], xlimits[2], by = xlimits[3])
                                              )} 
  
  }

plot <- type + {
    
    if(!is.null(ylimits)) {
      scale_y_continuous(
        limits = ylimits[1:2] 
        , breaks = seq(ylimits[1], ylimits[2], by = ylimits[3])
        , expand = c(0,0)
      ) 
    }
    
  } +
  
  scale_fill_manual(values = color
                     , labels = if(!is.null(gtext)) gtext else waiver()) +
  
  scale_shape_discrete(labels = if(!is.null(gtext)) gtext else waiver()) +
  
  scale_color_manual(values = color
                     , labels = if(!is.null(gtext)) gtext else waiver()) +

  labs(
    x = if(is.null(xlab)) x else xlab
    , y = if(is.null(ylab)) y else ylab
    , fill = if(is.null(glab)) group else glab
    , shape = if(is.null(glab)) group else glab
    , color = if(is.null(glab)) group else glab
    ) 

layers <- 'plot +
  theme_minimal() +
  theme(legend.position = legend
    , panel.border = element_rect(colour = "black", fill=NA)
    , panel.background = element_rect(fill = "transparent")
    , legend.background = element_rect(colour = "transparent", fill = "transparent")
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
