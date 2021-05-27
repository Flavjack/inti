#' Plot raw data
#'
#' @description Function use the raw data for made a boxplot graphic
#' @param data raw data
#' @param type Type of graphic. "boxplot" 
#' @param x Axis x variable
#' @param y Axis y variable
#' @param group Group variable
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param glab Title for the legend
#' @param legend the position of legends ("none", "left", "right", "bottom",
#'   "top", or two-element numeric vector)
#' @param ylimits Limits and break of the y axis c(init, end, brakes)
#' @param xrotation Rotation in x axis c(angle, h, v)
#' @param xtext Text labels in x axis
#' @param gtext Text labels in groups
#' @param color Colored figure (TRUE), otherwise black & white (FALSE)
#' @param opt Add new layers to the plot
#' @return plot
#' @import dplyr
#' @import ggplot2
#' @export
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
#' fb %>%
#'   plot_raw(type = "boxplot"
#'            , x = "geno"
#'            , y = "lfa"
#'            , group = "treat"
#'            , color = T
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
                     , xrotation = NULL
                     , legend = "top"
                     , xtext = NULL
                     , gtext = NULL
                     , color = TRUE
                     , opt = NULL
                     ){
  
# test --------------------------------------------------------------------

if(FALSE) {
  
  x <- "temp" 
  group <- "nacl"
  y <- "grp"
  xlab <- NULL # "label x"
  ylab <- NULL # "label y"
  glab <- NULL # "legend"
  ylimits <- NULL # c(0, 120, 10)
  xrotation <- NULL #c(0, 0.5, 0.5)
  legend <- "top"
  gtext <- NULL
  xtext <- NULL
  
  data <- fb
  
}

# -------------------------------------------------------------------------

if(!c(x %in% colnames(data))) stop("colum no exist")
if(!c(y %in% colnames(data))) stop("colum no exist")

if(is.null(xrotation)) xrotation <- c(0, 0.5, 0.5) 
if(is.null(group)) group <- x else group <- group 

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

# -------------------------------------------------------------------------

plotdt <- data %>% 
  mutate(across(c({{x}}, {{group}}), as.factor))

plot <- plotdt %>% 
  ggplot(., aes(x = .data[[x]]
                , y = .data[[y]]
                , fill = .data[[group]]
  )) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2.5) +
  geom_point(position = position_jitterdodge()) + {
    
    if(!is.null(ylimits)) {
      scale_y_continuous(
        limits = ylimits[1:2] 
        , breaks = seq(ylimits[1], ylimits[2], by = ylimits[3])
        , expand = c(0,0)
      ) 
    }
    
  } +
  labs(
    x = if(is.null(xlab)) x else xlab
    , y = if(is.null(ylab)) y else ylab
    , fill = if(is.null(glab)) group else glab
  ) + 
  scale_fill_manual(values = color
                    , labels = if(!is.null(gtext)) gtext else waiver()) +
  {if(!is.null(xtext)) scale_x_discrete(labels = xtext)} 


layers <- 'plot +
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
