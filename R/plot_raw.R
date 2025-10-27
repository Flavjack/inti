#' Plot raw data
#'
#' Function use the raw data for made a boxplot graphic
#'
#' @param data raw data
#' @param type Type of graphic. "boxplot" or "scatterplot"
#' @param x Axis x variable
#' @param y Axis y variable
#' @param group Group variable
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param glab Title for the legend
#' @param ylimits Limits and break of the y axis c(initial, end, brakes)
#' @param xlimits For scatter plot. Limits and break of the x axis c(initial,
#'   end, brakes)
#' @param xrotation Rotation in x axis c(angle, h, v)
#' @param xtext Text labels in x axis using a vector
#' @param gtext Text labels in groups using a vector
#' @param legend the position of legends ("none", "left", "right", "bottom",
#'   "top", or two-element numeric vector)
#' @param color Colored figure (TRUE), black & white (FALSE) or color vector
#' @param linetype Line type for regression. Default = 0
#' @param opt Add new layers to the plot
#'
#' @details
#'
#' You could add additional layer to the plot using "+" with ggplot2 options
#'
#' @return plot
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom stats lm
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#'
#' fb <- potato
#' 
#' fb %>%
#'   plot_raw(type = "box"
#'            , x = "geno"
#'            , y = "twue"
#'            #, group = "treat"
#'            , ylab = NULL
#'            , xlab = NULL
#'            , glab = ""
#'            ) 
#'            
#' fb %>%
#'   plot_raw(type = "sca"
#'            , x = "hi"
#'            , y = "twue"
#'            , group = "geno"
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

if (FALSE) {
  
  data <- potato
  type = "scat"
  x = "hi"
  y = "twue"
  group = NULL
  color = "yes"
  
  xlab = "test"
  ylab = "hello"
  
  glab = ""
  
  ylimits = NULL
  xlimits = NULL
  xrotation = NULL
  legend = "top"
  xtext = NULL
  gtext = NULL
  color = TRUE
  linetype = 1
  opt = NULL
  
}

# -------------------------------------------------------------------------

  type <- match.arg(type, c("boxplot", "scatterplot"))
  
# -------------------------------------------------------------------------

if(!c(x %in% colnames(data))) stop("colum no exist")
if(!c(y %in% colnames(data))) stop("colum no exist")

# -------------------------------------------------------------------------
  
  group <- if(is.null(group) || group == "") {x} else {group}

  xlab <- if(is.null(xlab) || is.na(xlab)) {NULL} else {xlab}
  ylab <- if(is.null(ylab) || is.na(ylab)) {NULL} else {ylab}
  glab <- if(is.null(glab) || is.na(glab) ) {NULL} else {glab}
  
  opt <- if(is.null(opt) || is.na(opt) || opt == "") {NULL} else {opt}

  color <- if(length(color) <= 1 && (is.null(color) || is.na(color) || color == "" || color == "yes")) {
    TRUE} else {color}
  
  ylimits <- if(any(is.null(ylimits)) || any(is.na(ylimits)) || any(ylimits == "")) { 
    NULL
  } else if(is.character(ylimits)) {
    ylimits %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else {ylimits}
  
  xtext <- if(length(xtext) <= 1 && (is.null(xtext) || is.na(xtext) || xtext == "")) {
    NULL} else if (is.character(xtext)){ 
      xtext %>%
        strsplit(., ",") %>%
        unlist() %>% 
        base::trimws()
    } else {xtext}
  
  gtext <- if (length(gtext) <= 1 && (is.null(gtext) || is.na(gtext) || gtext == "")) {
    NULL} else if (is.character(gtext)){ 
      gtext %>%
        strsplit(., ",") %>%
        unlist() %>% 
        base::trimws()
    } else {gtext}
  
  xrotation <- if(any(is.null(xrotation)) || any(is.na(xrotation)) || any(xrotation == "")) {
    c(0, 0.5, 0.5)
  } else if (is.character(xrotation)){ 
    xrotation %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else {xrotation}

# graph-color -------------------------------------------------------------

ncolors <-  if(type == "boxplot") {
    
    length(unique(data[[group]]))
    
  } else if (type == "scatterplot") {
    
    if(is.null(group)) { 1 } else {length(unique(data[[group]])) }
    
  }
  
color <- if (isTRUE(color)) {
    
   paleta()[1:ncolors]
    
  } else if (isFALSE(color)) {
    
    gray.colors(n = ncolors, start = 0.8, end = 0.3)
    
  } else { color }

# sci-labels --------------------------------------------------------------

xlab <- if ( !is.null(xlab) ) {
  
  xlab <- xlab %>%
    gsub(pattern = " ", "~", .)
  xlab <- eval(expression(parse(text = xlab)))
  }

ylab <- if ( !is.null(ylab) ) { 
  
  ylab <- ylab %>%
    gsub(pattern = " ", "~", .)
  
  ylab <- eval(expression(parse(text = ylab)))
  }

glab <- if ( !is.null(glab) ) {
  
  glab <- glab %>%
    gsub(pattern = " ", "~", .)
  glab <- eval(expression(parse(text = glab)))
  } 

  lab_x <- if(is.null(xlab)) x else xlab
  lab_y <- if(is.null(ylab)) y else ylab
  lab_group <- if(is.null(glab)) group else glab
  
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
  
  group <- if(x == group) {group <- NULL} else {group}

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
    x = lab_x
    , y = lab_y
    , fill = lab_group
    , shape = lab_group
    , color = lab_group
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
