
#' Plot line or bar graphic
#'
#' @description Function for present the results in line or bar plot
#' @param data Output dtsm fuction
#' @param type Type of graphic. "bar" or "line"
#' @param x Axis x variable
#' @param y Axis y variable
#' @param group Group variable
#' @param x_lab Title for the axis x
#' @param y_lab Title for the axis y
#' @param g_lab Title for the legend
#' @param lgd the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param sig Significance of the result (letters)
#' @param erb Show the error bar.
#' @param y_lmt limits of the y axis
#' @param y_brk break of the y axis
#' @param x_brk axis brakes labels in strign with doble space
#' @param g_brk legend label in strign with doble space
#' @param color colored figure (TRUE), otherwise black & white (FALSE)
#' @param ang x text label angle
#' @param font letter size in plot
#' @return Line o bar plot
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes aes_string element_blank element_rect element_text geom_bar geom_errorbar geom_line geom_point geom_text ggplot position_dodge scale_color_discrete scale_fill_hue scale_shape_discrete scale_x_discrete scale_y_continuous theme theme_bw unit scale_fill_discrete geom_col position_dodge2 scale_shape_manual geom_col position_dodge2 scale_shape_manual
#' @importFrom gtools mixedsort
#' @export

plot_gr <- function(data, type= "bar", x, y, group, x_lab = NULL, y_lab = NULL, g_lab = NULL, lgd = "top", sig = NULL, erb = FALSE, y_lmt = NULL, y_brk = NULL, x_brk = NULL, g_brk = NULL, color = TRUE, ang = 0, font = 1.5){
  
  ste <- NULL #To avoid this NOTE: fplot: no visible binding for global variable 'ste'
  
  if(is.null(sig)){
    
    data
    
  } else{
    
    data <- data %>% mutate(ymax = mean+ste)
    
  }
  
  if(is.null(y_brk)){
    
    y_brk <- ggplot2::waiver() } else {
      
      y_brk <- (((round(mean(data[,y]), 0))*(-20)):((round(mean(data[,y]), 0))*(+20)))*y_brk
      
    }
  
  data[,group] <- factor(data[,group], levels = gtools::mixedsort(levels(as.factor(data[, group]))))
  
  data[,x] <- factor(data[,x], levels = gtools::mixedsort(levels(as.factor(data[, x]))))
  
  if ( is.null(y_lab)){
    
    y_lab <- y
    
  } else {
    
    yl <- gsub(pattern = " ",replacement = "~", y_lab)
    y_lab <- eval(expression(parse(text = yl)))
    
  }
  
  if (is.null(x_lab)){
    
    x_lab <- x
    
  } else {
    
    xl <- gsub(pattern = " ",replacement = "~", x_lab)
    x_lab <- eval(expression(parse(text = xl)))
  }
  
  if ( is.null(g_lab)){
    
    g_lab <- group
    
  } else {
    
    
    ll <- gsub(pattern = " ",replacement = "~", g_lab)
    g_lab <- eval(expression(parse(text = ll)))
    
  }
  
  if( !is.null(x_brk)){
    
    x_brk <- unlist(strsplit(x_brk, split = "  "))
    x_brk <- factor(unique(x_brk[x_brk != "  "]))
    x_brk <- as.character(x_brk)
    
  } else {
    
    x_brk <- ggplot2::waiver()
    
  }
  
  if( !is.null(g_brk) ){
    
    g_brk <- unlist(strsplit(g_brk, split = "  "))
    g_brk <- factor(unique(g_brk[g_brk != "  "]))
    g_brk <- as.character(g_brk)
    
  } else {
    
    g_brk <- ggplot2::waiver()
    
  }
  
  if (type == "bar"){
    
    bsp <- ggplot(data, aes_string(x , y, fill= group))+
      geom_col(position = position_dodge2(0.9, preserve = "single"), colour="black", size=.4) +
      scale_x_discrete(x_lab, labels = x_brk)+
      
      if ( color == TRUE ){
        
        scale_fill_discrete(g_lab, labels = g_brk)
        
        
      } else if ( color == FALSE ) {
        
        scale_fill_grey(g_lab, labels = g_brk, start = 1, end = 0.1)
        
      }
    
    if (is.null(y_lmt)){
      
      gr <- bsp + scale_y_continuous(y_lab, breaks = y_brk)
      
    }
    
    if ( !is.null(y_lmt)){
      
      gr <- bsp + scale_y_continuous(y_lab, limits = y_lmt, breaks = y_brk, expand = c(0,0))
      
    }
    
    if( erb == TRUE && !(is.null(sig)) ){
      
      p <-   gr +
        
        geom_errorbar(
          aes(ymin = mean - ste, ymax = mean + ste),
          position = position_dodge(0.9),
          width = 0.2) +
        
        geom_text(
          aes_string(label = sig, y = "ymax"),
          position = position_dodge(0.9),
          vjust = -0.5, size= 2*font)
      
    }
    
    if ( erb == TRUE && is.null(sig) ){
      
      p <- gr +
        geom_errorbar(
          aes(ymin = mean - ste, ymax = mean + ste),
          position = position_dodge(0.9),
          width = 0.2)
      
    }
    
    if ( erb == FALSE && !(is.null(sig)) ){
      
      p <- gr +
        geom_text(
          aes_string(label = sig, y = "ymax"),
          position = position_dodge(0.9),
          vjust = -0.5, size= 2*font)
      
    }
    
    if ( erb == FALSE && is.null(sig) ) {
      
      p <- gr
      
    }
    
  } else if(type == "line"){
    
    if ( color == TRUE ){
      
      bsp <- ggplot(data, aes_string(x, y, group = group, shape= group, color= group))+
        geom_line(size = 0.4)+
        geom_point(size = 1.3*font)+
        scale_x_discrete(x_lab, labels = x_brk)+
        scale_color_discrete(g_lab, labels = g_brk)+
        scale_shape_manual(g_lab, labels = g_brk, values = 1:nlevels(data[,group]))
      
      
    } else if (color == FALSE ){
      
      bsp <- ggplot(data, aes_string(x, y, group = group, shape= group, color= group))+
        geom_line(size = 0.4)+
        geom_point(size = 1.3*font)+
        scale_x_discrete(x_lab, labels = x_brk)+
        scale_color_grey(g_lab, labels = g_brk, start = 0, end = 0) +
        scale_shape_manual(g_lab, labels = g_brk, values = 1:nlevels(data[,group]))
      
    }
    
    if (is.null(y_lmt)){
      
      gr <- bsp + scale_y_continuous(y_lab, breaks = y_brk)
      
    }
    
    if ( !is.null(y_lmt)){
      
      gr <- bsp + scale_y_continuous(y_lab, breaks = y_brk, limits = y_lmt, expand = c(0,0))
      
    }
    
    if( erb == TRUE && !(is.null(sig)) ){
      
      p <-   gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), width=.2)+
        geom_text(aes_string(label= sig, y = "mean"), colour="black", size= 2*font, vjust=-.5, hjust = -.5,angle = 0)
    }
    
    if ( erb == TRUE && is.null(sig) ){
      
      p <- gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), width=.2)
      
    }
    
    if ( erb == FALSE && !(is.null(sig)) ){
      
      p <- gr +
        geom_text(aes_string(label= sig, y = "mean"), colour="black", size= 2*font, vjust=-.5, hjust = -.5,angle = 0)
      
    }
    
    if ( erb == FALSE && is.null(sig) ) {
      
      p <- gr
      
    }
  }
  
  p +
    theme_bw()+
    theme(
      axis.title.x = element_text(size= 8*font),
      axis.title.y = element_text(size= 8*font),
      axis.text.x = element_text(angle = ang),
      panel.background = element_rect(fill = "transparent"), 
      plot.background = element_rect(fill = "transparent"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.background = element_rect(fill = "transparent"), 
      legend.box.background = element_rect(fill = "transparent"),
      legend.position = lgd,
      legend.title = element_text(size= 8*font),
      legend.text = element_text(size= 8*font),
      legend.key.size = unit(0.8*font, "lines"),
      legend.key = element_blank(),
      text = element_text(size = 8*font)
      
    )
}


