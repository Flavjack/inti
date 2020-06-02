#' Boxplot graphic
#'
#' @description Function use the raw data for made a boxplot graphic
#' @param data raw data
#' @param x Axis x variable
#' @param y Axis y variable
#' @param z Group variable
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param lgl Title for the legend
#' @param lgd the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param brk break of the y axis
#' @param font letter size in plot
#' @return boxplot
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes aes_string element_blank element_rect element_text geom_bar geom_boxplot geom_errorbar geom_line geom_point geom_text ggplot position_dodge position_jitterdodge scale_color_discrete scale_fill_hue scale_shape_discrete scale_x_discrete scale_y_continuous theme theme_bw unit scale_fill_discrete
#' @importFrom gtools mixedsort
#' @export


plot_box <- function(data, x, y, z, ylab = NULL, xlab = NULL, lgl = NULL, lgd = "top", brk = NULL, font = 1){


  data[,x] <- factor(data[,x], levels = gtools::mixedsort(levels(as.factor(data[, x]))))
  data[,z] <- factor(data[,z], levels = gtools::mixedsort(levels(as.factor(data[, z]))))


  if( !is.null(xlab) ){

    xl <- gsub(pattern = " ",replacement = "~", xlab)
    xlab <- eval(expression(parse(text = xl)))

  } else {

    xlab <- x

  }

  if( !is.null(ylab) ){

    yl <- gsub(pattern = " ",replacement = "~", ylab)
    ylab <- eval(expression(parse(text = yl)))


  } else {

    ylab <- y

  }


  if( !is.null(lgl) ){

    ll <- gsub(pattern = " ",replacement = "~", lgl)
    lgl  <- eval(expression(parse(text = ll)))

  } else {

    lgl <- z

  }


  if(is.null(brk)){

    brks <- ggplot2::waiver() } else {

      brks <- (((round(mean(data[,y]), 0))*(-20)):((round(mean(data[,y]), 0))*(+20))) * brk


    }

  ggplot(data, aes_string( x = x , y = y, fill = z))+
    geom_boxplot(outlier.colour = "red", outlier.size = 2.5)+
    geom_point(position = position_jitterdodge())+
    scale_x_discrete( xlab )+
    scale_y_continuous( ylab, breaks = brks)+
    scale_fill_discrete( lgl )+
    theme_bw()+
    theme(
      axis.title.x = element_text(size= 8*font),
      axis.title.y = element_text(size= 8*font, angle=90),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = lgd,
      legend.title = element_text(size= 8*font),
      legend.text = element_text(size= 8*font),
      legend.key.size = unit(0.8*font, "lines"),
      legend.key = element_blank(),
      legend.background = element_rect(fill= "transparent"),
      text = element_text(size = 8*font)
    )
}

