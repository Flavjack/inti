#' Plot line regression
#'
#' @description Function plot linea regression
#' @param data Output dtsm fuction
#' @param x Axis x variable
#' @param y Axis y variable
#' @param z Group variable
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param lgl Title for the legend
#' @param lgd the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param xbrk brakes for x axis
#' @param ybrk brakes for y axis
#' @param zbl legend label in strign with doble space
#' @param color colored figure (TRUE), otherwise black & white (FALSE)
#' @param font letter size in plot
#' @param rlx regression line position in axis x.
#' @param rly regression line position in axis y.
#' @return Line regression plot
#' @importFrom dplyr mutate
#' @importFrom ggplot2 annotate geom_smooth scale_x_continuous scale_color_grey aes aes_string element_blank element_rect element_text geom_bar geom_errorbar geom_line geom_point geom_text ggplot position_dodge scale_color_discrete scale_fill_hue scale_shape_discrete scale_x_discrete scale_y_continuous theme theme_bw unit scale_fill_discrete scale_fill_grey annotate
#' @importFrom gtools mixedsort
#' @importFrom stats lm
#' @export


plot_linereg <- function(data, x, y, z, ylab = NULL, xlab = NULL, lgl = NULL,lgd = "top", xbrk = NULL, ybrk = NULL, zbl = NULL, color = TRUE, font = 1, rlx = NULL, rly = NULL){


  if( !is.null(z) ){

    data[,z] <- factor(data[,z], levels = gtools::mixedsort(levels(as.factor(data[, z]))))

  }

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


  if( !is.null(zbl) ){

    zbl <- unlist(strsplit(zbl, split = "  "))
    zbl <- factor(unique( zbl[ zbl != "  "]))
    zbl <- as.character(zbl)

  } else {

    zbl <- ggplot2::waiver()

  }




  if(is.null(xbrk)){

    xbrks <- ggplot2::waiver() } else {

      xbrks <- (((round(mean(data[,x]), 0))*(-20)):((round(mean(data[,x]), 0))*(+20))) * xbrk


    }


  if(is.null(ybrk)){

    ybrks <- ggplot2::waiver() } else {

      ybrks <- (((round(mean(data[,y]), 0))*(-20)):((round(mean(data[,y]), 0))*(+20))) * ybrk


    }

  rgl <-  lm_eqn(x, y, data)

  if ( is.null(rlx) ){ rlx = -Inf } else { rlx = rlx }
  if ( is.null(rly) ){ rly = Inf } else { rly = rly }


  p <- ggplot(data, aes_string(  x = x , y = y, group = z, shape= z, color= z))+
    geom_smooth(method = lm, se = FALSE, fullrange = TRUE, size = 0.3)+
    geom_point(size = 1.2*font)+
    scale_x_continuous( xlab, expand = c(0,0), breaks = xbrks)+
    scale_y_continuous( ylab, expand = c(0,0), breaks = ybrks )+
    scale_shape_discrete(lgl, labels = zbl)+
    annotate("text", label = rgl, parse = T, x = rlx, y = rly, vjust = "inward", hjust = "inward", size= 2.5*font)


  if ( color == TRUE ){

    p <- p +
      scale_color_discrete(lgl, labels = zbl)



  } else if (color == FALSE ){

    p <- p +
      scale_color_grey(lgl, labels = zbl, start = 0, end = 0)


  }

    p + theme_bw() +
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

