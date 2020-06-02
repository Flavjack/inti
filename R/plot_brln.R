#' Plot line or bar graphic
#'
#' @description Function use the dtsm funtion for plot the results
#' @param data Output dtsm fuction
#' @param type Type of graphic. "bar" or "line"
#' @param x Axis x variable
#' @param y Axis y variable
#' @param z Group variable
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param lgl Title for the legend
#' @param lgd the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param sig Significance of the result (letters)
#' @param erb Show the error bar.
#' @param lmt limits of the y axis
#' @param brk break of the y axis
#' @param xbl axis brakes labels in strign with doble space
#' @param zbl legend label in strign with doble space
#' @param color colored figure (TRUE), otherwise black & white (FALSE)
#' @param font letter size in plot
#' @return Line o bar plot
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes aes_string element_blank element_rect element_text geom_bar geom_errorbar geom_line geom_point geom_text ggplot position_dodge scale_color_discrete scale_fill_hue scale_shape_discrete scale_x_discrete scale_y_continuous theme theme_bw unit scale_fill_discrete
#' @importFrom gtools mixedsort
#' @export

plot_brln <- function(data, type= "bar", x, y, z, ylab = NULL, xlab = NULL, lgl = NULL,lgd = "top", sig = NULL, erb = FALSE, lmt = NULL, brk = NULL, xbl = NULL, zbl = NULL, color = TRUE, font = 1){

  ste <- NULL #To avoid this NOTE: fplot: no visible binding for global variable 'ste'

  if(is.null(brk)){

    brks <- ggplot2::waiver() } else {

      brks <- (((round(mean(data[,y]), 0))*(-20)):((round(mean(data[,y]), 0))*(+20))) * brk


    }

  data[,x] <- factor(data[,x], levels = gtools::mixedsort(levels(as.factor(data[, x]))))
  data[,z] <- factor(data[,z], levels = gtools::mixedsort(levels(as.factor(data[, z]))))


  if ( is.null(ylab)){

    ylab <- y

  } else {

    yl <- gsub(pattern = " ",replacement = "~", ylab)
    ylab <- eval(expression(parse(text = yl)))

  }


  if ( is.null(xlab)){

    xlab <- x

  } else {


    xl <- gsub(pattern = " ",replacement = "~", xlab)
    xlab <- eval(expression(parse(text = xl)))


  }


  if ( is.null(lgl)){

    lgl <- z

  } else {


    ll <- gsub(pattern = " ",replacement = "~", lgl)
    lgl  <- eval(expression(parse(text = ll)))


  }




  data <- data %>% mutate(ymax = mean+ste)


  if( !is.null(xbl) ){

    xbl <- unlist(strsplit(xbl, split = "  "))
    xbl <- factor(unique( xbl[ xbl != "  "]))
    xbl <- as.character(xbl)

  } else {

    xbl <- ggplot2::waiver()

  }

  if( !is.null(zbl) ){

    zbl <- unlist(strsplit(zbl, split = "  "))
    zbl <- factor(unique( zbl[ zbl != "  "]))
    zbl <- as.character(zbl)

  } else {

    zbl <- ggplot2::waiver()

  }



  if (type == "bar"){

    bsp <- ggplot(data, aes_string(x , y, fill= z))+
      geom_bar(position=position_dodge(),colour="black",stat="identity", size=.4)+
      scale_x_discrete(xlab, labels = xbl)+


      if ( color == TRUE ){

        scale_fill_discrete(lgl, labels = zbl)


      } else if ( color == FALSE ) {

        scale_fill_grey(lgl, labels = zbl, start = 1, end = 0.1)


      }


    if (is.null(lmt)){

      gr <- bsp + scale_y_continuous(ylab, breaks = brks)

    }

    if ( !is.null(lmt)){

      gr <- bsp + scale_y_continuous(ylab, expand = c(0,0), limits = lmt, breaks = brks)

    }



    if( erb == TRUE && !(is.null(sig)) ){

      p <-   gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.2, width=.2, position=position_dodge(.9)) +
        geom_text(aes_string(label= sig, y = "ymax"), colour="black", size= 2*font, vjust=-.5, angle = 0, position=position_dodge(.9))


    }

    if ( erb == TRUE && is.null(sig) ){

      p <- gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.2, width=.2, position=position_dodge(.9))


    }

    if ( erb == FALSE && !(is.null(sig)) ){

      p <- gr +
        geom_text(aes_string(label= sig, y = "mean"), colour="black", size= 2*font, vjust=-.5, angle = 0, position=position_dodge(.9))

    }

    if ( erb == FALSE && is.null(sig) ) {

      p <- gr

    }


  } else if(type == "line"){




    if ( color == TRUE ){

      bsp <- ggplot(data, aes_string(x, y, group = z, shape= z, color= z))+
        geom_line(size = 0.3)+
        geom_point(size = 1.2*font)+
        scale_x_discrete(xlab, labels = xbl)+
        scale_color_discrete(lgl, labels = zbl)+
        scale_shape_discrete(lgl, labels = zbl)


    } else if (color == FALSE ){

      bsp <- ggplot(data, aes_string(x, y, group = z, shape= z, color= z))+
        geom_line(size = 0.3)+
        geom_point(size = 1.2*font)+
        scale_x_discrete(xlab, labels = xbl)+
        scale_color_grey(lgl, labels = zbl, start = 0, end = 0) +
        scale_shape_discrete(lgl, labels = zbl)

    }


    if (is.null(lmt)){

      gr <- bsp + scale_y_continuous(ylab, breaks = brks)

    }

    if ( !is.null(lmt)){

      gr <- bsp + scale_y_continuous(ylab, expand = c(0,0), limits = lmt, breaks = brks)

    }


    if( erb == TRUE && !(is.null(sig)) ){

      p <-   gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.2, width=.2)+
        geom_text(aes_string(label= sig, y = "mean"), colour="black", size= 2*font, vjust=-.5, hjust = -.5,angle = 0)

    }

    if ( erb == TRUE && is.null(sig) ){

      p <- gr +
        geom_errorbar(aes(ymin= mean - ste , ymax= mean + ste), size=.2, width=.2)


    }

    if ( erb == FALSE && !(is.null(sig)) ){

      p <- gr +
        geom_text(aes_string(label= sig, y = "mean"), colour="black", size= 2*font, vjust=-.5, hjust = -.5,angle = 0)

    }

    if ( erb == FALSE && is.null(sig) ) {

      p <- gr

    }



  }



  p + theme_bw()+
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

