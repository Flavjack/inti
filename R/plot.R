globalVariables(".")

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

    brks <- (((round(min(data[,y]), 0))*(-20)):((round(min(data[,y]), 0))*(+20))) * brk


    }

  data[,x] <- factor(data[,x], levels = gtools::mixedsort(data[,x]))
  data[,z] <- factor(data[,z], levels = gtools::mixedsort(data[,z]))


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


  data[,x] <- factor(data[,x], levels = gtools::mixedsort(data[,x]))
  data[,z] <- factor(data[,z], levels = gtools::mixedsort(data[,z]))


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



#' Correlation graphic
#'
#' @description Function use to plot correlation matrix
#' @param data matrix with numeric data
#' @param method method of the correlation analisis: c("pearson", "kendall", "spearman", "lin")
#' @param sig level of significance
#' @param color colors in the correlation matrix
#' @return correlation plot
#' @importFrom agricolae correlation
#' @importFrom corrplot corrplot
#' @export

plot_correlation <- function(data, method = "pearson", sig = 0.05, color = NULL, font = 1){

  data <-  data %>% dplyr::select_if(is.numeric) %>% as.data.frame()
  sig <- as.numeric(sig)

  cor <- agricolae::correlation(x = data, method = "pearson")

  #write.csv(cor$pvalue, "pvalues.csv")

  if ( is.null(color)  ){

    col <- colorRampPalette(c("#DD5143", "#F38A78","#FEC9B8", "#FFFFFF", "#FFFFFF","#CFEDFB", "#68C7EC", "#00A0DC"))

  } else {

    palet <- unlist(strsplit(color, split = " "))
    clr <-  palet[ palet != ""]

    col <- colorRampPalette(clr)

  }


  crp <- corrplot::corrplot(

    corr = cor$correlation,
    method = "color",
    type = "upper",
    tl.col="black",
    insig = "blank",
    tl.srt=30,
    addCoef.col = "black",
    addgrid.col = "black",
    col=col(8),
    p.mat = cor$pvalue,
    sig.level = sig,
    tl.cex = 1*font,
    number.cex = 0.9*font
    )



}


#' Principal component analisys graphic
#'
#' @description Function use to plot biplot principal component analisys
#' @param data matrix with numeric data
#' @param type type of plot PCA: c("ind", "var", "biplot")
#' @param quali.sup number of colum of qualitative variable
#' @param lgl label legend
#' @return PCA biplot graph
#' @importFrom FactoMineR PCA
#' @importFrom ggplot2 theme_minimal scale_shape scale_color_brewer
#' @importFrom factoextra fviz_pca_biplot
#' @importFrom grDevices colorRampPalette
#' @export

plot_PCA <- function(data, type = "biplot", quali.sup = NULL, lgl = NULL){


  if( is.null(quali.sup)){

    data  <- data %>%   dplyr::select_if(is.numeric)
    hab <- "none"
    qsp <- NULL

  } else {

    fn <- colnames(data[quali.sup])
    data[ ,fn] <- as.factor(data[ ,fn])
    tn <- data %>% dplyr::select_if(is.numeric) %>% colnames()
    data <- data[c(fn,tn)]

    hab <- 1
    qsp <- 1

    if( is.null(lgl) ){

      lgl <- fn

    } else {

      lgl <- lgl

    }

  }


  pca <- FactoMineR::PCA(
    data,
    quali.sup = qsp,
    scale.unit = TRUE,
    ncp = 5,
    graph = FALSE
    )

  if(type == "ind"){

  plot <- factoextra::fviz_pca_ind(
    X = pca,
    habillage = hab,
    addEllipses =F,
    ellipse.level = 0.68,
    title = "") +
    scale_color_brewer(palette="Dark2") +
    theme_minimal()+
    scale_shape( lgl ) +
    scale_color_discrete( lgl )

  } else if (type == "var") {


   plot <- factoextra::fviz_pca_var(
     X = pca,
     title = "") +
      scale_color_brewer(palette="Dark2") +
      theme_minimal()

  } else if (type == "biplot"){

    plot <- factoextra::fviz_pca_biplot(
      pca,
      habillage = hab,
      cex = 1,
      autoLab = "auto",
      col.var = "black",
      addEllipses = FALSE,
      title = "") +
      scale_shape( lgl ) +
      scale_color_discrete( lgl )



  }



  plot +
    theme_bw() +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12, angle = 90), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 12), legend.text = element_text(size = 12),
      legend.key.size = unit(2, "lines"), legend.key = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      text = element_text(size = 12)
    )



  # summary(pca, nbelements = Inf, file="PCA.txt")


}



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
#' @param lmt limits of the y axis
#' @param brk break of the y axis
#' @param xbl axis brakes labels in strign with doble space
#' @param zbl legend label in strign with doble space
#' @param color colored figure (TRUE), otherwise black & white (FALSE)
#' @param font letter size in plot
#' @return Line regression plot
#' @importFrom dplyr mutate
#' @importFrom ggplot2 geom_smooth scale_x_continuous scale_color_grey aes aes_string element_blank element_rect element_text geom_bar geom_errorbar geom_line geom_point geom_text ggplot position_dodge scale_color_discrete scale_fill_hue scale_shape_discrete scale_x_discrete scale_y_continuous theme theme_bw unit scale_fill_discrete scale_fill_grey
#' @importFrom gtools mixedsort
#' @importFrom stats lm
#' @export


plot_linereg <- function(data, x, y, z, ylab = NULL, xlab = NULL, lgl = NULL,lgd = "top", xbrk = NULL, ybrk = NULL, zbl = NULL, color = TRUE, font = 1){


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



  p <- ggplot(data, aes_string(  x = x , y = y, group = z, shape= z, color= z))+
    geom_smooth(method = lm, se = FALSE, fullrange = TRUE, size = 0.3)+
    geom_point(size = 1.2*font)+
    scale_x_continuous( xlab, expand = c(0,0), breaks = xbrks)+
    scale_y_continuous( ylab, expand = c(0,0), breaks = ybrks )+
    scale_shape_discrete(lgl, labels = zbl)


  if ( color == TRUE ){

    p <- p +
      scale_color_discrete(lgl, labels = zbl)



  } else if (color == FALSE ){

    p <- p +
      scale_color_grey(lgl, labels = zbl, start = 0, end = 0)


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




