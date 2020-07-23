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

  #pca <-  prcomp(data, center = T, scale. = T)


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

    #plot <- factoextra::fviz_pca(pca, repel = TRUE, title = "PCA")


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
