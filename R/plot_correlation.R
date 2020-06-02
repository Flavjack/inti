#' Correlation graphic
#'
#' @description Function use to plot correlation matrix
#' @param data matrix with numeric data
#' @param method method of the correlation analisis: c("pearson", "kendall", "spearman", "lin")
#' @param sig level of significance
#' @param font size of the font
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
