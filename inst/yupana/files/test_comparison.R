#' Multiple comparison test
#'
#' @description Function analysis of variance for summary data.
#' @param aov lm o aov result function.
#' @param comp treatments will be compared.
#' @param type method for made comparison analysis: c("snk", "tukey", "duncan").
#' @param sig significance level. Default 0.05
#' @return Table with complete data for graphics
#' @importFrom agricolae SNK.test HSD.test duncan.test
#' @export


test_comparison <- function( aov, comp, type = "snk", sig = 0.05){

  if( type == "snk"){

    mc <- agricolae::SNK.test(y = aov, trt = comp, alpha = sig)

  } else if (type == "tukey"){

    mc <- agricolae::HSD.test(y = aov, trt = comp, alpha = sig)

  } else if (type == "duncan"){

    mc <- agricolae::duncan.test(y = aov, trt = comp, alpha = sig)

  }

  data_summary(mc)

}
