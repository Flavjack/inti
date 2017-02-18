#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`





#' Multiple comparison test
#'
#' @description Function analisis of variance for summary data.
#' @param aov lm o aov result function.
#' @param comp treatments will be compared.
#' @param type method for made comparision analysis: c("snk", "tukey", "duncan").
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

  sapiens::data_summary(mc)

}


#' Field book design for data collection
#'
#' @description Function to construct your field book for your experiment
#' @param treat1 strign with the name of the level factor with only space
#' @param treat2 strign with the name of the level factor with only space
#' @param rep number of repetition
#' @param design experimental design c("rcbd","crd","lsd")
#' @param lbl_treat1 col label for treat 1
#' @param lbl_treat2 col label for treat 2
#' @param variables name of the variable evaluated in string with only space
#' @return Table with the experimental design
#' @importFrom agricolae design.ab
#' @importFrom data.table setnames
#' @export


design_fieldbook <- function( treat1 = NULL, treat2 = NULL, rep = NULL, intime = 1, design = "crd", lbl_treat1 = NULL, lbl_treat2 = NULL, variables = NULL){


  if( is.null(treat1) && is.null(treat2) && is.null(rep) ){

    stop("You need to insert level for your treatments")

  }


  tr1 <- treat1
  tr2 <- treat2
  dsg <- design

  if( !is.null(tr1) ) {

    tr1 <- treat1

    if(is.null(lbl_treat1)){

      lbt1 <- "treat1"

    } else {

      lbt1 <- gsub("\\s", "_", lbl_treat1)

    }

  }

  if(is.null(tr2)){

    tr2 = as.character("1")
    lbt2 = "treat2"

  } else if( !is.null(tr2) ) {

      tr2 <- treat2

      if(is.null(lbl_treat2)){

        lbt2 <- "treat2"

      } else {

        lbt2 <- gsub("\\s", "_", lbl_treat2)

      }

  }


  if ( !is.null(variables) ){

    varst <- unlist(strsplit(variables, split = " "))
    varfb <- factor(unique( varst[ varst != ""]))
    varfb <- as.character(varfb)

  }



  vc1 <- unlist(strsplit(tr1, split = " "))
  vc2 <- unlist(strsplit(tr2, split = " "))

  trt1 <- factor(unique( vc1[ vc1 != ""]))
  trt2 <- factor(unique( vc2[ vc2 != ""]))

  lt1 <- length(trt1)
  lt2 <- length(trt2)

  fact <-c( lt1, lt2)

  table <- agricolae::design.ab(
    trt = fact,
    r = rep,
    design = dsg,
    serie = 2
  )

  book <- table$book

  lv1 <- factor(1:lt1)
  lv2 <- factor(1:lt2)

  book[,"A"] <- factor(book[,"A"], levels = lv1, labels = trt1)
  book[,"B"] <- factor(book[,"B"], levels = lv2, labels = trt2)


  fb <- data.table::setnames(x = book, old = c("plots", "A", "B"), new = c("ID", lbt1, lbt2))

  if( tr2 == "1"){

    fb[, lbt2] <- NULL
    fb


  } else {

    fb

  }


  if (design == "crd"){

    fb[,"r"] <- paste("r", fb[,"r"], sep = "")
    fb <- data.table::setnames(x = fb, "r", "rep")
    fb[,"rep"] <- as.factor(fb[,"rep"])
    fb

  } else if ( design == "rcbd" ){

    fb[,"block"] <- paste("b", fb[,"block"], sep = "")
    fb[,"block"] <- as.factor(fb[,"block"])
    fb

  } else if ( design == "lsd" ){

    fb[,"row"] <- paste("r", fb[,"row"], sep = "")
    fb[,"row"] <- as.factor(fb[,"row"])
    fb[,"col"] <- paste("c", fb[,"col"], sep = "")
    fb[,"col"] <- as.factor(fb[,"col"])
    fb

  }

  if(intime == 1){

    fb[,"ID"] <- paste("U", fb[,"ID"], sep = "")
    fb

  } else {


    fk <- fb[rep(seq_len(nrow(fb)), intime),] # if add each = intime!! you can  use for sub sample
    tm <- as.factor(1:intime)
    fk[,"intime"] <- rep(tm, each = nrow(fb))
    fk[,"intime"] <- paste("E", fk[,"intime"], sep = "")
    fk[,"ID"] <- paste("U", fb[,"ID"], sep = "")
    fb <- fk

  }


  if ( !is.null(variables) ){

    fb[, varfb ] <- ""
    fb

  } else { fb }



}



#' Mean Comparison Table Summary
#'
#' @description Function using resulting output from mean comparison test from agricolae package optimized for graphs.
#' @param meanComp Object list with the result from mean comparison test
#' @return Table with complete data for graphics
#' @importFrom dplyr mutate funs select rename group_by_ summarise full_join
#' @importFrom tidyr separate
#' @export

data_summary <- function(meanComp){

  #to avoid no bisible global variable function
  std <- r <- trt <- means <- Min <- Max <- ste <- M <- NULL

  fct <- as.character(meanComp$parameters$name.t)
  fcts <- as.expression(strsplit(fct, split = ":"))

  dtmn <- meanComp$means
  dtgr <- meanComp$groups

  dtgr$trt <- gsub("\\s", "", as.character(dtgr$trt))

  dta <- dtmn %>%
    dplyr::mutate(ste = std/sqrt(r), trt = as.character(row.names(dtmn)))

  sm <- dplyr::full_join(dta[2:7], dtgr, by = "trt") %>%
    dplyr::select(trt, means, Min, Max, r, std, ste, M) %>%
    tidyr::separate("trt", sep = ":", into = eval(fcts)) %>%
    dplyr::rename(mean = means, min = Min, max = Max, sg = M) %>%
    dplyr::mutate_each_(funs(factor(.)), fct) %>% as.data.frame()

}













