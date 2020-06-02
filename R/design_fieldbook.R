#' Field book design for data collection
#'
#' @description Function to construct your field book for your experiment
#' @param treat1 strign with the name of the level factor with only space
#' @param treat2 strign with the name of the level factor with only space
#' @param rep number of repetition
#' @param intime number of evaluation in time
#' @param sample number or samples for each plot
#' @param design experimental design c("rcbd","crd","f2crd","f2rcbd","lsd")
#' @param lbl_treat1 col label for treat 1
#' @param lbl_treat2 col label for treat 2
#' @param variables name of the variable evaluated in string with only space
#' @return Table with the experimental design
#' @importFrom agricolae design.ab
#' @importFrom data.table setnames
#' @importFrom stringr str_trim
#' @export


design_fieldbook <- function( treat1 = NULL, treat2 = NULL, rep = NULL, intime = 1, sample = 1, design = "crd", lbl_treat1 = NULL, lbl_treat2 = NULL, variables = NULL){


  if( is.null(treat1) && is.null(treat2) && is.null(rep) ){

    stop("You need to insert level for your treatments")

  }

  tr1 <- treat1
  tr2 <- treat2
  dsg <- design

  if(dsg == "f2crd")  {dsg <- "crd"}
  if(dsg == "f2rcbd") {dsg <- "rcbd"}
  if(dsg == "f2lsd")  {dsg <- "lsd"}

  if( !is.null(tr1) ) {

    tr1 <- treat1

    if(is.null(lbl_treat1)){

      lbt1 <- "treat1"

    } else {

      lbl_treat1 <- stringr::str_trim(lbl_treat1, side = "both")
      lbt1 <- gsub("\\s+", "_", lbl_treat1)

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

      lbl_treat2 <- stringr::str_trim(lbl_treat2, side = "both")
      lbt2 <- gsub("\\s+", "_", lbl_treat2)

    }

  }


  if ( !is.null(variables) ){

    #varst <- unlist(strsplit(variables, split = " "))
    varst <- unlist(strsplit(variables, split = ","))

    varst <- stringr::str_trim(varst, side = "both")
    varst <- gsub("\\s+", "_", varst)

    varfb <- factor(unique( varst[ varst != ""]))
    varfb <- as.character(varfb)

  }

  # vc1 <- unlist(strsplit(tr1, split = " "))
  # vc2 <- unlist(strsplit(tr2, split = " "))

  vc1 <- unlist(strsplit(tr1, split = ","))
  vc2 <- unlist(strsplit(tr2, split = ","))

  vc1 <- stringr::str_trim(vc1, side = "both")
  vc1 <- gsub("\\s+","_", vc1)
  vc2 <- stringr::str_trim(vc2, side = "both")
  vc2 <- gsub("\\s+","_", vc2)


  trt1 <- factor(unique( vc1[ vc1 != ""]))
  trt2 <- factor(unique( vc2[ vc2 != ""]))

  lt1 <- length(trt1)
  lt2 <- length(trt2)

  fact <-c(lt1, lt2)

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


  if (design == "crd"||design == "f2crd"){

    fb[,"r"] <- paste("r", fb[,"r"], sep = "")
    fb <- data.table::setnames(x = fb, "r", "rep")
    fb[,"rep"] <- as.factor(fb[,"rep"])
    fb

  } else if ( design == "rcbd" || design == "f2rcbd"){

    fb[,"block"] <- paste("b", fb[,"block"], sep = "")
    fb[,"block"] <- as.factor(fb[,"block"])
    fb

  } else if ( design == "lsd" || design == "f2lsd"){

    fb[,"row"] <- paste("r", fb[,"row"], sep = "")
    fb[,"row"] <- as.factor(fb[,"row"])
    fb[,"col"] <- paste("c", fb[,"col"], sep = "")
    fb[,"col"] <- as.factor(fb[,"col"])
    fb

  }

  if(sample == 1){

    fb

  } else {


    fk <- fb[rep(seq_len(nrow(fb)), each = sample),] # if add each = intime!! you can  use for sub sample
    tm <- as.factor(1:sample)
    fk[,"sample"] <- rep(tm, nrow(fb))
    fk[,"sample"] <- paste("S", fk[,"sample"], sep = "")
    fb <- fk

  }


  if(intime == 1){

    fb

  } else {


    fk <- fb[rep(seq_len(nrow(fb)), intime),] # if add each = intime!! you can  use for sub sample
    tm <- as.factor(1:intime)
    fk[,"intime"] <- rep(tm, each = nrow(fb))
    fk[,"intime"] <- paste("E", fk[,"intime"], sep = "")
    fb <- fk

  }


  if ( !is.null(variables) ){

    fb[, varfb ] <- ""
    fb

  } else { fb }

  # names(fb) <- stringr::str_replace_all(names(fb),pattern = " ","_")
  # fb

  fb[,"ID"] <- paste("U", fb[,"ID"], sep = "")
  fb

}
