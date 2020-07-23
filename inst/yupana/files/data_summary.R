#' Mean Comparison Table Summary
#'
#' Function using resulting output from mean comparison test from agricolae package optimized for graphs.
#'
#' @param meanComp Object list with the result from mean comparison test
#'
#' @return Table with complete data for graphics
#'
#' @importFrom dplyr mutate funs select rename rename_ group_by_ summarise full_join
#' @importFrom tidyr separate
#' @importFrom tibble rownames_to_column
#'
#' @export

data_summary <- function(meanComp){

  #to avoid no bisible global variable function
  std <- r <- trt <- mean <- Min <- Max <- ste <- groups <- NULL

  fct <- as.character(meanComp$parameters$name.t)
  fct <- as.expression(strsplit(fct, split = ":"))

  dtmn <- meanComp$means %>% rename_(mean = names(meanComp$means[1]))

  dtgr <- meanComp$groups %>% tibble::rownames_to_column(var = "trt")

  dtgr$trt <- gsub("\\s", "", as.character(dtgr$trt))

  dta <- dtmn %>%
    dplyr::mutate(ste = std/sqrt(r), trt = as.character(row.names(dtmn)))

  sm <- dplyr::full_join(dta, dtgr, by = "trt") %>%
    dplyr::select(trt, mean, Min, Max, r, std, ste, groups) %>%
    tidyr::separate("trt", sep = ":", into = eval(fct)) %>%
    dplyr::rename(min = Min, max = Max, sg = groups)

}
