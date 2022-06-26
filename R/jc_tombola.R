#' Journal Club Tombola
#'
#' Function for arrange journal club schedule
#'
#' @param data Data frame withe members and their information.
#' @param members Columns with the members names.
#' @param papers Number of paper by meeting 
#' @param group Column for arrange the group.
#' @param gr_lvl Levels in the groups for the arrange. See details.
#' @param status Column with the status of the members.
#' @param st_lvl Level to confirm the assistance in the JC. See details.
#' @param frq Number of the day for each session.
#' @param date Date when start the first session of JC.
#' @param seed Number for replicate the results (default = date).
#'
#' @details
#'
#' The function could consider n levels for \code{gr_lvl}. In the case of more
#' levels using "both" or "all" will be the combination. The suggested levels 
#' for \code{st_lvl} are: active or spectator. Only the "active" members will 
#' enter in the schedule.
#'
#' @return data frame with the schedule for the JC
#' 
#' @export
#'

jc_tombola <- function(data
                       , members
                       , papers = 1
                       , group = NA
                       , gr_lvl = NA
                       , status = NA
                       , st_lvl = "active"
                       , frq = 7
                       , date = NA
                       , seed = NA
                       ){

# test --------------------------------------------------------------------
  
if (FALSE) {

source("https://inkaverse.com/setup.r")
url <- paste0("https://docs.google.com/spreadsheets/d/"
              , "15TaF0lCCByg0dgOLqfoTlLbAWnau8IiLOklpS4cvm4M")
gs <- as_sheets_id(url)

data <- gs %>% 
  range_read("members") 

members = "Member"
papers = 1
group = "Language"
gr_lvl = c("english")
status = "Status"
st_lvl = "activo"
frq = 7
date = "2022-06-30"
seed = NA

}
  
# -------------------------------------------------------------------------

grp <- NULL

date <- as.Date(date) 

if(is.na(seed)){ set.seed(date)} else {set.seed(seed)} 

param <- c({{members}}, {{group}}, {{status}}) %>% purrr::discard(is.na)

jc <- data %>% 
  dplyr::select({{param}}) %>% 
  dplyr::mutate(dplyr::across(everything(), as.character)) %>%
  {
    if(!is.na(status)) {dplyr::filter(.data = ., .data[[status]] %in% st_lvl)} else {.}
  } %>% 
  {
    if(!is.na(group)) {
      
      dplyr::mutate(.data = ., "{group}" := dplyr::case_when(
        .data[[group]] %in% gr_lvl ~ as.character(.data[[group]])
        , .data[[group]] %in% c("both", "all") ~ paste(gr_lvl, collapse = " ")
        , TRUE ~ "exclude"
      )) %>% 
        tidyr::separate_rows(data = ., .data[[group]], sep = " ")
      
    } else {
      mutate(.data = ., group = row.names(.))
    }
  }

group <- if(is.na(group)) {"group"} else {group}

tb <- jc %>% 
  {
  
    if(papers == length(gr_lvl)) {  
      dplyr::group_by(.data = ., .data[[group]] ) } else {.}
  
  } %>% 
  dplyr::mutate(grp = sample.int(dplyr::n())) %>% 
  dplyr::arrange(.data$grp,  {{group}}) %>%
  {

    if(papers == length(gr_lvl)) {
      dplyr::ungroup(x = ., .data[[group]] ) } else {.}

  } %>%
  dplyr::select(.data$grp, {{group}}, {{members}}) %>% 
  dplyr::mutate(grp = date + rep(seq(0, nrow(.)/length(gr_lvl)*frq, by = frq)
                                 , each = papers
                                 , len = nrow(.))) %>%
  dplyr::rename(Date = grp, Leader = members)

tb

}
