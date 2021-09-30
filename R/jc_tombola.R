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
#' The function could consider n levels for \code{gr_lvl}. In the case of two
#' level the third level will be both. The suggested levels for \code{st_lvl}
#' are: active or spectator. Only the active members will enter in the schedule.
#'
#' @return data frame with the schedule for the JC
#' 
#' @export
#'

jc_tombola <- function(data
                       , members
                       , papers = 1
                       , group
                       , gr_lvl
                       , status
                       , st_lvl
                       , frq
                       , date
                       , seed = NULL
                       ){
  
grp <- NULL

date <- as.Date(date)

if(is.null(seed)){ set.seed(date) } else {set.seed(seed)} 

dt <- data %>% 
  mutate(across(c({{members}}, {{group}}, {{status}}), as.character)) 

gr.lvl <- dt %>% 
  select({{group}}) %>% 
  mutate( {{group}} := case_when(
    {{group}} %in% gr_lvl ~ {{group}}
    , TRUE ~ paste0(gr_lvl, collapse = ' ')
  )) %>% 
  tidyr::separate_rows({{group}}) %>% 
  unique() %>% 
  rownames_to_column() %>% 
  select({{group}}, .data$rowname) %>% 
  deframe()
  
jc <- dt %>%
  dplyr::filter(.data[[status]] %in% st_lvl) %>%
  mutate( {{group}} := case_when(
    .data[[group]] %in% gr_lvl ~ .data[[group]]
    , TRUE ~ paste0(gr_lvl, collapse = ' ')
  )) %>% 
  tidyr::separate_rows({{group}}, sep = ' ') %>% 
  mutate(ngrp = case_when(
    .data[[group]] %in% gr_lvl ~ gr.lvl[.data[[group]]]
    ))

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
  dplyr::select(.data$grp, {{group}}, members) %>% 
  dplyr::mutate(grp = date + rep(seq(0, nrow(.)/length(gr_lvl)*frq, by = frq)
                                 , each = papers
                                 , len = nrow(.))) %>%
  dplyr::rename(Date = grp, Leader = members)

tb

}
