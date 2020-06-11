#' Journal Club Tombola
#'
#' Function for arrange journal club schedule
#'
#' @param data Data frame withe members and their information
#' @param members Columns with the members names
#' @param group Column for arrange the group
#' @param gr_lvl Levels in the groups for the arrange
#' @param status Column with the status of the members
#' @param st_lvl Level to confirm the assistance in the JC
#' @param frq Number of the day for each session
#' @param date Date when start the first session of JC
#' @param seed Number for replicate the results
#'
#' @return data frame with the schedule for the JC
#'
#' @author Flavio Lozano-Isla
#'
#' @example
#'
#' library(inti)
#' library(googlesheets4)
#'
#' url <- "https://docs.google.com/spreadsheets/d/15TaF0lCCByg0dgOLqfoTlLbAWnau8IiLOklpS4cvm4M/edit#gid=272604925"
#' gs <- as_sheets_id(url)
#' # browseURL(url)
#'
#' jc <-  gs %>% range_read("members")
#'
#' jc %>% jc_tombola(data = .
#'                   , members = "Member"
#'                   , group = "Language"
#'                   , gr_lvl = c("english", "spanish")
#'                   , status = "Status"
#'                   , st_lvl = "activo"
#'                   , frq = 7
#'                   , date = "2020-06-17"
#'                   ) %>%
#'                   web_table(rnames = T, caption = "JC")
#'

jc_tombola <- function(data
                       , members
                       , group
                       , gr_lvl
                       , status
                       , st_lvl
                       , frq
                       , date
                       , seed = NULL
                       ){

date <- as.Date(date)
members <- as.name(members)
group <- as.name(group)
status <- as.name(status)

if(is.null(seed)){
  set.seed(date)
} else {set.seed(seed)} # replicate results

jc <- data %>%
  dplyr::filter(!!status %in% st_lvl) %>%
  dplyr::mutate(grp = dplyr::case_when(
    !!group == "spanish" ~ (1 + 0),
    !!group == "english" ~ (1 + 0),
    !!group == "both" ~ (1 + 1),
  )) %>%
  tidyr::uncount(grp) %>%
  dplyr::group_by(!!members) %>%
  dplyr::mutate(!!group := if(n() > 1) {paste0(!!group, row_number())}
                else {paste0(!!group)}) %>%
  dplyr::mutate(!!group := case_when(
    !!group == "both1" ~ "english",
    !!group == "both2" ~ "spanish",
    TRUE ~ as.character(!!group)
  ))

tb <- jc %>%
  dplyr::group_by(!!group) %>%
  dplyr::mutate(grp := sample.int(n())) %>%
  dplyr::arrange(grp, !!group) %>%
  dplyr::select(grp, !!group, !!members) %>%
  dplyr::ungroup(!!group) %>%
  dplyr::mutate(grp = date + rep(seq(0, nrow(.)/length(gr_lvl)*frq, by = frq)
                                 , each = length(gr_lvl)
                                 , len = nrow(.))) %>%
  dplyr::rename(Date = grp, Leader = members)

tb

}
