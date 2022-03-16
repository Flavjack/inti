#' Import information from data summary
#'
#' Graph summary data
#'
#' @param data Summary information with options
#'
#' @return list
#'
#' @import dplyr
#' 
#' @export
#'
#' @examples
#'
#' \dontrun{
#' 
#' library(inti)
#' library(gsheet)
#' 
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=338518609")
#' # browseURL(url)
#' 
#' fb <- gsheet2tbl(url)
#' 
#' info <- yupana_import(fb)
#' 
#' }
#' 

yupana_import <- function(data){
  
  type <- x <- y <- group <- NULL
  glab <- legend <- sig <- error <- NULL
  model <- test_comp <- sig_level <- NULL
  dimension <- gtext <- opt <- NULL
  xrotation <- xtext <- ylimits <- NULL
  
# -------------------------------------------------------------------------
# yupana_analysis ---------------------------------------------------------
# -------------------------------------------------------------------------
  
if(!is.data.frame(data)) {
  
  # data <- fb

  smr <- data$meancomp
  
  stats <- data$stats
  
  stats_args <- data$stats %>% 
    rownames_to_column() %>% 
    mutate(across(everything(), as.character)) %>% 
    pivot_longer(!.data$rowname, ) %>% 
    select(!.data$rowname) %>% 
    add_row(name = "model", value = data$model) %>% 
    deframe() %>% 
    as.list()
  
  aov <- NULL
  
  plot <- NULL
  
  plot_opts <- list(
    type = "bar"
    , y = data$response 
    , x = data$comparison[1]
    , group = data$comparison[2]
    , xlab = NA 
    , ylab = NA
    , glab = NA
    , sig = "sig"
    , error = "ste"
    , legend = "top"
    , ylimits = NA
    , xrotation = c(0, 0.5, 0.5)
    , gtext = NA
    , xtext = NA
    , opt = NA
    , dimension = c(20, 10, 100)
    , color = TRUE
    ) 
  
  factors <- data$factors
  
  info <- data$tabvar

  
  
} else if (is.data.frame(data)) {
  
  
# -------------------------------------------------------------------------
# import from web ---------------------------------------------------------
# -------------------------------------------------------------------------

  # data <- fb
  
  smr <- data %>% 
    select(1:.data$`[plot]`) %>% 
    select(!.data$`[plot]`) %>% 
    filter(if_any(everything(), ~ !is.na(.)))
  
  aov <- data %>%
    select(.data$`[aov]`:length(.)) %>% 
    select(!c(.data$`[aov]`)) %>% 
    filter(if_any(everything(), ~ !is.na(.)))
  
  stats <- data %>%
    select(.data$`[stats]`:.data$`[aov]`) %>% 
    select(!c(.data$`[stats]`, .data$`[aov]`)) %>% 
    filter(if_any(everything(), ~ !is.na(.)))
  
  stats_args <- stats %>% 
    deframe() %>% 
    as.list()

  plot <- data %>%
    select(.data$`[plot]`:.data$`[stats]`) %>% 
    select(!c(.data$`[plot]`, .data$`[stats]`)) %>% 
    select(!colors) %>% 
    filter(if_any(everything(), ~ !is.na(.)))
  
  plot_args <- plot %>% 
    deframe() %>% 
    as.list()
  
  list2env(plot_args, environment())
  
# -------------------------------------------------------------------------

  plot_color <- data %>% 
    select(colors) %>%
    drop_na() %>%
    deframe()
  
# plot opts ---------------------------------------------------------------

 plot_opts <- list(
   type = if(is.na(plot_args$type)) "bar" else plot_args$type
   , x = plot_args$x
   , y = plot_args$y
   , group = plot_args$group
   , xlab = plot_args$xlab 
   , ylab = plot_args$ylab
   , glab = plot_args$glab
   , sig = plot_args$sig
   , error = plot_args$error
   , legend = plot_args$legend
   #>
   , ylimits = ylimits
   , xrotation = xrotation
   , gtext = gtext
   , xtext = xtext
   , opt = opt
   , dimension = dimension
   , color = plot_color
   ) 
  
# -------------------------------------------------------------------------

  info <- smr %>% 
    select({{y}}:ncol(.)) %>% 
    names()
  
  factors <- smr %>% 
    select(!{{info}}) %>% 
    names()

}
  
# results -----------------------------------------------------------------
  
  graph_opt <- list(smr = smr
                    , stats = stats
                    , stats_args = stats_args
                    , aov = aov
                    , plot = plot
                    , plot_args = plot_opts
                    , factors = factors
                    , tabvar = info
                    ) 
  
}

