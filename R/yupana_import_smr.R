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
#'               , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=1877968284")
#' # browseURL(url)
#' 
#' data <- gsheet2tbl(url)
#' 
#' info <- yupana_import_smr(data)
#' 
#' info
#'  
#' plot_smr(data = info$data
#'          , type = info$type
#'          , x = info$x
#'          , y = info$y
#'          , group = info$group
#'          , ylimits = info$ylimits
#'          )
#'
#' }
#' 

yupana_import_smr <- function(data){
  
  type <- x <- y <- group <- NULL
  glab <- legend <- sig <- error <- NULL
  model <- test_comp <- sig_level <- NULL
  
  smr <- data %>% 
    as.data.frame() %>%
    select(!starts_with("{") | !ends_with("}")) %>% 
    filter(!across(everything(), is.na))
  
  arg_dt <- data %>%
    as.data.frame() %>%
    select(starts_with("{") | ends_with("}")) %>%
    rename_with(~ gsub("\\{|\\}", "", .))
  
  opts <- arg_dt %>%
    select(.data$arguments, .data$values) %>%
    drop_na(.data$arguments) %>%
    deframe() %>%
    as.list()
  
  list2env(opts, environment())
  
  plot_color <- arg_dt %>% 
    select(colors) %>%
    drop_na() %>%
    deframe()
  
  if(!is.na(xtext)) xtext <- xtext %>% 
    strsplit(split = ",") %>% unlist() 

  if(!is.na(gtext)) gtext <- gtext %>%
    strsplit(split = ",") %>% unlist()

  if(!is.na(opt)) opt <- opt %>% 
    gsub(' +', " ", .) %>% gsub("[\r\n]", "", .)

  if(!is.na(xrotation)) xrotation <- xrotation %>%
    strsplit(split = "[*]") %>%
    unlist() %>%
    as.numeric()

  if(!is.na(ylimits)) ylimits <- ylimits %>%
    strsplit(split = "[*]") %>%
    unlist() %>%
    as.numeric()

  dimension <- dimension %>%
    strsplit(split = "[*]") %>%
    unlist() %>%
    as.numeric()
  
  comparison <- comparison %>%
    strsplit(split = "[*]") %>%
    unlist() 
  
  info <- smr %>% 
    select({{y}}:ncol(.)) %>% 
    names()
  
  factors <- smr %>% 
    select(!{{info}}) %>% 
    names()

# results -----------------------------------------------------------------
  
  graph_opt <- list(data = smr
                    , type = type
                    , y = y
                    , x = x
                    , group = group
                    , xlab = xlab
                    , ylab = ylab
                    , glab = glab
                    , ylimits = ylimits
                    , xrotation = xrotation
                    , xtext = xtext
                    , gtext = gtext
                    , legend = legend
                    , sig = sig
                    , error = error
                    , color = plot_color
                    , opt = opt
                    , dimension = dimension
                    , model = model
                    , factors = factors
                    , tabvar = info
                    , comparison = comparison
                    , test_comp = test_comp
                    , sig_level = sig_level
                    ) 
  
}

