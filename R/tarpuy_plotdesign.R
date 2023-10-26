#' Fieldbook plot experimental designs
#'
#' Plot fieldbook sketch designs based in experimental design
#'
#' @param data Experimental design data frame with the factors and level. See
#'   examples.
#' @param factor Vector with the name of the columns with the factors.
#' @param fill  Value for fill the experimental units (default = "plots").
#' @param xlab Title for x axis.
#' @param ylab Title for y axis.
#' @param glab Title for group axis.
#'
#' @details The function allows to plot the experimental design according the
#'   field experiment design.
#'
#' @return plot
#'
#' @import dplyr
#' @importFrom purrr pluck as_vector
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom utils tail
#' @importFrom purrr discard
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
#'               , "1grAv_2po804pPGg9nj1o5nli01IcEGvSevDruq_ssHk/edit#gid=1807254932")
#' # browseURL(url)
#' 
#' fb <- gsheet2tbl(url) 
#' 
#' dsg <- fb %>% tarpuy_design() 
#' 
#' dsg
#' 
#' dsg %>% str()
#' 
#' dsg %>% 
#'   tarpuy_plotdesign()
#' 
#' }

tarpuy_plotdesign <- function(data
                        , factor = NA
                        , fill = "plots"
                        , xlab = NULL
                        , ylab = NULL
                        , glab = NULL
                        ) {

# -------------------------------------------------------------------------

  # data <- fb dsg <- sketch
  # factor <- "rows"
  # data <- fb
  # xlab <- ylab <- glab <- NULL
  # factor <- "geno"

# -------------------------------------------------------------------------
  
  if( is.data.frame(data) ) {
    
    design <- data %>% list() %>%  purrr::pluck(1)
    
    param <- NULL
    
    factor <- if(is.na(factor) | is.null(factor)) { names(design)[3] } else {factor}
    
    lengthfactor <- nlevels(as.factor(design[[factor]])) 
    
    nrow <- design$rows %>% unique() %>% length()
    
    ncol <- design$cols %>% unique() %>% length()
    
  } else if ( length(data) > 1 ) {
    
    design <- data %>% purrr::pluck(1)
    
    param <- data %>% purrr::pluck(2)
    
    factor <- if(is.na(factor) | is.null(factor)) { param$factornames[1] }
    
    lengthfactor <- length(param$factors[[factor]])
    
    nrow <-  param$dim[1]
    
    ncol <-  param$dim[2]
    
  }

# plot --------------------------------------------------------------------

 color_grps <- colorRampPalette(
      c("#86CD80"   # green
        , "#F4CB8C" # orange
        , "#F3BB00" # yellow
        , "#0198CD" # blue
        , "#FE6673" # red
      ))(lengthfactor)

# -------------------------------------------------------------------------

  if (is.null(xlab)) { xlab <-  "columns" }
  if (is.null(ylab)) { ylab <-  "row" }
  if (is.null(glab)) { glab <- factor }

# -------------------------------------------------------------------------

legend <- if(nlevels(design[[factor]]) > 20) "none" else "top"
 
plot <- design %>%
  arrange(.data$rows, .data$cols) %>%
  ggplot(aes(x = .data$cols, y = .data$rows, fill = as.factor(.data[[factor]]))) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(aes(label = .data[[fill]])) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse', breaks = 1:nrow) +
  scale_x_continuous(expand = c(0, 0), breaks = 1:ncol) +
  scale_fill_manual(values = color_grps) +
  labs(x = xlab, y = ylab, fill = glab) +
  theme_bw() +
  theme(legend.position = legend) 

return(plot)

}

