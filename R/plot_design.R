#' Fieldbook plot experimental designs
#'
#' Plot fieldbook sketch designs based in experimental design
#'
#' @param data Experimental design data frame with the factors and level. See
#'   examples.
#' @param factor Vector with the name of the columns with the factors.
#' @param dim Dimension for reshape the design arrangement.
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
#' \donttest{
#'
#' library(inti)
#' library(googlesheets4)
#' library(purrr)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#' , "1ilw0NHT7mihaM-3U48KzkuMt927xe8ukX6rNuIw2fT0/edit#gid=0")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (fb <- gs %>%
#'     range_read("tarpuy"))
#'
#' data <- fb %>% inti::fieldbook_design(n_factors = 2
#'                                       , type = "rcbd"
#'                                       , rep = 3) %>% pluck("design")
#'
#' plot_design(data
#'             , factor = "temp"
#'             , dim = "block"
#'             )
#'
#' plot_design(data
#'             , factor = "temp"
#'             , dim = "9x5"
#'             )
#'
#' }
#'
#' @export

plot_design <- function(data
                        , factor
                        , dim = NULL
                        , fill = "plots"
                        , xlab = NULL
                        , ylab = NULL
                        , glab = NULL
                        ) {

# -------------------------------------------------------------------------

  # xlab <- ylab <- glab <- NULL
  # fill <- "plots"

  if (is.null(xlab)) { xlab <-  "Experimental Units" }
  if (is.null(ylab)) { ylab <-  "Blocks" }
  if (is.null(glab)) { glab <- factor }

 if ( is.numeric(dim) ) {

    ncols <- dim[1]
    nrows <- dim[2]

  } else if ( any(dim %in% names(data)) ) {

    nrows <- data %>%
      select(.data[[dim]]) %>%
      unique() %>%
      deframe() %>% length()

    ncols <- data %>%
      nrow() / nrows

  }  else if ( is.character(dim) ) {

    dim <- dim %>%
      strsplit(., "x") %>% deframe() %>% as.numeric()

    ncols <- dim[1]
    nrows <- dim[2]

  }

  dsg <- data %>%
    mutate(cols = rep(1:ncols, times = nrow(.)/ncols )) %>%
    mutate(rows = rep(1:nrows,  each = nrow(.)/nrows ))

# plot --------------------------------------------------------------------

 color_grps <- colorRampPalette(
      c("#86CD80"   # green
        , "#F4CB8C" # orange
        , "#F3BB00" # yellow
        , "#0198CD" # blue
        , "#FE6673" # red
      ))(length(data[[factor]] %>% unique()))

plot <- dsg %>%
  ggplot(aes(x = .data$cols, y = .data$rows, fill = .data[[factor]])) +
  geom_tile(color = "black", size = 0.5) +
  geom_text(aes(label = .data[[fill]])) +
  scale_x_continuous(expand = c(0, 0), n.breaks = ncols) +
  scale_y_continuous(expand = c(0, 0), n.breaks = nrows, trans = 'reverse') +
  scale_fill_manual(values = color_grps) +
  labs(x = xlab, y = ylab, fill = glab) +
  theme_bw() +
  theme(legend.position = "top")

plot

# result ------------------------------------------------------------------
# -------------------------------------------------------------------------

  return(plot)

}

