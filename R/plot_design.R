#' Fieldbook plot experimental designs
#'
#' Function to plot the fieldbook design
#'
#' @param data Experimental design data frame with the factors and level. See examples.
#' @param factors Vector with the name of the columns with the factors.
#' @param dim Dimension for reshape the design arrangement.
#' @param fill  Value for fill the experimental units (default = "plots").
#' @param colour Color for the experimental units (default = first factor).
#' @param xlab Title for x axis
#' @param ylab Title for y axis
#' @param glab Title for group axis
#'
#' @details The function allows to plot the experimental design according the
#' field experiment design.
#'
#' @return plot
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @import dplyr
#' @importFrom purrr pluck as_vector
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom utils tail
#' @importFrom purrr discard
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' library(googlesheets4)
#' library(tidyverse)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#' , "1ilw0NHT7mihaM-3U48KzkuMt927xe8ukX6rNuIw2fT0/edit#gid=0")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (fb <- gs %>%
#'     range_read("tarpuy"))
#'
#' data <- fb %>% inti::fieldbook_design(n_factors = 3
#'                                       , type = "rcbd"
#'                                       , rep = 3) %>% pluck("design")
#' plot_design(data
#'             , factors = c("temp", "prof", "condición")
#'             , dim = "block"
#'             , glab = "Temperature"
#'             )
#'
#' }
#'
#' @export

plot_design <- function(data
                        , factors
                        , dim = NULL
                        , fill = "plots"
                        , colour = NULL
                        , xlab = NULL
                        , ylab = NULL
                        , glab = NULL
                        ) {

# -------------------------------------------------------------------------

  if (is.null(xlab)) { xlab <-  "Experimental Units" }
  if (is.null(ylab)) { ylab <-  "Blocks" }
  if (is.null(glab)) { glab <-  factors[[1]] }

  if ( !is.null(colour) ) {

    factors <- colour

  } else { factors <- factors[[1]] }

  if ( is.numeric(dim) ) {

    ncols <- dim[1]
    nrows <- dim[2]

  } else if ( is.character(dim) ) {

    nrows <- data %>%
      select(.data[[dim]]) %>%
      unique() %>%
      deframe() %>% length()

    ncols <- data %>%
      nrow() / nrows

    }

  dsg <- data %>%
    mutate(cols = rep(1:ncols, times = nrow(.)/ncols )) %>%
    mutate(rows = rep(1:nrows,  each = nrow(.)/nrows ))

# plot --------------------------------------------------------------------

plot <- dsg %>%
  ggplot(aes(x = .data$cols, y = .data$rows, fill = .data[[factors]])) +
  geom_tile(color = "black", size = 0.5) +
  geom_text(aes(label = .data[[fill]])) +
  scale_x_continuous(expand = c(0, 0), n.breaks = ncols) +
  scale_y_continuous(expand = c(0, 0), n.breaks = nrows, trans = 'reverse') +
  scale_fill_brewer(palette = "Set3") +
  labs(x = xlab, y = ylab, fill = glab) +
  theme_bw() +
  theme(legend.position = "top")

plot

# result ------------------------------------------------------------------
# -------------------------------------------------------------------------

  return(plot)

}

