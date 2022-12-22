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

tarpuy_plotdesign <- function(data
                        , factor = NA
                        , fill = "plots"
                        , xlab = NULL
                        , ylab = NULL
                        , glab = NULL
                        ) {

# -------------------------------------------------------------------------

  # data <- fb
  # xlab <- ylab <- glab <- NULL
  # factor <- "geno"

# -------------------------------------------------------------------------

  design <- data %>% purrr::pluck(1)
  
  param <- data %>% purrr::pluck(2)
  
  factor <- if(is.na(factor) | is.null(factor)) { param$factornames[1] }

# plot --------------------------------------------------------------------

 color_grps <- colorRampPalette(
      c("#86CD80"   # green
        , "#F4CB8C" # orange
        , "#F3BB00" # yellow
        , "#0198CD" # blue
        , "#FE6673" # red
      ))(length(param$factors[[factor]]))

# -------------------------------------------------------------------------

  if (is.null(xlab)) { xlab <-  "columns" }
  if (is.null(ylab)) { ylab <-  "row" }
  if (is.null(glab)) { glab <- factor }

# -------------------------------------------------------------------------

plot <- design %>%
  mutate(across({{factor}}, as.factor)) %>% 
  arrange(.data$rows, .data$cols) %>%
  ggplot(aes(x = .data$cols, y = .data$rows, fill = .data[[factor]])) +
  geom_tile(color = "black", size = 0.5) +
  geom_text(aes(label = .data[[fill]])) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse', n.breaks = param$dim[1]) +
  scale_x_continuous(expand = c(0, 0), n.breaks = param$dim[2]) +
  scale_fill_manual(values = color_grps) +
  labs(x = xlab, y = ylab, fill = glab) +
  theme_bw() +
  theme(legend.position = "top")

return(plot)

}

