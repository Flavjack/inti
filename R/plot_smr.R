#' Plot summary data
#'
#' Graph summary data into bar o line plot
#'
#' @param data data frame with summary data. See details
#' @param type type of plot  (default = "bar"). Others: "line".
#' @param x Variable in the x axis.
#' @param y Variable in the y axis.
#' @param groups Groups the x axis (legend) .
#' @param xlab Label of x axis.
#' @param ylab Label of y axis.
#' @param glab Label of groups or legend.
#' @param limits Limits max and min.
#' @param brakes Units for break the limits.
#' @param sig Comparison test values (default = "sig").
#' @param error Variable used for error bars .
#' @param legend Position of legend (default = "top"). Others: "left", "right", "bottom", "none"
#'
#' @details
#'
#' If the table is a out put of `mean_comparison(graph_opts = TRUE)` function.
#' Its contain all the parameter for the plot
#'
#' @return plot
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @import dplyr
#' @importFrom grDevices colorRampPalette colors
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
#'               , "15uwCgQRtR01B3FJaZBE8t_0bOC_8Bbey9ccwVlZH0jg/edit#gid=56711214")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (data <- gs %>%
#'     range_read("plot"))
#'
#' plot_smr(data, type = "bar")
#'
#' }
#'
#' @export

plot_smr <- function(data
                     , type = c("bar", "line")
                     , x
                     , y
                     , groups
                     , xlab
                     , ylab
                     , glab
                     , limits
                     , brakes
                     , sig
                     , error
                     , legend = "top"
                     ) {

# data --------------------------------------------------------------------
# -------------------------------------------------------------------------

plot_dt <- data %>%
  select(!starts_with("{") | !ends_with("}")) %>%
  select_if(~ !all(is.na(.))) %>%
  drop_na(names(.[1]))

# arguments ---------------------------------------------------------------
# -------------------------------------------------------------------------

arg_dt <- data %>%
  select(starts_with("{") | ends_with("}")) %>%
  select_if(~ !all(is.na(.))) %>%
  rename_with(~ gsub("\\{|\\}", "", .))

if ( "colors" %in% names(arg_dt)  ) {

  color_grps <- arg_dt %>%
    select(colors) %>%
    drop_na() %>%
    deframe()

} else { color_grps <- NULL }

graph_opts <- arg_dt %>%
  select(.data$arguments, .data$values) %>%
  deframe()

# -------------------------------------------------------------------------

x <- graph_opts[["x"]]
y <- graph_opts[["y"]]
groups <- graph_opts[["groups"]]

if ( is.null(color_grps) ) {

  color_grps <- colorRampPalette(
    c("#86CD80"   # green
      , "#F3BB00" # yellow
      , "#F4CB8C" # orange
      , "#0198CD" # blue
      , "#FE6673" # red
    ))(length(plot_dt[[groups]]))

} else { color_grps <- color_grps }

xlab <- graph_opts[["xlab"]] %>%
  gsub(pattern = " ", "~", .)
xlab <- eval(expression(parse(text = xlab)))
ylab <- graph_opts[["ylab"]] %>%
  gsub(pattern = " ", "~", .)
ylab <- eval(expression(parse(text = ylab)))
glab <- graph_opts[["glab"]] %>%
  gsub(pattern = " ", "~", .)
glab <- eval(expression(parse(text = glab)))
limits <- graph_opts[["limits"]] %>%
  strsplit(., "x") %>% deframe() %>% as.numeric()
brakes <- (limits[1]:limits[2]) * as.numeric(graph_opts[["brakes"]])
sig <- graph_opts[["sig"]]
error <- graph_opts[["error"]]
legend <- graph_opts[["legend"]]


# bar plot ----------------------------------------------------------------
# -------------------------------------------------------------------------

  barplot <- function(plot_dt
                      , x
                      , y
                      , groups
                      , xlab
                      , ylab
                      , glab
                      , limits
                      , brakes
                      , sig
                      , error
                      , legend = "top"
                      ) {

    plot_dt %>%
      ggplot( aes( .data[[x]] , .data[[y]], fill = .data[[groups]] ) ) +
      geom_col(position = position_dodge2(0.9, preserve = "single")
                , colour="black"
                , size=.4
                , na.rm = TRUE) +

      labs(x = xlab, y = ylab, fill = glab) +

      scale_y_continuous(limits = limits
                         , breaks = brakes
                         , expand = c(0,0)) +

      scale_fill_manual(values = color_grps) +

      geom_errorbar(aes(ymin = .data[[y]] - .data[[error]]
                        , ymax = .data[[y]] + .data[[error]])
                    , position = position_dodge(0.9)
                    , width = 0.15) +

      geom_text(aes(label = .data[[sig]], y = .data[[y]] + .data[[error]])
                , position = position_dodge(0.9)
                , colour = "black"
                , vjust = -0.5
                , hjust = 0.5
                , angle = 0)
    }

# line plot ---------------------------------------------------------------
# -------------------------------------------------------------------------

  lineplot <- function(plot_dt
                       , x
                       , y
                       , groups
                       , xlab
                       , ylab
                       , glab
                       , limits
                       , brakes
                       , sig
                       , error
                       , legend = "top"
                      ) {

    plot_dt %>%
      ggplot( aes( .data[[x]] , .data[[y]]
                   , shape = .data[[groups]]
                   , colour = .data[[groups]]
                   ) ) +

      labs(x = xlab, y = ylab, group = glab) +

      geom_point( aes(group =  .data[[groups]]
                      , shape = .data[[groups]]
                      , color = .data[[groups]]
                      ), size = 2.5 ) +

      geom_line( aes( group =  .data[[groups]]
                     , color = .data[[groups]]
                     , linetype = .data[[groups]]
                     ) ,  size = 1 ) +

      scale_y_continuous(limits = limits
                         , breaks = brakes
                         , expand = c(0,0)) +

      scale_color_manual(values = color_grps) +

      geom_errorbar(aes(ymin = .data[[y]] - .data[[error]]
                        , ymax = .data[[y]] + .data[[error]])
                    , width = 0.08) +
      geom_text(aes(label = .data[[sig]], y = .data[[y]] + .data[[error]])
                , colour = "black"
                , vjust = -0.5
                , hjust = 0.5
                , angle = 0)
    }

# apply functions----------------------------------------------------------
# -------------------------------------------------------------------------

  barplot <- barplot(plot_dt
                     , x
                     , y
                     , groups
                     , xlab
                     , ylab
                     , glab
                     , limits
                     , brakes
                     , sig
                     , error
                     , legend = "top"
                     )

  lineplot <- lineplot(plot_dt
                       , x
                       , y
                       , groups
                       , xlab
                       , ylab
                       , glab
                       , limits
                       , brakes
                       , sig
                       , error
                       , legend = "top"
                       )

  if ( type == "bar" ) { plot <- barplot }

  if ( type == "line" ) { plot <- lineplot }

# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  plot +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      legend.position = legend
    )

}
