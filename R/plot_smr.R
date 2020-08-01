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
#'               , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=172957346")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (data <- gs %>%
#'     range_read("LA"))
#'
#' plot_smr(data)
#'
#' plot_smr(data
#'          , type = "bar"
#'          , limits = c(0, 14000)
#'          , brakes = 2000
#'          , sig = "sig"
#'          , error = "ste"
#'          , legend = "left"
#'          )
#'
#' }
#'
#' @export

plot_smr <- function(data
                     , type = NULL
                     , x = NULL
                     , y = NULL
                     , groups = NULL
                     , xlab = NULL
                     , ylab = NULL
                     , glab = NULL
                     , limits = NULL
                     , brakes = NULL
                     , sig = NULL
                     , error = NULL
                     , legend = NULL
                     ) {

  # type <- x <- y <- groups <- xlab <- ylab <- NULL
  # glab <- limits <- brakes <- sig <- error <- legend <- NULL

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

# -------------------------------------------------------------------------

if ( length(arg_dt) >= 2 ) {

  graph_opts <- arg_dt %>%
    select(.data$arguments, .data$values) %>%
    deframe()

  if ( "colors" %in% names(arg_dt)  ) {

    color_grps <- arg_dt %>%
      select(colors) %>%
      drop_na() %>%
      deframe()

  } else { color_grps <- NULL }

}

# -------------------------------------------------------------------------

if ( !is.null(graph_opts[["type"]]) & !is.null(type) ) {
  type <- type
} else if ( is.null(graph_opts[["type"]]) ) {
  type <- type
} else if ( !is.null(graph_opts[["type"]]) ) {
  type <- graph_opts[["type"]]
  } else { type <- "bar"}

  type <- match.arg(type, c("bar", "line"))

# -------------------------------------------------------------------------

  if ( !is.null(graph_opts[["sig"]]) & !is.null(sig) ) {
    sig <- sig
  } else if ( !is.null(sig) ) {
      sig <- sig
  } else if ( !is.null(graph_opts[["sig"]]) ) {
        sig <- graph_opts[["sig"]]
        } else { sig <- NULL}

# -------------------------------------------------------------------------

  if ( !is.null(graph_opts[["error"]]) & !is.null(error) ) {
    error <- error
  } else if ( !is.null(error) ) {
    error <- error
  } else if ( !is.null(graph_opts[["error"]]) ) {
    error <- graph_opts[["error"]]
    } else { error <- "ste" }

# -------------------------------------------------------------------------

  if ( !is.null(graph_opts[["legend"]]) & !is.null(legend) ) {
    legend <- legend
  } else if ( !is.null(legend) ) {
    legend <- legend
  } else if ( !is.null(graph_opts[["legend"]]) ) {
    legend <- graph_opts[["legend"]]
  } else { legend <- "top" }

  legend <- match.arg(legend, c("top", "left", "right", "bottom", "none"))

# -------------------------------------------------------------------------

if ( is.null(x) ) { x <- graph_opts[["x"]] }

if ( is.null(y) ) { y <- graph_opts[["y"]] }

if ( is.null(groups) ) { #
  groups <- graph_opts[["groups"]] } else { groups <- x }

# -------------------------------------------------------------------------

if ( is.null(color_grps) ) {

  color_grps <- colorRampPalette(
    c("#86CD80"   # green
      , "#F4CB8C" # orange
      , "#F3BB00" # yellow
      , "#0198CD" # blue
      , "#FE6673" # red
    ))(length(plot_dt[[groups]]))

  }

# -------------------------------------------------------------------------

  if ( is.null(xlab) ) { #

    xlab <- graph_opts[["xlab"]] %>%
      gsub(pattern = " ", "~", .)
    xlab <- eval(expression(parse(text = xlab)))

    }

  if ( is.null(ylab) ) { #

    ylab <- graph_opts[["ylab"]] %>%
      gsub(pattern = " ", "~", .)

    ylab <- eval(expression(parse(text = ylab)))

    }

  if ( is.null(glab) ) {

    glab <- graph_opts[["glab"]] %>%
      gsub(pattern = " ", "~", .)
    glab <- eval(expression(parse(text = glab)))

    }

  if ( is.null(limits) ) {

    limits <- graph_opts[["limits"]] %>%
      strsplit(., "x") %>% deframe() %>% as.numeric()

    }

  if ( is.null(brakes) ) {

    brakes <- as.numeric(graph_opts[["brakes"]])

    }

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

      scale_y_continuous(limits = limits
                         , breaks = ((limits[1]*-50):(limits[2]*+50)) * brakes
                         , expand = c(0,0)) +

      scale_fill_manual(values = color_grps) +

      geom_errorbar(aes(ymin = .data[[y]] - .data[[error]]
                        , ymax = .data[[y]] + .data[[error]])
                    , position = position_dodge(0.9)
                    , width = 0.15) +

      {if (!is.null(sig))  geom_text(aes(label = .data[[sig]], y = .data[[y]] + .data[[error]])
                , position = position_dodge(0.9)
                , colour = "black"
                , vjust = -0.5
                , hjust = 0.5
                , angle = 0) } +

      labs(x = xlab
           , y = ylab
           , fill = glab
           )
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

      geom_point( aes(group =  .data[[groups]]
                      , shape = .data[[groups]]
                      , color = .data[[groups]]
                      ), size = 2.5 ) +

      geom_line( aes( group =  .data[[groups]]
                     , color = .data[[groups]]
                     , linetype = .data[[groups]]
                     ) ,  size = 1 ) +

      scale_y_continuous(limits = limits
                         , breaks = ((limits[1]*-50):(limits[2]*+50)) * brakes
                         , expand = c(0,0)) +

      scale_color_manual(values = color_grps) +

      geom_errorbar(aes(ymin = .data[[y]] - .data[[error]]
                        , ymax = .data[[y]] + .data[[error]])
                    , width = 0.08) +

      {if (!is.null(sig))  geom_text(aes(label = .data[[sig]], y = .data[[y]] + .data[[error]])
                      , colour = "black"
                      , vjust = -0.5
                      , hjust = 0.5
                      , angle = 0) } +

      labs(x = xlab, y = ylab
           , shape = glab, color = glab, linetype = glab)

    }

# apply functions----------------------------------------------------------
# -------------------------------------------------------------------------

  if ( type == "bar" ) {

    plot <- barplot(plot_dt
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
    }

  if ( type == "line" ) {

    plot <- lineplot(plot_dt
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
                     , legend = "top")
    }

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
