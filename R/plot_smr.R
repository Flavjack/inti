#' Plot summary data
#'
#' Graph summary data into bar o line plot
#'
#' @param data Output from summary data
#' @param type Type of graphic. "bar" or "line"
#' @param x Axis x variable
#' @param y Axis y variable
#' @param group Group variable
#' @param ylab Title for the axis y
#' @param xlab Title for the axis x
#' @param glab Title for the legend
#' @param ylimits limits of the y axis c(initial, end, brakes)
#' @param xrotation Rotation in x axis c(angle, h, v)
#' @param xtext Text labels in x axis using a vector
#' @param gtext Text labels in group using a vector
#' @param legend the position of legends ("none", "left", "right", "bottom",
#'   "top", or two-element numeric vector)
#' @param sig Column with the significance
#' @param sigsize Font size in significance letters
#' @param error Show the error bar ("ste" or "std")
#' @param color colored figure (TRUE), black & white (FALSE) or color vector
#' @param opt Add news layer to the plot
#'
#' @details
#'
#' If the table is a out put of \code{mean_comparison(graph_opts = TRUE)}
#' function. Its contain all the parameter for the plot.
#' 
#' You could add additional layer to the plot using "+" with ggplot2 options
#'
#' @return plot
#'
#' @import dplyr
#' @importFrom grDevices colorRampPalette colors
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' 
#' fb <- potato
#' 
#' yrs <- yupana_analysis(data = fb
#'                        , response = "hi"
#'                        , model_factors = "geno*treat"
#'                        , comparison = c("geno", "treat")
#'                        )
#' 
#' yrs$meancomp %>% 
#'   plot_smr(type = "bar"
#'            , x = "geno"
#'            , y = "hi"
#'            , xlab = ""
#'            , group = "treat"
#'            , glab = "Tratamientos"
#'            , error = "ste"
#'            , sig = "sig"
#'            #, ylimits = c(0, 1, 0.2)
#'            , color = T #c("red", "black")
#'            , gtext = c("Irrigado", "Sequia")
#'            )
#'            
#' yrs$meancomp %>% 
#'   plot_smr(type = "bar"
#'            , x = "treat"
#'            , y = "hi"
#'            , group = "geno"
#'            , glab = "Genotipo"
#'            , error = "ste"
#'            , sig = "sig"
#'            )
#'            
#' }
#' 

plot_smr <- function(data
                     , type = NULL
                     , x = NULL
                     , y = NULL
                     , group = NULL
                     , xlab = NULL
                     , ylab = NULL
                     , glab = NULL
                     , ylimits = NULL
                     , xrotation = c(0, 0.5, 0.5)
                     , xtext = NULL
                     , gtext = NULL
                     , legend = "top"
                     , sig = NULL
                     , sigsize = 3
                     , error = NULL
                     , color = TRUE
                     , opt = NULL
                     ) {
  
  
  if (FALSE) {
    
    data <- yrs$meancomp
    type = NULL
    x = NULL
    y = NULL
    group = NULL
    xlab = NULL
    ylab = NULL
    glab = NULL
    ylimits = NULL
    xrotation = c(0, 0.5, 0.5)
    xtext = NULL
    gtext = NULL
    legend = "top"
    sig = NULL
    sigsize = 3
    error = NULL
    color = TRUE
    opt = NULL
    
    
  }
  
# match args --------------------------------------------------------------

legend <- match.arg(legend, c("top", "left", "right", "bottom", "none"))
type <- match.arg(type, c("barra", "linea"))

if(!c(x %in% colnames(data))) stop("colum no exist")
if(!c(y %in% colnames(data))) stop("colum no exist")

# -------------------------------------------------------------------------

group <- if(is.null(group) || group == "") {x} else {group}

xlab <- if(is.null(xlab) || is.na(xlab) ) {NULL} else {xlab}
ylab <- if(is.null(ylab) || is.na(ylab) ) {NULL} else {ylab}
glab <- if(is.null(glab) || is.na(glab) ) {NULL} else {glab}
opt <- if(is.null(opt) || is.na(opt) || opt == "") {NULL} else {opt}
sig <- if(is.null(sig) || is.na(sig) || sig == "" || sig == "none") {NULL} else {sig}
error <- if(is.null(error) || is.na(error) || error == "" || error == "none") {NULL} else {error}

color <- if(length(color) <= 1 && (is.null(color) || is.na(color) || color == "" || color == "yes")) {
  TRUE} else {color}

ylimits <- if(any(is.null(ylimits)) || any(is.na(ylimits)) || any(ylimits == "")) { 
  NULL
  } else if(is.character(ylimits)) {
    ylimits %>%
          gsub("[[:space:]]", "", .) %>%
          strsplit(., "[*]") %>%
          unlist() %>% as.numeric()
    } else {ylimits}

xtext <- if(length(xtext) <= 1 && (is.null(xtext) || is.na(xtext) || xtext == "")) {
  NULL} else if (is.character(xtext)){ 
    xtext %>%
      strsplit(., ",") %>%
      unlist() %>% 
      base::trimws()
  } else {xtext}

gtext <- if (length(gtext) <= 1 && (is.null(gtext) || is.na(gtext) || gtext == "")) {
  NULL} else if (is.character(gtext)){ 
    gtext %>%
      strsplit(., ",") %>%
      unlist() %>% 
      base::trimws()
  } else {gtext}

xrotation <- if(any(is.null(xrotation)) || any(is.na(xrotation)) || any(xrotation == "")) {
  c(0, 0.5, 0.5)
  } else if (is.character(xrotation)){ 
    xrotation %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else {xrotation}

# graph-color -------------------------------------------------------------

color <- if (isTRUE(color)) {
  paleta()[as.numeric(as.factor(unique(data[[group]])))]
} else if (isFALSE(color)) {
  grDevices::gray.colors(n = nlevels(as.factor(data[[group]])), start = 0.8, end = 0.3)
} else {
  color
}

# sci-labels --------------------------------------------------------------

if ( !is.null(xlab) ) { 
  
  xlab <- xlab %>%
    gsub(pattern = " ", "~", .)
  xlab <- eval(expression(parse(text = xlab)))
  
}

if ( !is.null(ylab) ) { #
  
  ylab <- ylab %>%
    gsub(pattern = " ", "~", .)
  
  ylab <- eval(expression(parse(text = ylab)))
  
}

if ( !is.null(glab) ) {
  
  glab <- glab %>%
    gsub(pattern = " ", "~", .)
  glab <- eval(expression(parse(text = glab)))
  
} 

lab_x <- if(is.null(xlab)) x else xlab
lab_y <- if(is.null(ylab)) y else ylab
lab_group <- if(is.null(glab)) group else glab

# type --------------------------------------------------------------------

plotdt <- data %>% 
  select(!starts_with("{") | !ends_with("}")) %>%
  select_if(~ !all(is.na(.))) %>%
  drop_na(names(.[1])) %>% 
  mutate(across(c({{group}}), as.factor))

# bar plot ----------------------------------------------------------------

if(type == "barra") {
  
  plot <- plotdt %>% 
    ggplot(., aes(x = .data[[x]]
                  , y = .data[[y]]
                  , fill = .data[[group]])
           ) +
    
    geom_col(
      position = position_dodge2(preserve = "single")
      , colour = "black"
      , linewidth = 0.4
      , na.rm = T
    ) +
    labs(
      x = lab_x
      , y = lab_y
      , fill = lab_group
    ) +
    
    {
      if (!is.null(error)) 
        geom_errorbar(
          aes(ymin = .data[[y]] - .data[[error]]
              , ymax = .data[[y]] + .data[[error]] )
          , position = position_dodge(width = 0.9)
          , width = 0.15
          , na.rm = T) 
      
    } +
    {
      if (!is.null(sig) )  
        
        geom_text(
          aes(label = .data[[sig]]
              , y = if(!is.null(error)) .data[[y]] + .data[[error]] else .data[[y]])
          , position = position_dodge(width = 0.9)
          , na.rm = T
          , colour = "black"
          , vjust = -0.5
          , hjust = 0.5
          , angle = 0
          , size = sigsize
          ) 
    } +
    scale_fill_manual(values = color
                      , labels = if(!is.null(gtext)) gtext else waiver()) 
}

# line plot ---------------------------------------------------------------

if (type == "linea") {
  
  plot <- plotdt %>% 
    ggplot( aes(x = .data[[x]]
                , y = .data[[y]]
                , shape = .data[[group]]
                , colour = .data[[group]]
    ) ) +
    
    geom_point( aes(group =  .data[[group]]
                    , shape = .data[[group]]
                    , color = .data[[group]]
    ), size = 2.5 ) +
    
    geom_line( aes( group =  .data[[group]]
                    , color = .data[[group]]
                    , linetype = .data[[group]]
    ) ,  linewidth = 1 ) +
    labs(x = lab_x
         , y = lab_y
         , shape = lab_group
         , color = lab_group
         , linetype = lab_group
    ) +
    
    {
      if (!is.null(error))  
        geom_errorbar(aes(ymin = .data[[y]] - .data[[error]]
                          , ymax = .data[[y]] + .data[[error]])
                      , width = 0.08) 
    } +
    
    {
      if (!is.null(sig))  
        
        geom_text(
          aes(label = .data[[sig]]
              , y = if(!is.null(error)) .data[[y]] + .data[[error]] else .data[[y]])
          , colour = "black"
          , vjust = -0.3
          , hjust = 0.3
          , angle = 0
          , na.rm = T
          , size = sigsize
          # , position = position_jitter(width = 0.03, height = 0.03)
        ) 
        
        
    } +
    
    scale_color_manual(
      labels = if(!is.null(gtext)) gtext else waiver()
      , values = color
    ) + 
    scale_linetype_discrete(labels = if(!is.null(gtext)) gtext else waiver()) +
    scale_shape_discrete(labels = if(!is.null(gtext)) gtext else waiver())
  
}

# layers ------------------------------------------------------------------

graph <- plot + 
  { if(!is.null(xtext)) scale_x_discrete(labels = xtext) } +
  {
    
    if (!is.null(type)) {
      
      if (type == "linea") {
        
        if (!is.null(ylimits)) {

          scale_y_continuous(
            limits = ylimits[1:2], 
            breaks = seq(ylimits[1], ylimits[2], by = abs(ylimits[3])),
            expand = c(0, 0))
          
        } else { 
          
          scale_y_continuous(expand = expansion(mult = c(0.3, 0.3)))
          
          }
        
      } else if (type == "barra") {
        
        if (!is.null(ylimits)) {
          # Configurar el eje Y para gráficos de barra cuando hay límites definidos
          scale_y_continuous(
            limits = ylimits[1:2], 
            breaks = seq(ylimits[1], ylimits[2], by = abs(ylimits[3])),
            expand = c(0, 0)
          )
        } else {
          # Definir expansión para gráficos de barra cuando 'ylimits' es NULL
          ymin <- min(plotdt[[y]], na.rm = TRUE)
          ymax <- max(plotdt[[y]], na.rm = TRUE)
          
          if (ymin < 0 & ymax > 0) {
            # Si hay valores positivos y negativos, expandir 0.3 a ambos lados
            scale_y_continuous(expand = expansion(mult = c(0.3, 0.3)))
            
          } else if (ymin >= 0) {
            # Si solo hay valores positivos, expandir solo en la parte superior
            scale_y_continuous(expand = expansion(mult = c(0, 0.3)))
            
          } else {
            # Si solo hay valores negativos, expandir solo en la parte inferior
            scale_y_continuous(expand = expansion(mult = c(0.3, 0)))
          }
        }
      }
  }
}   
    

layers <- 'graph +
  theme_minimal() +
  theme(legend.position = legend
    , panel.border = element_rect(colour = "black", fill=NA)
    , panel.background = element_rect(fill = "transparent")
    , legend.background = element_rect(colour = "transparent", fill = "transparent")
    , axis.text.x = element_text(angle = xrotation[1]
                                 , hjust= xrotation[2]
                                 , vjust = xrotation[3])
    )'

if(is.null(opt)) {
  eval(parse(text = layers)) 
} else {
  eval(parse(text = paste(layers, opt, sep = " + ")))
}

}
