library(hexSticker)
library(magick)
library(showtext)

#> https://fonts.google.com/

logo <- list.files("C:/Users/User/git/inti/pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "inkaverse.jpg"
                   ) %>% 
  image_read() 


# -------------------------------------------------------------------------
# logo --------------------------------------------------------------------
# -------------------------------------------------------------------------

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.0
        , s_y = 0.81
        , s_width = 1.7
        , s_height = 1.7
        , package = ""
        , h_color = "#6b594b"
        , h_fill = "#ffe701"
        , h_size = 1
        , url = "inkaverse.com"
        , u_size = 8
        , u_angle = 90
        , u_x = 1.8
        , u_y = 0.7
        , filename = "man/figures/logo.png"
        )

#> Fechas en el quipo

# 2007    00-x-x--0000000-- (verde)          (ingreso a la universidad) 
# 2010    00-x-0----------- (naranja)        (Descubrí mi vocación)
# 2012    00-x-0--00------- (azul)           (Kelvin el sensei)
# 2016    00-x-0--000000--- (rosa/violeta)   (Nacimiento de Alice/idea proj.)
# 2020    00-x-00---------- (rojo)           (Lanzamiento en CRAN- pandemia)

# -------------------------------------------------------------------------
# inti --------------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("C:/Users/User/git/inti/pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "inti_lines"
) %>% 
  image_read()

font <- "Ewert"

font_add_google(font, "logo")
showtext_auto()

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.0
        , s_y = 0.81
        , s_width = 1
        , s_height = 1
        , package = "INTI"
        , p_color = "red"
        , p_size = 15
        , p_family = "logo"
        , h_color = "#6b594b"
        , h_fill = "#ffe701"
        , h_size = 1
        , url = "inkaverse.com"
        , u_size = 8
        , u_angle = 90
        , u_x = 1.8
        , u_y = 0.7
        , filename = "man/figures/inti.png"
)

# -------------------------------------------------------------------------
# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("C:/Users/User/git/inti/pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "yupana_graph"
                   ) %>% 
  image_read()

font <- "Ewert"

font_add_google(font, "logo")
showtext_auto()

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.0
        , s_y = 0.81
        , s_width = 1
        , s_height = 1
        , package = "YUPANA"
        , p_color = "red"
        , p_size = 15
        , p_family = "logo"
        , h_color = "#6b594b"
        , h_fill = "#ffe701"
        , h_size = 1
        , url = "inkaverse.com"
        , u_size = 8
        , u_angle = 90
        , u_x = 1.8
        , u_y = 0.7
        , filename = "man/figures/yupana.png"
        )

# -------------------------------------------------------------------------
# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("C:/Users/User/git/inti/pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "tarpuy_lines"
                   ) %>% 
  image_read()

font <- "Ewert"

font_add_google(font, "logo")
showtext_auto()

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.0
        , s_y = 0.81
        , s_width = 1
        , s_height = 1
        , package = "TARPUY"
        , p_color = "red"
        , p_size = 15
        , p_family = "logo"
        , h_color = "#6b594b"
        , h_fill = "#ffe701"
        , h_size = 1
        , url = "inkaverse.com"
        , u_size = 8
        , u_angle = 90
        , u_x = 1.8
        , u_y = 0.7
        , filename = "man/figures/tarpuy.png"
        )








