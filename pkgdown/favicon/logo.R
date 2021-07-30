library(hexSticker)
library(magick)
library(showtext)
library(dplyr)

# open https://fonts.google.com/
# font poma

# -------------------------------------------------------------------------
# inkaverse ---------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "logo_inkaverse.jpg"
                   ) %>% 
  image_read() 

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
        , filename = "pkgdown/favicon/img/inkaverse.png"
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

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "logo_inti.jpg"
                   ) %>% 
  image_read()

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.0
        , s_y = 0.90
        , s_width = 1.7
        , s_height = 1.7
        , package = ""
        , p_color = "red"
        , p_size = 15
        , h_color = "#6b594b"
        , h_fill = "#ffe701"
        , h_size = 1
        , url = "inkaverse.com"
        , u_size = 8
        , u_angle = 90
        , u_x = 1.8
        , u_y = 0.7
        , filename = "pkgdown/favicon/img/inti.png"
        )

file.copy(from = "pkgdown/favicon/img/inti.png"
          , to = "man/figures/logo.png"
          , overwrite = T)

# -------------------------------------------------------------------------
# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "logo_yupana"
                   ) %>% 
  image_read()

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.0
        , s_y = 0.90
        , s_width = 1.7
        , s_height = 1.7
        , package = ""
        , p_color = "red"
        , p_size = 15
        , h_color = "#6b594b"
        , h_fill = "#ffe701"
        , h_size = 1
        , url = "inkaverse.com"
        , u_size = 8
        , u_angle = 90
        , u_x = 1.8
        , u_y = 0.7
        , filename = "pkgdown/favicon/img/yupana.png"
        )

# -------------------------------------------------------------------------
# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "logo_tarpuy"
                   ) %>% 
  image_read()

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.0
        , s_y = 0.87
        , s_width = 1.7
        , s_height = 1.7
        , package = ""
        , p_color = "red"
        , p_size = 15
        , h_color = "#6b594b"
        , h_fill = "#ffe701"
        , h_size = 1
        , url = "inkaverse.com"
        , u_size = 8
        , u_angle = 90
        , u_x = 1.8
        , u_y = 0.7
        , filename = "pkgdown/favicon/img/tarpuy.png"
        )


# -------------------------------------------------------------------------
# quipolab ----------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "logo_quipolab.png"
                   ) %>% 
  image_read()

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.1
        , s_y = 1.0
        , s_width = 2.5
        , s_height = 2.5
        , package = ""
        , p_color = "red"
        , p_size = 15
        , h_color = "#6b594b"
        , h_fill = "#ffe701"
        , h_size = 1
        # , url = "inkaverse.com"
        , u_size = 8
        , u_angle = 90
        , u_x = 1.8
        , u_y = 0.7
        , filename = "pkgdown/favicon/img/quipolab.png"
        )

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "logo_quipolab_w.png"
                   ) %>% 
  image_read()

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.1
        , s_y = 1.0
        , s_width = 2.5
        , s_height = 2.5
        , package = ""
        , p_color = "red"
        , p_size = 15
        , h_color = "#6b594b"
        , h_fill = "#ffe701"
        , h_size = 1
        # , url = "inkaverse.com"
        , u_size = 8
        , u_angle = 90
        , u_x = 1.8
        , u_y = 0.7
        , filename = "pkgdown/favicon/img/quipolab_w.png"
        )
