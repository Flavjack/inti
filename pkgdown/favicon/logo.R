# -------------------------------------------------------------------------
# inkaverse ---------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)
library(tidyverse)

#> open https://github.com/terinjokes/StickersStandard

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "png|jpg") 

# huito_fonts()

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = "#ffe701"
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/logo_inkaverse.jpg" 
                , size = c(3.97, 3.97)
                , position = c(2.55, 2.06)
                ) %>%
  include_shape(size = 4.1
                , border_width = 3
                , border_color = "#505456"
                , margin = -0.8
                , position = c(2.54, 2.54)
                , panel_color = "pink"
                , panel_size = 5.08
                ) %>%
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.6, 0.75)
               , angle = 30
               , color = "white") %>%
  label_print(filename = "pkgdown/favicon/img/inkaverse"
              , margin = 0
              , paper = c(5.5, 5.5)
              , viewer = T
              , smpres = 200
              , mode = "c"
              )

logo <- list.files("pkgdown/favicon/img/"
                   , full.names = T
                   , pattern = "inkaverse.pdf") %>%
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent('pink') %>% 
  image_write("pkgdown/favicon/img/inkaverse.png")

# open man/figures/logo.png

#> Fechas en el quipo

# 2007    00-x-x--0000000-- (verde)          (ingreso a la universidad) 
# 2010    00-x-0----------- (naranja)        (Descubrí mi vocación)
# 2012    00-x-0--00------- (azul)           (Kelvin el sensei)
# 2016    00-x-0--000000--- (rosa/violeta)   (Nacimiento de Alice/idea proj.)
# 2020    00-x-00---------- (rojo)           (Lanzamiento en CRAN- pandemia)

# -------------------------------------------------------------------------
# inti --------------------------------------------------------------------
# -------------------------------------------------------------------------

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = "#ffe701"
) %>% 
  include_image(value = "pkgdown/favicon/img/logo_inti.jpg" 
                , size = c(3.97, 3.97)
                , position = c(2.54, 2.35)
  ) %>%
  include_shape(size = 4.1
                , border_width = 3
                , border_color = "#505456"
                , margin = -0.8
                , position = c(2.54, 2.54)
                , panel_color = "pink"
                , panel_size = 5.08
  ) %>%
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.6, 0.75)
               , angle = 30
               , color = "white") %>%
  label_print(filename = "pkgdown/favicon/img/inti"
              , margin = 0
              , paper = c(5.5, 5.5)
              , viewer = T
              , smpres = 200
              , mode = "c"
              )

logo <- list.files("pkgdown/favicon/img/"
                   , full.names = T
                   , pattern = "inti.pdf") %>%
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent('pink') %>% 
  # image_write("man/figures/logo.png")
  image_write("pkgdown/favicon/img/inti.png")

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

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "yupana.png") %>% 
  image_read()  %>% 
  image_transparent('white') %>% 
  image_write("pkgdown/favicon/img/yupana.png")

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

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "tarpuy.png") %>% 
  image_read()  %>% 
  image_transparent('white') %>% 
  image_write("pkgdown/favicon/img/tarpuy.png")

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

logo <- image_read("pkgdown/favicon/img/quipolab.png")  %>% 
  image_transparent('white') %>% 
  image_write("pkgdown/favicon/img/quipolab.png")

# -------------------------------------------------------------------------

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

logo <- image_read("pkgdown/favicon/img/quipolab_w.png")  %>% 
  image_transparent('white') %>% 
  image_write("pkgdown/favicon/img/quipolab_w.png")

