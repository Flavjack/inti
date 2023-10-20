# -------------------------------------------------------------------------
# inkaverse ---------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

font <- c("Permanent Marker")

huito_fonts(font)

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "png|jpg") 

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = "#ffe701"
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/logo_inkaverse.jpg" 
                , size = c(3.97, 3.97)
                , position = c(2.55, 2.06)
                ) %>%
  include_shape(size = 5.08
                , border_width = 3
                , border_color = "#505456"
                , position = c(2.54, 2.54)
                , panel_color = "pink"
                ) %>%
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.6, 0.75)
               , angle = 30
               , color = "white"
               , font
               )

label %>% label_print()

logo <- label %>% 
  label_print(filename = tempfile()
              , margin = 0
              , paper = c(5.5, 5.5)
              , mode = "complete"
              )

sticker <- logo %>%
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent('pink') %>% 
  image_write("pkgdown/favicon/img/inkaverse.png")

#> Fechas en el quipo

# 2007    00-x-x--0000000-- (verde)          (ingreso a la universidad) 
# 2010    00-x-0----------- (naranja)        (Descubrí mi vocación)
# 2012    00-x-0--00------- (azul)           (Kelvin el sensei)
# 2016    00-x-0--000000--- (rosa/violeta)   (Nacimiento de Alice/idea proj.)
# 2020    00-x-00---------- (rojo)           (Lanzamiento en CRAN pandemia)

# -------------------------------------------------------------------------
# inti --------------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = "#ffe701"
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/logo_inti.jpg" 
                , size = c(3.97, 3.97)
                , position = c(2.54, 2.35)
  ) %>%
  include_shape(size = 5.08
                , border_width = 3
                , border_color = "#505456"
                , position = c(2.54, 2.54)
                , panel_color = "pink"
  ) %>%
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.6, 0.75)
               , angle = 30
               , color = "white"
               , font
               ) 

label %>% label_print()

logo <- label %>% 
  label_print(filename = tempfile()
              , margin = 0
              , paper = c(5.5, 5.5)
              , mode = "complete"
              )

sticker <- logo %>%
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent('pink') %>% 
  image_write("pkgdown/favicon/img/inti.png")

file.copy("pkgdown/favicon/img/inti.png", "man/figures/logo.png"
          , overwrite = T)

# -------------------------------------------------------------------------
# yupana ------------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = "#ffe701"
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/logo_yupana.jpeg" 
                , size = c(4.4, 4.4)
                , position = c(2.54, 2)
                ) %>%
  include_shape(size = 5.08
                , border_width = 3
                , border_color = "#505456"
                , position = c(2.54, 2.54)
                , panel_color = "pink"
                ) %>%
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.6, 0.75)
               , angle = 30
               , color = "white"
               , font
               ) 

label %>% label_print()

logo <- label %>% 
  label_print(filename = tempfile()
              , margin = 0
              , paper = c(5.5, 5.5)
              , mode = "complete"
              )

sticker <- logo %>%
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent('pink') %>% 
  image_write("pkgdown/favicon/img/yupana.png")

# -------------------------------------------------------------------------
# tarpuy ------------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = "#ffe701"
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/logo_tarpuy.jpg" 
                , size = c(3.95, 3.95)
                , position = c(2.57, 2.21)
                ) %>%
  include_shape(size = 5.08
                , border_width = 3
                , border_color = "#505456"
                , position = c(2.54, 2.54)
                , panel_color = "pink"
                ) %>%
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.6, 0.75)
               , angle = 30
               , color = "white"
               , font
               ) 

label %>% label_print()

logo <- label %>% 
  label_print(filename = tempfile()
              , margin = 0
              , paper = c(5.5, 5.5)
              , mode = "complete"
              )

sticker <- logo %>%
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent('pink') %>% 
  image_write("pkgdown/favicon/img/tarpuy.png")

# -------------------------------------------------------------------------
# quipolab ----------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

font <- c("Righteous", "Fredoka One")

huito_fonts(font)

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = NA
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/quipo_icon.png" 
                , size = c(1.5, 1.5)
                , position = c(1.45, 3)
                ) %>%
  include_shape(size = 5.08
                , border_width = 3
                , border_color = "black"
                , position = c(2.54, 2.54)
                , panel_color = "pink"
                ) %>%
  include_image(value = "pkgdown/favicon/img/bulb.png" 
                , size = c(1.1, 1.1)
                , position = c(2.99, 3.54)
                , opts = 'image_transparent("white")'
                ) %>%
  include_text(value = "uipo"
               , size = 32
               , position = c(3.35, 2.75)
               , font = "Righteous"
               ) %>% 

  include_text(value = "Lab"
               , size = 25
               , position = c(2.89, 1.75)
               , font = "Fredoka One"
               , color = "#0a3faf"
               ) %>% 
  include_text(value = "quipolab.com"
               , size = 6
               , position = c(3.6, 0.75)
               , angle = 29
               , font = "Righteous"
               , color = "white") 

label %>% label_print()

logo <- label %>% 
  label_print(filename = tempfile()
              , margin = 0
              , paper = c(5.5, 5.5)
              , mode = "complete")

sticker <- logo %>%
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent('pink') %>% 
  image_write("pkgdown/favicon/img/quipolab.png")


# -------------------------------------------------------------------------
# slogan ------------------------------------------------------------------
# -------------------------------------------------------------------------

font <- c("Special Elite")

huito_fonts(font)

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "png|jpg") 

label <- label_layout(size = c(20, 5)
                      , border_color = NA
                      , border_width = 1
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/inkaverse.png"
                , size = c(4.5, 4.5)
                , position = c(2.1, 2.5)
                ) %>% 
  include_text(value = "Flavio L."
               , size = 90
               , position = c(11.8, 2.5)
               , angle = 0
               , color = "black"
               , font
               ) 

label %>% label_print()

logo <- label %>% 
  label_print(filename = tempfile()
              , margin = 0
              , paper = c(20, 5)
              , mode = "complete"
              )

sticker <- logo %>%
  image_read_pdf()  %>% 
  image_write("pkgdown/favicon/img/slogan.png")

