library(hexSticker)
library(magick)

logo <- list.files("C:/Users/User/git/inti/pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "inkaverse.jpg"
                   ) %>% 
  image_read() 

sticker(subplot = logo
        , white_around_sticker = TRUE
        , s_x = 1.0
        , s_y = 0.84
        , s_width = 1.65
        , s_height = 1.65
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
