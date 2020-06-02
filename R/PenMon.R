#' Penman-Monteith method
#'
#' @description Function for calculate the daily evapotranspiration.
#' @param data data frame with the metrerological information.
#' @param date  date of the weather measure. format: yyy-mm-dd.
#' @param Tmin temperatura minima (ºC).
#' @param Tmax temperatura maxima (ºC).
#' @param RHmin humedad relativa minima (\%).
#' @param RHmax humedad relativa maxima (\%).
#' @param sunshine sunshine (hours).
#' @param wind wind velocity (m.seg-1).
#' @param lat latitud (deciaml radians).
#' @param alt altitud (meters)
#' @param Hws heigth of data collect for wind speed (meters).
#' @param alb albedo
#' @return data frame with daily evapotranspiration
#' @importFrom lubridate yday
#' @importFrom zoo as.Date
#' @export


PenMon <- function(data, date, Tmin, Tmax, RHmin, RHmax, sunshine, wind, lat, alt, Hws, alb = 0.23) {


    data <- as.data.frame(data)

    date <- data[, date] %>% zoo::as.Date()
    Tmin <- data[, Tmin] %>% as.numeric()
    Tmax <- data[, Tmax] %>% as.numeric()
    RHmin <- data[, RHmin] %>% as.numeric()
    RHmax <- data[, RHmax] %>% as.numeric()
    sunshine <- data[, sunshine] %>% as.numeric()
    wind <- data[, wind] %>% as.numeric()
    lat <- data[, lat] %>% as.numeric()
    alt <- data[, alt] %>% as.numeric()
    Hws <- data[, Hws] %>% as.numeric()


    # wind <> 2 m
    # -------------------------------------------------------------

    # Ec. 47

    uz <- wind * ((4.87)/(log(67.8 * Hws - 5.42)))


    # Temperature media
    # -------------------------------------------------------

    # Ec. 9

    Tm <- (Tmax + Tmin)/2

    # pressure
    # ----------------------------------------------------------------

    # Ec. 7

    P <- 101.3 * ((293 - 0.0065 * alt)/(293))


    # Constante psicrometrica
    # -------------------------------------------------

    # Ec. 8

    kp <- (1.013 * 10^(-3) * P)/(2.45 * 0.622)


    # Pendiente de la curva de presion de saturacion de vapor
    # -----------------

    # Ec. 13

    psv <- (4098 * (0.6108 * exp((17.27 * Tm)/(Tm + 237.3))))/(Tm + 237.3)^2


    # Deficit de presion de vapor
    # ---------------------------------------------

    # Ec. 11

    tx <- 0.6108 * exp((17.27 * Tmax)/(Tmax + 237.3))
    tm <- 0.6108 * exp((17.27 * Tmin)/(Tmin + 237.3))

    es <- (tx + tm)/2

    ea <- (tm * (RHmax/100) + tx * (RHmin/100))/2

    # Ec. 17

    dpv <- es - ea


    # Radiation
    # ---------------------------------------------------------------


    J <- lubridate::yday(date)

    # Ec. 23

    dr <- 1 + 0.033 * cos((2 * pi/365) * J)

    # Ec. 24

    ds <- 0.409 * sin(((2 * pi)/(365)) * J - 1.39)

    # Ec. 25

    ws <- acos(-tan(lat) * tan(ds))

    # Ec. 21

    Ra <- ((24 * 60)/pi) * 0.082 * dr * (ws * sin(lat) * sin(ds) + cos(lat) *
        cos(ds) * sin(ws))


    # Ec. 34

    N <- (24/pi) * ws

    # Ec. 35

    Rs <- (0.25 + 0.5 * (sunshine/N)) * Ra

    # Ec. 37

    Rso <- (0.75 + 2 * 10^(-5) * alt) * Ra

    # Ec. 38

    Rns <- (1 - alb) * Rs

    # Ec. 39

    Rnl <- ((4.903 * 10^(-9)) * (((Tmax + 273.16)^4 + (Tmin + 273.16)^4)/2)) *
        (0.34 - 0.14 * sqrt(ea)) * (1.35 * (Rs/Rso) - 0.35)

    # Ec. 40

    Rn <- Rns - Rnl

    # Ec. 42 Gday = 0

    a <- psv/(psv + kp * (1 + 0.34 * uz))

    b <- kp/(psv + kp * (1 + 0.34 * uz))

    c <- (900/(Tm + 273)) * uz


    et <- 0.408 * Rn * a + c * dpv * b


}
