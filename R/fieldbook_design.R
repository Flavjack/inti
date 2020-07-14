#' Field book experimental designs
#'
#' Function to deploy experimental designs based in agricolae package.
#' 1. Field book for one and multiple factors
#' 2. Sketch design.
#'
#' @param data Experimental design data frame with the factors and level. See examples.
#' @param nFactors Number of factor in the experiment  (default = 1). See details.
#' @param type Type of experimental arrange  (default = "crd"). See details.
#' @param rep  Number of replications in the experiment (default = 3).
#' @param serie Digits in the plot id (default = 2).
#' @param seed Replicability of draw results (default = 0) ~ always random. See details.
#'
#' @details The function allows to include the arguments in the sheet that have the information of the design.
#' You should include 2 columns in the sheet: "\code{[arguments]}" and "\code{[values]}". See examples.
#' The information will be extracted automatically and deploy the design.
#'
#' nFactor = 1
#' - crd
#' - rcbd
#' - lsd
#' - lattice
#'
#' nFactor = 2 (factorial)
#' - split-crd
#' - split-rcbd
#'
#' nFactors >= 2 (factorial)
#' factorial .: crd, rcbd, lsd
#'
#' @return A list with two objects:
#'
#' 1. fieldbook design
#'
#' 2. field design (sketch)
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @import dplyr
#' @importFrom purrr pluck as_vector
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom utils tail
#' @importFrom purrr discard
#'
#' @source
#'
#' https://tarwi.lamolina.edu.pe/~fmendiburu/
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
#' , "1ilw0NHT7mihaM-3U48KzkuMt927xe8ukX6rNuIw2fT0/edit#gid=0")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (data <- gs %>%
#'     range_read("tarpuy"))
#'
#' data %>% fieldbook_design()
#'
#' }
#'
#' @export

fieldbook_design <- function(data,
                             nFactors = 1,
                             type = c(
                               "crd", "rcbd", "lsd", "lattice",
                               "split-crd", "split-rcbd"
                               ),
                             rep = 3,
                             serie = 2,
                             seed = 0) {

  plots <- Row.names <- NULL

# arguments ---------------------------------------------------------------
# -------------------------------------------------------------------------

type <- match.arg(type)

data_fb <- data %>%
  select_if(~ !all(is.na(.))) %>%
  rename_with(~ gsub("\\s+|\\.", "_", .)) %>%
  mutate(across(everything(), ~ gsub("\\s+|\\.", "_", .))) %>%
  dplyr::tibble()

treatments_names <- data_fb %>%
  select(!starts_with("[") | !ends_with("]")) %>%
  names()

treatments_levels <- data_fb %>%
  select({{ treatments_names }}) %>%
  as.list() %>%
  purrr::map(discard, is.na)

if ( "[argument]" %in% colnames(data_fb) ) {

  arguments <- data_fb %>%
    select(starts_with("[") | ends_with("]")) %>%
    rename_with(~ gsub("\\[|\\]", "", .)) %>%
    drop_na() %>%
    tibble::deframe()

  } else { arguments <- data.frame() }

if ("nFactors" %in% names(arguments)) {
  nFactors <- arguments %>%
    pluck("nFactors") %>%
    as.numeric()
} else {
  nFactors
}

if ("type" %in% names(arguments)) {
  type <- arguments %>% pluck("type")
} else {
  type
}

if ("rep" %in% names(arguments)) {
  rep <- arguments %>%
    pluck("rep") %>%
    as.numeric()
} else {
  rep
}

if ("serie" %in% names(arguments)) {
  serie <- arguments %>%
    pluck("serie") %>%
    as.numeric()
} else {
  serie
}

if ("seed" %in% names(arguments)) {
  seed <- arguments %>%
    pluck("seed") %>%
    as.numeric()
} else {
  seed
}

treat_name <- names(treatments_levels)[1:nFactors]
treat_fcts <- treatments_levels[treat_name]

# Factor = 1 --------------------------------------------------------------
# -------------------------------------------------------------------------

          if (nFactors == 1) {

            onefact <- treat_fcts %>% pluck(1)

            if (type == "crd") {
              design <- agricolae::design.crd(
                trt = onefact,
                r = rep,
                serie = serie,
                seed = seed
              )

              result <- design %>%
                pluck("book") %>%
                dplyr::rename({{ treat_name }} := "onefact")
            }

            if (type == "rcbd") {
              design <- agricolae::design.rcbd(
                trt = onefact,
                r = rep,
                serie = serie,
                seed = seed
              )
              result <- list(
                design = design %>%
                  pluck("book") %>%
                  dplyr::rename({{ treat_name }} := "onefact"),
                sketch = design %>% pluck("sketch")
              )
            }

            if (type == "lsd") {

              design <- agricolae::design.lsd(
                trt = onefact,
                r = rep,
                serie = serie,
                seed = seed
              )
              result <- list(
                design = design %>%
                  pluck("book") %>%
                  dplyr::rename({{ treat_name }} := "onefact"),
                sketch = design %>% pluck("sketch")
              )
            }

            if (type == "lattice") {
              design <- agricolae::design.lattice(
                trt = onefact,
                r = rep,
                serie = serie,
                seed = seed
              )

              result <- list(
                design = design$book,
                sketch = design$sketch %>% as.data.frame()
              )
            }

          }

# Factor >= 2 -------------------------------------------------------------
# -------------------------------------------------------------------------

        if( nFactors == 2 & startsWith(type, "split") ) {

# split-plot --------------------------------------------------------------
# -------------------------------------------------------------------------

          twofact_lvl <- treat_fcts[1:2]
          treat_name <- twofact_lvl %>% names()
          fact1 <- twofact_lvl %>% pluck(1)
          fact2 <- twofact_lvl %>% pluck(2)

          if (type == "split-crd") {

            design <- agricolae::design.split(
              trt1 = fact1,
              trt2 = fact2,
              r = rep,
              design = "crd",
              serie = serie,
              seed = seed
            )

            result <- design %>%
              pluck("book") %>%
              rename_with(~ {{ treat_name }}, tail(names(.), 2))
          }

          if (type == "split-rcbd") {

            design <- agricolae::design.split(
              trt1 = fact1,
              trt2 = fact2,
              r = rep,
              design = "rcbd",
              serie = serie,
              seed = seed
            )

            result <- design %>%
              pluck("book") %>%
              rename_with(~ {{ treat_name }}, tail(names(.), 2))
          }

        }

# factorial ---------------------------------------------------------------
# -------------------------------------------------------------------------

        if ( nFactors >= 2 && ( type == "crd" | type == "rcbd" | type == "lsd" ) ) {

          treat_lvls <- lengths(treat_fcts)

          design <- agricolae::design.ab(
            trt = treat_lvls,
            r = rep,
            serie = serie,
            design = type,
            seed = seed
          )

          # rename cols -------------------------------------------------------------
          # -------------------------------------------------------------------------

          col_rnm <- function(renamed_fb, treat, new_names) {
            oldn <- renamed_fb %>%
              select(treat) %>%
              unique() %>%
              as_vector()

            names <- structure(as.character(new_names),
                               names = as.character(oldn)
            )

            renamed_fb %>%
              mutate_at({{ treat }}, ~ recode(., !!!names)) %>%
              select({{ treat }})
            }

          # -------------------------------------------------------------------------

          renamed_fb <- design %>%
            pluck("book") %>%
            rename_with(~ {{ treat_name }}, tail(names(.), nFactors))

          ini <- length(renamed_fb) - nFactors + 1
          fin <- length(renamed_fb)

          fb_recoded <- lapply(ini:fin, function(x) {
            renamed_fb %>%
              col_rnm(.,
                      treat = colnames(.)[x],
                      new_names = treat_fcts[[colnames(.)[x]]]
              )
            })

          result <- do.call(cbind, fb_recoded) %>%
            tibble() %>%
            merge(renamed_fb %>% select(-{{ treat_name }}),
                  .,
                  by = 0) %>%
            dplyr::arrange(plots) %>%
            select(-Row.names)
          }

  # result ------------------------------------------------------------------
  # -------------------------------------------------------------------------

  return(result)

}

