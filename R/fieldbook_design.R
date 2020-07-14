
fieldbook_design <- function(data,
                             nFactors = NULL,
                             type = c(
                               "crd", "rcbd", "lattice",
                               "split-crd", "split-rcbd",
                               ),
                             rep = NULL,
                             series = 2,
                             seed = 0) {

  # arguments ---------------------------------------------------------------

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
  map(discard, is.na)

arguments <- data_fb %>%
  select(starts_with("[") | ends_with("]")) %>%
  rename_with(~ str_replace_all(., "\\[|\\]", "")) %>%
  drop_na() %>%
  tibble::deframe()

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

if ("series" %in% names(arguments)) {
  series <- arguments %>%
    pluck("series") %>%
    as.numeric()
} else {
  series
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
                serie = series,
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
                serie = series,
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
                serie = series,
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

        if(nFactors >= 2) {

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
              serie = series,
              seed = seed
            )

            result <- design %>%
              pluck("book") %>%
              rename_with(~ {{ treat_name }}, tail(names(.), 2))
          }

          if (type == "split-rcbd") {

            twofact_lvl <- treat_fcts[1:2]
            treat_name <- twofact_lvl %>% names()
            fact1 <- twofact_lvl %>% pluck(1)
            fact2 <- twofact_lvl %>% pluck(2)

            design <- agricolae::design.split(
              trt1 = fact1,
              trt2 = fact2,
              r = rep,
              design = "rcbd",
              serie = series,
              seed = seed
            )

            result <- design %>%
              pluck("book") %>%
              rename_with(~ {{ treat_name }}, tail(names(.), 2))
          }

        }

# factorial ---------------------------------------------------------------
# -------------------------------------------------------------------------

        if (type == "factorial") {


          treat_lvls <- lengths(treat_fcts)

          design <- agricolae::design.ab(
            trt = treat_lvls,
            r = rep,
            serie = series,
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
            select(-starts_with("Row"))
          }

  # result ------------------------------------------------------------------
  # -------------------------------------------------------------------------

  return(result)

}

source("http://lozanoisla.com/setup.r")
url <- "https://docs.google.com/spreadsheets/d/1ilw0NHT7mihaM-3U48KzkuMt927xe8ukX6rNuIw2fT0/edit#gid=0"
# browseURL(url)
gs <- as_sheets_id(url)

(data <- gs %>%
  range_read("tarpuy"))

data %>% fieldbook_design()

