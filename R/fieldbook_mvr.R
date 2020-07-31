#' Multivariate Analysis
#'
#' Mutilvariate analysis: PCA and HCPC
#'
#' @param data Field book data.
#' @param fb_smr Summary of the variables in the fieldbook.
#' @param quali_sup Variables for group the analysis.
#'
#' @details
#'
#' Compute and plot information for multivarite analysis
#'
#' @return result and plots
#'
#' @author
#'
#' Flavio Lozano-Isla
#'
#' @import dplyr
#' @importFrom FactoMineR HCPC PCA
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' library(googlesheets4)
#' library(tidyverse)
#' library(FactoMineR)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs/edit#gid=172957346")
#' # browseURL(url)
#' gs <- as_sheets_id(url)
#'
#' (data <- gs %>%
#'     range_read("fb"))
#'
#' (fb_smr <- gs %>%
#'   range_read("fbsm"))
#'
#'
#' mvr <- fieldbook_mvr(data, fb_smr
#' , quali_sup = c("genotype", "treat"))
#'
#' FactoMineR::plot.PCA(mvr$pca)
#' FactoMineR::plot.HCPC(mvr$hcpc)
#'
#' }
#'
#' @export

fieldbook_mvr <- function(data
                          , fb_smr
                          , quali_sup
                            ) {

  where <- NULL

  # fieldbook structure -----------------------------------------------------
  # -------------------------------------------------------------------------

  factor_list <- fb_smr %>%
    filter(.data$type %in% "factor") %>%
    select(where(~!all(is.na(.))))

  factor_opt <- factor_list %>%
    select(!.data$type) %>%
    deframe()

  vars_num <- fb_smr %>%
    filter(.data$type %in% "numeric")

  vars_cha <- fb_smr %>%
    filter(.data$type %in% "character")

  fb <- data %>%
    select(where(~!all(is.na(.)))) %>%
    mutate(across( factor_list[["variables"]], as.character)) %>%
    mutate(across( factor_list[["variables"]], as.factor)) %>%
    mutate(across( vars_num[["variables"]], as.numeric)) %>%
    mutate(across( vars_cha[["variables"]], as.character)) %>%
    select({{quali_sup}}, vars_num[["variables"]]) %>%
    group_by( across( {{quali_sup}} )) %>%
    summarize(across(everything(),  ~ mean(., na.rm = TRUE) ))

# pca ---------------------------------------------------------------------
# -------------------------------------------------------------------------

  quali_ncol <- which(names(fb) %in% {{quali_sup}})

  pca_info <- fb %>%
    PCA(X = .
        , scale.unit = T
        , quali.sup = quali_ncol
        , graph = FALSE)

# hcpc --------------------------------------------------------------------
# -------------------------------------------------------------------------

  clt_info <- pca_info %>%
    HCPC(res = .
         , nb.clust=-1
         , graph = FALSE)


# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  multvr = list(pca = pca_info, hcpc = clt_info, fb_mvr = fb)

}
