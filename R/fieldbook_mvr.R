#' Multivariate Analysis
#'
#' Multivariate analysis for PCA and HCPC
#'
#' @param data Field book data.
#' @param fb_smr Summary of the variables in the fieldbook.
#' @param summary_by Variables for group the analysis.
#' @param groups Groups for color in PCA.
#'
#' @details
#'
#' Compute and plot information for multivariate analysis (PCA, HCPC and
#' correlation).
#'
#' @return result and plots
#'
#' @import dplyr
#' @importFrom FactoMineR HCPC PCA
#' @importFrom tibble column_to_rownames
#'
#' @export
#'
#' @examples
#'
#' library(inti)
#' library(googlesheets4)
#' library(FactoMineR)
#'
#' if (gs4_has_token()) {
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "15r7ZwcZZHbEgltlF6gSFvCTFA-CFzVBWwg3mFlRyKPs")
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
#' , summary_by = c("genotype", "treat"), groups = "treat")
#'
#' FactoMineR::plot.PCA(mvr$pca
#'                      , choix = "ind"
#'                      , habillage = mvr$param$groups_n
#'                      , invisible = "quali"
#' )
#'
#' FactoMineR::plot.HCPC(mvr$hcpc)
#'
#' }
#' 

fieldbook_mvr <- function(data
                          , fb_smr
                          , summary_by
                          , groups
                            ) {

  where <- NULL

# fieldbook structure -----------------------------------------------------
# -------------------------------------------------------------------------

  factor_list <- fb_smr %>%
    filter(.data$type %in% "factor") %>%
    select(where(~!all(is.na(.))))

  vars_num <- fb_smr %>%
    filter(.data$type %in% "numeric") %>% 
    filter(levels > 0) # drop variables with values

  vars_cha <- fb_smr %>%
    filter(.data$type %in% "character") %>% 
    filter(levels > 0) # drop variables with values

  fb <- data %>%
    select(where(~!all(is.na(.)))) %>%
    mutate(across( factor_list[["variables"]], as.character)) %>%
    mutate(across( vars_num[["variables"]], as.numeric)) %>%
    mutate(across( vars_cha[["variables"]], as.character)) %>%
    select({{summary_by}}, vars_num[["variables"]]) %>%
    group_by( across( {{summary_by}} )) %>%
    summarize(across(everything(),  ~ mean(., na.rm = TRUE) )) %>%
    unite("rnames", {{summary_by}} , sep = "_", remove = F) %>%
    column_to_rownames("rnames")

# parameters --------------------------------------------------------------
# -------------------------------------------------------------------------

quali_ncol <- which(names(fb) %in% {{summary_by}})
groups_ncol <- which(names(fb) %in% {{groups}})

par <- list(quali = summary_by
            , quali_n = quali_ncol
            , groups = groups
            , groups_n = groups_ncol
            )

# pca ---------------------------------------------------------------------
# -------------------------------------------------------------------------

pca_info <- fb %>%
  select(where(~ length(unique(.)) > 1)) %>%  # drop variables with variation
  PCA(X = .
    , scale.unit = T
    , quali.sup = quali_ncol
    , graph = FALSE
    )

# hcpc --------------------------------------------------------------------
# -------------------------------------------------------------------------

  clt_info <- pca_info %>%
    HCPC(res = .
         , nb.clust = -1
         , graph = FALSE
         )

# Correlation -------------------------------------------------------------
# -------------------------------------------------------------------------

cor <- fb %>% 
  select(where(is.numeric)) %>%
  select(where(~ length(unique(.)) > 1)) %>% # drop variables without variation
  agricolae::correlation(method = "pearson")

# results -----------------------------------------------------------------
# -------------------------------------------------------------------------

  multvr = list(
    pca = pca_info
    , hcpc = clt_info
    , corr = cor
    , data = fb
    , param = par
    )

}
