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
    select(where(~!all(is.na(.)))) %>% 
    filter(levels > 0)

  vars_num <- fb_smr %>%
    filter(.data$type %in% "numeric") %>% 
    filter(levels > 0) 

  vars_cha <- fb_smr %>%
    filter(.data$type %in% "character") %>% 
    filter(levels > 0) 

  fb <- data %>%
    select(where(~!all(is.na(.)))) %>%
    mutate(across( factor_list[["variables"]], as.factor)) %>%
    mutate(across( vars_cha[["variables"]], as.factor)) %>%
    mutate(across( vars_num[["variables"]], as.numeric)) %>%
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

clt_info <- HCPC(pca_info
                 , nb.clust=-1
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
