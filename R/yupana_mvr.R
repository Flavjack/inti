#' Multivariate Analysis
#'
#' Multivariate analysis for PCA and HCPC
#'
#' @param data Field book data.
#' @param last_factor The last factor in your fieldbook.
#' @param variables Variables to be use in the analysis.
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
#' @importFrom tidyr unite
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(inti)
#' library(gsheet)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'               , "17vZ6ZqFlEi2XsYyCcZD3WRa0lBQUQDrzMqs-3F0TZ2Y/"
#'               , "edit#gid=120747730")
#' # browseURL(url)
#'
#' fb <- gsheet2tbl(url)
#' 
#' mv <- fieldbook_mvr(data = fb
#'                     , last_factor = "block"
#'                     , summary_by = c("treat")
#'                     , groups = "treat"
#'                     )
#' 
#' }
#' 

yupana_mvr <- function(data
                       , last_factor = NULL
                       , summary_by = NULL
                       , groups = NULL
                       , variables = NULL
                       ) {
  
  where <- NULL
  
  if(FALSE) {
    
data = fb
last_factor = "block"
summary_by = c("treat")
groups = "treat"    
variables = NULL   
    
    
  }
  
# fieldbook structure -----------------------------------------------------
# -------------------------------------------------------------------------
  
  factor_list <- data %>%
    {if(!is.null(last_factor)) select(.data = ., 1:{{last_factor}}) else .}  %>% 
    names()
  
  vars_num <- data %>%
    select(!{{factor_list}}) %>% 
    {if(is.null(variables)) select(., everything()) else select(., {{variables}})} %>% 
    names() 

  
  fb <- data %>%
    select(where(~!all(is.na(.)))) %>%
    {if(!is.null(last_factor)) 
      mutate(.data = ., across({{factor_list}}, as.factor)) else . } %>% 
    mutate(across( {{vars_num}}, as.numeric)) %>%
    select({{summary_by}}, {{vars_num}}) %>%
    group_by( across( {{summary_by}} )) %>%
    summarise(across(everything(),  ~ mean(., na.rm = TRUE) ))  %>%
    ungroup() %>% 
    unite("rnames", {{summary_by}} , sep = "_", remove = FALSE) %>%
    column_to_rownames("rnames") %>% 
    as.data.frame()
    
  
  
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
    data.frame() %>% 
    PCA(X = .
        , scale.unit = T
        , quali.sup = quali_ncol
        , graph = FALSE
    )
  
  # hcpc --------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  clt_info <- HCPC(res = pca_info
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
