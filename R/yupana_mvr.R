#' Multivariate Analysis
#'
#' Multivariate analysis for PCA and HCPC
#'
#' @param data Field book data.
#' @param last_factor The last factor in your fieldbook  `[string: NULL]`.
#' @param variables Variables to be use in the analysis  `[string: NULL]`.
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
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#'
#' library(inti)
#' 
#' fb <- inti::potato 
#' 
#' mv <- yupana_mvr(data = fb
#'                  , last_factor = "geno"
#'                  , summary_by = c("geno", "treat")
#'                  , groups = "treat"
#'                  , variables = c("all")
#'                  #, variables = c("wue", "twue")
#'                  )
#'                  
#' mv$plot[1] 
#' 
#' mv$data
#' 
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
  
# fieldbook structure -----------------------------------------------------
# -------------------------------------------------------------------------
  
  groups <- if(is.null(groups)) summary_by[1] else groups
  
  variables <- if("all" %in% variables | all(is.null(variables))) { NULL } else {variables}
  
  fb <- data %>% 
    {
      if(!is.null(last_factor) & all(!is.null(variables)) ) {
        dplyr::select(.data = ., c(1:{{last_factor}}), {{variables}})
      } else .
      } %>%
    dplyr::mutate(.data = ., across(c(1:{{last_factor}}), as.factor)) %>% 
    dplyr::mutate(.data = ., across(!c(1:{{last_factor}}), as.numeric)) %>% 
    dplyr::group_by(across({{summary_by}})) %>%
    dplyr::summarise(across(everything(),  ~ mean(., na.rm = TRUE) ))  %>%
    dplyr::ungroup() %>% 
    tidyr::unite("rnames", {{summary_by}} , sep = "-", remove = FALSE) %>%
    tibble::column_to_rownames("rnames") %>% 
    dplyr::select(where(~!all(is.na(.)))) %>%
    as.data.frame()
  
# condition ---------------------------------------------------------------

  varnum <- fb %>% 
    dplyr::select(!c(1:tail({{summary_by}}, n = 1))) %>% 
    ncol()
  
  if(varnum <= 1) stop("Select at least two (2) variables")  
  
# parameters --------------------------------------------------------------
# -------------------------------------------------------------------------
  
  quali_ncol <- which(names(fb) %in% {{summary_by}})
  groups_ncol <- which(names(fb) %in% {{groups}})
  
  par <- list(quali = summary_by
              , quali_n = quali_ncol
              , groups = groups
              , groups_n = groups_ncol
              )
  
# condtions ---------------------------------------------------------------

  n <- fb %>%
    dplyr::select(par$quali)  %>% 
    as.list() %>% 
    purrr::map(discard, is.na) %>% 
    lengths() %>% 
    prod()
  
  if(n <= 2) stop("The factors should have more than 2 levels")
  
# pca ---------------------------------------------------------------------
# -------------------------------------------------------------------------
  
  pca_info <- fb %>%
    dplyr::select(where(~length(unique(.)) > 1)) %>%  # drop variables without variation
    FactoMineR::PCA(X = .
        , scale.unit = T
        , quali.sup = quali_ncol
        , graph = FALSE
        )
  
  plot_pca_var <- FactoMineR::plot.PCA(x = pca_info
                           , choix = "var"
                           , autoLab = "auto"
                           , shadowtext = T
                           , graph.type = "ggplot"
                           ) 
  
  legend <- if(nlevels(fb[[par$groups]]) > 20) "none" else "bottom"
  
  plot_pca_ind <- FactoMineR::plot.PCA(x = pca_info
                   , choice = "ind"
                   , habillage = par$groups_n
                   , invisible = "quali"
                   , autoLab = "auto"
                   , shadowtext = T
                   , graph.type = "ggplot"
                   ) +
    theme(legend.position = legend) 
  
# hcpc --------------------------------------------------------------------
# -------------------------------------------------------------------------
  
  clt_info <- FactoMineR::HCPC(res = pca_info
                   , nb.clust = -1
                   , graph = FALSE
                   )
  
  # plot.HCPC(x = clt_info
  #           , choice = "map"
  #           , legend = list(x = "topright"
  #                           , cex = 0.6
  #                           , inset = 0.001
  #                           , box.lty=0
  #                           )
  #           , draw.tree = F
  #           )
  
# plot --------------------------------------------------------------------
# -------------------------------------------------------------------------

plots <- list(pca_var = plot_pca_var
              , pca_ind = plot_pca_ind)
  
# results -----------------------------------------------------------------
# -------------------------------------------------------------------------
  
multvr = list(
    pca = pca_info
    , hcpc = clt_info
    , data = fb
    , param = par
    , plots = plots
    )
  
}
