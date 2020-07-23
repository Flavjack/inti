#' Descriptive Statistics for a model
#'
#' @description Function to summary descriptive statistics from a model
#' @param modelo an object containing the results returned by a model fitting function
#' @param data data set used for the model
#' @return data frame
#' @importFrom dplyr summarise
#' @importFrom dplyr '%>%' select_
#' @importFrom stats anova
#' @export

stat_sm <- function(modelo, data){
  
  avt <- anova(modelo)
  
  varn <- colnames(modelo[["model"]][1])
  
  
  smr <- data %>% 
    select_( varn )
  
  MSerror <- SDev <- Mean <- NULL
  
  smd <- smr %>%
    dplyr::summarise(
      MSerror  =  avt["Residuals", 3],
      Mean = mean(smr[[varn]], na.rm=TRUE),
      SDev = sd(smr[[varn]], na.rm=TRUE),
      Min = min(smr[[varn]], na.rm=TRUE),
      Max = max(smr[[varn]], na.rm=TRUE),
      Num = sum(!is.na(smr[[varn]])),
      CV = sqrt(MSerror) * 100/Mean
      
    )
  
  smd
  
}