#' Swedish cultivar trial data.
#'
#' The datasets were obtained from official Swedish cultivar tests. Dry matter
#' yield was analyzed. All trials were laid out as alpha-designs with two
#' replicates. Within each replicate, there were five to seven incomplete
#' blocks. 
#'
#' @format A data frame with 1069 rows and 8 variables: 
#' 
#' \describe{
#'   \item{zone}{Sweden is divided into three different agricultural zones:
#'    South, Middle, and North}
#'   \item{location}{Locations: 18 location in the Zones}
#'   \item{rep}{Replications (4): number of replication in the experiment}
#'   \item{alpha}{Incomplete blocks (8) in the alpha-designs}
#'   \item{cultivar}{Cultivars (30): genotypes evaluated}
#'   \item{yield}{Yield in kg/ha}
#'   \item{year}{Year (1): 2016}
#'   \item{env}{enviroment (18): combination zone + location + year}      
#'   }
#'   
#' @source \url{https://doi.org/10.1002/csc2.20177}

"met"

# met <- read.csv("inst/extdata/met.csv") %>%
#   mutate(env = paste(Zone, Location, Year,sep = "_")) %>%
#   mutate(across(where(is.character), tolower)) %>%
#   rename_with(stringr::str_to_lower) %>% 
#   mutate(across(!yield, as.factor)) %>%
#   na.omit()
# 
# met %>% str()
# 
# usethis::use_data(met, overwrite = T)