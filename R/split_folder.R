#' Split folder
#'
#' Function to split folder by size or number of elements
#'
#' @param folder Path of folder to split (path).
#' @param export Path to export the split folders (path).
#' @param units Units to split folder (string: "megas", "number").
#' @param size Folder size by the units selected (numeric).
#' @param zip Zip split folders (logical).
#' @param remove Remove  the split folder after zip (logical).
#'
#' @return zip files
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' split_folder("pictures/QUINOA 2018-2019 SC SEEDS EDWIN - CAMACANI/"
#'    , "pictures/split_num", remove = T, size = 400, units = "number")
#' 
#' }
#' 

split_folder <- function(folder
                         , export
                         , units = "megas" 
                         , size = 500
                         , zip = TRUE
                         , remove = FALSE) {
  
  listfiles <- folder %>% 
    list.files(full.names = T)
  
  if (units == "megas") {
    
    dt <- listfiles %>% 
      tibble::enframe() %>% 
      dplyr::mutate(size_mb = file.size(.data$value)*(9.537*10^-7 )) %>% 
      dplyr::mutate(size_sum = purrr::accumulate(.data$size_mb, ~ifelse(.x + .y <= size, .x + .y, .y))) %>% 
      dplyr::mutate(split = cumsum(.data$size_mb == .data$size_sum))
    
  } else if (units == "number") {
    
    dt <- listfiles %>% 
      tibble::enframe() %>% 
      dplyr::mutate(img = 1) %>% 
      dplyr::mutate(size_sum = purrr::accumulate(.data$img, ~ifelse(.x + .y <= size, .x + .y, .y))) %>% 
      dplyr::mutate(split = cumsum(.data$img == .data$size_sum))  
    
  }
  
  sp <- dt %>% 
    dplyr::select(split) %>% 
    unique() %>% 
    purrr::as_vector()
  
  export <- 1:length(sp) %>% purrr::map( function(x) {
    
    newpath <- paste0(export, "/", basename(folder) , "_", sp[[x]])
    
    dir.create(newpath)
    
    files <- dt %>% 
      dplyr::filter(split %in% sp[[x]]) %>% 
      dplyr::select(.data$value) %>% 
      tibble::deframe()
    
    file.copy(files, newpath, recursive = T)
    
    if(isTRUE(zip)) utils::zip(zipfile = paste0(newpath, ".zip")
                             , files = newpath
                             , flags = "-r9Xj"
                             )
    
    if(isTRUE(remove)) unlink(newpath, recursive = T)
    
  })
  
}
