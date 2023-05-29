#' Include PDF in markdown documents
#'
#' Insert PDF files in markdown documents
#'
#' @param file file path from pdf file.
#' @param width width preview file.
#' @param height height preview file.
#'
#' @return html code for markdown
#' 
#' @export
#'   


include_pdf <- function(file
                        , width = "100%"
                        , height = "600"
                        ) {
  
  paste0('<embed src= "', file, '" height="', height, '" width="', width, '" alt="pdf" pluginspage="http://www.adobe.com/products/acrobat/readstep2.html">') %>% 
    knitr::asis_output()
  
}
