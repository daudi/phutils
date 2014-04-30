##' Display a dataframe in the Rstudio viewer with a search filter
##' 
##' @param x A data.frame
##' @author David Whiting <david.whiting@@publichealth.me.uk>
##' 
##' @export
##' @return Nothing. Nada. 

searchable.dataframe.viewer <- function(x) {

  ## Create HTML file to display in the viewer
  htmlFile <- tempfile(fileext=".html")
  unlink(htmlFile)
  searchable.html.table(x,
                        title = "",
                        header = paste(nrow(x), "rows"),
                        file = htmlFile,
                        html.before.table = "",
                        html.after.table = "")
  
  jquery.source <- "v:/33 - Pembroke Court/Public Health Intelligence/metadata/jquery-1.4.1.min.js"
  jquery.destination <- paste0(tempdir(), "\\jquery-1.4.1.min.js")
  if (file.exists(jquery.source) & !file.exists(jquery.destination)) {
    file.copy(jquery.source, jquery.destination)
  }
    
  rstudio::viewer(htmlFile)  
}