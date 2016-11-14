##' Create an infographic image showing a proportion
##' 
##' This creates a roughly square image representing a proportion. Use png() to 
##' save it to a png file.
##' 
##' @param prop The proportion to represent
##' @param denoms A vector of denominators to try. Defaults to 1:20
##' @param case The png image to use for a case (for now it must be square)
##' @param notcase The png image to use to represent those that are not cases (must be square)
##' @param units The units to be used in the message. Defaults to 'people'
##' @export
##' @details This calls cases_noncases()

proportion_graphic <- function(prop, denoms = 1:20, 
                               case, notcase, 
                               units = "people") {
  infog <- cases_noncases(prop, denoms = denoms, units = units)
  notcase <- png::readPNG(notcase)
  case <- png::readPNG(case)
  
  num_images <- infog$full_row_size
  num_rows <- infog$num_full_rows + 1
  img_width <- 1 / num_images
  mar_orig <- par("mar")
  par(mar = c(0, 0, 0, 0))
  plot.new()
  total_images <- 0
  numer_images <- 0
  for (j in 1:num_rows) {
    for (i in 1:num_images) {
      total_images <- total_images + 1
      numer_images <- numer_images + 1
      if (numer_images <= infog$numerator) {
        img <- case
      } else {
        img <- notcase
      }
      x1 <- 0.0 + ((i - 1) * img_width)
      y1 <- 0.0 + ((j - 1) * img_width)
      x2 <- 0.0 + (i * img_width)
      y2 <- 0.0 + (j * img_width)
      ##print(paste0(x1, ", ", y1, ", ", x2, ", ", y2))
      if (total_images <= infog$denominator) rasterImage(img, x1, y1, x2, y2)
    }
  }
  par(mar = mar_orig)
  infog
}
