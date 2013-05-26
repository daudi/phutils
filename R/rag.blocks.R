#' Add a block of rag-rated rectangles
#' 
#' The blocks are coloured according to the rag-rating and have the letter 
#' of the rag-rating (helpful when printing in black and white). 
#' 
#' The original idea was to use this with sparklines but they might be useful more generally.
#' 
#' 
#' @param status A vector of "R", "A", and "G"s.
#' @param cex Character expansion factor for the letters in the blocks.
#' @author David Whiting, david.whiting@@publichealth.me.uk
#' @keywords utils
#' @export
#' @examples
#' 
#' x <- c("R", "G", "A", "G")
#' rag.blocks(x)
#' 


rag.blocks <- function(status, cex = 1) {
  num.items <- length(status)
  rag <- c("R", "A", "G")
  rag.colours <- c("red", "orange", "green")
  rag.status <- rag.colours[match(status, rag)]
  rag.xpos <- seq(from = 0, by = 1 / num.items, length.out = num.items)
  rag.xpos2 <- seq(to = 1, by = 1 / num.items, length.out = num.items)
  rag.text.xpos <- (rag.xpos + rag.xpos2) / 2
  plot.new()
  rect(rag.xpos, 0, 1, 1, col = rag.status)
  text(rag.text.xpos, 0.5, labels = status, cex = cex)
}
