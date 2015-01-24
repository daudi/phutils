#' Add a block of rag-rated rectangles, or symbols.
#' 
#' The blocks are coloured according to the rag-rating and have the letter 
#' of the rag-rating (helpful when printing in black and white). 
#' 
#' The original idea was to use this with sparklines but they might be useful more generally.
#' 
#' 
#' @param status A vector of "R", "A", and "G"s.
#' @param cex Character expansion factor for the letters in the blocks.
#' @param blocks If TRUE (the default) draw rectangular blocks. If FALSE, use the pch symbol(s)
#' @param pch The symbol to use if blocks = FALSE. 
#' @param symbol.cex Character expansion for the symbol. This is going to have to be large to be useful. The default is 20. This will need to be changed depending on the symbol (pch) used.
#' @param col A list with two elements named \code{rag.colours} and \code{rag}. \code{rag.colours} 
#' is a character vector of colour names, and \code{rag} is a corresponding vector of labels. The 
#' defaults are c("red", "orange", "green") and c("R", "A", "G") respectively for rag-rating.
#' @param show.text If TRUE (the default) show the text label (e.g. R, A, G).
#' 
#' @details When using symbols (blocks = FALSE) pch 15 to 20 are good, others also.
#' @author David Whiting, david.whiting@@publichealth.me.uk
#' @keywords utils
#' @return Invisibly returns a list with the x co-ordinates of the rectangles and text.
#' @export
#' @examples
#' 
#' x <- c("R", "G", "A", "G")
#' rag.blocks(x)
#' rag.blocks(x, block = FALSE)
#' 
#' 


rag.blocks <- function (status, cex = 1, blocks = TRUE, pch = 16, symbol.cex = 20,
                        show.text = TRUE,
                        col = list(rag.colours = c("red", "orange", "green"),
                                   rag = c("R", "A", "G"))
                        ) {
  num.items <- length(status)

  if (length(col$rag) != length(col$rag.colours))
    warning("The number of colours and colour look-up codes don't match (rag and rag.colours).")

  rag <- col$rag
  rag.colours <- col$rag.colours
  rag.status <- rag.colours[match(status, rag)]
  rag.status[is.na(rag.status)] <- "white"
  rag.xpos <- seq(from = 0, by = 1 / num.items, length.out = num.items)
  rag.xpos2 <- seq(to = 1, by = 1 / num.items, length.out = num.items)
  rag.text.xpos <- (rag.xpos + rag.xpos2) / 2
  plot.new()
  if (blocks) {
    rect(rag.xpos, 0, 1, 1, col = rag.status)
  } else {
    points(rag.text.xpos, rep(0.5, length(rag.text.xpos)), pch = pch, cex = symbol.cex, col = rag.status)
  }
  if (show.text)
    text(rag.text.xpos, 0.5, labels = status, cex = cex)
  invisible(list(rag.xpos = rag.xpos, rag.text.xpos = rag.text.xpos))
}
