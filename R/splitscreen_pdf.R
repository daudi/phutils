#' Create a multipage PDF with split screens
#' 
#' @description This function creates a multi-page, multi-screen PDF, in the 
#' same manner as the dashboard PDFs. The default is six on a page.
#' 
#' @details This works by supplying a vector of indices to iterate over, which could 
#' be as simple as e.g. 1:6, and a function to call for each screen. The function must 
#' take an object called x, and for each screen it will be run with each value of x. So,
#' for example, `splitscreen_pdf(c("a", "b", "c", "d", "e", "f"), FUN = function(x) plot(1:10, main = x))`
#' will create one page with six screens showing a plot going from 1 to 10, with the 
#' title being "a" for the first one, "b" for the next and so on.
#' 
#' 
#' @param x A vector of indices that will be interated over. 
#' @param FUN A function that will be called, with x[i]
#' as the parameter. The function will draw a single plot.
#' @examples 
#' splitscreen_pdf(c("a", "b", "c", "d", "e", "f"), FUN = function(x) plot(1:10, main = x))
#' 
#' @export
#' 
#' 
#' 
splitscreen_pdf <- function(x, FUN,
                            prefix = "splitscreen",
                            path = ".",
                            title = NULL,
                            screen.dim = c(2, 3),
                            paper = "special", height = 11.7, width = 16.5,
                            orientation = "landscape",
                            cex = 4,
                            font = 2) {
  
  plots.per.page <- screen.dim[1] * screen.dim[2]
  screen.num <- 0
  file.num <- 1
  md <- list()

  ppsize <- switch(paper,
                   A0 = c(33.1, 46.8),
                   A1 = c(23.4, 33.1),
                   A2 = c(16.5, 23.4),
                   A3 = c(11.7, 16.5),
                   A4 = c(8.3, 11.7),
                   A5 = c(5.8, 8.3),
                   c(width, height))

  if (orientation == "landscape")
    ppsize <- rev(ppsize)

  if (substring(paper, 1, 1) == "A") {
    paper <- "special"
    width <- ppsize[1]
    height <- ppsize[2]
  }
  
  while (dev.cur() > 1) {
    dev.off()
  }
  pdf(file = paste0(path, "/", prefix, ".pdf"),
      paper = paper, height = height, width = width)
  if(!is.null(title)) {
    close.screen(all.screens = TRUE)
    split.screen(c(1, 1))
    plot.new()
    cover.title <- paste(strwrap(title, width = 40), collapse = "\n")
    text(0.5, 0.5, cover.title, cex = cex, font = font)
    close.screen(all.screens = TRUE)
  }
  for (i in 1:length(x)) {
    if (!(screen.num %in% 1:plots.per.page)) {
      split.screen(screen.dim, erase = TRUE)
      screen.num <- 1
    }
    screen(screen.num)

    ## Run plots here
    FUN(x[i])

    if (screen.num %% plots.per.page == 0) {
      close.screen(all.screens = TRUE)
    }
    screen.num <- screen.num + 1
  }
  close.screen(all.screens = TRUE)

  while (dev.cur() > 1) {
    dev.off()
  }
}
