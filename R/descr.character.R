##' describe a character vector
##' 
##' @param x A character vector
##' @param colname The name of the vector (optional)
##' @export
##' 
descr.character <- function(x, colname = NULL) {
  z <- table(x)
  names(z) <- gsub("\\n", " ", names(z))
  z_length <- length(z)
  z_pc <- round(z / length(x) * 100, 1)
  zz <- cbind(z, z_pc)
  colnames(zz) <- c("Count", "Percent")
  zz <- data.frame(zz)
  zz <- zz[rev(order(zz$Count)), ]
  zz$`Cum. percent` <- cumsum(zz$Percent)
  if (nrow(zz) > 12) {
    zz_head <- head(zz)
    zz_tail <- tail(zz)
  } else {
    zz_head <- head(zz, n = 12)
    zz_tail <- NULL
  }
  num_na <- sum(is.na(x))
  pc_na <- round(num_na / length(x) * 100, 1)
  ret <- list(number = z_length, head = zz_head, tail = zz_tail, num_na = num_na, pc_na = pc_na)
  ## Print output
  msg <- paste0("Number of categories: ", ret$number, "\n\nNAs: ", ret$num_na, " (", ret$pc_na, "%)\n")
  cat(msg)
  yyh <- ret$head
  cat("\n\n")
  if (!is.null(ret$tail)) {
    yy <- rbind(ret$head, c(NA, NA), ret$tail)
    dot_row <- nrow(ret$head) + 1
    rownames(yy)[dot_row] <- "..."
    yy <- knitr::kable(yy)
    dot_row <- grepl("\\.\\.\\.", yy)
    yy[dot_row] <- gsub("NA", "  ", yy[dot_row])
    cat(paste(yy, collapse = "\n"), "\n")
  } else {
    cat(paste(knitr::kable(yyh), collapse = "\n"), "\n")
  }
 invisible(ret)
}
