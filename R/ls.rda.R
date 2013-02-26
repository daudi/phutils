#' List the contents of and Rda file
#' 
#' @param x The path to an Rda file.
#' @param show.str If TRUE show the structure of each object in the Rda file. The default is FALSE.
#' @return A vector of object names.
#' @keywords utils



ls.rda <- function(x, details = FALSE) {
  local.ls.rda.y <- local({
    local.ls.rda.y <- load(x)
    
    if (details) {
      this.class <- character(length(local.ls.rda.y))
      desc <- character(length(local.ls.rda.y))
      obj.size <- numeric(length(local.ls.rda.y))
      for (i in 1:length(local.ls.rda.y)) {
        dat <- get(local.ls.rda.y[i])
        this.class[i] <- class(dat)
        
        if (is.list(dat))
          desc[i] <- paste0("list[", length(dat), "]")
        if (is.data.frame(dat))
          desc[i] <- paste0(nrow(dat), " obs. of ", ncol(dat), " variables")
        if (is.matrix(dat))
          desc[i] <- paste(paste(dim(dat), collapse = "x"), "character matrix")
        if (is.function(dat))
          desc[i] <- head(dat, n = 1)    
        
        obj.size[i] <- object.size(dat)
      }
      local.ls.rda.y <- data.frame(object = local.ls.rda.y, 
                                   class = this.class, 
                                   desc = desc,
                                   size_Kb = round(obj.size / 1024, 0))
    }
    local.ls.rda.y
  })
  local.ls.rda.y
}


