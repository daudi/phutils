##' grep in files
##' 
##' Standard R grep() allows you to search for expressions within a character
##' vector. file.grep() allows you to search for expressions within text files 
##' (more like the standard unix grep command). 
##'
##' @details This combines list.files() with readLines() and grep() to search 
##' for an expresssion in lines of text files.
##'   
##' @param pattern The expression/pattern to search for. This can be a regular 
##' expression.
##' @param path The path from which to start the search for files. The default 
##' is ".", i.e. the current working directory.
##' @param file.pattern This is the file pattern to search for. This is a 
##' regular expression. See list.files() for details of the pattern. 
##' @param recursive If TRUE search the current directory and all sub-directories. 
##' The default is FALSE, i.e. to only search the current directory.
##' @param ignore.case Ignore case in the file pattern, i.e. if TRUE 
##' "txt" will find "txt" and "TXT" files. The default is FALSE, i.e. to be case 
##' sensitive.
##' @return Returns a data.frame with the name of files that contain the search
##' expression, the line number of any matches, and the content of the line that matches.
##' @export
##' @examples
##' x <- file.grep("wibble", ".", "txt")

file.grep <- function(pattern = NULL, 
                      path = ".", file.pattern = NULL, recursive = FALSE,
                      ignore.case = FALSE) {
  
  x <- list.files(path = path, pattern = file.pattern, recursive = recursive, 
                  ignore.case = ignore.case)
  print(x)
  
  grep.in.file <- function(infile, pattern) {  
    ret <- NULL
    xx <- readLines(infile)
    lines.found <- grep(pattern, xx)
    if (length(lines.found) > 0) {
      ret <- data.frame(file = infile, line = lines.found, content = xx[lines.found])
      ret
    }
  }

  ret <- list()
  for (i in 1:length(x)){
    ret[[i]] <- grep.in.file(x[i], pattern)
  }
  invisible(do.call("rbind", ret))
}
