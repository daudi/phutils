##' describe a data.frame
##' 
##' Describe a data.frame and display the results in the Rstudio viewer.
##' 
##' @param x A data.frame
##' @export


descr.data.frame <- function(x) {
  print("Creating describe.html")
  dir <- tempfile()
  dir.create(dir)
  RmdFile <- file.path(dir, "describe.Rmd")
  htmlFile <- file.path(dir, "describe.html")
  cssFile <- file.path(dir, "describe.css")

  cat("---
title: \"Describe dataframe ", substitute(x), "\"
output:
    html_document:
      css: describe.css
      number_sections: yes
      toc: yes
---
      
**********

`r nrow(", substitute(x), ")` rows, `r ncol(", substitute(x), ")` columns. 

      
```{r review_dataset, echo = FALSE, results='asis'}
      
run_desc <- function(x) {
  col_names <- names(x)
  for (i in 1:ncol(x)) {
    this_col <- col_names[i]
    cat(paste0(\"\n\n# \", this_col, \"\n\"))
    y <- x[, i]
    yy <- desc(y, this_col)
  }
}

run_desc(", substitute(x), ")

```
    
", file = RmdFile)
  

cat(
"#TOC {
  position: fixed;
  left: 0;
  top: 0;
  width: 200px;
  height: 100%;
  overflow:auto;
}


body {
  max-width: 800px;
  margin: auto;
  margin-left:210px;
  line-height: 20px;
} ", file = cssFile
)
  
  
  
rmarkdown::render(RmdFile)

##file.copy("describe.html", htmlFile)
rstudioapi::viewer(htmlFile)
}
