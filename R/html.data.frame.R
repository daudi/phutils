##' Customised version of HTML.data.frame
##' 
##' Hacking HTML.data.frame to see if I can add options for COLSPAN and ROWSPAN in the header

##' @export

##' @examples
##' EXAMPLE OF HOW TO USE MULTILINE HEADER ROW
##' x <- 1:10
##' y <- letters[1:10]
##' z <- rep(c("x", "y"), 5)
##' z <- data.frame(x, y, z)

##' jina <- matrix(c("Deaths", "Pop", NA,
##'                   NA, "Males", "Females"),
##'                   ncol = 3, byrow = TRUE)
##' jina.col <- matrix(c(1, 2, 0, 1, 1, 1), ncol = 3, byrow = TRUE)
##' jina.row <- matrix(c(2, 1, 1, 0, 1, 1), ncol = 3, byrow = TRUE)

##' my.table.header <- list(names = jina,
##'                         col.spec = jina.col,
##'                         row.spec = jina.row,
##'                         css.class = c("firstline", "secondline"))

##' HTML.data.frame(x, file = "0.html", table.header = my.table.header)
##' HTML(x, file = "0.html", caption = "", big.mark = ",", nsmall = 1, table.header = my.table.header)



"HTML.data.frame" <- function(
                              x, file=get(".HTML.file"),
                              Border = 1, innerBorder = 0,
                              classfirstline = "firstline",
                              classfirstcolumn = "firstcolumn",
                              classcellinside = "cellinside",
                              append = TRUE,
                              align = "center",
                              caption = "",
                              captionalign = "bottom",
                              classcaption = "captiondataframe",
                              classtable = "dataframe",
                              digits = 2,
                              nsmall = 0,
                              big.mark = "",
                              big.interval = 3,
                              decimal.mark = ".",
                              sortableDF = FALSE,
                              row.names = TRUE,
                              table.header = NULL,
                              CSV.path = NULL,
                              CSV.local.root = NULL,
                              CSV.server.root = CSV.local.root,
                              ...)
{

    check.for.small.counts(x)
    if (!is.null(CSV.path)) write.csv(x, file = paste0(CSV.local.root, "/", CSV.path), row.names = TRUE)  # Modified by MC 2013-01-30 from row.names = FALSE
    
    cat("\n", file = file, append = append)

    # Handle sortableDF argument
    if (is.null(sortableDF)) sortableDF = FALSE
    if (sortableDF)
      cat(paste(c("\n<style>", ".tablesort  {",
                  "cursor: pointer ;",
                  " behavior:url(tablesort.htc);",
                  " -moz-binding: url(moz-behaviors.xml#tablesort.htc);",
                  "}",
                  "</style>\n"),
                  collapse="\n"),
          file = file, append = TRUE)


   # if (!is.null(digits)) x[] = lapply(x, FUN = function(vec) if (is.numeric(vec)) round(vec, digits) else vec)

   txt <- ""
   txtcaption <- ifelse(is.null(caption),
                        "",
                        paste("\n<caption class=\"", classcaption, "\">",
                              caption, "</caption>\n", sep=""))

   if (!is.null(Border))
     txt <- paste(txt, "\n<table cellspacing=\"0\" border=\"", Border, "\">",
                  txtcaption,"<tr><td>",
                  "\n\t<table border=\"", innerBorder, "\" class=\"",classtable,"\">",
                  sep = "")
   else txt <- paste(txt, "\n<table border=", innerBorder,
                     " class=\"",classtable,"\" cellspacing=\"0\">",
                     txtcaption, sep = "")

   txt <- paste(txt,"\t<tbody>",sep="\n")

   ## Create the table header
   if(!is.null(table.header)) {
     table.header <- html.table.header.rows(table.header, ncol(x), row.names, classfirstline)
     txt <- paste(txt, table.header, sep = "\n")
   } else {

     VecDebut <- c(
                   if(row.names) paste("\n\t\t<th>",
                                       if(sortableDF) '<b class="tablesort">',
                                       sep = "", collapse = ""),
                   rep(paste("\n\t\t<th>",
                             if(sortableDF) '<b class="tablesort">',
                             sep = "", collapse = ""), ncol(x) - 1)
                   )

     ## Table header names
     VecMilieu <- c(
                    if(row.names) "&nbsp;",
                    as.character(dimnames(x)[[2]])
                    )


     VecFin <- c(
                 if(row.names)
                 paste(if(sortableDF) '</b>', "", "</th>", collapse = ""),
                 rep(
                     paste(if(sortableDF) '</b>',"", "</th>", collapse = ""), ncol(x) - 1
                     ),
                 "</th>"
                 )
     dmf <- paste(VecDebut, VecMilieu, VecFin, sep = "", collapse = "")
     txt <- paste0(txt, "\n\t<tr class=\"", classfirstline, "\">",
                  dmf,
                  "\n\t</tr>"
                  )
   }

  
   x.formatted <- format(x, digits = digits, nsmall = nsmall,
                         big.mark = big.mark, big.interval = big.interval,
                         decimal.mark = decimal.mark)
   x.formatted <- as.matrix(x.formatted)
   x.formatted[is.na(x.formatted)] <- " "
   x.formatted[is.nan(x.formatted)] <- " "

   for(i in 1:dim(x)[1]) {
     if (is.matrix(classcellinside)) {
       this.classcellinside <- classcellinside[i, ]
     } else {
       this.classcellinside <- rep(classcellinside, ncol(x))
     }
     VecDebut <- c(if(row.names)
                   paste("\n<td class=\"", classfirstcolumn, "\">",
                         sep = ""),
                   paste("\n<td class=\"", this.classcellinside, "\">", sep = "")
                   )

     VecMilieu <- c(if(row.names)
                    dimnames(x)[[1]][i],
                    HTMLReplaceNA(x.formatted[i,]))
     VecFin <- c(if(row.names) "\n</td>",
                 rep("\n</td>", dim(x)[2] - 1),
                 "\n</td></tr>\n")

     txt <- paste(txt,  "\n<tr>",
                  paste(VecDebut, VecMilieu, VecFin, sep = "", collapse = ""))
   }
   txt <- paste(txt, "\n\t</tbody>\n</table>\n",
                if (!is.null(Border)) "</td></tr></table>\n",
                if (!is.null(CSV.path)) paste0("<a href=\"", paste0(CSV.server.root, "/", CSV.path),"\">Download these data</a>\n"),
                "<br />")
   cat(txt, "\n", file = file, sep = "", append = TRUE)
 }


"HTMLReplaceNA"<-
    function(Vec, Replace = " ")
{
    Vec <- as.character(Vec)
    for(i in 1:length(Vec))
    {
        Vec[i] <- gsub(" ", "", Vec[i])
        if((Vec[i] == "NA") | (Vec[i] == "NaN") | is.na(Vec[i])){
            Vec[i] <- Replace
        }
    }
    Vec
}







html.table.header.rows <- function(x, num.cols, row.names, classfirstline) {
  ## x: a list specifying table header names, and column and row specifications.
  ## Extract the parts we need from x
  col.spec <- x$col.spec
  row.spec <- x$row.spec
  css.class <- x$css.class
  if (is.null(css.class)) 
    css.class <- classfirstline
  x <- x$names


  ## Make sure we have the right number of columns in the matrix. Also
  ## allow the user to send a name for the rownames column.
  if (row.names) {
    num.cols.expected <- num.cols + 1
  } else {
    num.cols.expected <- num.cols
  }
  if (ncol(x) == num.cols.expected - 1) {
    x <- cbind(rep("", nrow(x)), x)
    col.spec <- cbind(rep(1, nrow(x)), col.spec)
    row.spec <- cbind(c(nrow(x), rep(0, nrow(x) - 1)), row.spec)
  }
  stopifnot(ncol(x) == num.cols.expected)

  ## Why do I do this?
  if (nrow(x) > 1) {
    x[2:nrow(x), 1] <- NA
  }
  
  html <- NULL

  col.spec <- span.spec(col.spec, span.type = "colspan")
  row.spec <- span.spec(row.spec, span.type = "rowspan")

  if (is.matrix(x) | is.data.frame(x)) {
    y <- x
    for (i in 1:nrow(x)) {
      html <- paste0(html, "<tr class=\"", css.class[i], "\">")
      for (j in 1:ncol(x)) {
        if (!is.na(x[i, j]))
          y[i, j] <- paste0("<th ", col.spec[i, j], row.spec[i, j], ">", x[i, j], "</th>")
      }
      html <- paste0(html, paste0(y[i , !is.na(y[i, ])], collapse = "\n"))
      html <- paste0(html, "\n</tr>\n")
    }
  }
  paste(html, "\n")
}



span.spec <- function(x, span.type) {
  y <- x
  y[x == 0] <- NA
  y[x == 1] <- ""
  y[x > 1] <- paste(span.type, "=\"", x[x > 1], "\"", sep = "")
  y
}

status.log <- function(x) {
  right.now <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  cat(paste(right.now, x, "\n"), file = "STATUS.LOG", append = TRUE)
}


check.for.small.counts <- function(x, threshold = 5) {
    ## Warn if we may need to suppress small counts
    warn <- FALSE
    for (i in 1:ncol(x)) {
        if (class(x[, i]) == "integer" && any(x[, i] <= threshold, na.rm = TRUE)) {
            warn <- TRUE
            break ## out of the for loop
        }
    }
    if (warn) {
        msg <- "Small counts detected in this table. Should these data be suppressed?"
        warning(msg)
        status.log(msg)
    }
}


