##' Function to create an HTML page with a searchable table.
##' 
##' This function takes a data.frame and creates an HTML page with an HTML
##' table. The table has a search box to provide a rapid search of the table.
##' 
##' For the search to work jquery-1.4.1.min.js or newer needs to be in the same
##' directory as the generated HTML page.
##' 
##' @param x A data.frame object.
##' @param title The title that will in the browser tab.
##' @param header A header that will appear above the table.
##' @param width.pc A vector of values to specify the width of each column in
##' percents. The default NULL will allow the browser to decide the width of
##' each column.
##' @param file The name of the output file.
##' @param html.before.table Any arbitrary HTML code to include before the
##' table (and after the header).
##' @param html.after.table Any arbitrary HTML code to include after the table.
##' @author David Whiting david.whiting@@publichealth.me.uk
##' @keywords utils
##' @export

searchable.html.table <- function(x,
                                  title = "My table",
                                  header = "Searchable HTML table",
                                  width.pc = NULL,
                                  file = "searchable.html.table.html",
                                  html.before.table = NULL,
                                  html.after.table = NULL
                                  ) {


html.top <- function(title = "My table", header = "Searchable HTML table", table.header) {
    paste("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
<html lang=\"en\">
<head>
<title>", title,"</title>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
<script src=\"jquery-1.4.1.min.js\"></script>
<script type=\"text/javascript\">

 $(document).ready(function() {
 //Declare the custom selector 'containsIgnoreCase'.
      $.expr[':'].containsIgnoreCase = function(n,i,m){
          return jQuery(n).text().toUpperCase().indexOf(m[3].toUpperCase())>=0;
      };

      $(\"#searchInput\").keyup(function(){

          $(\"#fbody\").find(\"tr\").hide();
          var data = this.value.split(\" \");
          var jo = $(\"#fbody\").find(\"tr\");
          $.each(data, function(i, v){

               //Use the new containsIgnoreCase function instead
               jo = jo.filter(\"*:containsIgnoreCase('\"+v+\"')\");
          });

          jo.show();

      }).focus(function(){
          this.value=\"\";
          $(this).css({\"color\":\"black\"});
          $(this).unbind('focus');
      }).css({\"color\":\"#C0C0C0\"});
  });


</script>

<style type=\"text/css\">
body { background-color: white; font-family: \"Trebuchet MS\", Arial, sans-serif; font-size: 12px; line-height: 1.2; padding: 1em; color: #2E2E2E; }

#qs { width: auto; border-style: solid; border-color: gray; border-width: 1px 1px 0px 1px; padding: 0.5em 0.5em; display:none;  }
#qs form { padding: 0px; margin: 0px; }
#qs form p { padding: 0px; margin: 0px; }

.invalidsearch { background-color: red; }

table {
	border-width: 1px 1px 1px 1px;
	border-spacing: 2px;
	border-style: none none none none;
	border-color: gray gray gray gray;
	border-collapse: collapse;
	background-color: white;
}

th {
	border-width: 1px 1px 1px 1px;
	padding: 4px 4px 4px 4px;
	border-style: inset inset inset inset;
	border-color: gray gray gray gray;
	background-color: gray;
	color: white;
}


td {
	border-width: 1px 1px 1px 1px;
	padding: 4px 4px 4px 4px;
	border-style: inset inset inset inset;
	border-color: gray gray gray gray;
	background-color: white;
}


td a { color: navy; text-decoration: none; }
td a:hover  { text-decoration: underline; }

tr.noshow { display: none;}

@media print {
	p.infolinks, #qssettings, #qs { display: none !important; }
	table { border-width: 0px; }
	tr { page-break-inside: avoid; }
	tr > * + * + * + * + * {display: none; }
	thead tr::before { content: \"Reference\"; border: 1px gray solid; padding: 0.5em; vertical-align: top; font-weight: bold; text-align: center; display: table-cell; background-color: #EFEFEF; }
	tr[id]::before { content: attr(id); display: table-cell; border: 1px gray solid; padding: 0.5em; vertical-align: top; font-style: italic; }
}
</style>
</head>
<body>

<h1>", header, "</h1>", html.before.table, "


<input id=\"searchInput\" value=\"Type To Filter\"><br/>

<table id=\"qstable\" border=\"1\">", table.header, "<tbody id=\"fbody\">", sep = "")

}


html.bottom <- function() {

    paste("</tbody>
</table>
          <p>", html.after.table, "</p>
<p>
 <small>Updated:", Sys.Date(), "</small>
</p>
</body>
</html>")

}


table.head <- function(x, width.pc = NULL) {
    th <- colnames(x)
    html <- "<thead><tr>"
    if (!is.null(width.pc)) {
        th.open <- paste("<th width=\"", width.pc, "\">", sep = "")
    } else {
        th.open <- rep("<th>", ncol(x))
    }
    for (i in 1:ncol(x)) {
        html <- paste(html, th.open[i], th[i], "</th>", sep = "")
    }
    html <- paste(html, "</tr></thead>", sep = "")
    html
}

table.body <- function(x) {
    html <- ""
    for (i in 1:nrow(x)) {
        html <- paste(html, "<tr>", sep = "")
        for (j in 1:ncol(x)) {
            html <- paste(html, "<td>", as.character(x[i, j]), "</td>", sep = "")
        }
        html <- paste(html, "</tr>", sep = "")
    }
    html
}





    table.header <- table.head(x, width.pc)

    cat(
        html.top(title, header, table.header = table.header),
        table.body(x),
        html.bottom(),
        file = file)

}


### EXAMPLE
## x <- 1:20
## y <- letters[x]
## z <- data.frame(x = x, y = y)


