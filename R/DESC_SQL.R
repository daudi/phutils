##' Describe the structure of an SQL server database or table
##' 
##' This returns a description of the structure (columns types, etc.) of an SQL Server 
##' database or table. This is the equivalent of the MySQL command DESC.
##' 
##' @param conn A database connection
##' @param tablename The name of a table. If NULL (the default) information will be 
##' returned for all tables. If specified, information will only be returned for 
##' the named table.
##' @export

DESC <- function(conn, tablename = NULL) {
  WHERE <- ""
  if (!is.null(tablename)) WHERE <- paste0("WHERE TABLE_NAME = '", tablename, "'")
  mq <- paste("SELECT * FROM INFORMATION_SCHEMA.Columns", WHERE)
  sqlQuery(conn, mq)
}
