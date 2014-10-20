##' A function to send emails from R
##' 
##' This function sends an email via MS Outlook by writing and then calling a vbscript.
##' 
##' @param to A character vector of email addresses to send the email to.
##' @param subject A character string that will be used as the subject line. shQuote() is used. See details.
##' @param body A character string for the body of the email. shQuote() is NOT used. See details.
##' @param send A logical value determining if the email should be sent. Defaults to TRUE. 
##' @return Invisibly returns the string that is written to the vbscript file.
##' @details This function uses Outlook to send an email. shQuote() is used to make 
##' sure that the subject is quoted correctly. shQuote is not used for the body to 
##' make it easier to construct an email body that uses vbCrLf to create new 
##' lines/carriage returns in the body of the email. If you are having trouble 
##' with the body of the email try using shQuote() on the character string that 
##' you send as the body paramter.
##' 
##' @export

email <- function(to, subject, body, send = TRUE) {
  
  to  <- paste0("NewMail.Recipients.Add ", shQuote(to), collapse = "\n")
  
  vbs <- paste0("\nOn Error Resume Next\n",
                "Set Outlook = CreateObject(\"Outlook.Application\")
                Set MAPI = Outlook.GetNameSpace(\"MAPI\")
                Set NewMail = Outlook.CreateItem(0)\n",
                "NewMail.Subject = ", shQuote(subject), "\n",
                "NewMail.Body = ", body, "\n",
                to, "\n",
                "NewMail.Send\n")
  
  if (send) {
    tmp <- paste0(tempfile(), ".vbs")
    cat(vbs, file = tmp)
    system(paste("cscript", tmp))
  }
  invisible(vbs)
}
