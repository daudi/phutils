#' Add speaker notes to powerpoint slides
#' 
#' ReporteRs is a great package for creating powerpoint presentations from R,
#' but it does not have a facility (at the moment) to add speaker notes.
#' 
#' This function enables you to add speaker notes to a completed powerpoint.
#' 
#'  @details You need to accumulate the notes as you go along and keep track of
#'  the slide number for the notes. This will all go wrong if you change the slide 
#'  order or add a new slide. 
#'  
#'  To create paragraphs you need to use vbCrLf. See the example code below.
#'  
#'  @param pptfile The path to a powerpoint file. This must be a fullpath, relative
#'  paths don't seem to work.
#'  @param slidenotes A dataframe with two columns: a numeric column with the slide 
#'  number and a character column with the slide notes content. At the moment this needs 
#'  to be simple text.
#'  @export
#'  
#'  @examples 
#'  dontrun{
#'  vbCrLf <- "& vbCrLf &"
#'  ex1 <- paste(shQuote("Notes for slide 1"), vbCrLf, shQuote("This is on the next line"))
#'  slidenotes <- data.frame(slide = 1:2, notes = c(ex1, "Notes for Slide 2"))
#'  add_notes_to_ppt(mfile, slidenotes)
#'  }



add_notes_to_ppt <- function(pptfile, slidenotes) {
  
  names(slidenotes) <- c("slide", "notes")
  
  vbs <- "Set objPPT = CreateObject(\"Powerpoint.Application\")
  
' Make it visible. You have to do this, hiding it is not allowed.
objPPT.Visible = True
  
' Open existing presentation
Set objPresentation = objPPT.Presentations.Open("
  
  vbs <- paste0(vbs, shQuote(pptfile), ")\n\n")
  
  for (i in 1:nrow(slidenotes)) {
    vbs <- paste0(vbs, "Set slide = objPresentation.Slides(", slidenotes$slide[i], ")\n")
    this_note <- slidenotes$notes[i]
    if (!grepl("\"", this_note)) this_note <- shQuote(this_note)
    vbs <- paste0(vbs, "slide.NotesPage.Shapes(2).TextFrame.TextRange = ", this_note, "\n\n")
  }
  
  vbs <- paste0(vbs, "
' Save and quit
objPresentation.Save
objPresentation.Close

")
  
  cat(vbs, file = "temp.vbs")
  system("cscript temp.vbs")
}

