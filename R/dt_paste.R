#----------------------------------------------------------------------------#

#' Paste elements of a frequency table into a string. 
#' 
#' @export
#' @import data.table
#' @param TBC 
#' @return TBC
#' @examples
#' TBC

dt_paste <- function(list_dt, text, length=10, text_col=NA, number_col=NA, line_sep=F) {

  if (is.na(text_col)) text_col <- names(list_dt)[which(sapply(list_dt,function(x) 
    class(x)[1]) %in% c("character", "factor"))]
  if (is.na(number_col)) number_col <- names(list_dt)[which(sapply(list_dt,function(x) 
    !(class(x)[1] %in% c("character","factor"))))]

   if (line_sep==FALSE)  { 
      comb <- paste0(list_dt[[text_col]][1:length], " - ", list_dt[[number_col]][1:length],  
          "  // ", collapse=" ")
      comb <- gsub(" // $", "",comb)
      comb <- paste(comb, text)

    } else if (line_sep==TRUE) {

     comb <- lapply(1:length, function(x) {

        comb <- paste0(list_dt[[text_col]][x], " - ", list_dt[[number_col]][x],
           "  // ", collapse=" ")
        comb <- gsub(" // $", "",comb)
        comb <- paste(comb, text)
     })

    }

    comb <- gsub("^[ ]*|[ ]*$", "", comb)

    return(comb)

}

#----------------------------------------------------------------------------#
