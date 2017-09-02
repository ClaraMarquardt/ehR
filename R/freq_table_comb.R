#----------------------------------------------------------------------------#

#' Combine elements of a frequency table into a (character) string or formatted table. 
#' 
#' \
#' 
#' @details Maintainer:Maintainer: Clara Marquardt
#' 
#' @export
#' @import data.table
#' 
#' @param freq_table frequency table (table(x) output).
#' @param expl_text text which is to be appended at the end of the string (e.g. "(Percent of diagnoses)") (character). 
#' @param element_number number of rows of the frequency table which are to be included (integer) [default:10].  
#' @param sort whether to sort the frequency table (descending order) (logical - TRUE/FALSE) [default: TRUE].
#' @param item_sep symbol used to separate the elements in the string (character) [default: "//"]. 
#' @param string_table whether to return a string or table (character - "string"/"table") [default: "string"].
#' @param text_col name of text column (character) [default: automatically identify column based on column types]. 
#' @param number_col name of number column (character) [default: automatically identify column based on column types]. 
#' 
#' @return formatted (character) string or data.table. 
#' 
#' @examples
#' freq_table_raw  <- table(dia$dia_name)
#' freq_string <- freq_table_comb(freq_table=freq_table_raw, expl_text="Percent of Principal Diagnoses", 
#' element_number=10, sort=TRUE, item_sep="/", string_table="string")
#' print(freq_string)
#' freq_table <- freq_table_comb(freq_table=freq_table_raw, expl_text="Percent of Principal Diagnoses", 
#' element_number=10, sort=TRUE, item_sep="/", string_table="table")
#' print(freq_table)

freq_table_comb <- function(freq_table, expl_text, element_number=10, sort=TRUE, 
  string_table="string", item_sep="/", text_col=NA, number_col=NA ) {

  # format frequency table
  freq_table <- data.frame(freq_table)
  
  # identify columns
  if (is.na(text_col)) text_col <- names(freq_table)[which(sapply(freq_table,function(x) 
    class(x)[1]) %in% c("character", "factor"))]
  if (is.na(number_col)) number_col <- names(freq_table)[which(sapply(freq_table,function(x) 
    !(class(x)[1] %in% c("character","factor"))))]

  # order
  freq_table[[text_col]] <- as.character(freq_table[[text_col]] )
  if (sort==TRUE) freq_table <- freq_table[with(freq_table, rev(order(get(number_col)))),]
  freq_table <- freq_table[1:element_number,]

  # generate string
  if (string_table=="string")  { 
      comb <- paste0(freq_table[[text_col]], " - ", freq_table[[number_col]],  
          " ", item_sep," ", collapse=" ")
      comb <- gsub(paste0(" ", item_sep," $"), "",comb)
      comb <- paste0(expl_text, ": ", comb)

      comb <- gsub("^[ ]*|[ ]*$", "", comb)

    } else if (string_table=="table") {

     comb <- lapply(1:nrow(freq_table), function(x) {

        comb <- paste0(freq_table[[text_col]][x], " - ", freq_table[[number_col]][x],
           " ",item_sep," ", collapse=" ")
        comb <- gsub(paste0(" ",item_sep," $"), "",comb)
        comb <- paste(comb, expl_text)
        comb <- gsub("^[ ]*|[ ]*$", "", comb)

     })

     comb <- data.table(data.frame(cbind(comb)))
     setnames(comb, "")
    
    }

  # return
  return(comb)

}

#----------------------------------------------------------------------------#
