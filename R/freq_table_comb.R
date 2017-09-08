#----------------------------------------------------------------------------#

#' Combine elements of a frequency table into a (character) string or formatted table. 
#' 
#' \
#' 
#' @details Maintained by: Clara Marquardt
#' 
#' @export
#' @import data.table
#' 
#' @param data frequency table (table(x) output).
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
#' data_raw  <- table(dia$dia_name)
#' freq_string <- freq_table_comb(data=data_raw, expl_text="Percent of Principal Diagnoses", 
#' element_number=10, sort=TRUE, item_sep="/", string_table="string")
#' print(freq_string)
#' data <- freq_table_comb(data=data_raw, expl_text="Percent of Principal Diagnoses", 
#' element_number=10, sort=TRUE, item_sep="/", string_table="table")
#' print(data)

freq_table_comb <- function(data, expl_text, element_number=10, sort=TRUE, 
  string_table="string", item_sep="/", text_col=NA, number_col=NA ) {

  # format frequency table
  data <- data.frame(data)
  
  # identify columns
  if (is.na(text_col)) text_col <- names(data)[which(sapply(data,function(x) 
    class(x)[1]) %in% c("character", "factor"))]
  if (is.na(number_col)) number_col <- names(data)[which(sapply(data,function(x) 
    !(class(x)[1] %in% c("character","factor"))))]

  # order
  data[[text_col]] <- as.character(data[[text_col]] )
  if (sort==TRUE) data <- data[with(data, rev(order(get(number_col)))),]
  data <- data[1:element_number,]

  # generate string
  if (string_table=="string")  { 
      comb <- paste0(data[[text_col]], " - ", data[[number_col]],  
          " ", item_sep," ", collapse=" ")
      comb <- gsub(paste0(" ", item_sep," $"), "",comb)
      comb <- paste0(expl_text, ": ", comb)

      comb <- gsub("^[ ]*|[ ]*$", "", comb)

    } else if (string_table=="table") {

     comb <- lapply(1:nrow(data), function(x) {

        comb <- paste0(data[[text_col]][x], " - ", data[[number_col]][x],
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
