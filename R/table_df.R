#----------------------------------------------------------------------------#

#' Convert a frequency table to a data.frame.
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

table_df <- function(x, orientation="vert", round_digit=1 ) {

  if(orientation=="hor") {

    if(ncol(as.data.frame(x)>1)) {

      temp <- setNames(data.table(t(as.data.frame(x)[,-1])), 
         as.character(as.data.frame(x)[,1]))
      return(temp)
    } 

  }

  if(orientation=="vert") {

    
  if(ncol(as.data.frame(x))>1) {

      temp <- data.table((as.data.frame(x)))
      setnames(temp, c("var_name","freq"))
      temp[, freq:=round(freq, digit=round_digit)]
      return(temp)
         
    } 

    if(ncol(as.data.frame(x))==1) {

      temp <- data.table((as.data.frame(x)))
      temp$var_name <- as.character(row.names(as.data.frame(x)))
      setnames(temp, "x","freq")
      setcolorder(temp, c("var_name", "freq"))
      temp[, freq:=round(freq, digit=round_digit)]
      return(temp)
         
    } 
  }
}

#----------------------------------------------------------------------------#
