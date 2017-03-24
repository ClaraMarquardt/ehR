#----------------------------------------------------------------------------#

#' Shorten a character column of a data.table/a character vector.
#' @export
#' @import data.table
#' @param  DT data.table or character vector [data.table / character vecor]
#' @param  colname_list List of column names which are to be processed (NA if processing a vector)[character list]
#' @param  max_length Max character length of shortened labels [integer]
#' @param  beg_fraction: Number of characters to to be kept from the label's beginning (as a % of 
#'   max_length characters)  [numeric]
#' @param  in_place Modify in place,i.e. replace original labels [logical]
#' @param  new_colname_list List of new column names to be generated if in_place==False (name of new vector object 
#'    to be created if parsing a vector) [character list / vector]
#' @return TBC
#' @examples
#' temp <- data.table::copy(dia)
#' shorten_label(temp, list("dia_name"), new_colname_list=list("dia_name_short"), in_place=FALSE)

shorten_label <- function(DT, colname_list=NA, max_length=20, beg_fraction=0.8, 
  in_place=TRUE, new_colname_list=NA) {

  if(class(DT)=="character")  {
    vector_name <- deparse(substitute(DT))
    DT <- data.table(x=DT)
    colname_list <- list("x")
    data_table_check <- FALSE
  } else if(class(DT)=="data.table") {
    data_table_check <- TRUE
  }

  for (i in 1:length(colname_list)) {

    # define parameters based on specification
    colname <- colname_list[[i]]
    length <- nchar(DT[, get(colname)])
    cutoff <- round(beg_fraction*max_length, 0)
    end <- max_length-cutoff

    # shorten
    DT[, paste0(colname, "_1"):=ifelse(nchar(get(colname))>max_length, 
      substring(get(colname),1,cutoff), get(colname))]
    DT[, paste0(  colname, "_2"):=ifelse(nchar(get(colname))>max_length, 
      substring(get(colname),length - end,length), get(colname))] 
    DT[, c(paste0(colname, "_new")):=ifelse(nchar(get(colname))>max_length, 
      do.call(function(...) paste(...,sep="__"), .SD[,mget(c(paste0(colname, "_1"), 
      paste0(colname, "_2")))]), get(colname))]
    DT[, c(paste0(colname, "_new")):=gsub("[_]{3,}", "__", get(paste0(colname, "_new")))]

    DT[, c(paste0(colname, "_1"), paste0(colname, "_2")):=NULL]

    if(data_table_check==TRUE) {
      # update in place or return new variable
      if(in_place==TRUE) {
              print("a")
        DT[, c(colname):=NULL]
        setnames(DT, paste0(colname, "_new"), colname)
      } else {
        setnames(DT, paste0(colname, "_new"), new_colname_list[[i]])
      }
    } else {
     if(in_place==TRUE) {
        assign(vector_name, DT$x_new, sys.frame(sys.parent(n=1)))
      } else {
        assign(new_colname_list[[i]], DT$x_new,sys.frame(sys.parent(n=1)))
      }
    }

  }  
}

#----------------------------------------------------------------------------#
