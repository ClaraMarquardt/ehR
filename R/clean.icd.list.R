#----------------------------------------------------------------------------#

#' Expand a list of icd9 code ranges. 
#' @export
#' @import icd
#' @import data.table
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

clean.icd.list <- function(list, icdcol, namescol, decimal=TRUE, out.name=namescol) {
  
  # omit codes which are excluded/commented out (rows that start with "#")
  list <- list[!(list[[icdcol]] %like% "#" | list[[namescol]] %like% "#") ]
  
  # codes are separated by semicolon in csv file 
  x <- list[[icdcol]] %>% strsplit(";")
  names(x) <- list[[namescol]]

  expand_and_convert <- function(code) {
    # expand explicit ranges containing dash
    if(grepl("-",code)) {
      split <- strsplit(code, "-")[[1]]
      # convert decimals to short if needed
      if(decimal){
        split[1] <- icd9DecimalToShort(split[1])
        split[2] <- icd9DecimalToShort(split[2])
      }
      icd9ExpandRangeShort(split[1], split[2], onlyReal=FALSE)
    }
    # expand implicit ranges of 3- and 4-digit codes
    else {
      # convert decimals to short if needed
      if(decimal){
        code <- icd9DecimalToShort(code)
      }
      icd9ExpandRangeShort(code, code, onlyReal=FALSE)
    }
  }

  # apply expand_and_convert to each term & convert to list of data.tables
  clean <- lapply(x,
    function(xi) data.table("code"=unlist(lapply(xi,
      function(xij) expand_and_convert(xij)
      )))
    )

  # create concept flag and collapse list 
  lapply(seq_along(clean), function(i) clean[[i]][,paste(out.name):=names(clean)[[i]]])
  rbindlist(clean) %>% return

}

#----------------------------------------------------------------------------#
