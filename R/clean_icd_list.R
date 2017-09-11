#----------------------------------------------------------------------------#

#' Expand a list of icd9 code ranges. 
#' 
#' @description: Given a data.table with a column containing one or more ICD9 code ranges (separated by ";"), and another with the name of the corresponding condition, return a data.table with an expanded, comprehensive list of all valid ICD9 codes falling in the listed range(s), along with names of the condition inherited from the original table.
#' 
#' @details Maintained by: Clara Marquardt
#' 
#' @export
#' @import icd
#' @import data.table
#' @import magrittr
#' 
#' @param data data.table containing a column with icd9 code ranges and code range names (data.table).
#' @param code_col name of the column containing the icd9 code ranges (see the below example) - commented out rows (start with '#') are ignored (character). 
#' @param name_col name of the column containing the code range names (character).  
#' @param decimal whether or not the icd9 codes are in decimal format (logical - TRUE/FALSE) [default: TRUE]. 
#' @param validate whether or not to subset to 'defined' icd9 codes (see www.rdocumentation.org/packages/icd/versions/2.2/topics/icd_expand_range for more detail) (logical - TRUE/FALSE) [default: FALSE]. 
#' 
#' @return data.table with the expanded icd9 code list and the associated code range names. 
#' 
#' @examples
#' orig_table <- copy(gagne_code) 
#' print(orig_table$code)
#' code_table  <- clean_icd_list(data=orig_table, code_col="code", name_col="condition", decimal=FALSE, validate=FALSE) 
#' print(code_table)


clean_icd_list <- function(data, code_col, name_col, decimal=TRUE, validate=FALSE) {
  
  # omit codes which are excluded/commented out (rows that start with "#")
  data <- data[!(data[[code_col]] %like% "#" | data[[name_col]] %like% "#") ]
  
  # codes are separated by semicolon  - split
  x <- data[[code_col]] %>% strsplit(";")
  names(x) <- data[[name_col]]

  # define a function to expand ranges
  expand_and_convert <- function(code) {
   
    # [1] expand explicit ranges containing dash
    if(grepl("-",code)) {
      
      split <- strsplit(code, "-")[[1]]
      
      # convert decimals to short if needed
      if(decimal){
        split[1] <- icd_decimal_to_short(split[1])
        split[2] <- icd_decimal_to_short(split[2])
      }

      # expand range
      expand_temp <- ""
      tryCatch({expand_temp <- icd_expand_range(split[1], split[2], defined=validate, short=TRUE)}, 
        error=function(e) {print(paste0("Undefined code range - ", code))})

     return(expand_temp)
    
    # [2] expand implicit ranges of 3-and 4-digit codes
    } else {

      # convert decimals to short if needed
      if(decimal){
        code <- icd_decimal_to_short(code)
      }

      # expand range
      expand_temp <- ""
      tryCatch({expand_temp <- icd_expand_range(code, code, defined=validate, short=TRUE)}, 
        error=function(e) {print(paste0("Undefined code range - ", code))})

      return(expand_temp)
    }
  }

  # apply the expand_and_convert function to each code & generate list of data.tables (one 
  # table per concept)
  clean <- lapply(x,
    function(xi) { 
      data.table("code"=unlist(lapply(xi, function(xij) {
        expand_and_convert(xij)
      })))
    })

  # collapse table list and generate the final table
  lapply(seq_along(clean), function(i) clean[[i]][,paste(name_col):=names(clean)[[i]]])
  rbindlist(clean) %>% return

}

#----------------------------------------------------------------------------#
