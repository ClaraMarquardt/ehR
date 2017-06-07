#----------------------------------------------------------------------------#

#' Generate classification schemes.  
#' @export
#' @import data.table
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

var_cat_gen <- function(class_scheme_name) {

  ## class schemes
  cent_temp <- c(30,36.5,37,37.5 ,38, 38.5,45)

  wbc_wide <- c(0,0.5, 1,1.5, 2,2.5, 3,3.5, 4,4.5, 5,5.5, 6,6.5, 7, 8,10,12, 14, 16,20)
  wbc_narrow <- c(0,0.5,1,2,3,4,5,6,8,10,12, 14, 16, 20)

  anc_wide <-c(0,0.5, 1,1.5, 2,2.5, 3,3.5, 4,4.5, 5,5.5, 6,6.5,7, 8,10,12, 14, 16, 20)
  anc_narrow <- c(0,0.5, 1,2,3,4,5,6,8,10, 12, 14, 16, 20)

  ## categories
  cat_min  <- get(class_scheme_name)[1:(length(get(class_scheme_name))-1)]
  cat_max  <- get(class_scheme_name)[2:(length(get(class_scheme_name)))]
  cat_name <- c(paste0(get(class_scheme_name)[1], "-"), sprintf("%s-%s",cat_min,cat_max), 
    paste0(get(class_scheme_name)[length(get(class_scheme_name))], "+"))
  cat <- c(1:length(cat_min))

  # return
  return(list(get(class_scheme_name), cat_min, cat_max, cat_name, cat))

}

#----------------------------------------------------------------------------#
