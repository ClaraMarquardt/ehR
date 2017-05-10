#----------------------------------------------------------------------------#

#' Return object/object returned by expression if the object exists, return NULL otherwise. 
#' 
#' @description 
#' 
#' @export
#' @param TBC 
#' @return TBC
#' @examples
#' TBC

exist_add <- function(obj_exp) {

  temp <- tryCatch(eval(parse(text=obj_exp)), error=function(e) return(NULL))

  return(temp)

}


#----------------------------------------------------------------------------#
