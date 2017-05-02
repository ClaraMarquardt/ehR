#----------------------------------------------------------------------------#

#' Store multiple arguments returned from function as separate objects - structure. 
#' @export
#' @return TBC
#' @examples
#' \dontrun{
#' within function_test: return(list(obj_1,obj_2, obj_3))
#' outside of function: return_mult[obj_1, obj_2, obj_3] <- function_test()
#' }

return_mult <- structure(NA,class="result")


#----------------------------------------------------------------------------#
