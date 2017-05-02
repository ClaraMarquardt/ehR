#----------------------------------------------------------------------------#

#' Store multiple arguments returned from function as separate objects.
#' @export
#' @param  topad List of elements to pad [list]
#' @param  num Final length [integer]
#' @param  method Whether to pad front or back [character]
#' @param  replace What to pad with [character]
#' @return TBC
#' @examples
#' \dontrun{
#' within function_test: return(list(obj_1,obj_2, obj_3))
#' outside of function: return_mult[obj_1, obj_2, obj_3] <- function_test()
#' }

return_mult <<- structure(NA,class="result")
"[<-.result" <<- function(x,...,value) {

   args <- as.list(match.call())
   args <- args[-c(1:2,length(args))]
   length(value) <- length(args)
   for(i in seq(along=args)) {
     a <- args[[i]]
     if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
   }
   x
}

#----------------------------------------------------------------------------#
