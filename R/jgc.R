#----------------------------------------------------------------------------#

#' Java garbage collection.
#' 
#' @export
#' @import rJava
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

jgc <- function() {

   gc()
  .jcall("java/lang/System", method = "gc")
}  

#----------------------------------------------------------------------------#
