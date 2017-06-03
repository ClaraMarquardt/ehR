#----------------------------------------------------------------------------#

#' Generate a function programatically. 
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

make_function <- function(arg, body, env = parent.frame()) {

      f <- function() {}
      formals(f) <- arg
      body(f)    <- body
      environment(f) <- env 

      return(f)

}

#----------------------------------------------------------------------------#
