#----------------------------------------------------------------------------#

#' mclapply function useable across Mac/Windows. 
#' @export
#' @import parallel
#' @param  TBC
#' @return TBC
#' @examples
#' #' \dontrun{
#' 	result_robust <- mclapply_robust(X=seq(1:100), FUN=function(x) x*3)
#' 	result        <- mclapply(seq(1:100), function(x) x*3)
#' }

mclapply_robust <- function(max_core=8, ...) {



	## check platform
	if( Sys.info()[['sysname']] == 'Windows' ) {
		core_count <- 1
	} else {
		core_count <- min(max_core, detectCores())
	}
	print(max_core)

	## execute
	temp <- mclapply(..., mc.cores=core_count)

	## return
	return(temp)
}

#----------------------------------------------------------------------------#
