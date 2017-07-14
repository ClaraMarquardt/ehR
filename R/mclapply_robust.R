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

mclapply_robust <- function(max_core=8, quiet=TRUE, ...) {

  ## check platform
  if( Sys.info()[['sysname']] == 'Windows' ) {

    if (quiet==FALSE) ps("windows: %s / number of cores: %d", 
      as.character(Sys.info()[['sysname']] == 'Windows'),core_count)

    temp <- lapply(...)

  } else {
    if (max_core %like% "*") {
      core_count <- as.numeric(gsub("\\*", "", max_core))
    } else {
      core_count <- min(max_core, detectCores())
    }

    if (quiet==FALSE) ps("windows: %s / number of cores: %d", 
      as.character(Sys.info()[['sysname']] == 'Windows'),core_count)
    
    temp <- mclapply(..., mc.cores=core_count)
  }
  
  ## return
  return(temp)
}


#----------------------------------------------------------------------------#
