#----------------------------------------------------------------------------#

#' Load or install R packages (Local).
#'
#' Given a list of paths to locally stored packages, i.e. R package folders (e.g. list("~/Desktop/ehR")) load all functions/data sets into the global namespace.
#'
#' @details Maintained by: Clara Marquardt
#'
#' @export
#' @import data.table
#' @import devtools
#' 
#' @param package_list List of package paths which are to be installed/loaded (e.g. list("~/Desktop/ehR")) - character).
#' @param custom_lib_path Custom library path (character) [Default: Default library path].
#' @param quiet Verbosity (logical - TRUE/FALSE) [Default: TRUE]. 
#' 
#' @return List of packages which were successfully installed/loaded. 
#' 
#' @examples \dontrun{
#' package <- list("[package path]", "[package path]") ## do NOT end in a /
#' load_or_install_local(package_list=package, custom_lib_path=paste0(getwd(), "/test/"), quiet=FALSE)
#' }

load_or_install_local <- function(package_list, custom_lib_path="", quiet=FALSE) {  

  # load - functions & data 
  # ----------------------------
  invisible(lapply(package_list, function(x)  {

    if (quiet==FALSE) {

      print(sprintf("Load: %s", x))

    }

    # load functions
    for (y in list.files(paste0(x, "/R"))) {

		if (quiet==FALSE) print(sprintf("source: %s", y))

		source(paste0(x,"/R/",y))

	}

	# load data
	for (y in list.files(paste0(x, "/data"))) {

		if (quiet==FALSE) print(sprintf("load: %s", y))

		load(paste0(x,"/data/",y))

	}

   }))


} 

#----------------------------------------------------------------------------#



