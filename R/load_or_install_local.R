#----------------------------------------------------------------------------#

#' Load or install R packages (Local)
#' @export
#' @import data.table
#' @import devtools
#' @param package_list List of package paths which are to be installed/loaded (list - character).
#' @param custom_lib_path Custom library path (character) [Default: Default library path].
#' @param verbose Verbosity (logical - TRUE/FALSE) [Default: TRUE]. 
#' @return List of packages which were succesfully installed/loaded. 
#' @examples \dontrun{
#' package <- list("[package path]", "[package path]") ## do NOT end in a /
#' load_or_install_local(package_list=package, custom_lib_path=paste0(getwd(), "/test/"), verbose=TRUE)
#' }

load_or_install_local <- function(package_list, custom_lib_path="", verbose=TRUE) {  

  # Point Person: Clara

  # library path
  # -----------------------------

  ## default
  lib_path     <- .libPaths()[1]

  ## custom 
  if (custom_lib_path!="") {

    if (!dir.exists(custom_lib_path)) {
      dir.create(custom_lib_path)
    }

    lib_path   <- custom_lib_path

  } 

  print(sprintf("lib_path: %s", lib_path))

  # devtools
  # ----------------------------
  library(devtools)
  dev_mode(TRUE)


  # install 
  # ----------------------------
  invisible(lapply(package_list, function(x)  {

    if (verbose==TRUE) {

      print(sprintf("Load: %s", x))

    }

    # load functions
    for (y in list.files(paste0(x, "/R"))) {

		if (verbose==TRUE) print(sprintf("source: %s", y))

		source(paste0(x,"/R/",y))

	}

	# load data
	for (y in list.files(paste0(x, "/data"))) {

		if (verbose==TRUE) print(sprintf("load: %s", y))

		load(paste0(x,"/data/",y))

	}

   }))


} 

#----------------------------------------------------------------------------#



