#----------------------------------------------------------------------------#

#' Load or install R packages (from CRAN or Github)
#' @export
#' @import data.table
#' @import devtools
#' @param package_list List of package names which are to be installed/loaded (list - character).
#' @param custom_lib_path Custom library path (character) [Default: Default library path].
#' @param custom_repo R repository from which to download packages [Default: https://cran.rstudio.com"]
#' @param custom_package_version Whether to take into account version specifications for key packages (data.table, ggplot2) (logical - TRUE/FALSE) [Default: TRUE]. 
#' @param verbose Verbosity (logical - TRUE/FALSE) [Default: TRUE]. 
#' @return List of packages which were succesfully installed/loaded. 
#' @examples
#' package <- list("data.table", "trinker/plotflow")
#' load_or_install(package_list=package, custom_lib_path=paste0(getwd(), "/test/"), 
#' verbose=TRUE)

load_or_install <- function(package_list, custom_lib_path="", 
  custom_repo="https://cran.rstudio.com", custom_package_version=TRUE, 
  verbose=TRUE) {  

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
  invisible(lapply(package_list, function(x) if(!x %in% c(installed.packages(
       lib.loc=lib_path))) {
  

    # cran package
    if (!(x %like% "/")) {

      if (verbose==TRUE) {

        print(sprintf("Fresh Install (CRAN): %s", x))

      }

      if (custom_package_version==TRUE & x %in% c("data.table", "ggplot2")) {
    
          # special case - "data.table" (1.9.6 version)
          if (x=="data.table") {
    
            suppressMessages(withr::with_libpaths(new = lib_path,
              install_version("data.table", version = "1.9.6",
              repos = "http://cran.us.r-project.org",
              dependencies=TRUE)))
    
          # special case - "ggplot" (dev version)
          } else if (x=="ggplot2") {
        
            suppressMessages(withr::with_libpaths(new = lib_path, 
              install_github("hadley/ggplot2")))

          }
    
         # general
         } else {
    
            suppressMessages(install.packages(x,repos=custom_repo, 
               dependencies=TRUE, lib=lib_path))
    
         }

    # github package
    } else {

      if (verbose==TRUE) {

        print(sprintf("Fresh Install (Github): %s", x))

      }

      suppressMessages(withr::with_libpaths(new = lib_path, 
           install_github(x)))

    }
   
  }))

  # load
  # -----------------------------

  package_loaded <- lapply(package_list, function(x) {
    
    if (verbose==TRUE) {
      print(sprintf("Loading: %s", x))
    }

    suppressMessages(library(gsub("(.*)/(.*)", "\\2", x),character.only=TRUE, quietly=TRUE,
        verbose=FALSE, lib.loc=lib_path))

  })

  # output
  # -----------------------------
  cat("\n\n*****************\n\nThe Following Packages Were Succesfully Installed/Loaded:\n\n")
  print(package_loaded[[length(package_list)]])
  cat("\n*****************\n\n")

  rm("package_loaded")

} 

#----------------------------------------------------------------------------#

