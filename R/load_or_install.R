#----------------------------------------------------------------------------#

#' Load or install R packages (public or local).
#' @export
#' @import data.table
#' @import devtools
#' @param  TBC
#' @return TBC
#' @examples
#' TBC


load_or_install <- function(package_names, custom_lib_path=FALSE, 
  custom_path=NA, verbose=FALSE, local_package=FALSE, 
  local_package_path=NA) {  


  # obtain & save default path
  # -----------------------------
  library(devtools)
  dev_mode(FALSE)
  default_path     <- .libPaths()[1]

  # dev tools & dev mode
  # -----------------------------
  library(devtools)
  # set to dev_mode 
  dev_mode(TRUE)

  # lib path
  # -----------------------------
  if (custom_lib_path==TRUE) {

    if (!dir.exists(custom_path)) {
      dir.create(custom_path)
    }

    if (!("dev" %in% list.files(custom_path))) {
      dir.create(paste0(custom_path, "/dev"))
    }

    lib_path     <- custom_path
    lib_dev_path <- paste0(custom_path, "/dev")

    print(sprintf("lib_path: %s", lib_path))

  } else if (custom_lib_path==FALSE) {

    lib_path     <- default_path
    lib_dev_path <- default_path

    print(sprintf("lib_path: %s", lib_path))

  }

  if (local_package==FALSE) {

     # install (if required, i.e. not yet installed)  
     # -----------------------------
     
     ## extended - handle special cases
     lapply(package_names, function(x) if(!x %in% c(installed.packages(
       lib.loc=lib_path), installed.packages(lib.loc=lib_dev_path))) {
    
       print(sprintf("Fresh Install: %s", x))
    
    
       # install
       if (x=="data.table") {
    
         suppressMessages(withr::with_libpaths(new = lib_path,
           install_version("data.table", version = "1.9.6",
           repos = "http://cran.us.r-project.org",
           dependencies=TRUE)))
    
       } else if (x=="ggplot2") {
        
         suppressMessages(withr::with_libpaths(new = lib_path, 
           install_github("hadley/ggplot2")))
    
       } else if (x=="plotflow") {
        
         suppressMessages(withr::with_libpaths(new = lib_path, 
           install_github("trinker/plotflow")))
    
      } else if (x=="FEATure") {

        suppressMessages(withr::with_libpaths(new = lib_path, 
           install_github("ClaraMarquardt/FEATure")))
    
       } else {
    
         suppressMessages(install.packages(x,repos="http://cran.cnr.berkeley.edu/", 
           dependencies=TRUE, lib=lib_path))
    
      }
   
  })
 
  } else if (local_package==TRUE) {

    mapply(function(x,path) if(!x %in% c(installed.packages(
       lib.loc=lib_path), installed.packages(lib.loc=lib_dev_path))) {
   

      print(sprintf("Fresh Install: %s", x))
    
      # navigate to project directory
      setwd(path)

      # install 
      suppressMessages(withr::with_libpaths(new = lib_path, 
          devtools::install()))
 

  }, x=package_names, path=local_package_path)

}

  # load
  # -----------------------------

  packages_loaded <- lapply(package_names, function(x) {
    if (verbose==TRUE) {
      print(sprintf("Loading: %s", x))
    }

    suppressMessages(library(x,
        character.only=TRUE, quietly=TRUE,verbose=FALSE, 
        lib.loc=lib_path))

  })

  # output
  # -----------------------------
  cat("\n\n*****************\n\nThe Following Packages Were Succesfully Loaded:\n\n")
  print(packages_loaded[[length(package_names)]])
  cat("\n*****************\n\n")

  rm("packages_loaded")

} 

#----------------------------------------------------------------------------#

