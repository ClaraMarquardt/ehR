#----------------------------------------------------------------------------#

#' Insert element - string - into a list. 
#' @export
#' @param list_name List name [character] 
#' @param argument String to be inserted[character]
#' @return List modified in place
#' @examples
#'  sample_list <- list()
#'  sample_list$nfold <- 10
#'  list_space("sample_list")
 
list_space <- function(list_name, argument="space") {

if(!exists(as.character(as.name(argument)))) {
	assign(argument, 1, envir=globalenv())
}
  
 count <- get(argument, sys.frame(sys.parent(n=1)))
 temp <- get(list_name, sys.frame(sys.parent(n=1)))
 
 temp[[paste0("space_", count)]] <- "space"
 
 assign(list_name, temp, sys.frame(sys.parent(n=1)))
 assign(argument, count+1, sys.frame(sys.parent(n=1)))

}

#----------------------------------------------------------------------------#
