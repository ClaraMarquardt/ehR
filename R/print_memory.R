#----------------------------------------------------------------------------#

#' Print table with memory usage of objects in namespace.
#' @export
#' @param  namespace Namespace
#' @return TBC
#' @examples
#' print_memory()


print_memory <- function(namespace=ls(parent.env(environment()))) {
   
   mem_table <- data.table(var_name=namespace, 
    mem_MB=sapply(namespace, function(x) {object.size(get(x))/1000000}))

   setorder(mem_table, -mem_MB)
   
   print(head(mem_table, 25))
}

#----------------------------------------------------------------------------#
