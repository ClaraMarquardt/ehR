#----------------------------------------------------------------------------#

#' Parse and assign (in parent namespace) the arguments in a function call.
#' @export
#' @import data.table
#' @param x Function call (see examples) [character].
#' @examples
#' \dontrun{
#'  parse_arg(quote(test_function(arg1="xxxx", arg2=object_name, arg3="xxxx")))
#' }


parse_arg <- function(x) {

  command_text <- gsub("[ ]{2,}"," ", paste0(deparse(x), collapse=""))


  setting <- as.data.table(strsplit(command_text, "="))

  ## deal with unnamed arguments
  setting[gsub("[^]\\[\\(),]","", V1) %like% "[^(\\(\\[)](,){2,}", V1:=gsub("([^,]*),.*,([^,]*)",
    "\\1,\\2",V1)]

  setting[, ':='(arg1=gsub("(.*),([^,]*)", "\\1",V1), arg2=(gsub("(.*),([^,]*)",
   "\\2",V1)))]
  setting[, arg2:=shift(arg2, 1, "lead")]
  setting[2,arg2:=gsub(".*\\(", "", arg2)]
    setting[nrow(setting),arg1:=gsub("\\)", "", arg1)]
    setting[, command_arg:=paste0(arg2, "<-", arg1)]

  inv_lapply(setting$command_arg[-1], function(x) eval(parse(text=x), 
    envir=sys.frame(sys.parent(n=4))))

}

#----------------------------------------------------------------------------#
