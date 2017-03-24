#----------------------------------------------------------------------------#

#' Paste multiple tables/character strings into a single csv file.
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC


write_append_csv <- function(element_list, file_path) {

  write.table(element_list[1], file_path, sep=",", row.names=F, col.names= rep(" ", length(element_list[1][[1]])))
  # write.table(" ", file_path, append=T,sep=",", row.names=F, col.names = "")

  for (i in 2:(length(element_list))) {
      if (class(element_list[i][[1]]) == "character") {
       write.table(element_list[i], file_path, append=T,sep=",", row.names=F, col.names = "")     
      } else {
       write.table(element_list[i], file_path, append=T,sep=",", row.names=F)
      }
      # write.table(" ", file_path, append=T,sep=",", row.names=F, col.names = "")
  }

}

#----------------------------------------------------------------------------#
