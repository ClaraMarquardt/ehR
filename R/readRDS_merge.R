#----------------------------------------------------------------------------#

#' @title Read in (a) a single or (b) a list of RDS files (which are to be 
#' merged).
#'
#' @description /
#'
#' @export
#' @import data.table
#' @param file_name_list 
#' @param nested
#' @return
#' @examples

readRDS_merge <- function(file_name_list, nested=FALSE) {

      if (length(file_name_list)==1) {

        return(readRDS(file_name_list[[1]]))

      } else if ( length(file_name_list)>1 & nested==FALSE) {

        for(i in 1:length(file_name_list)) {
           assign(paste0("temp_dt_", i), readRDS(file_name_list[i][[1]]))
        }

        temp <- rbindlist(mget(paste0("temp_dt_", 1:length(file_name_list))),use.names=T, fill=T)
        temp <- unique(temp,by=c(setdiff(names(temp), grep("_id$", names(temp),value=T))))

        file_temp_list <- ls(pattern="temp_dt_[0-9]")
        lapply(file_temp_list, function(x) rm(x))
        return(temp)

  } else if ( length(file_name_list)>1 & nested==TRUE) {
        
        for(i in 1:length(file_name_list)) {
           assign(paste0("temp_dt_", i), readRDS(file_name_list[i][[1]]))
        }

        temp_name <- names(temp_dt_1)
        temp <- lapply(   temp_name, function(table_name) lapply(mget(paste0("temp_dt_", 1:length(file_name_list)), 
          sys.frame(sys.parent(n=2))), function(x) x[[table_name]]))
        temp <- lapply(temp, function(x) rbindlist(x, use.names=T, fill=T))
        temp <- lapply(temp, function(x) unique(x, by=c(setdiff(names(x), 
          grep("_id$", names(x),value=T)))))

        names(temp) <-  temp_name

        file_temp_list <- ls(pattern="temp_dt_[0-9]")
        lapply(file_temp_list, function(x) rm(x))
        return(temp)

  }

}

#----------------------------------------------------------------------------#
