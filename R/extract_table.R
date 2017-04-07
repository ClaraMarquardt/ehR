#----------------------------------------------------------------------------#

#' Extract tables from a PDF journal article. 
#'
#' @description 
#'
#' @export
#' @param temp_table List with text rows extracted from PDF [list - character].
#' @return
#' @examples

extract_table <- function(temp_table) {


  min_table <- min(unique(temp_table$table))
  max_table <- max(unique(temp_table$table))

  orig_table_list <- lapply(min_table:max_table, function(i) {
  
    print(i)
    
    temp_table_raw_temp <- temp_table[table==(i)]
    temp_table_raw_temp <- c(temp_table_raw_temp$V1)
    temp_table_raw_temp <- temp_table_raw_temp[!temp_table_raw_temp==""]
  

    temp_table_raw_temp <- gsub("\\(([0-9,\\*\\.]*)\\)", "[\\1]", temp_table_raw_temp)
    temp_table_raw_temp <- gsub("([a-zA-Z\\(\\)])( )([a-zA-Z\\(\\)])", "\\1_\\3", temp_table_raw_temp)
    temp_table_raw_temp <- gsub("([a-zA-Z\\(\\)])( )([a-zA-Z\\(\\)])", "\\1_\\3", temp_table_raw_temp)
    temp_table_raw_temp <- gsub("# ", "Num_", temp_table_raw_temp)
    temp_table_raw_temp <- gsub("% ", "Perc_", temp_table_raw_temp)
    temp_table_raw_temp <- gsub(", ", "_", temp_table_raw_temp)
    temp_table_raw_temp <- gsub(" - ", "-", temp_table_raw_temp)

    temp_table_raw_temp <- gsub("([A-Za-z$])([ ]*)\\*([ ]*)([0-9A-Za-z$])", "\\1_\\4", temp_table_raw_temp)
    temp_table_raw_temp <- gsub("([A-Za-z$]) ([0-9])", "\\1_\\2", temp_table_raw_temp)
    temp_table_raw_temp <- gsub("([0-9]) ([A-Za-z$])", "\\1_\\2", temp_table_raw_temp)
  
    temp_table_raw_temp[!(temp_table_raw_temp %like% "^( )*[a-zA-Z\\(\\)\\%\\#]")] <- paste0("---",
        temp_table_raw_temp[!(temp_table_raw_temp %like% "^( )*[a-zA-Z\\(\\)\\%\\#]")] )
  
    temp_table_temp     <- list()
  
    for (j in 1:length(temp_table_raw_temp) ) {
      con  <- textConnection(temp_table_raw_temp[[j]])
      data <- read.table(con, stringsAsFactors=F)
      close(con)
      # print(as.character(data))
      temp_table_temp[[j]] <-c(as.character(data))
    }
  
    max_col_temp    <- max(sapply(temp_table_temp,length))
    temp_table_temp <- lapply(temp_table_temp,function(x) c(x, 
                        rep("", max_col_temp-length(x))))
  
    names(temp_table_temp) <- c(letters, toupper(letters))[1:length(temp_table_temp)]
    temp_table_temp <- as.data.table(t(rbindlist(list(temp_table_temp),fill=F)))
  
    # ## small fixes
    temp_table_temp[1, marker:=0]
    temp_table_temp[V2=="[1]", marker:=1] 
    if (nrow(temp_table_temp[marker==1])==0) {
      first <- which(!(is.na(as.numeric(gsub("(\\*|,|−)*","",unlist(temp_table_temp[, c(2), 
                  with=F]))))))[1]
      temp_table_temp[c(first-1), marker:=1]
    }
    temp_table_temp[, marker:=na.locf(marker)]
    temp_table_temp <- temp_table_temp[marker==1][, marker:=NULL]
    temp_table_temp <- temp_table_temp[-1]
  
    temp_table_temp[, V1:=gsub("_", " ", V1), by=1:nrow(temp_table_temp)]
  
    temp_table_temp[,c(names(temp_table_temp)):=lapply(.SD,function(col) 
      gsub("\\(","[",col))]
    temp_table_temp[,c(names(temp_table_temp)):=lapply(.SD,function(col) 
      gsub("\\)","]",col))]
    temp_table_temp[,c(names(temp_table_temp)):=lapply(.SD,function(col) 
      gsub("−","--",col))]
    # temp_table_temp[,c(names(temp_table_temp)):=lapply(.SD,function(col) 
    #   ifelse(col %like% "\\(", paste0(col, ")"), col))]
  
  
    if(sum(temp_table_temp$V1=="---")==nrow(temp_table_temp)) {
        temp_table_temp[, V1:=NULL]
    }
  
    return(temp_table_temp)
  
  })
}

#----------------------------------------------------------------------------#
