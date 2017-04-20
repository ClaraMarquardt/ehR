#----------------------------------------------------------------------------#

#' @title Generate, format and save tabulation of the most important predictors 
#' in a model. 
#'
#' @description 
#'
#' @export
#' @import data.table
#' @import xlsx
#' @param feat_dt_raw 
#' @param feat_imp_var_list 
#' @param output_path 
#' @param feat_lim 
#' @param add_dt
#' @return
#' @examples

feat_dt <- function(feat_dt_raw, feat_imp_var_list, output_path, feat_lim=200, add_dt=NULL) {

      # prepare wb
      wb <- createWorkbook(type="xlsx")
      sheet = createSheet(wb, "feat_importance")

      # generate feat imp dt
      feat_dt <- feat_dt_raw[,mget(feat_imp_var_list)]
      setnames(feat_dt, tolower(names(feat_dt)))
      feat_dt <- feat_dt[1:feat_lim]

      num_col  <- which(sapply(feat_dt, function(x) class(x)[1]) %in% c("numeric"))
      char_col <- which(sapply(feat_dt, function(x) class(x)[1]) %in% c("character", "factor"))
      
      feat_dt[, c(num_col):=lapply(.SD, function(x) round(x, digits=4)), .SDcols=num_col]
      setnames(feat_dt, names(feat_dt)[char_col], "var_name")

      feat_dt[, var_cat:=gsub("^([^\\.]*)(\\.).*","\\1",var_name)]
      feat_dt[, var_timeframe:=gsub(".*_timeframe_(.*)$","\\1",var_name)]
      feat_dt[var_timeframe %like% "days_to_last", var_timeframe:="days_to_last"]
      feat_dt[var_timeframe==var_name, var_timeframe:="-"]

      if (length(feat_imp_var_list)<=char_col) {
          setcolorder(feat_dt, c(names(feat_dt)[1:char_col], "var_cat", "var_timeframe"))
      } else {
    
       setcolorder(feat_dt, c(names(feat_dt)[1:char_col], "var_cat", "var_timeframe", 
         names(feat_dt)[(char_col+1):(ncol(feat_dt)-2)]))
      }


      # add stats
      if(!is.null(add_dt[[1]])) {
        add_dt <- add_dt[var_name %in% feat_dt$var_name]
        feat_dt <- Reduce(function(x,y) mymerge(x, y, var_list="var_name"), 
                list(feat_dt, add_dt))
      }
    
      # add to xlsx & format
      addDataFrame(feat_dt,sheet, row.names=FALSE, col.names=TRUE)

      # cell_style
      var_cat     <- c("dia", "enc", "prc", "dem", "med", "mic", "lab", "lvs", "ed")
      var_cat_col <- brewer.pal(length(var_cat),"Spectral")

      inv_mapply(function(cat, col) {

        cs = CellStyle(wb) + Border(color=col) + Font(wb, color=col, isBold=TRUE)

        rows_raw <- (which(feat_dt$var_cat %like% cat))+1
        rows     <- getRows(sheet, rows_raw)
        cells    <- getCells(rows, colIndex = c(char_col+1)) 
    
        lapply(names(cells), function(ii)setCellStyle(cells[[ii]],cs))
      }, cat=var_cat, col=var_cat_col)

      saveWorkbook(wb,output_path)


  }


#----------------------------------------------------------------------------#



