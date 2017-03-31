#----------------------------------------------------------------------------#

#' Write toxslx sheet with a number of formatting options and safety checks.
#' @export
#' @import rJava 
#' @import xlsx
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

write_xlsx_mod <- function(dt, file, sheetName, lock=FALSE, password="", 
  unlock_col=NA, col_width=NA, alignment_var="left", bold=FALSE, format_pattern="") {

  # note
  # sample col width specification: list(list(c(1,2),30), list(2,20), list(3,2))

    if (!(file.exists(file))) {
      print("wb does not exist - cannot use write.xlsx_safe")
      break()
    }

    wb = loadWorkbook(file)
    sheet = createSheet(wb, sheetName )
    addDataFrame(dt,sheet, row.names=FALSE, col.names=TRUE)

    if (!is.na(col_width)) {

      lapply(col_width, function(x) {
        lapply(x[[1]], function(y) {
             if(unlist(x[2])=="auto") {
              autoSizeColumn(sheet, colIndex=y)
             } else {
              setColumnWidth(sheet, colIndex=y, colWidth=unlist(x[2]))
             }
          })
      })
    }

    if(lock==TRUE) {
       cs = CellStyle(wb, cellProtection = CellProtection(locked=T)) 
       if(!is.na(unlock_col)) {
        cs = CellStyle(wb, cellProtection = CellProtection(locked=F)) # setting style to unlock cells
        rows <- getRows(sheet, 1:1000)
        cells <- getCells(rows, colIndex = unlock_col) 
        lapply(names(cells), function(ii)setCellStyle(cells[[ii]],cs))
       }
      .jcall(sheet, "V", "protectSheet", password)
    
    }

    if(bold==TRUE) {
       
      BOLD_CELL_STYLE <- CellStyle(wb)+ Font(wb,isBold=TRUE)

      rows <- getRows(sheet)
      cells <- getCells(rows) 
      inv_lapply(1:length(cells),function(x){

        if (cells[x][[1]]$getStringCellValue() %like% format_pattern) {
      
          setCellStyle(cells[[x]], BOLD_CELL_STYLE)

        }

      })

  }

    # if(alignment_var=="right") {
    #   align <- Alignment(horizontal="ALIGN_RIGHT")

    #    cs = CellStyle(wb, alignment= align) 
    # }

  
    saveWorkbook(wb, file)
}

#----------------------------------------------------------------------------#
