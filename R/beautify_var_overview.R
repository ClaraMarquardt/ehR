#----------------------------------------------------------------------------#

#' @title: Format a given var_overview() outcome as a formatted two tab .xlsx sheet.
#' 
#' @description: Convert "var_overview" returned by Clara Marquardt's var_overview function into a production ready Excel file, with a separate tab for an overview of the data and a different one for a list of summary statistics from the data.
#' 
#' @detail: Maintained by: Shreyas Lakhtakia; References: var_overview(), http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r
#'
#' @export
#' @import splitstackshape
#' @import data.table
#' @import xlsx
#' @param var_overview_dt df returned by var_overview() (data.frame from var_overview)
#' @param data_definition_dt notes to be appended at the end of the overview, such              as variable definitions and other points. This *MUST* be a 2 COLUMN data.table (data.table)
#' @param output_file path and filename to output resulting Excel to (character)
#' @param source source of data / cohort name (to print in Excel) (character)
#' @param sheet_title Title of overview sheet (character)
#' @examples \dontrun{
#' var_overview_dt <- var_overview(dem)
#' beautify_var_overview(var_overview_dt = var_overview_dt, output_file = "Summary.xlsx")
#' }

beautify_var_overview <- function(var_overview_dt, data_definition_dt = NA, output_file, 
							source = "", sheet_title = "Data Summary") {

	# dependencies: Helper function to add titles
	# - sheet : sheet object to contain the title
	# - rowIndex : numeric value indicating the row to contain the title
	# - title : the text to use as title
	# - titleStyle : style object to use for title
	xlsx.addTitle <- function(sheet, rowIndex, colIndex = 1, title, titleStyle){
	  rows       <-createRow(sheet,rowIndex=rowIndex)
	  sheetTitle <-createCell(rows, colIndex=colIndex)
	  setCellValue(sheetTitle[[1,1]], title)
	  setCellStyle(sheetTitle[[1,1]], titleStyle)
	}

	wb <- createWorkbook(type = "xlsx")

	# identity row number to split var_overview at 
	var_overview_dt <- data.table(var_overview_dt)
	break_row <- var_overview_dt[var_name == "unit of observation:", which = TRUE ]
	# split into overview and summary stats
	
	# 
	# Overview
	# ---------------------------------------------------------------------

	# extract file name from the second row, and other details from the bottom of the dt
	overview <- copy(var_overview_dt[c(2, (break_row-1):(nrow(var_overview_dt)-3)), ]) 
	# keep only first two columns
	overview <- overview[, 1:2, with = FALSE]
	# convert the values to numeric
	overview[, var_type := as.numeric(var_type)]

	# bind in the notes if provided
	if(!is.na(data_definition_dt)){
		setnames(data_definition_dt, "var_name", "var_type")
		overview <- rbind(overview, data_definition_dt)
	}

	# shift everything one left - first column becomes rownames, ...
	rownames(overview) <- overview[, var_name, ]
	overview[, var_name := var_type, ]
	overview[, var_type := NULL, ]

	# 
	# Summary
	# ---------------------------------------------------------------------
	
	summary  <- copy(var_overview_dt[4:(break_row - 1), ]) # extract summary portion of the dt

	# move 5 most common values column to the end and split into 5 columns
	summary[, most_common_value := most_common_five_non_NA, ]
	summary <- cSplit(summary, splitCols = "most_common_value", sep = " , ")
	summary[, most_common_five_non_NA := NULL, ]
	
	# move variable type to the end
	summary[, type := var_type, ]
	summary[, var_type := NULL, ]
	
	# turn var_name to rownames and drop var_name column
	rownames(summary) <- summary[, var_name, ]
	summary[, var_name := NULL, ]

	# format colnames
	setnames(summary, names(summary), sapply(names(summary), code_to_human))

	# 
	# Style and write
	# ---------------------------------------------------------------------

	# create workbook
	wb <- createWorkbook(type = "xlsx")

	# Title and sub title styles
	TITLE_STYLE <- CellStyle(wb)+ Font(wb, heightInPoints=13, isBold=TRUE, underline=1)
	SUB_TITLE_STYLE <- CellStyle(wb) + Font(wb, heightInPoints=11, isItalic=TRUE, isBold=FALSE)
	
	# Styles for the data table row/column names
	TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
	TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
	    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
	    Border(color="black", position=c("TOP", "BOTTOM"), 
	           pen=c("BORDER_THIN", "BORDER_THICK")) 
	
	# create sheets for overview and summary
	overview_sheet <- createSheet(wb, sheetName = "Overview")
	summary_sheet  <- createSheet(wb, sheetName = "Summary")

	# add the title and source to overview sheet
	# Add title
	xlsx.addTitle(overview_sheet, rowIndex=1, title= sheet_title, titleStyle = TITLE_STYLE)
	# Add sub title
	xlsx.addTitle(overview_sheet, rowIndex=2, title=source, titleStyle = SUB_TITLE_STYLE)


	# write the overview
	addDataFrame(overview, overview_sheet, startRow=3, startColumn=1, col.names = FALSE, rownamesStyle = TABLE_ROWNAMES_STYLE)
	# write the summary stats
	addDataFrame(summary, summary_sheet, startRow=1, startColumn=1,              colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE)

	# Save the workbook to a file
	saveWorkbook(wb, output_file)

	# write.xlsx(overview, file = output_file, sheetName = "Overview")
	# write.xlsx(summary, file = output_file, sheetName = "Summary", append = TRUE)

}
#----------------------------------------------------------------------------#