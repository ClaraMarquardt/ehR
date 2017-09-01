#----------------------------------------------------------------------------#

#' Convert nuanced, specific races in a data.table into this aggregated set:
#' {white, black, hispanic, other}.
#' 
#' Author: Shreyas Lakhtakia (slakhtakia [at] bwh.harvard.edu)
#' 
#' @export
#' @import data.table
#' @param data a data.table with the column containing races
#' @param race_col a string specifying the name of column containing race info, "race" by default
#' @param inplace if TRUE old race column is replaced with aggregated race infor, if FALSE (default), a new column called "race_group" is added 
#' @return data.table with the a new race_group column containing aggregated race / old column replaced with this column if inplace = TRUE
#' @examples
#' mod_dem <- group_race(dem, inplace = TRUE)
#' unique(mod_dem[, race, ], by = "race")
#' 
#' grp_dem <- group_race(dem)
#' unique(grp_dem[, .(race, race_group), ], by = "race")

group_race <- function(data, race_col = "race", inplace = FALSE) {

	# create copy so as to not modify provided data.table
	fn_data <- copy(data)

	# create new temporary column
	fn_data[, race_group := "none", ]

	# race regular expressions
	black_race_regex    <- "black|african"
	hispanic_race_regex <- "hispanic"
	white_race_regex    <- "white|european"

	# convert races to grouped versions	
	fn_data[(tolower(get(race_col)) %like% black_race_regex & race_group == "none"), race_group := "black"]
	fn_data[(tolower(get(race_col)) %like% hispanic_race_regex & race_group == "none"), race_group := "hispanic"]
	fn_data[(tolower(get(race_col)) %like% white_race_regex & race_group == "none"), race_group := "white"]
	fn_data[race_group == "none", race_group := "other"]

	# make replacement in-place if so desired
	if(inplace){
		fn_data[, as.character(race_col) := race_group, ]
		fn_data[, race_group := NULL, ]
	}

	# return
	return(fn_data)
}

#----------------------------------------------------------------------------#