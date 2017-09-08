#----------------------------------------------------------------------------#

#' @title: Format a given plot by adding a summary of the data as a footnote.
#' 
#' @description: A function that, given a ggplot object, list of summaries (each obtained using summary() in base R), a name for each summary object and an output file, returns (and saves) a version of the plot with a the summary (summaries) at the bottom of the plot.
#' 
#' @detail: Maintained by: Shreyas Lakhtakia
#'
#' @export
#' @import gridExtra
#' @import grid
#' @import magrittr
#' @import ggplot2
#' @param ggplt The ggplot to which the summary(summaries) are to be appended (ggplot2 object)
#' @param summary_list The list of summaries obtained using summary() (list of summary() object [technically same as data type being summarized])
#' @param summary_title_vec Vector of strings to be used as titles for the summary (summaries) (vector/array of character)
#' @param ndigit Decimal precision to use in writing the summary statistics (integer)
#' @param plot_theme Whether to format the plot (using theme_basic()) (logical)
#' @param font fontfamily used in writing footnote, "Helvetica" by default (character)
#' @param font_size footnote text size, 8 by default (numeric)
#' @param font_color color of footnote text, dark grey by default (character expressing hex or RGB color)
#' @param output_file Path and filename to save resulting plot with footnote as (NA, means don't save - default) (character)
#' 
#' @examples \dontrun{
#' 		gender_summary <- list(summary(dem[gender == "female", ]$age), summary(dem[gender == "male", ]$age))
#' 		age_distribution <- ggplot(data = dem, aes(x = age, color = factor(gender))) + geom_density()
#' 		ggplot_footnote_summary(ggplt = age_distribution, summary_list = gender_summary, summary_title_vec = c("Female", "   Male"), output_file = "age_distribution_by_gender.png")
#' }

ggplot_footnote_summary <- function(ggplt, summary_list, summary_title_vec, ndigit = 3, font = "Helvetica", font_size = 8, font_color = "#3A3F3F", output_file = NA, plot_theme=TRUE){

	##################### concatenate summary list ################

	summ_string     <- lapply(summary_list, function(x) paste(names(x), format(x, digits = ndigit), collapse = "; ")) # for each item in list, paste summary stats together
	footnote_string <- paste(summary_title_vec, summ_string, sep = ": ") %>% paste(., collapse = "\n ") # combine list into a string

	##################### add footnote to plot #####################

	if (plot_theme==TRUE) {
		ggplt <- ggplt + theme_basic()
	}

	g <- arrangeGrob(ggplt, 
					 bottom = textGrob(footnote_string, x = 0, hjust = -0.2, vjust=0.2, 
					 	 gp = gpar(fontfamily = font, font_size = font_size, col = font_color))
					 )
	if(!is.na(output_file)){
		ggsave(output_file, g)
	}

	return(g)
}

#----------------------------------------------------------------------------#