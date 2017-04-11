#----------------------------------------------------------------------------#

#' @title ggplot theme.
#'
#' @description \
#'
#' @export
#' @import ggplot2
#' @return
#' @examples

theme_remove_all <- function() 

  theme(axis.text = element_blank(),
  	axis.title = element_blank(),
  	axis.ticks =  element_blank(),
  	# axis.text = element_text(margin=margin(0,0,0,0, "lines")),
  	axis.ticks.length = unit(0, "cm"), 
  	panel.background = element_blank())


#----------------------------------------------------------------------------#
