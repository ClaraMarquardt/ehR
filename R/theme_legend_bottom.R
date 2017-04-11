#----------------------------------------------------------------------------#

#' @title ggplot theme.
#'
#' @description \
#'
#' @export
#' @import ggplot2
#' @return
#' @examples

theme_legend_bottom <- function(title_size=0.5, text_size=0.4, tick_size=0.08,
  legend_width=0.5, legend_height=0.2, hjust_title=0.5, font_gen ="URWHelvetica", 
  col_gen  ="grey50") 

  theme(
   	legend.position="bottom", 
   	legend.key.height=unit(legend_height,"cm"),
   	legend.key.width=unit(legend_width,"cm"),
   	axis.ticks.length=unit(tick_size,"cm"),
   	legend.title=element_text(size=rel(title_size), colour=col_gen, family=font_gen, 
      hjust=hjust_title, face="plain"),
   	legend.text=element_text(size=rel(text_size), colour=col_gen, family=font_gen))
  


#----------------------------------------------------------------------------#
