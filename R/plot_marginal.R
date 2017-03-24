#----------------------------------------------------------------------------#

#' Annotate a plot with one/two side plots. 
#' 
#' Note: This function is designed to work with a Partners EHR master dataset, i.e. a data set
#' containing a dia_date_1 and gagne column.
#' @export
#' @import data.table
#' @import ggplot2
#' @import plotflow
#' @import grid
#' @import gridExtra
#' @import scales 
#' @param  TBC
#' @return TBC
#' @examples
#' TBC



plot_marginal <-  function(main_plot, hor_plot, vert_plot=NA, 
  layout_width=c(4, 0.5), 
  layout_height=c(0.5, 0.5, 2.5), plot_count=2, title_obj="", subtitle_obj="") {
  

  if (plot_count==2) {

  plot_1 <- ggplot_gtable(ggplot_build(main_plot))
  plot_2 <- ggplot_gtable(ggplot_build(hor_plot))
  plot_3 <- ggplot_gtable(ggplot_build(vert_plot))

  # Get maximum widths and heights
  maxWidth <- unit.pmax(plot_1$widths[2:3], plot_2$widths[2:3])
  maxHeight <- unit.pmax(plot_1$heights[4:5], plot_3$heights[4:5])

  # Set the maximums in the gtables for gt1, gt2 and gt3
  plot_1$widths[2:3] <- as.list(maxWidth)
  plot_2$widths[2:3] <- as.list(maxWidth)

  plot_1$heights[4:5] <- as.list(maxHeight)
  plot_3$heights[4:5] <- as.list(maxHeight)

  # Create a new gtable
  final_plot <- gtable(widths = unit(layout_width, "null"), 
    height = unit(layout_height, "null"))

  # Insert gt1, gt2 and gt3 into the new gtable
  final_plot <- gtable_add_grob(final_plot, plot_1, 3, 1)
  final_plot <- gtable_add_grob(final_plot, plot_2, 2, 1)
  final_plot <- gtable_add_grob(final_plot, plot_3, 3, 2)

  return(final_plot)

}

if (plot_count==1) {


  plot_1 <- ggplot_gtable(ggplot_build(main_plot))
  plot_2 <- ggplot_gtable(ggplot_build(hor_plot))

  # get max widths and heights
  maxWidth  <- unit.pmax(plot_1$widths[2:3], plot_2$widths[2:3])
  maxHeight <- unit.pmax(plot_1$heights[4:5], plot_1$heights[4:5])

  # Set the maximums in the gtables for gt1, gt2 and gt3
  plot_1$widths[2:3] <- as.list(maxWidth)
  plot_2$widths[2:3] <- as.list(maxWidth)

  plot_1$heights[4:5] <- as.list(maxHeight)
  
  # Create a new gtable

  title <- textGrob(title_obj, gp = gpar(fontsize = 10, fontfamily="URWHelvetica"), 
    just="left")
  subtitle <- textGrob(subtitle_obj, gp = gpar(fontsize = 5, fontfamily="URWHelvetica", 
    fontface="italic"), just="left")

  # final_plot <- gtable(widths=unit(rep(0.5,12), "null"),
  #              heights=unit(rep(0.5,15), "null"))
  
  # gtable_add_grobs <- gtable_add_grob # alias
  
  # final_plot <- gtable_add_grobs(final_plot, list(title, plot_2, plot_1), 
  #                        l=c(1,3,3),
  #                        r=c(8,12,12),
  #                        t=c(1,2,4),
  #                        b=c(1,3,15))
  
  final_plot <- gtable(widths = unit(layout_width, "null"), 
    height = unit(layout_height, "null"))

  # Insert gt1, gt2 and gt3 into the new gtable
  final_plot <- gtable_add_grob(final_plot, plot_1, 3, 1)
  final_plot <- gtable_add_grob(final_plot, plot_2, 2, 1)
  # final_plot <- gtable_add_grob(final_plot, subtitle, 2, 1)
  # final_plot <- gtable_add_grob(final_plot, title, 1, 1)

  return(final_plot)


 }
}

#----------------------------------------------------------------------------#


