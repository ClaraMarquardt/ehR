#----------------------------------------------------------------------------#
#' @title ggplot theme.
#'
#' @description ddd
#'
#' @export
#' @import ggplot2
#' @return
#' @examples

theme_basic <-  function(axis_size=0.5, title_size=8, subtitle_size=6, 
                  font_gen ="URWHelvetica", col_gen="grey50")  
  theme_bw() +
  theme(
    axis.text.x = element_text(size=rel(axis_size), colour = col_gen,
      family=font_gen),
    axis.text.y = element_text(size=rel(axis_size), colour = col_gen,
      family=font_gen), 
    axis.title.x = element_text(size=rel(axis_size), colour = col_gen,
    family=font_gen),
      axis.title.y = element_text(size=rel(axis_size), colour = col_gen,
    family=font_gen),
    plot.title = element_text(size = title_size, colour = col_gen, face = "bold",
      family=font_gen),
    plot.subtitle = element_text(size = subtitle_size, colour = col_gen, 
      face = "plain",family=font_gen),
    plot.caption = element_text(size = (subtitle_size-1), colour = col_gen, 
      face = "plain",family=font_gen)
  )

#----------------------------------------------------------------------------#
