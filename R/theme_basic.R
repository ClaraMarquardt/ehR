#----------------------------------------------------------------------------#

#' Basic ggplot theme.
#'
#' \
#' 
#' @details Maintained by: Clara Marquardt
#'
#' @export
#' @import ggplot2
#' @import extrafont
#' @import extrafontdb
#' 
#' @param title_size Font size - title (numeric) [default: 8].
#' @param subtitle_size Font size - sub title (numeric) [default: 6].
#' @param axis_size Font size - axes labels (numeric) [default: 0.5].
#' @param font Font type (character) [default: "URWHelvetica"].
#' @param col_gen Font color (character) [default: "grey50"].
#' @param legend_title_size  Font size - legend title (numeric) [default: 0.5].
#' @param legend_text_size Font size - legend text (numeric) [default: 0.4].
#' @param legend_tick_size Tick length (numeric) [default: 0.08].
#' @param legend_width Legend width (numeric) [default: 0.5].
#' @param legend_height Legend height (numeric) [default: 0.2].
#' @param legend_hjust_title Legend title adjustment (horizontal) (numeric) [default: 0.5].
#'
#' @return Formatted ggplot object. 
#' 
#' @examples \dontrun{
#' test_plot <- ggplot(data=dia[1:25]) + 
#'    geom_bar(aes(x=dia_code)) + 
#'    labs(
#'      title="Test Title", 
#'      subtitle="Test Subtitle", 
#'      x="Diagnosis Code", 
#'      y="Frequency (Number of Observations)", 
#'      caption="Test Plot - Details: * ------------------------ *") + 
#'    theme_basic(legend_tick_size=0.001)
#' ggsave("theme_basic_test.pdf", test_plot)
#'}

theme_basic <-  function(axis_size=0.5, title_size=8, subtitle_size=6, 
                  font ="URWHelvetica", col_gen="grey50", 
                  legend_title_size=0.5, legend_text_size=0.4, legend_tick_size=0.08, 
                  legend_width=0.5, legend_height=0.2, legend_hjust_title=0.5)  { 



  ## pre-built base theme
  theme_bw() +
  #
  # basic theme
  #
  theme(
    axis.text.x = element_text(size=rel(axis_size), colour = col_gen,
      family=font),
    axis.text.y = element_text(size=rel(axis_size), colour = col_gen,
      family=font), 
    axis.title.x = element_text(size=rel(axis_size), colour = col_gen,
    family=font),
      axis.title.y = element_text(size=rel(axis_size), colour = col_gen,
    family=font),
    plot.title = element_text(size = title_size, colour = col_gen, face = "bold",
      family=font),
    plot.subtitle = element_text(size = subtitle_size, colour = col_gen, 
      face = "plain",family=font),
    plot.caption = element_text(size = (subtitle_size-1), colour = col_gen, 
      face = "plain",family=font),
  #
  # basic theme extension - legend bottom
  #
    legend.position="bottom", 
    legend.key.height=unit(legend_height,"cm"),
    legend.key.width=unit(legend_width,"cm"),
    axis.ticks.length=unit(legend_tick_size,"cm"),
    legend.title=element_text(size=rel(legend_title_size), colour=col_gen, 
      family=font, hjust=legend_hjust_title, face="plain"),
    legend.text=element_text(size=rel(legend_text_size), colour=col_gen, 
     family=font)
   )

}


#----------------------------------------------------------------------------#
