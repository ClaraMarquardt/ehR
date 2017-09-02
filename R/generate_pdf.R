#----------------------------------------------------------------------------#

#' Convert a list of ggplot objects into a (multi-page) PDF.
#' 
#' \
#' 
#' @details Maintainer: Clara Marquardt
#' 
#' @export
#' @import ggplot2
#' @import plotflow
#' @import grid
#' @import gridExtra
#' @import scales 
#' 
#' @param ggplot_list List of ggplot objects (list - ggplot).
#' @param ncol_plot Number of plot columns on each page of the final PDF (integer) [default: 3]. 
#' @param nrow_plot Number of plot rows on each page of the final PDF (integer) [default: 3]. 
#' @param file_path Path where the final PDF is to be saved ("...../") (character).
#' @param file_name Namer under which the final PDF is to be saved ("...pdf") (character).
#' @param orientation Page orientation of the final PDF (character - "vertical"/"horizontal") [default: "vertical"].
#' @param share_legend Whether or not all plots on a page are to share one legend (logical - TRUE/FALSE) [default: FALSE].
#' @param quiet Whether to print status updates as individual pages are generated (logical - TRUE/FALSE) [default: FALSE].
#'
#' @return Generate and save a PDF with all plots. 
#' 
#' @examples \{dontrun
#' plot_1 <- ggplot(data=dia[1:15]) + 
#'	geom_bar(aes(x=dia_code)) + 
#'	theme_basic()
#' plot_list <- list(plot_1, plot_1, plot_1, plot_1,plot_1, plot_1, plot_1)
#' generate_pdf(ggplot_list=plot_list, ncol_plot=3, nrow_plot=2, 
#'  file_path="", file_name="merge_pdf_test", orientation="horizontal", 
#'  share_legend=FALSE, quiet=FALSE) 
#'}

generate_pdf <- function(ggplot_list, ncol_plot=3, nrow_plot=3, 
  file_path, file_name, orientation="vertical", share_legend=FALSE, quiet=FALSE, 
  height_plot=NULL, width_plot=NULL, height_overall=NULL, width_overall=NULL) {
  
  # define parameters
  graph_cat <- length(ggplot_list)
  graph_count <- nrow_plot*ncol_plot
  graph_page <- ceiling(graph_cat/(graph_count))

  # helper function
  grid_arrange_shared_legend <- function(...) {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
      legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(arrangeGrob(grobs=lapply(plots, function(x)
      x + theme(legend.position="none")),ncol = ncol_plot, nrow=nrow_plot),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight))

   }

   # create a temporary directory
   temp_folder_plot <- paste0("temp_plot_folder_", as.character(format(Sys.time(), 
   	"%H_%M_%S_%d_%m_%y")), "_", toupper(sample(letters, 1)))
   dir.create(temp_folder_plot)

   # loop over graphs - generate PDF pages
   temp1 <- 1
   temp2 <- graph_count

   for (k in 1:graph_page) {
    
    if(orientation=="vertical") pdf(paste0(temp_folder_plot, "/", file_name, "_", k, ".pdf")) 
     else pdf(paste0(temp_folder_plot, "/", file_name, "_", k, ".pdf"), 
      height=ifelse(is.null(height_overall), 7.6, height_overall), 
      width=ifelse(is.null(width_overall), 11, width_overall))

   	  temp2 <- min(temp2, length(plot_list))

      onepage <- plot_list[temp1:temp2]

   	  if(share_legend==FALSE) {
       
       width_value  <- NULL
       height_value <- ifelse(is.null(height_plot), list(7.6/(ncol_plot+0.5)), list(height_plot))[[1]]
       width_value  <- NULL
       width_value  <- if(!is.null(width_plot))  width_value  <- width_plot
    
       do.call(grid.arrange,c(onepage, list(ncol=ncol_plot, 
      	heights=rep(height_value,nrow_plot), 
      	widths=rep(width_value,ncol_plot))))
   
   	  } else {
        
        do.call(grid_arrange_shared_legend,c(onepage))
  
  	  }

  	  dev.off()

      temp1 <- temp1 + graph_count
      temp2 <- temp2 + graph_count

      if (quiet==FALSE) print(paste0("successfully generated page ", k, 
      	" out of ", graph_page))
  
  }

  # ensure that the file list is correctly sorted 
  file_list <- file.path(temp_folder_plot, dir(temp_folder_plot))
  file_list <- file_list[order(as.numeric(gsub("(.*)(_)([0-9]*)(\\.pdf)$", "\\3", file_list)))]

  # merge the generated PDF & save
  merge_PDF(input_pdf_list=file_list,
  	output_file_name=paste0(file_path, file_name, "_combined.pdf"))

  # delete the temporary folder
  unlink(temp_folder_plot, recursive=TRUE)

}

#----------------------------------------------------------------------------#


