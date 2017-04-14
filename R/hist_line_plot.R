#----------------------------------------------------------------------------#

#' @title Generate and save a predicted vs. actual probability (i.e. calibration 
#' plot).
#'
#' @description 
#'
#' @export
#' @param dt_list 
#' @param outcome_var 
#' @param cat_var 
#' @param cat_num 
#' @param unit_desc 
#' @param plot_title 
#' @param subtitle_list 
#' @param "") 
#' @param return_data 
#' @param plot_point 
#' @param plot_id_line 
#' @param outcome_name 
#' @param output_path 
#' @param output_name 
#' @param hor_scaling_factor 
#' @return
#' @examples


hist_line_plot <- function(dt_list, outcome_var, cat_var, cat_num=10, unit_desc="deciles", 
  plot_title="", subtitle_list=list("", ""), return_data=FALSE, plot_point=TRUE,
  plot_id_line=TRUE, outcome_name="outcome", output_path, output_name, 
  hor_scaling_factor=10) {

	plot_data_list <- mapply(function(data, subtitle) {
  	
    	# collapse
    	dt_temp <- copy(data)
    	dt_temp[, temp_id:=1:nrow(dt_temp)]
    	dt_temp[, c(outcome_var):=as.numeric(as.character(get(outcome_var)))]
	
	
    	dt_temp[, c("cat"):=as.numeric(cut(get(cat_var),seq(0,1, by=(1/cat_num)), labels=F))]
    	dt_temp[is.na(cat) & pred==0, cat:=1]
    	dt_temp[, c("cat_mid_point"):=(cat*(1/cat_num)-(1/cat_num/2))]
	
    	dt_temp[, c("cat_count"):=.N, by=c("cat")]
    	dt_temp[, c("outcome_mean"):=lapply(.SD, function(x)  mean(x)), 
    	 .SDcols=c(outcome_var), by=c("cat")]
	
    	dt_temp_min <- data.table(outcome_mean=dt_temp[pred==min(pred)]$outcome_mean,
    	  cat_mid_point=dt_temp[pred==min(pred)]$pred, cat=min(dt_temp$cat))[1]
    	dt_temp_max <- data.table(outcome_mean=dt_temp[pred==max(pred)]$outcome_mean,
    	  cat_mid_point=dt_temp[pred==max(pred)]$pred, cat=max(dt_temp$cat))[1]
    	
    	dt_temp <-  unique(dt_temp, by=c("cat"))
    	dt_temp <- rbindlist(list(dt_temp_min, dt_temp, dt_temp_max), use.names=T, fill=T)
    	dt_temp_melt <- melt.data.table(dt_temp, id.vars=c("cat", "cat_count", "cat_mid_point"), 
    	  measure.vars=patterns("_mean$"))
    	setnames(dt_temp_melt, c("variable", "value1"), c("outcome_var_coll", "outcome_var_coll_mean"))
	
    	dt_temp[,cat_mid_point:=round(cat_mid_point,digits=3)]
	
    	dt_temp_expand <- dt_temp[!is.na(cat_count)][data.table(cat_mid_point=seq(1/cat_num/2, 
    	    1-(1/cat_num/2),by=(1/cat_num))),on=c("cat_mid_point"),nomatch=NA]
    	dt_temp_expand[is.na(cat_count),cat_count:=0]
	
    	# hor plot
    	temp_plot_hor <- ggplot(data=dt_temp_expand, aes(x=cat_mid_point, y=cat_count/hor_scaling_factor)) +
    	  geom_bar(stat="identity",width=0.08) + 
    	  scale_x_continuous(expand = c(0, 0)) + 
    	  geom_text(data=dt_temp[!is.na(cat_count)], aes(label=cat_count, x=cat_mid_point), 
    	    y=(-1*max(dt_temp[!is.na(cat_count)]$cat_count/hor_scaling_factor)/4),size=1, colour = "black",
    	    family="URWHelvetica", vjust=-0.5, fontface="plain")+
    	  coord_cartesian(ylim = c((-1*max(dt_temp[!is.na(cat_count)]$cat_count/hor_scaling_factor)/3),
    	    (max(dt_temp[!is.na(cat_count)]$cat_count/hor_scaling_factor)+2))) +
    	  theme_remove_all() +
    	 theme(plot.margin= unit(c(0, 3.3, 0,3.2), "lines"), legend.position="none")
	
    	# main plot 
    	 temp_plot_main <- ggplot(data=dt_temp_melt, aes(x=cat_mid_point, y=outcome_var_coll_mean)) +
    	  geom_line(aes(colour=as.factor(outcome_var_coll))) + 
    	  labs(
    	    y="Observed probability (averaged observed outcome)", 
    	    x=sprintf("Predicted probability (%s)", unit_desc)
    	  ) +
    	  scale_color_discrete(labels = outcome_name) + 
    	  theme_basic() +
    	  theme_legend_bottom() +
    	  guides(colour = guide_legend(title = "", labels=c(outcome_name)))
	
    	temp_plot_main_title <- ggplot(data=dt_temp_melt, aes(x=cat_mid_point, y=outcome_var_coll_mean)) +
  			  geom_line(aes(colour=as.factor(outcome_var_coll))) + 
  		  labs(
  		    title=plot_title, 
  		    subtitle=subtitle, 
  		    y="Observed probability (averaged observed outcome)", 
  		    x=sprintf("Predicted probability (%s)", unit_desc)
  		  ) +
  		  theme_basic() +
  		  scale_color_discrete(labels = outcome_name) + 
  		  theme_legend_bottom() +
  		  guides(colour = guide_legend(title = "", labels=outcome_name))


  		if(plot_point==TRUE) {
  		  temp_plot_main <- temp_plot_main +
  		    geom_point(data=dt_temp_melt[!is.na(cat_count)], aes(x=cat_mid_point, y=outcome_var_coll_mean), 
  		      size=0.1)
  		}
  		
  		if(plot_id_line==TRUE) {
  		  max_x <- ceiling(dt_temp_melt[nrow(dt_temp_melt)]$cat_mid_point/(1/cat_num))*(1/cat_num)
  		  max_y <- dt_temp_melt[nrow(dt_temp_melt)]$outcome_var_coll_mean
  		
  		  min_x <- dt_temp_melt[1]$cat_mid_point
  		  min_y <- dt_temp_melt[1]$outcome_var_coll_mean
  		
  		  temp_plot_main <- temp_plot_main +
  		    geom_line(data=data.table(xvar=c(0,1), yvar=c(0,1)), aes(x=xvar, y=yvar), 
  		    linetype="dashed", color="grey30", size=0.3) +
  		    coord_cartesian(xlim = c(0,1), ylim = c(0,1)) 
  		   
  		}
  		
  		# combine
  		  temp_plot <- plot_marginal(temp_plot_main, hor_plot=temp_plot_hor, layout_width=c(6), 
  		    layout_height=c(1, 0.3, 4), plot_count=1, title_obj=plot_title, 
  		    subtitle_obj=subtitle) 

  		 return(list(temp_plot=temp_plot, temp_plot_title=temp_plot_main_title, 
  		    dt_temp=dt_temp))

  	}, data=dt_list, subtitle=subtitle_list, SIMPLIFY = FALSE)

    generate_pdf(lapply(plot_data_list, "[[", 1),  ncol_plot=1, nrow_plot=1, 
     file_path=output_path, file_name=output_name, 
     orientation="horizontal")
    generate_pdf(lapply(plot_data_list, function(x) x["temp_plot_title"][[1]]),  
        ncol_plot=1, nrow_plot=1, file_path=output_path, file_name=paste0(output_name, "_title"), 
       orientation="horizontal")

     ## return
  	if(return_data==TRUE) {
    write.csv(plot_data_list[[1]]$dt_temp, gsub("\\.pdf$", "_train\\.csv", 
    	paste0(output_path, output_name)), row.names=F)
    write.csv(plot_data_list[[2]]$dt_temp, gsub("\\.pdf$", "_test\\.csv", 
    	paste0(output_path, output_name)), row.names=F)

  }

}

#----------------------------------------------------------------------------#


