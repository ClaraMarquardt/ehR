#----------------------------------------------------------------------------#

#' Generate a correlation matrix, summary statistics and a distribution graph for the variables in a given dt. 
#' @export
#' @import data.table
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

corr_stat <- function(dt, var_list_list, save=FALSE, path=NA) {

    stat <- lapply(var_list_list, function(x) {

    cat("\n\n")
    cat(x)
    cat("\n\n")

    # generate corr matrix
    cat("\n\ngenerate corr matrix\n\n")
    corr_matrix <- cor(as.matrix(dt[, mget(c(x))][complete.cases(dt[, mget(c(x))])]))

    # generate stats
    cat("\n\ngenerate summ stats\n\n")
    summ_stat <- data.table(do.call(data.frame,lapply(x, function(y) data.table(stat=names(summary(dt[, get(y)])),
      var=(matrix(summary(dt[, get(y)])))))))
    setnames(summ_stat, "stat", "stat_name")
    summ_stat[, c(grep("stat.[0-9]", names(summ_stat), value=T)):=NULL]
    setnames(summ_stat, c("stat_name", x))

    # generate dist graphs
    cat("\n\ngenerate dist graphs\n\n")
    dt_graph <-copy(dt)
    dt_graph[,melt_id:=1:nrow(dt)]
    dt_graph <- melt(dt_graph, id.vars="melt_id", measure.vars=x)
    dist_graph <- ggplot(data=dt_graph, aes(x=value, colour=variable)) +
      geom_density() +
      labs(
        title="variable distribution"
      ) +
      theme_basic(axis_size=0.3, title_size=4,subtitle_size=3) +
        theme_legend_bottom(title_size=0.3, text_size=0.3, tick_size=0, legend_width=0.5, 
          legend_height=0.15)

    return(list(corr_matrix, summ_stat, dist_graph))

  })

  if (save==TRUE) {

    for(i in 1:length(var_list_list)) {
      cat(sprintf("begin saving output from %s", paste0(var_list_list[i], collapse="  ")))

      write.csv(sapply(stat, "[", 1)[i], paste0(path,"corr_matrix_", 
        paste0(var_list_list[i][[1]][[1]],collapse="_") ,"_", i,"_", ".csv"))
     
      write.csv(sapply(stat, "[", 2)[i], paste0(path, "summ_stat_", 
        paste0(var_list_list[i][[1]][[1]],collapse="_") ,"_", i,"_", ".csv"), row.names=F)
     
      ggsave(paste0(path, "dist_graph_", paste0(var_list_list[i][[1]][[1]],collapse="_"),
        "_", i,"_", ".pdf"),sapply(stat, "[", 3)[i][[1]])

    }

    } else {

      return(stat)

    }
}

#----------------------------------------------------------------------------#
