#----------------------------------------------------------------------------#

#' @title Generate and save a (test and train) ROC curve. 
#'
#' @description 
#'
#' @export
#' @import ROCR
#' @param pred_list 
#' @param outcome_list 
#' @param model_name 
#' @param title 
#' @param path 
#' @return
#' @examples

rocr_basic <- function(pred_list, outcome_list, model_name,  title, path, point=NULL, label_offset=0.02 ) {

  rocr_val  <- mapply(function(pred, y) {list(predictions=pred , labels=y)}, 
    				pred=pred_list, y=outcome_list, SIMPLIFY=F)
  rocr_stat  <- lapply(rocr_val, function(x) {prediction(x[[1]], x[[2]])} )
  rocr       <- lapply(rocr_stat, function(x) {performance(x, "tpr","fpr")} )
  
  auc.tmp <- lapply(rocr_stat, function(x) {performance(x,"auc")})
  auc 	  <- lapply(auc.tmp, function(x) {round(as.numeric(x@y.values), digits=4)})
    
  pdf(path,family="Verdana")

  plot(rocr[[1]],col="darkblue", lty=1, frame= FALSE, box.lwd=0.001, xlim = c(0,1), ylim=c(0,1), 
      main=paste0(title), sub=paste0("AUC: ", paste0(auc, collapse=" / ")), 
      xlab="FPR", ylab="TPR", axes=FALSE, xaxt="n", yaxt="n", xaxs="i", yaxs="i", lwd=1.5)
 
  abline(0,1,col="grey30", lty=2, lwd=0.5)

  text(0.7,0.05,model_name, cex=0.5)

  points(point[[1]], point[[2]], col = "blue", cex = 0.5)
  text(point[[1]], point[[2]]-label_offset, point[[3]], cex = 0.5)

  lapply(rocr, function(x) {
    plot(x,col="darkgreen", lty=1, frame= FALSE, box.lwd=0.001, xlim = c(0,1), ylim=c(0,1), 
      main=paste0(title), sub=paste0("AUC: ", paste0(auc, collapse=" / ")), 
      xlab="FPR", ylab="TPR", axes=FALSE, xaxt="n", yaxt="n", xaxs="i", yaxs="i", add=T, lwd=1.5)
  })

  dev.off()

}

#----------------------------------------------------------------------------#
