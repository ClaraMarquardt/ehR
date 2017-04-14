#----------------------------------------------------------------------------#

#' @title Simple auc calculation. 
#'
#' @description Generate auc for e.g. test and train predictions/outcomes 
#' at the same time.
#'
#' @export
#' @import ROCR
#' @param pred 
#' @param outcome 
#' @param auc_digits 
#' @return
#' @examples

model_auc <- function(pred, outcome, auc_digits=6) {

    rocr_val   <- list(predictions=pred , labels=outcome)
    rocr_stat  <- prediction(rocr_val[[1]], rocr_val[[2]])

    auc.tmp <- performance(rocr_stat,"auc")
    auc     <- round(as.numeric(auc.tmp@y.values), digits=auc_digits)

    return(auc)

}

#----------------------------------------------------------------------------#

