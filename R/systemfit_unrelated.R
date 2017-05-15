#----------------------------------------------------------------------------#

#' Generate a systemfit object from a truly unrelated system of regressions (equivalent to systemfit vs. faster). 
#' @export
#' @import data.table
#' @import testit
#' @import systemfit
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

systemfit_unrelated <- function(..., sample_systemfit) {
     
  # ensure that sample fit object has the correct number of dimensions
  assert("appropriate sample_systemfit object (systemfit_unrelated())", 
    length(formula)==length(sample_systemfit$eq))

  # estimate each model
  temp_model <- lapply(formula, function(x) {

    temp <- lm(x, data=df)

  })

  # replace terms in sample_systemfit

  ## generate 'blan' var_cov matrix
  sample_systemfit$coefCov <- matrix(rep(0, 
      (length(temp_model)*length(coef(temp_model[[1]])))^2),
      ncol= (length(temp_model)*length(coef(temp_model[[1]]))))

  ## modify terms
  for (i in 0:(length(temp_model)-1)) {

    sample_systemfit$eq[i+1][[1]]$coefficients  <- coef(temp_model[[i+1]])
    
    sample_systemfit$eq[i+1][[1]]$coefCov       <- vcov(temp_model[[i+1]])
     
    sample_systemfit$coefCov[seq(from=length(coef(temp_model[[i+1]]))*i+1, 
              to=length(coef(temp_model[[i+1]]))*i+(length(coef(temp_model[[i+1]])))), 
              seq(from=length(coef(temp_model[[i+1]]))*i+1, 
              to=length(coef(temp_model[[i+1]]))*i+
              (length(coef(temp_model[[i+1]]))))]       <- vcov(temp_model[[i+1]])
    
    sample_systemfit$df.residual <- nrow(df)*length(temp_model)-
              length(temp_model)*length(coef(temp_model[[1]]))
  }

  return(sample_systemfit)

}

#----------------------------------------------------------------------------#
