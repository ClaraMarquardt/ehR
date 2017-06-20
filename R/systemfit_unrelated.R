#----------------------------------------------------------------------------#

#' Generate a systemfit object from a truly unrelated system of regressions (equivalent to systemfit vs. faster). 

#' @export
#' @import data.table
#' @import testit
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

systemfit_unrelated <- function(formula, df, sample_systemfit) {
  
  # --------------------------------------------------------------
  # ## Calculate the residual covariance matrix
  # ## source: https://rdrr.io/rforge/systemfit/src/R/calcResidCov.R
  # .calcResidCov <- function( resids, methodResidCov, validObsEq = NULL,
  #     nCoefEq = NULL, xEq = NULL, diag = FALSE, centered = FALSE,
  #     useMatrix = FALSE, solvetol = .Machine$double.eps ) {

  #  eqNames <- NULL
  #  if( class( resids ) == "data.frame" ) {
  #     resids <- as.matrix( resids )
  #     validObsEq <- !is.na( resids )
  #     eqNames <- names( resids )
  #  } else if( !is.null( validObsEq ) ) {
  #     residMat <- matrix( NA, nrow = nrow( validObsEq ),
  #        ncol = ncol( validObsEq ) )
  #     for( i in 1:ncol( validObsEq ) ) {
  #        residMat[ validObsEq[ , i ], i ] <- resids[
  #           ( 1 + sum( validObsEq[ , 0:(i-1) ] ) ):( sum(validObsEq[ , 1:i ] ) ) ]
  #     }
  #     resids <- residMat
  #     rm( residMat )
  #  } else {
  #     stop( "internal error in .calcResidCov: if argument 'validObsEq'",
  #        " is not provided, argument 'resids' must be a data.frame'" )
  #  }

  #  nEq <- ncol( validObsEq )
  #  result <- matrix( 0, nEq, nEq )
  #  if( centered ) {
  #     for( i in 1:nEq ) {
  #        resids[ , i ] <- resids[ , i ] - mean( resids[ validObsEq[ , i ], i ] )
  #     }
  #  }
  #  validObsAll <- rowSums( !validObsEq ) == 0
  #  for( i in 1:nEq ) {
  #     for( j in ifelse( diag, i, 1 ):ifelse( diag, i, nEq ) ) {

  #        if( methodResidCov == "noDfCor" ) {
  #           result[ i, j ] <-
  #              sum( resids[ validObsAll, i ] * resids[ validObsAll, j ] ) /
  #              sum( validObsAll )
  #        } else if( methodResidCov == "geomean" ) {
  #           result[ i, j ] <-
  #              sum( resids[ validObsAll, i ] * resids[ validObsAll, j ] ) /
  #              sqrt( ( sum( validObsAll ) - nCoefEq[i] ) * ( sum( validObsAll ) - nCoefEq[j] ) )
  #        } else if( methodResidCov == "Theil" ) {
  #           result[ i, j ] <-
  #              sum( resids[ validObsAll, i ] * resids[ validObsAll, j ] ) /
  #              ( sum( validObsAll ) - nCoefEq[i] - nCoefEq[j] + sum( diag(
  #              solve( crossprod( xEq[[i]] ), tol=solvetol ) %*%
  #              crossprod( xEq[[i]], xEq[[j]]) %*%
  #              solve( crossprod( xEq[[j]] ), tol=solvetol ) %*%
  #              crossprod( xEq[[j]], xEq[[i]] ) ) ) )

  #        } else if( methodResidCov == "max" ) {
  #           result[ i, j ] <-
  #              sum( resids[ validObsAll, i ] * resids[ validObsAll, j ] ) /
  #              ( sum( validObsAll ) - max( nCoefEq[ i ], nCoefEq[ j ] ) )
  #        } else {
  #           stop( paste( "Argument 'methodResidCov' must be either 'noDfCor',",
  #                 "'geomean', 'max', or 'Theil'." ) )
  #        }
  #     }
  #  }
  #  if( !is.null( eqNames ) ) {
  #     rownames( result ) <- eqNames
  #     colnames( result ) <- eqNames
  #  }

  #  if( useMatrix ){
  #     result <- as( result, "dspMatrix" )
  #  }
  #  return( result )
  # }


  # ---------------------------------------------------------------  

  # ensure that sample fit object has the correct number of dimensions
  assert("appropriate sample_systemfit object - length (systemfit_unrelated())", 
    length(formula)==length(sample_systemfit$eq))

  # ensure that sample fit object has the same Xs

  # estimate each model
  temp_model <- lapply(formula, function(x) {

    temp <- lm(x, data=df)

  })

  # replace terms in sample_systemfit

  ## generate 'blank' var_cov matrix
  sample_systemfit$coefCov <- matrix(rep(0, 
      (length(temp_model)*length(coef(temp_model[[1]])))^2),
      ncol= (length(temp_model)*length(coef(temp_model[[1]]))))

  ## modify terms
  for (i in 0:(length(temp_model)-1)) {

    # key - coefficients (aggregate)
    sample_systemfit$coefficients[seq(from=((i)* 
      length(coef(temp_model[[i+1]])))+1,
      to=((i+1)*
      length(coef(temp_model[[i+1]]))))]           <- coef(temp_model[[i+1]])
  
    # key var cov (as not externally supplied)
    sample_systemfit$coefCov[seq(from=length(coef(temp_model[[i+1]]))*i+1, 
              to=length(coef(temp_model[[i+1]]))*i+(length(coef(temp_model[[i+1]])))), 
              seq(from=length(coef(temp_model[[i+1]]))*i+1, 
              to=length(coef(temp_model[[i+1]]))*i+
              (length(coef(temp_model[[i+1]]))))]  <- vcov(temp_model[[i+1]])
    
   }

  # key - degree of freedoms
  sample_systemfit$df.residual <- nrow(df)*length(temp_model)-
              length(temp_model)*length(coef(temp_model[[1]]))

  # # re-estimate the variance cov matrix (residuals)
  # system_res     <- residuals(sample_systemfit)
  # system_var_cov <-.calcResidCov( system_res, methodResidCov = "geomean",
  #        validObsEq = rep(nrow(df), length(formula)), 
  #        nCoefEq = rep(length(coef(temp_model[[1]])), length(formula)),
  #        xEq = NULL, 
  #        centered = FALSE, 
  #        solvetol = 0.000000000000000222 )
  # sample_systemfit$residCov <- system_var_cov

  return(sample_systemfit)

}

#----------------------------------------------------------------------------#
