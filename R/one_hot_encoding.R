#----------------------------------------------------------------------------#

#' @title Encode categorical values in data.table. 
#'
#' @description Expand each categorical value into a full set of dummies. 
#'
#' @export
#' @import data.table
#' @param dt data.table containining the data [data.table].
#' @param var_list Names of variables which are to be encoded [character].
#' @param drop Whether to omit one category per variable (to avoid multicolinearity) [logical].
#' @return Modified data.table.
#' @examples

one_hot_encoding <- function(dt, var_list, drop=FALSE) {

  contr.onehot = function (n, contrasts, sparse = FALSE) {
    contr.sum(n = n, contrasts = FALSE, sparse = sparse)
  }

  current.na.action <- options('na.action')

  options(contrasts = c("contr.onehot", "contr.onehot"))
  options(na.action='na.pass')

  dt_factor <- lapply(var_list, function(x) {
  
    dt_factor_temp <- data.frame(model.matrix(~ . -1, data = dt[, mget(x)]))

    if (drop==TRUE) {
      dt_factor_temp[[names(dt_factor_temp)[1]]] <- NULL
    }

    return(dt_factor_temp)

  })

  dt_factor <- data.frame(dt_factor)
  dt_factor <- as.data.table(dt_factor)
 
  # format
  dt_factor[, names(dt_factor):=lapply(.SD, function(x) as.integer(x))]

  # combine with other data
  dt_non_factor <- dt[, mget(setdiff(names(dt), var_list))]
  dt_temp <- data.table(data.frame(dt_non_factor,dt_factor ))
 
  options(na.action=current.na.action)

  # return
  return(dt_temp)

}

#----------------------------------------------------------------------------#
