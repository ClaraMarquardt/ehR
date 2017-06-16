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

 one_hot_encoding <- function (dt, var_list, drop = FALSE) {

    contr.onehot = function(n, contrasts, sparse = FALSE) {
        contr.sum(n = n, contrasts = FALSE, sparse = sparse)
    }
    current.na.action <- options("na.action")
    options(contrasts = c("contr.onehot", "contr.onehot"))
    options(na.action = "na.pass")

    dt_factor <- dt[, c(var_list), with=F]
    dt_factor <- lapply(var_list, function(x) {
        
        # rename 
        dt_factor[, c(x):=lapply(.SD, function(y) paste0("_", gsub("(_*)$", "", y))), 
            .SDcols=c(x), by=1:nrow(dt_factor)]

        # cast
        dt_factor_temp <- data.frame(model.matrix(~. - 1, data = dt_factor[,
            mget(x)]))

        # drop one if only two columns
        if (ncol(dt_factor_temp)==2 & names(dt_factor_temp)[1] %like% "(0|1)$") {
            name_orig <- gsub("(0|1)$", "", names(dt_factor_temp)[1])
            dt_factor_temp <- dt_factor_temp[, 1]
            dt_factor_temp <- data.frame(dt_factor_temp)
            names(dt_factor_temp) <- name_orig
        }

        # drop one column - avoid colinearity
        if (drop == TRUE & ncol(dt_factor_temp)>1) {
            dt_factor_temp[[names(dt_factor_temp)[1]]] <- NULL
        }
        return(dt_factor_temp)
    })

    dt_factor <- data.frame(dt_factor)
    dt_factor <- as.data.table(dt_factor)

    # drop factor columns which end in NA
    na_col   <- grep("_NA$", names(dt_factor), value=T)
    dt_factor[, c(na_col):=NULL]

    dt_factor[,]
    # format
    dt_factor[, `:=`(names(dt_factor), lapply(.SD, function(x) as.integer(x)))]
    dt_non_factor <- dt[, c(setdiff(names(dt), var_list)), with=F]
    
    # check that row count aligns
    if (nrow(dt_factor) != nrow(dt_non_factor)) 
      print("warning - one hot encoding () - it appears that rows are dropped during the conversion")

    dt_temp <- data.table(data.frame(dt_non_factor, dt_factor))

    options(na.action = current.na.action)

    # return
    return(dt_temp)
}

#----------------------------------------------------------------------------#
