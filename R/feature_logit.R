#----------------------------------------------------------------------------#

#' @title Generate, format and save a tabulation of the most important predictors
#' in a logit model (glm/glmnet/speedglm).
#'
#' @description 
#'
#' @export
#' @import data.table
#' @import xlsx
#' @param model 
#' @param cluster_var_vector 
#' @param output_path 
#' @param add_dt
#' @param feat_lim
#' @return
#' @examples


feature_logit <- function(model, cluster_var_vector=NA, feat_lim=200, output_path, add_dt=NULL) {



    # stats
    norm_factor_0.9 <- 1.96
    norm_factor_0.95 <- 1.645 

    if (is.na(cluster_var_vector[1])) {

      if (class(model) !="cv.glmnet") {

        logit_model_output     <- summary(model)
        coeff_raw              <- logit_model_output$coefficients

        coeff <- as.data.table(coeff_raw)
        coeff[, var_name:=rownames(coeff_raw)]
        coeff <- coeff[!var_name %like% "Intercept"]

        setnames(coeff, c("estimate", "std", "z", "p", "var_name"))

        current_date_time_id <- paste0(gsub("_", "", as.character(format(Sys.time(), "%H_%M_%S__%d_%m_%y"))),
          "_", sample(1:40, 1, replace=F))
      
        write.csv(coeff, paste0("feature_update", current_date_time_id, ".csv"), row.names=F)
        coeff <- fread(paste0("feature_update", current_date_time_id ,".csv"))
    
     } else {

      coeff <- data.table(data.frame(coef.name = dimnames(coef(model))[[1]], 
        coef.value = matrix(coef(model))))
      setnames(coeff, c("var_name", "estimate"))
      coeff <- coeff[!var_name %like% "Intercept"]
      coeff <- coeff[!estimate==0]

     }

  } else {

    if (class(model) !="cv.glmnet") {
      # automatic (cluster.vcov ---- multiwayvcov package)
      #----------------------------------------------------------------------------#
      var_cov_adj_f <- cluster.vcov(model, cluster_var_vector)

      # none automatic (sandwich)
      #----------------------------------------------------------------------------#
      # get_CL_vcov <- function(model, cluster) {
        
      #   #calculate degree of freedom adjustment
      #   M <- length(unique(cluster))
      #   N <- length(cluster)
      #   K <- model$rank
      #   dfc <- (M/(M-1))*((N-1)/(N-K))

      #   # pearson residuals
      #   predict   <- model$fitted.values
      #   residuals <- model$y - predict

      #   x_matrix <- copy(model$data[complete.cases(model$data)])
      #   x_matrix[, c(outcome_var):=NULL]
      #   x_matrix <- as.matrix(x_matrix)

      #   est_function_model <- residuals * cbind(1,x_matrix)

      #   #calculate the uj's
      #   # uj  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum))
      #   uj  <- apply(est_function_model,2, function(x) 
      #     tapply(x, cluster, sum))
        
      #   #use sandwich to get the var-covar matrix
      #   print(dfc)
      #   vcovCL <- dfc * sandwich_mod(model, meat=crossprod(uj)/N)
        
      #   return(vcovCL)

      # }

      # sandwich_mod <- function (x, bread. = bread, meat. = meat, ...) {
      #     if (is.list(x) && !is.null(x$na.action))
      #         class(x$na.action) <- "omit"
      #     if (is.function(bread.))
      #         bread. <- bread.(x)
      #     if (is.function(meat.))
      #         meat. <- meat.(x, ...)
      #     n <- NROW(estfun(x))
      #     # print(str(bread.))
      #     # print(str(meat.))
      #     return(1/n * (bread. %*% meat. %*% bread.))
      # }

      # var_cov_adj_man <- get_CL_vcov(model, data[,get("empi")][complete.cases(data)])

      ## obtain coefficients,p-value, SE
      coeff_std_standard        <- coeftest(model) 
      coeff_std_adj_f           <- coeftest(model, var_cov_adj_f)
      # coeff_std_adj_man         <- coeftest(model, var_cov_adj_man)

      # print(coeff_std_standard[c(1:10),])
      # print(coeff_std_adj_f[c(1:10),])
      # print(coeff_std_adj_man[c(1:10),])

      ## format
      coeff <- data.table(var_name=rownames(coeff_std_adj_f), 
      estimate=coeff_std_adj_f[, 1], 
      std=coeff_std_adj_f[, 2], p=coeff_std_adj_f[, 4])
      coeff <- coeff[!var_name %like% "Intercept"]

    } else {

      coeff <- data.table(data.frame(coef.name = dimnames(coef(model))[[1]], 
        coef.value = matrix(coef(model))))
      setnames(coeff, c("var_name", "estimate"))
      coeff <- coeff[!var_name %like% "Intercept"]
      coeff <- coeff[!estimate==0]

    }
  }

  coeff[, estimate:=as.numeric(estimate)]
  coeff[, odds:=exp(estimate)]
  coeff[, odds:=round(odds, digits=5)]

  coeff[, abs_est:=abs(estimate)]
  setorder(coeff, -abs_est)
  coeff[, abs_est:=NULL]

  # ensure that feat_lim appropriate
  feat_lim <- min(feat_lim, nrow(coeff))

  coeff <- coeff[1:feat_lim]

  if (class(model) !="cv.glmnet") {
  
    coeff[, p:=as.numeric(p)]
  
    coeff[, sign:=ifelse(as.numeric(get("p")) < .001, "**** ", 
           ifelse(as.numeric(get("p"))< .01, "***  ", 
           ifelse(as.numeric(get("p")) < .05, "**   ",
           ifelse(as.numeric(get("p")) < .1, "*   ",
          "    ")))), by=1:nrow(coeff)]
      
    coeff[, CI_0.9_min:=estimate -(norm_factor_0.9 *  std)]
    coeff[, CI_0.9_max:=estimate + (norm_factor_0.9 *  std)]
    coeff[, CI_0.95_min:=estimate -(norm_factor_0.95 *  std)]
    coeff[, CI_0.95_max:=estimate + (norm_factor_0.95 *  std)]

    coeff[, odds_CI_0.9_min:=exp(CI_0.9_min)]
    coeff[, odds_CI_0.9_max:=exp(CI_0.9_max)]
    coeff[, odds_CI_0.95_min:=exp(CI_0.95_min)]
    coeff[, odds_CI_0.95_max:=exp(CI_0.95_max)]

    coeff[, p:=round(p, digits=5)]
    coeff[, std:=round(std, digits=5)]
    coeff[, odds_CI_0.9_min:=round(odds_CI_0.9_min, digits=5)]
    coeff[, odds_CI_0.9_max:=round(odds_CI_0.9_max, digits=5)]
    coeff[, odds_CI_0.95_min:=round(odds_CI_0.95_min, digits=5)]
    coeff[, odds_CI_0.95_max:=round(odds_CI_0.95_max, digits=5)]

  }

  coeff[, estimate:=round(estimate, digits=5)]

  var_coeff <- c("var_name", 'estimate', "std", "odds", "odds_CI_0.9_min", "odds_CI_0.9_max", 
    "odds_CI_0.95_min", "odds_CI_0.95_max", "p", "sign")
  var_coeff_name <- c("var_name", "estimate", "std_clust", "odds", "odds_CI_0.9_min_clust", "odds_CI_0.9_max_clust", 
    "odds_CI_0.95_min", "odds_CI_0.95_max", "p_clust", "sign_clust")
  var_coeff_name <- var_coeff_name[var_coeff %in% names(coeff)]
  var_coeff <- var_coeff[var_coeff %in% names(coeff)]

  coeff <- coeff[, mget(var_coeff)]
  setnames(coeff, var_coeff_name)

  # return(coeff)   
  # write.csv(coeff, paste0(output_folder, output_path),row.names=F)
  
  # prepare wb
  wb    <- createWorkbook(type="xlsx")
  sheet = createSheet(wb, "feat_importance")

  # generate feat imp dt
  feat_dt <- coeff
  
  num_col  <- which(sapply(feat_dt, function(x) class(x)[1]) %in% c("numeric"))
  char_col <- which(sapply(feat_dt, function(x) class(x)[1]) %in% c("character", "factor"))
  
  feat_dt[, c(num_col):=lapply(.SD, function(x) round(x, digits=4)), .SDcols=num_col]

  # setnames(feat_dt, names(feat_dt)[char_col], "var_name")

  feat_dt[, var_cat:=gsub("^([^\\.]*)(\\.).*","\\1",var_name)]
  feat_dt[, var_timeframe:=gsub(".*_timeframe_(.*)$","\\1",var_name)]
  feat_dt[var_timeframe %like% "days_to_last", var_timeframe:="days_to_last"]
  feat_dt[var_timeframe==var_name, var_timeframe:="-"]

  # add stats
  if(!is.null(add_dt[[1]])) {

      add_dt <- add_dt[var_name %in% feat_dt$var_name]
      feat_dt <- Reduce(function(x,y) mymerge(x, y, var_list="var_name"), 
          list(feat_dt, add_dt))
  }

  # order
  setorder(feat_dt, -odds)

  # fomat and add to xlsx
  addDataFrame(feat_dt,sheet, row.names=FALSE, col.names=TRUE)

  # cell_style
  var_cat     <- c("dia", "enc", "prc", "dem", "med", "mic", "lab", "lvs", "ed")
  var_cat_col <- brewer.pal(length(var_cat),"Spectral")

  inv_mapply(function(cat, col) {

    cs = CellStyle(wb) + Border(color=col) + Font(wb, color=col, isBold=TRUE)

    rows_raw <- (which(feat_dt$var_cat %like% cat))+1
    rows     <- getRows(sheet, rows_raw)
    cells    <- getCells(rows, colIndex = c(char_col)) 
  
    lapply(names(cells), function(ii)setCellStyle(cells[[ii]],cs))
  }, cat=var_cat, col=var_cat_col)


  saveWorkbook(wb,output_path)


}

#----------------------------------------------------------------------------#
