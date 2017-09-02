#----------------------------------------------------------------------------#

#' Calculate Gagne scores for a given set of patients over a given time period prior to t0.
#' 
#' Note: This function is designed to work with a Partners EHR master dataset, i.e. a data set
#' containing a dia_date_1 and gagne column.
#' @export
#' @import data.table
#' @param  TBC
#' @return TBC
#' @examples
#' \dontrun{
#' gagne_score <- gagne_score_calc(
#'    dia$dia_id[1:1000],        
#'    dia$empi[1:1000],             
#'    dia$dia_date[1:1000],      
#'    dia,     
#'    timeframe_day=365, 
#'    buffer_day=1, 
#'    gagne_count_var=TRUE)
#' }
  
gagne_score_calc <- function(id_list, empi_list, date_list, dia_dt, timeframe_day=365, 
  buffer_day=0, gagne_count_var=FALSE) {
  
  #############################################################################
  ## gagne cat
  gagne_cat <- copy(gagne_code)

  ##############################################################################
  ## id_dt
  id_dt <- data.table(empi=empi_list, event_date=date_list, id_id=id_list)
  id_dt[, event_date_end:=event_date-buffer_day]
  id_dt[, event_date_pre:=event_date_end-timeframe_day]

  ##############################################################################
  ## subset columns
  dia_dt <- dia_dt[, .(empi, dia_date, dia_date_1, gagne)]

  ##############################################################################
  ## subset to cohort 
  dia_dt  <- dia_dt [empi %in% id_dt$empi]

  ##############################################################################
  ## foverlaps 
  setkey(id_dt, empi, event_date_pre, event_date_end)
  dia_olap <-foverlaps(dia_dt, id_dt, by.x=c("empi","dia_date", "dia_date_1"), nomatch=0)

  print(min(dia_dt$dia_date))
  print(max(dia_dt$dia_date))

  print(min(id_dt$event_date))
  print(max(id_dt$event_date))

  print(min(dia_olap$dia_date))
  print(max(dia_olap$dia_date))


  ##############################################################################
  ### generating gagne scores

  # gagne_formula_exp <- quote(
  #   5 * metastatic_romano +
  #   2 * chf_romano +
  #   2 * dementia_romano +
  #   2 * renal_elixhauser +
  #   2 * wtloss_elixhauser +
  #   1 * hemiplegia_romano +
  #   1 * alcohol_elixhauser +
  #   1 * tumor_romano +
  #   1 * arrhythmia_elixhauser +
  #   1 * pulmonarydz_romano +
  #   1 * coagulopathy_elixhauser +
  #   1 * compdiabetes_elixhauser +
  #   1 * anemia_elixhauser +
  #   1 * electrolytes_elixhauser +
  #   1 * liver_elixhauser +
  #   1 * pvd_elixhauser +
  #   1 * psychosis_elixhauser +
  #   1 * pulmcirc_elixhauser +
  #  -1 * hivaids_romano +
  #  -1 * hypertension_elixhauser)

  # XXX NOTE: gagne_cat + gagne_weights + gagne_formula -> recreate gagne_formula_exp dynamically 
  gagne_name <- gagne_cat$gagne
  gagne_weight <- as.numeric(gsub("_", "-", gagne_cat$weight))
  gagne_formula_exp <- ""

  gagne_formula  <- function(cat, weight, ext) {
    for (i in 1:length(cat)) {
      gagne_formula_exp <- paste(gagne_formula_exp, weight[i], "*", paste0(ext, 
        cat[i]), "+", sep=" ")
    }
    gagne_formula_exp <- gsub("\\+$", "",gagne_formula_exp)
    return(gagne_formula_exp)
  }

  # (b) reshaping - create diagnosis code count vars (gagne)
  gagne_timeframe_comb <- dcast.data.table(dia_olap, id_id + empi + event_date ~  
    paste0("gagne_count_", gagne), length, value.var = "gagne", subset=.(!is.na(gagne)  & gagne!="" ))

  # expand to ensure that all the ids are represented 
  gagne_timeframe_comb <- gagne_timeframe_comb[id_dt[,.(id_id)], on=c("id_id"), nomatch=NA]
  gagne_timeframe_comb[,  grep("gagne_count", names(gagne_timeframe_comb), value=T) :=lapply(.SD, 
    function(x) ifelse(is.na(x), 0,x)), .SDcols=grep("gagne_count", names(gagne_timeframe_comb))]

  # (c) generate complete set of gagne category dummies (i.e. 0/1 if present 
  # or not at least once during time period) [impute 0]

  gagne_timeframe_comb[,  grep("gagne_count", names(gagne_timeframe_comb), value=T) :=lapply(.SD, 
    function(x) ifelse(x>=1, 1,0)), .SDcols=grep("gagne_count", names(gagne_timeframe_comb))]
  gagne_timeframe_comb[, setdiff(paste0("gagne_count_", gagne_name), names(gagne_timeframe_comb)):=0]

  # (d) determine the gagne score 
  gagne_timeframe_comb[,gagne_score:=eval(parse(text=gagne_formula(gagne_name[as.numeric(gsub("_", 
    "-", gagne_cat$weight))!=0], gagne_weight[as.numeric(gsub("_", "-", gagne_cat$weight))!=0], "gagne_count_")))]

  ##############################################################################
  ### returning the score

  setnames(gagne_timeframe_comb, "id_id", "id")
  
  if (gagne_count_var==FALSE) {
    return(gagne_timeframe_comb[, .(id, empi, event_date, gagne_score)])
  } else {
    return(gagne_timeframe_comb[, mget(c("id", "empi", "event_date", 
      "gagne_score", grep("gagne_count_*",  names(gagne_timeframe_comb), value=T)))])
  }

}

#----------------------------------------------------------------------------#
