#----------------------------------------------------------------------------#

#' Capture run-time stats for scripts executed in batch-mode. 
#' 
#' @export
#' @import data.table
#' @param TBC 
#' @return TBC
#' @examples
#' \dontrun{
#'  current_date_time <- as.character(format(Sys.time(), "%d/%m/%Y_%H:%M"))
#'  data_sample       <- "test_cohort"
#'  doc_path          <- "test.csv"
#'  ingest_meta("start", file_name_var="test_script", meta_batch_doc_arg=doc_path, data_sample_var=data_sample,
#'    source_date_time_var=current_date_time) ## start of script
#'  ingest_meta("end", file_name_var="test_script", , meta_batch_doc_arg=doc_path, data_sample_var=data_sample,
#'    source_date_time_var=current_date_time) ## end of script
#' }

ingest_meta <- function(start_var_end, file_name_var, meta_batch_doc_arg=meta_batch_doc, data_sample_var=data_sample, 
  date=as.character(format(Sys.time(), "%d/%m/%Y_%H:%M")), source_date_time_var=source_date_time) {

  jobid_var <- paste0(file_name_var, "_", source_date_time)
  date_date <- strptime(date, "%d/%m/%Y_%H:%M")
  source_date_time_var_date <- strptime(source_date_time, "%d/%m/%Y_%H:%M:%S")

  if (start_var_end=="start") {

    meta <- data.table(file_name=file_name_var, data_sample=data_sample_var, start_date=date, 
      var_list="/", end_date="/", run_time_min="/", job_id=jobid_var)

    if(file.exists(meta_batch_doc_arg)) {
      meta_master <- fread(meta_batch_doc_arg)
      meta <- rbindlist(list(meta_master, meta))
    }
    
    write.csv(meta, meta_batch_doc_arg, row.names=F)

  }

  if (start_var_end=="end") {

     meta <- fread(meta_batch_doc_arg)
     
     meta[job_id==jobid_var, end_date:=date]
     meta[job_id==jobid_var, run_time_min:=round(as.numeric(difftime(date_date, 
      source_date_time_var_date, units="mins")), digit=1)]

    write.csv(meta, meta_batch_doc_arg, row.names=F)

  }

}

#----------------------------------------------------------------------------#
