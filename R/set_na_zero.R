# #----------------------------------------------------------------------------#

# #' Replace (in place) NAs/+inf/-inf in data.table with another value.
# #' @export
# #' @param name Name of data.table [character]
# #' @param replace Value with which to replace Nas/+inf/-inf  [any]
# #' @return Data.table modified in place
# #' @examples
# #' TBC

# set_na_zero <- function(dt, replace=0, subset_col=names(dt)) {

#   for (j in which(names(dt) %in% subset_col))
#     set(dt, which(is.na(dt[[j]]) | dt[[j]] %in% c(-Inf, +Inf) ), j, replace)

# }


# #' Replace (in place) zeros in in data.table with another value.
# #' @export
# #' @param name Name of data.table [character]

# set_zero_na <- function(dt, replace=NA) {

#   for (j in seq_len(ncol(dt)))
#     set(dt, which(dt[[j]] %in% c(0)), j, replace)
# }


# #' Replace (in place) empty values ("[ ]*" or "") in data.table with another value.
# #' @export

# set_missing_na <- function(dt, replace=NA, subset_col=names(dt)) {

#   for (j in which(names(dt) %in% subset_col))
#     set(dt, which(gsub("[ ]*", "", dt[[j]])==""), j, replace)
# }

# #----------------------------------------------------------------------------#
