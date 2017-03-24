#----------------------------------------------------------------------------#

#' List all the functions used in a R script.
#' @export
#' @import reader
#' @param  filename Path to R script
#' @return TBC
#' @examples
#' TBC

list_function <- function (filename, alphabetic = TRUE) {

    if (!file.exists(filename)) {
        stop("couldn't find file ", filename)
    }
    if (!get.ext(filename) == "R") {
        warning("expecting *.R file, will try to proceed")
    }
    tmp  <- getParseData(parse(filename, keep.source = TRUE))
    crit <- quote(token == "SYMBOL_FUNCTION_CALL")
    tmp  <- dplyr::filter(tmp, crit)
    tmp  <- unique(if (alphabetic) {
        sort(tmp$text)
    }
    else {
        tmp$text
    })
    src <- paste(as.vector(sapply(tmp, find)))
    outlist <- tapply(tmp, factor(src), c)
    outlist_custom <- unlist(outlist[grep("package", names(outlist), value=T,
     invert=T)])
    names(outlist_custom) <- NULL
    outlist <- outlist[grep("package", names(outlist), value=T)]
    outlist$custom <- outlist_custom
    outlist <- outlist[c("custom", setdiff(names(outlist), "custom"))]
    
    print(outlist)
}

#----------------------------------------------------------------------------#
