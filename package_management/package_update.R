#----------------------------------------------------------------------------#

# Purpose:     Batch mode script to update package 
# Author:      Clara Marquardt
# Date:        2017


#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
#                                    CONTROL                                 #
#----------------------------------------------------------------------------#

# parameters
wd_path         <- commandArgs(trailingOnly = TRUE)[1]
package_name    <- commandArgs(trailingOnly = TRUE)[2]

print(sprintf("wd_path: %s",  wd_path))
print(sprintf("package_name: %s",  package_name))

# dependencies
library("devtools")
library(roxygen2)
library(data.table)
library(zoo)

# paths
setwd(wd_path)

#----------------------------------------------------------------------------#
#                                    CODE                                    #
#----------------------------------------------------------------------------#

# update the documentation
#-------------------------------------------
setwd(paste0("./", package_name))
document()

# uninstall existing versions
#-------------------------------------------
package_spec <- paste0("package:", package_name)
detach(package_spec, unload=TRUE,character.only = TRUE)

try(remove.packages(package_name))

# fresh install
#-------------------------------------------
setwd("..")
install(package_name, dependencies = FALSE)
library(package_name,character.only = TRUE)

# generate overview of package
#-------------------------------------------
overview <- as.data.table(library(help=package_name, 
				character.only = TRUE)$info[[2]])

## format
overview[V1 %like% "\\.$", id:=1:nrow(overview[V1 %like% "\\.$"])]
overview[, id:=na.locf(id, fromLast=TRUE)]
overview[, V1:=gsub("^[ ]*", "", V1)]
overview[, V1:=paste0(V1, collapse=" "), by=c("id")]
overview <- unique(overview, by=c("id"))

overview[, c("function_name"):=strsplit(V1, "   ")[[1]][1], by=c("id")]
overview[, c("function_desc"):=paste0(unlist(strsplit(V1, "   ")[[1]][-1]),collapse=""), 
		by=c("id")]
overview[,function_desc:=gsub("^[ ]*", "", function_desc)]
overview[, c("id", "V1"):=NULL]

## save
write.csv(overview, paste0(wd_path, package_name, "/function_overview.csv"), row.names=F)

#----------------------------------------------------------------------------#
#                                    END                                     #
#----------------------------------------------------------------------------#
