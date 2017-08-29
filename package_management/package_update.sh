#----------------------------------------------------------------------------#

# Purpose:     Shell script to update the ehR package 
# Author:      Clara Marquardt
# Date:        2017


#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
#                                    CODE                                    #
#----------------------------------------------------------------------------#

## Notes:
# - Assume that in direotory containing the ehR repository/folder
# - Does not update data sets (use devtools::use_data([object],overwrite=TRUE))

R CMD BATCH --no-save "--args $pwd ehR" \
	package_management/package_update.R \
	package_management/package_update.Rout

#----------------------------------------------------------------------------#
#                                    END                                     #
#----------------------------------------------------------------------------#
