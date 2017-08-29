#----------------------------------------------------------------------------#

# Purpose:     Load scripts locally
# Author:      Clara Marquardt
# Date:        2017


#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
#                                    CONTROL                                 #
#----------------------------------------------------------------------------#

# NOTE: manually define "package_path" (path to directory containing 
# the ehR repository (e.g. '/Users/../Desktop/'))

print(sprintf("package_path: %s",  package_path))

# paths
setwd(package_path)

#----------------------------------------------------------------------------#
#                                    CODE                                    #
#----------------------------------------------------------------------------#

# load scripts locally
#--------------------------------------# 
for (x in list.files(paste0(package_path, "ehR", "/R"))) {

  print(sprintf("source: %s", x))

  source(paste0(package_path, "ehR", "/R/", x))

}


# load datasets locally
#--------------------------------------# 
for (x in list.files(paste0(package_path, "ehR", "/data"))) {

  print(sprintf("load: %s", x))

  load(paste0(package_path, "ehR", "/data/", x))

}

#----------------------------------------------------------------------------#
#                                    END                                     #
#----------------------------------------------------------------------------#


