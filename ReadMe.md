# ehR

**Install** 

```
library("devtools")  
install_github("claramarquardt/ehR",dependencies = TRUE)    
library(ehR)
```  
**Documentation**
- See function_overview.csv for an overview of all the functions (and data sets) included in the package 

**Datasets**  
The package contains the following data sets:
- 'dia': Simulated EHR diagnosis data for 500 patients
- 'ed': Simulated EHR ED visit data for 500 patients
- 'dem': Simulated EHR demographics data for 500 patients
- 'test_dt': Simulated cohort & feature set
- 'gagne_code': ICD9 code - gagne co-morbidity category crosswalk (http://scholar.harvard.edu/gagne/software/combined-comorbidity-score) [* note the crosswalk included in the package includes gagne categories that are assigned a 0 weight (these are not included in the here referenced, publicly available version of the crosswalk)]
- 'zocat': ICD9 code - zocat crosswalk

**Notes**
- [1] Requires the newest version of devtools (https://github.com/hadley/devtools) to be installed
```
library(devtools)  
install_github("hadley/devtools")
```
- [2] Depends on rJava (http://www.rforge.net/rJava/)

- [3] ggplot themes included in the package (theme_[name of theme].R) *may* require a number of additional fonts to be installed and registered with ggplot/ggsave

````
# install the necessary packages
install.packages("extrafontdb",repos="http://cran.cnr.berkeley.edu/", 
   dependencies=TRUE)
install.packages("extrafont",repos="http://cran.cnr.berkeley.edu/", 
   dependencies=TRUE)

# load the necessary packages
library(extrafont)
library(extrafontdb)

# register the fonts (this step only needs to be execute once)
font_import()
loadfonts()

# * note that there may still be problems when using the themes on a plot saved in a non-PDF format

````

**Development**

- To contribute:
````
# 1. Clone the repo
git clone https://github.com/ClaraMarquardt/ehR.git

# 2. Make/Save any changes 
- Ensure that all dependencies are included in @import statements at the function top and are 
listed in the Imports (CRAN Dependency)/ Remotes (Github Dependency) fields of the DESCRIPTION file
- Recompile the package by executing the 'package_management/package_update.sh' script (Note: Prior to 
compilation all dependencies need to be (manually) installed)
- Confirm that the compilation was successful by checking the 'package_management/package_update.Rout' 
file and the 'function_overview.csv'

# 2. Create a new branch
git checkout -b [branch name]

# 3. Push all changes to the branch (assuming all changes have been committed)
git push origin [branch name]

* Note: Please see the contribution guide prior to committing any changes: 'package_management/contribution_guide.txt'

# 4. Test by installing from the branch
library(devtools)
install_git("git://github.com/ClaraMarquardt/ehR.git", branch = "[branch name])
````

- Package management tools
````
See 'package_management/' for 

- (a) a script to update the package ('package_update.sh')

- (b) a script to load the package data and functions locally (rather 
than as a package) ('load_package_locally.R') 

````