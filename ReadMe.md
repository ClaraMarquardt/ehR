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
- 'dem': Simulatd EHR demographics data for 500 patients
- 'gagne_code': Icd9 code - gagne comorbidity category crosswalk (http://scholar.harvard.edu/gagne/software/combined-comorbidity-score) [* note the crosswalk included in the package includes gagne categories that are assigned a 0 weight (these are not included in the here referenced, publicly available version of the crosswalk)]
- 'zocat': Icd9 code - zocat crosswalk

**Note**
- Requires the newest version of devtools (https://github.com/hadley/devtools) to be installed
```
library(devtools)  
install_github("hadley/devtools")
```
- Dependes on rJava (http://www.rforge.net/rJava/)

**Development**
- Package is actively being developed and extended  - see the ehR project board for a list of open issues

- To contribute:
````
# 1. Clone the repo
git clone https://github.com/ClaraMarquardt/ehR.git

# 2. Create a new branch
git checkout -b [branch name]

# 3. Push all changes to the branch (assuming all changes have been committed)
git push origin [branch name]

# 4. Test by installing from the branch
library(devtools)
install_git("git://github.com/ClaraMarquardt/ehR.git", branch = "[branch name])
````
