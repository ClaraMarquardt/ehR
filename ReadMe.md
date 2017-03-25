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
- 'gagne_code': Icd9 code - gagne comorbidity category crosswalk (http://scholar.harvard.edu/gagne/software/combined-comorbidity-score)
- 'zocat': Icd9 code - zocat crosswalk

**Note**
- Requires the newest version of devtools (https://github.com/hadley/devtools) to be installed
```
library(devtools)  
install_github("hadley/devtools")
```
- Dependes on rJava (http://www.rforge.net/rJava/)

**Development**
- Package is actively being developed and extended - see https://github.com/ClaraMarquardt/ehR_dev for a list of open issues
