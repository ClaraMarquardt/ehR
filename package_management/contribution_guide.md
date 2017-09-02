#----------------------------------------------------------------------------#

## Basic style guidelines

- Naming: Use '_' rather than '.' or CamelCase to separate words / Avoid plurals 
  where possible

- Parameters: Provide a definition of each parameters incl. its type / Provide a 
  default parameter value where possible

- Presentation: Use an existing function as a template

- Examples: Provide a working example making use of the sample data as useful/necessary. As necessary examples should be prevented from automatically running during compilation usign the 'dontrun' syntax
````
@examples \dontrun{
print("Test Examples - NOT automatically executed")
}
````

- Dependencies/Import & Export: Export each function / Import all dependencies 
  (then use the functions as usual, i.e. without the need to explicitly specify the package) (*) [This is a temporary set-up. In future versions dependency imports will be reduced (using importFrom and/or :: syntax)]

- Documentation (Function): 

* Function titles/description fields need to end with a '.' to ensure that the function_overview.csv file is correctly generated. Titles should be short (1 line at most). Where no description is given the field should be presented as a place holder
````
#' \
````

* The overall structure of the header file should follow the following example:

````
#' Expand a list of icd9 code ranges. 
#' 
#' \
#' 
#' @details Maintainer: Clara Marquardt
#' 
#' @export
#' @import icd
#' @import data.table
#' @import magrittr
#' 
#' @param range_table data.table containing a column with icd9 code ranges and code range names (data.table).
#' @param code_col name of the column containing the icd9 code ranges (see the below example) - commented out rows (start with '#') are ignored (character). 
#' @param name_col name of the column containing the code range names (character).  
#' @param decimal whether or not the icd9 codes are in decimal format (logical - TRUE/FALSE) [default: TRUE]. 
#' @param validate whether or not to subset to 'defined' icd9 codes (see \source{www.rdocumentation.org/packages/icd/versions/2.2/topics/icd_expand_range} for more detail) (logical - TRUE/FALSE) [default: FALSE]. 
#' 
#' @return data.table with the expanded icd9 code list and the associated code range names. 
#' 
#' @examples 
#' orig_table <- copy(gagne_code) 
#' print(orig_table$code)        
#' code_table  <- clean_icd_list(range_table=orig_table, code_col="code", name_col="condition", decimal=FALSE, validate=FALSE) 
#' print(code_table)
````


#----------------------------------------------------------------------------#
