## Basic Style Guidelines

- **Names**: Use '_' rather than '.' or CamelCase to separate words / Avoid plurals 
  where possible

- **Parameter Definition**: Provide a definition of each parameters incl. its type / Provide a 
  default parameter value where possible

- **Presentation**: Use an existing function as a template

- **Examples**: Provide a working example making use of the sample data as useful/necessary. As necessary examples should be prevented from automatically running during compilation usign the 'dontrun' syntax
```
@examples \dontrun{
print("Test Examples - NOT automatically executed")
}
```

- **Dependencies and Import/Export Statements**: Export each function / Import all dependencies 
  (then use the functions as usual, i.e. without the need to explicitly specify the package) (*) [This is a temporary set-up. In future versions dependency imports will be reduced (using importFrom and/or :: syntax)]

- **Function-Level Documentation**:

	- Function titles/description fields need to end with a '.' to ensure that the function_overview.csv file is correctly generated. Titles should be short (1 line at most). Where no description is given the field should be presented as a place holder: `#' \`

	- The overall structure of the header (Roxygen) section should follow the following template:

```
#' Expand a list of icd9 code ranges. 
#' 
#' Expand a list of icd9 code ranges into a data.table with all (valid) codes falling into the specified ranges.
#' 
#' @details Maintained by: Clara Marquardt
#' 
#' @export
#' @import icd
#' @import data.table
#' 
#' @param range_table data.table containing a column with icd9 code ranges and code range names (data.table).
#' @param code_col name of the column containing the icd9 code ranges (see the below example) 
#' @param name_col name of the column containing the code range names (character).  
#' @param decimal whether or not the icd9 codes are in decimal format (logical - TRUE/FALSE) [default: TRUE]. 
#' @param validate see (\source{rdocumentation.org/packages/icd/......}) (logical - TRUE/FALSE).
#' 
#' @return data.table with the expanded icd9 code list and the associated code range names. 
#' 
#' @examples 
#' orig_table <- copy(gagne_code) 
#' print(orig_table$code)        
#' code_table <- clean_icd_list(range_table=orig_table, code_col="code", 
#'			name_col="condition", decimal=FALSE, validate=FALSE) 
#' print(code_table)
```

- **Parameter Names**: While each added function will require it's own set of parameters, the functional nature of many is common across various methods in the package, for example, a `data.table` containing the source information / data that needs to be transformed. For uniformity, simplicity and to avoid sheer forgetfulness, the authors suggest using the following conventional variable names for parameters to the extent possible:

	- `data`: For any `data.frame` or `data.table` that requires transformation / is the source of data in any manner
	
	- `start_date`: Any date that represents the start of the timeframe of interest. By default, readers 
	may assume that this date is **inclusive** when specified as part of a range.
	
	- `end_date`: Analogous to `start_date`, and used to indicate when the timeframe of interest will 
		terminate. Like `start_date`, this date is also understood to be **inclusive** when specified as part of a 
		range, unless otherwise mentioned explicitly.
		
	- `[...]_col`: Any column name (in a data.table or data.frame), e.g. `date_col`
	
	- `quiet`: Indicate whether to run a function with more or less extensive output (TRUE/FALSE).
	
	- `ndigit`: Number of digits used in any rounding operations. 
	
	- `inplace`: Whether to modify a data.table in place (TRUE/FALSE).
	
	- `ggplt`: Refer to any ggplot object.
