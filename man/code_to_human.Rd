% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code_to_human.R
\name{code_to_human}
\alias{code_to_human}
\title{: Format code variable names to proper names.}
\usage{
code_to_human(string, capitalization_length = 1)
}
\arguments{
\item{string}{The string variable you would like to transform. (character)}

\item{capitalization_length}{The length at or below which, substrings should be turned into all CAPS (integer)}
}
\description{
: A function that turns code based variable names to human friendly names, useful in particular for converting variable names with underscores to more easily interpretable words as axis labels in plots and in spreadsheets.
}
\examples{
code_to_human("age_years") # "Age Years"
code_to_human("icu_patient_count", 3) # "ICU Patient Count"
}
