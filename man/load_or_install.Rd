% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_or_install.R
\name{load_or_install}
\alias{load_or_install}
\title{Load or install R packages (from CRAN or Github (Public or Private Repositories)).}
\usage{
load_or_install(package_list, custom_lib_path = "",
  custom_repo = "https://cran.rstudio.com", custom_package_version = TRUE,
  quiet = FALSE, github_auth_token = NA)
}
\arguments{
\item{package_list}{List of package names which are to be installed/loaded (list - character).}

\item{custom_lib_path}{Custom library path (character) [Default: Default library path].}

\item{custom_repo}{R repository from which to download packages [Default: https://cran.rstudio.com"]}

\item{custom_package_version}{Whether to take into account version specifications for key packages (data.table, ggplot2) (logical - TRUE/FALSE) [Default: TRUE].}

\item{github_auth_token}{Github API Authentication Token (only needed if installing Github repos from a private repository) (string) [Default: NA].}

\item{verbose}{Verbosity (logical - TRUE/FALSE) [Default: TRUE].}
}
\value{
List of packages which were successfully installed/loaded.
}
\description{
Given a list of packages (e.g. list("data.table", "claramarquardt/ehR")) install (where necessary)/load all packages from CRAN/Github as appropriate.
}
\details{
Maintained by: Clara Marquardt
}
\examples{
\dontrun{
package <- list("data.table", "trinker/plotflow")
load_or_install(package_list=package, custom_lib_path=paste0(getwd(), "/test/"), 
 quiet=FALSE)
}
}
