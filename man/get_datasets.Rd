% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vectraits_api.R
\name{get_datasets}
\alias{get_datasets}
\title{Retrieve multiple datasets from VecTraits}
\usage{
get_datasets(IDS, safety = TRUE)
}
\arguments{
\item{IDS}{integer, the datasets to retrieve.}

\item{safety}{logical, should an error be thrown if too many datasets are retreived?. Default is TRUE.}
}
\value{
A list of the datasets requested.
}
\description{
Retrieve multiple datasets from VecTraits
}
