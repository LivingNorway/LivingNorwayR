% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensitive_data.R
\name{Sensitive_data}
\alias{Sensitive_data}
\title{Sensitive_data
mask or reduce the resolution of sensitive location data}
\usage{
Sensitive_data(data, XField, YField, Cat = "NS")
}
\arguments{
\item{data}{A dataframe with X and Y coordinates (only works for UTM at the moment)}

\item{XField}{The X coordinate}

\item{YField}{The Y coordinate}

\item{Cat}{The GBIF Category. Defaults to "Not sensitive"}
}
\value{
Data frame with masked or reduced resolution of the locations
}
\description{
Sensitive_data
mask or reduce the resolution of sensitive location data
}
\keyword{internal}
