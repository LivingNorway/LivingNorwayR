% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensitive_data.R
\name{Full_decrypt}
\alias{Full_decrypt}
\title{Full decrypt
unencrypt encrpyted sensitive location data}
\usage{
Full_decrypt(data = data, target_cols = c(x, y))
}
\arguments{
\item{data}{A dataframe with encrpyted X and Y coordinates (only works for UTM at the moment)}

\item{target}{cols The X coordinate and the Y coordinate}
}
\value{
Data frame with unmasked coordinates - you need the password to decrypt (enter it twice when promted)
}
\description{
Full decrypt
unencrypt encrpyted sensitive location data
}
\keyword{internal}
