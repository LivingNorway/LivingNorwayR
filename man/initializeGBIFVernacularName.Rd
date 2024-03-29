% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GBIFVernacularName.R
\name{initializeGBIFVernacularName}
\alias{initializeGBIFVernacularName}
\title{Initialize a new GBIF VernacularName object}
\usage{
initializeGBIFVernacularName(
  objectData,
  idColumnInfo,
  nameAutoMap = FALSE,
  defDateFormat = "YYYY-MM-DD",
  ...
)
}
\arguments{
\item{objectData}{A \code{data.frame} containing the data to import into the object}

\item{idColumnInfo}{Either a \code{character} scalar containing the column name of
\code{objectData} or an \code{integer} scalar giving the index of the column of
\code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
may be the qualified name of the Darwin core term for which the appropriately mapped column
will be used as the ID variable (the possible Darwin core term names can be found by running
\code{names(getGBIFVernacularNameMembers())})}

\item{nameAutoMap}{A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
to their respective Darwin core terms based on the column names}

\item{defDateFormat}{A \code{character} scalar providing the default format for strings denoting dates in the
data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
for this string.}

\item{...}{A named set of paramaeters corresponding to Darwin core terms associated with the GBIFVernacularName
class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
term. Mappable terms can be found using: \code{sapply(X = getGBIFVernacularNameMembers(), FUN = function(curTerm) { curTerm$getTermName() })}}
}
\value{
A new \code{GBIFVernacularName} object
}
\description{
Initialize a new GBIF VernacularName object
}
\seealso{
\code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFVernacularNameMembers]{getGBIFVernacularNameMembers}}
}
