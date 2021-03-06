% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LNTagFunctions.R
\name{LNintellectualRights}
\alias{LNintellectualRights}
\title{Add a Living Norway metadata tag corresponding to the intellectualRights EML element}
\usage{
LNintellectualRights(tagText, tagID = NA, parentID = NA, isHidden = FALSE)
}
\arguments{
\item{tagText}{A \code{character} scalar containing the text to encolse in the tag}

\item{tagID}{A \code{character} scalar containing a unique identifier for the tag element. If
the parameter is \code{NA} then a UUID will be generated}

\item{parentID}{A \code{character} scalar containing the ID for the parent tag (if there is one).
\code{NA} denotes that there is no parent for the tag}

\item{isHidden}{A \code{logical} scalar that, if \code{TRUE}, denotes that the text should be
hidden when rendered}
}
\value{
A \code{character} scalar containing the rendered output including any HTML tags if a
HTML document is being knitted
}
\description{
Add a Living Norway metadata tag corresponding to the intellectualRights EML element
}
\author{
Joseph D. Chipperfield, \email{joechip90@googlemail.com}; Matthew Grainger, \email{matthew.grainger@nina.no}
}
