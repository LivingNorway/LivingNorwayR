% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GBIFGelImage.R
\name{GBIFGelImage}
\alias{GBIFGelImage}
\title{R6 class representing a data structure for a GBIF GelImage augmented data table (based on the Dawrin core)}
\description{
R6 class representing a data structure for a GBIF GelImage augmented data table (based on the Dawrin core)

R6 class representing a data structure for a GBIF GelImage augmented data table (based on the Dawrin core)
}
\details{
The \code{GBIFGelImage} class allows for the specification of data tables that comply with the GelImage
\url{http://data.ggbn.org/schemas/ggbn/terms/GelImage}{class specification} of GBIF.
}
\seealso{
\code{\link[DwCTerm]{DwCTerm}} \code{\link[getGBIFGelImageMembers]{getGBIFGelImageMembers}}
}
\section{Super class}{
\code{\link[LivingNorwayR:DwCGeneric]{LivingNorwayR::DwCGeneric}} -> \code{GBIFGelImage}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GBIFGelImage-new}{\code{GBIFGelImage$new()}}
\item \href{#method-GBIFGelImage-clone}{\code{GBIFGelImage$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="exportAsDataFrame"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-exportAsDataFrame'><code>LivingNorwayR::DwCGeneric$exportAsDataFrame()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="exportTable"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-exportTable'><code>LivingNorwayR::DwCGeneric$exportTable()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="getAssociatedTerms"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-getAssociatedTerms'><code>LivingNorwayR::DwCGeneric$getAssociatedTerms()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="getDefaultYearFormat"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-getDefaultYearFormat'><code>LivingNorwayR::DwCGeneric$getDefaultYearFormat()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="getDwCClassName"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-getDwCClassName'><code>LivingNorwayR::DwCGeneric$getDwCClassName()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="getDwCTermInfo"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-getDwCTermInfo'><code>LivingNorwayR::DwCGeneric$getDwCTermInfo()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="getIDIndex"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-getIDIndex'><code>LivingNorwayR::DwCGeneric$getIDIndex()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="getIDName"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-getIDName'><code>LivingNorwayR::DwCGeneric$getIDName()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="getTableName"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-getTableName'><code>LivingNorwayR::DwCGeneric$getTableName()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="getTableTermName"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-getTableTermName'><code>LivingNorwayR::DwCGeneric$getTableTermName()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="getTermMapping"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-getTermMapping'><code>LivingNorwayR::DwCGeneric$getTermMapping()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="importDataTable"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-importDataTable'><code>LivingNorwayR::DwCGeneric$importDataTable()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="print"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-print'><code>LivingNorwayR::DwCGeneric$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="setDefaultYearFormat"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-setDefaultYearFormat'><code>LivingNorwayR::DwCGeneric$setDefaultYearFormat()</code></a></span></li>
<li><span class="pkg-link" data-pkg="LivingNorwayR" data-topic="DwCGeneric" data-id="setTableName"><a href='../../LivingNorwayR/html/DwCGeneric.html#method-DwCGeneric-setTableName'><code>LivingNorwayR::DwCGeneric$setTableName()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GBIFGelImage-new"></a>}}
\if{latex}{\out{\hypertarget{method-GBIFGelImage-new}{}}}
\subsection{Method \code{new()}}{
Create a new GBIFGelImage object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GBIFGelImage$new(
  objectData,
  idColumnInfo,
  nameAutoMap = FALSE,
  defDateFormat = "YYYY-MM-DD",
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{objectData}}{A \code{data.frame} containing the data to import into the object}

\item{\code{idColumnInfo}}{Either a \code{character} scalar containing the column name of
\code{objectData} or an \code{integer} scalar giving the index of the column of
\code{objectData} that corresponds to the ID variable.  Alternatively, this parameter
may be the qualified name of the Darwin core term for which the appropriately mapped column
will be used as the ID variable (the possible Darwin core term names can be found by running
\code{names(getGBIFGelImageMembers())})}

\item{\code{nameAutoMap}}{A \code{logical} scalar that if \code{TRUE} maps the columns of \code{objectData}
to their respective Darwin core terms based on the column names}

\item{\code{defDateFormat}}{A \code{character} scalar providing the default format for strings denoting dates in the
data table.  See the \url{https://dwc.tdwg.org/text/#1-introduction}{Darwin Core text guide} for expected values
for this string.}

\item{\code{...}}{A named set of paramaeters corresponding to Darwin core terms associated with the GBIFGelImage
class type.  Each is either a \code{character} scalar containing the column name of \code{objectData}
or an \code{integer} scalar giving the index of the column of \code{objectData} that corresponds to the
term. Mappable terms can be found using: \code{sapply(X = getGBIFGelImageMembers(), FUN = function(curTerm) { curTerm$getTermName() })}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{GBIFGelImage} object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GBIFGelImage-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GBIFGelImage-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GBIFGelImage$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
