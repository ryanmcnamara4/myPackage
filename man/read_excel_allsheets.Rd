% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_excel_allsheets.R
\name{read_excel_allsheets}
\alias{read_excel_allsheets}
\title{Load all excel sheets}
\usage{
read_excel_allsheets(filepath, cnames = TRUE, tibble = FALSE)
}
\arguments{
\item{filepath}{Path to an excel file.}

\item{cnames}{Logical value determining if column names should be included.
Defaults to TRUE.}

\item{tibble}{Logical value determining if the data should be loaded as tibbles
or data.frames. Defaults to FALSE.}
}
\value{
List of tibbles or data.frames, each corresponding to one sheet.
}
\description{
Loads all of the sheets of the excel file \code{filepath}, and returns them as
a list of tibbles or data.frames.
}
\examples{
file = system.file('extdata', 'human_viruses.xls', package = 'myPackage')
read_excel_allsheets(file, cnames = TRUE, tibble = TRUE)
read_excel_allsheets(file)
}
