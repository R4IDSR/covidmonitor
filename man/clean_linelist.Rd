% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linelist_clean.R
\name{clean_linelist}
\alias{clean_linelist}
\title{Clean merged COVID-19 linelists from WHO AFRO}
\usage{
clean_linelist(
  inputfile,
  outputdirectory = tempdir(),
  outputname = "Cleaned_linelist_",
  cleaningdict = system.file("inst", "cleaning_dictionary.xlsx", package =
    "covidmonitor")
)
}
\arguments{
\item{inputfile}{path to file created by \code{\link[covidmonitor:merge_linelist]{covidmonitor::merge_linelist()}} function}

\item{outputdirectory}{path to folder where merged file is to be saved}

\item{outputname}{character string to name merged file}

\item{cleaningdict}{path to dictionary file containing cleaning rule definitions.
Default is a predefined within the package. For details of how to use your own
file see \href{https://github.com/R4IDSR/covidmonitor}{README}}
}
\description{
Clean merged COVID-19 linelists from WHO AFRO
}
\author{
Alice Carr, Alex Spina
}
