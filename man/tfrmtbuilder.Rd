% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tfrmtbuilder.R
\name{tfrmtbuilder}
\alias{tfrmtbuilder}
\title{Run tfrmt Builder Shiny App}
\usage{
tfrmtbuilder(tfrmt = NULL, data = NULL, mockmode = TRUE, run = TRUE)
}
\arguments{
\item{tfrmt}{tfrmt object to be loaded into app from R session. Defaults to \code{NULL}}

\item{data}{data frame to be loaded into app from R session. Defaults to \code{NULL}}

\item{mockmode}{Whether to initialize the app in mock mode. Defaults to \code{TRUE}}

\item{run}{Boolean for whether the created object should be run directly. Set to \code{FALSE} for deployment}
}
\value{
Shiny app for creating and modifying tfrmt objects
}
\description{
Run tfrmt Builder Shiny App
}
\examples{
if (interactive()){
  tfrmtbuilder()
}
}
