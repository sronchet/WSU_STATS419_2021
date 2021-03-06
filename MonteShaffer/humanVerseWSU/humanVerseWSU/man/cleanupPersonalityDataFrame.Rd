% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-cleanup.R
\name{cleanupPersonalityDataFrame}
\alias{cleanupPersonalityDataFrame}
\title{cleanupPersonalityDataFrame}
\usage{
cleanupPersonalityDataFrame(
  personality.raw = readRDS(system.file("extdata", "personality-raw.rds", package =
    "humanVerseWSU"))
)
}
\arguments{
\item{personality.raw}{The raw dataframe which can auto-populate}
}
\value{
An updated dataframe, cleaned and sorted
}
\description{
cleanupPersonalityDataFrame
}
