% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_scores.R
\name{process_scores}
\alias{process_scores}
\title{Process Scores}
\usage{
process_scores(
  data,
  scores,
  predData,
  v,
  rmse,
  replace,
  pmm.k,
  threshold,
  max_n_outliers,
  max_prop_outliers,
  allow_predictions,
  obj = NULL
)
}
\arguments{
\item{data}{Data set.}

\item{scores}{Matrix with outlier scores.}

\item{predData}{Prediction data.frame.}

\item{v}{Variables checked.}

\item{rmse}{rmse.}

\item{replace}{replace.}

\item{pmm.k}{pmm.k.}

\item{threshold}{threshold.}

\item{max_n_outliers}{max_n_outliers.}

\item{max_prop_outliers}{max_prop_outliers.}

\item{allow_predictions}{allow_predictions.}

\item{obj}{outForest object.}
}
\value{
A list.
}
\description{
Internal function used to process scores and replace outliers.
}
