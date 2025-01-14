% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_cate_data.R
\name{prepare_cate_data}
\alias{prepare_cate_data}
\title{Create Feature Table for CATE Estimation}
\usage{
prepare_cate_data(named_args, aggregation_method)
}
\arguments{
\item{named_args}{list. A list of pre-specified argument values from \link{estimate_cate}.}

\item{aggregation_method}{string. Method for aggregating results across studies.}
}
\value{
A list with the following elements:
\itemize{
\item \verb{feature tbl}: tbl with columns corresponding to pre-specified covariates
\item \code{treatment_vec}: vector of treatment indicators for each observation
\item \code{outcome_vec}: vector of outcome values for each observation
\item \code{study_vec}: vector of study IDs for each observation
\item \code{study_col}: name of study ID column
}
}
\description{
If \code{estimation_method} is "studyindicator", creates feature table with pre-specified covariates
and adds set of dummy variables for each study to \code{trial_tbl}. If \code{estimation_method} is
"ensembleforest", creates feature table with only pre-specified covariates (which will include
removing study identifier column).
}
