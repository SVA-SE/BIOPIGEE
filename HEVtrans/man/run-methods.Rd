% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{run,HEV-method}
\alias{run,HEV-method}
\title{Run the model}
\usage{
\S4method{run}{HEV}(model, solver = c("ssm", "aem"), ...)
}
\arguments{
\item{model}{The model to run.}

\item{solver}{Which numerical solver to utilize. Default is 'ssm'.}

\item{...}{Additional arguments.}
}
\value{
A model with a single stochastic solution trajectory attached to it.
}
\description{
Run the model
}
