% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network.R
\name{network}
\alias{network}
\alias{network,HEV-method}
\title{Extract the between farm movements from a HEV model}
\usage{
network(model, keep_internal = FALSE)

\S4method{network}{HEV}(model, keep_internal = FALSE)
}
\arguments{
\item{model}{the HEV model to extract the network from.}

\item{keep_internal}{keep internal movements.}
}
\value{
a \code{data.table} with the columns \code{time} (the time
    for when the movement occured), \code{from} (the farm the
    animals are moved from), \code{to} (the farm the animals are
    moved to), and \code{n} (the number of animals moved).
}
\description{
Extract the between farm movements from a HEV model
}
