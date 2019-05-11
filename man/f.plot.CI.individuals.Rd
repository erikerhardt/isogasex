\name{f.plot.CI.individuals}
\alias{f.plot.CI.individuals}
\title{plot all variables with bs values, mean value, and CI intervals}

\usage{f.plot.CI.individuals(x.bs, x.sum, x.CI, x.title, x.time, x.n, 
    R.bootstrap, plot.format.list, output.fn.prefix)}
\arguments{
  \item{x.bs}{}
  \item{x.sum}{}
  \item{x.CI}{}
  \item{x.title}{}
  \item{x.time}{}
  \item{x.n}{}
  \item{R.bootstrap}{}
  \item{plot.format.list}{}
  \item{output.fn.prefix}{}
}
\details{For every variable, plots the mean and bootstrap CI, after removing NAs with \code{\link{plot.not.na}}.}
\value{NULL}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





