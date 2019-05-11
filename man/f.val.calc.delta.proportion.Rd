\name{f.val.calc.delta.proportion}
\alias{f.val.calc.delta.proportion}
\title{delta as proportion (Isotopic composition (d) as fraction)}
\description{I prefer using the fraction for calculations and then converting to composition at the end}
\usage{f.val.calc.delta.proportion(obs.12C, obs.13C, R.std)}
\arguments{
  \item{obs.12C}{}
  \item{obs.13C}{}
  \item{R.std}{}
}
\details{First calculates R.obs with \code{\link{f.val.calc.isotope.ratio}}
\deqn{delta.proportion <- (R.obs / R.std) - 1}
this quantity is not in per mil}
\value{delta.proportion}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





