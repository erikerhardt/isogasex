\name{f.val.bs.CI}
\alias{f.val.bs.CI}
\title{create CI for each calculated value}

\usage{f.val.bs.CI(val.calc.bs, val.bs.TDL, val.bs.Licor, R.bootstrap, 
    sig.CI, sw)}
\arguments{
  \item{val.calc.bs}{}
  \item{val.bs.TDL}{}
  \item{val.bs.Licor}{}
  \item{R.bootstrap}{}
  \item{sig.CI}{}
  \item{sw}{}
}
\details{Create val.CI CI endpoints for $TDL, $Licor, and $calc.

Calculate quantile indices of sorted data for sig.CI.

For reasonable variables, calculates CIs using \code{\link{f.val.bs.CI.endpoints}}.}
\value{val.CI}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





