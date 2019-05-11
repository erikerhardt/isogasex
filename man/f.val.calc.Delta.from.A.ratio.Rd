\name{f.val.calc.Delta.from.A.ratio}
\alias{f.val.calc.Delta.from.A.ratio}
\title{D from A ratio, (Ro/(13A/12A))-1, should be the same as Dobs above}

\usage{f.val.calc.Delta.from.A.ratio(cham.12Co, cham.13Co, TDL.13A, 
    TDL.12A)}
\arguments{
  \item{cham.12Co}{}
  \item{cham.13Co}{}
  \item{TDL.13A}{}
  \item{TDL.12A}{}
}
\details{First calculates R.cham with \code{\link{f.val.calc.isotope.ratio}}

\deqn{Delta.from.A.ratio <- (R.cham / (TDL.13A / TDL.12A)) - 1}
this quantity is not in per mil}
\value{Delta.from.A.ratio}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





