\name{f.val.calc.Delta.from.ratios.in.out}
\alias{f.val.calc.Delta.from.ratios.in.out}
\title{Delta from ratios in and out?, (Re/Ro)-1, is this really the same?}

\usage{f.val.calc.Delta.from.ratios.in.out(ref.12Ce, ref.13Ce, cham.12Co, 
    cham.13Co)}
\arguments{
  \item{ref.12Ce}{}
  \item{ref.13Ce}{}
  \item{cham.12Co}{}
  \item{cham.13Co}{}
}
\details{First calculates R.ref and R.cham with \code{\link{f.val.calc.isotope.ratio}}

\deqn{Delta.from.ratios.in.out <- (R.ref / R.cham) - 1}
this quantity is not in per mil}
\value{Delta.from.ratios.in.out}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





