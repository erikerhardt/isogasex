\name{f.val.calc.Delta.i.complex.for.gm}
\alias{f.val.calc.Delta.i.complex.for.gm}
\title{Delta.i complex for gm  CHECK DERIVATION  predicted discrimination including boundary layer effects AND decarboxylation effects}

\usage{f.val.calc.Delta.i.complex.for.gm(a, b, a.b, pa, ps, pi, b.gm, 
    f.photo, Gamma.star, e, Rd, k)}
\arguments{
  \item{a}{}
  \item{b}{}
  \item{a.b}{}
  \item{pa}{}
  \item{ps}{}
  \item{pi}{}
  \item{b.gm}{}
  \item{f.photo}{}
  \item{Gamma.star}{}
  \item{e}{}
  \item{Rd}{}
  \item{k}{}
}
\details{\deqn{Delta.i.complex.for.gm = a.b * ((pa - ps) / pa) + a * ((ps - pi) / pa) + b * (pi / pa) - (f.photo * Gamma.star / pa) - (e * Rd / (k * pa))}}
\value{Delta.i.complex.for.gm}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





