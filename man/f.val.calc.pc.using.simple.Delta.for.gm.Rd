\name{f.val.calc.pc.using.simple.Delta.for.gm}
\alias{f.val.calc.pc.using.simple.Delta.for.gm}
\title{pc using simple D for gm, includes boundary layer}

\usage{f.val.calc.pc.using.simple.Delta.for.gm(Delta, pa, ps, a, b.gm, 
    a.b)}
\arguments{
  \item{Delta}{}
  \item{pa}{}
  \item{ps}{}
  \item{a}{}
  \item{b.gm}{}
  \item{a.b}{}
}
\details{\deqn{pc = (Delta * pa - a.b * (pa - ps) - a * ps) / (b.gm - a)}}
\value{pc}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





