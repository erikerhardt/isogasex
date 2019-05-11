\name{f.val.calc.gm.point.complex}
\alias{f.val.calc.gm.point.complex}
\title{gm point complex, internal leaf (mesophyll) conductance calculated for every D value estimating decarboxylation effects}

\usage{f.val.calc.gm.point.complex(b, bs, al, A, pa, Di, D.obs, f.photo, 
    Gamma.star, e, Rd, k)}
\arguments{
  \item{b}{}
  \item{bs}{}
  \item{al}{}
  \item{A}{}
  \item{pa}{}
  \item{Di}{}
  \item{D.obs}{}
  \item{f.photo}{}
  \item{Gamma.star}{}
  \item{e}{}
  \item{Rd}{}
  \item{k}{}
}
\details{\deqn{gm.point.complex = ((b - bs - al) * A / pa) / (Di - D.obs - (f.photo * Gamma.star / pa) - ((e * Rd) / (k * pa)))}}
\value{gm.point.complex}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





