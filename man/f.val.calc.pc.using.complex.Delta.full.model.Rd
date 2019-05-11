\name{f.val.calc.pc.using.complex.Delta.full.model}
\alias{f.val.calc.pc.using.complex.Delta.full.model}
\title{pc using complex D, full model  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-(eRd/k+fG*)-Dpa]/(bs+al-b)  includes boundary layer and decarboxylation effects}

\usage{f.val.calc.pc.using.complex.Delta.full.model(Delta, pa, ps, pi, 
    a, a.b, al, b, bs, e, Rd, k, f, Gamma.star)}
\arguments{
  \item{Delta}{}
  \item{pa}{}
  \item{ps}{}
  \item{pi}{}
  \item{a}{}
  \item{a.b}{}
  \item{al}{}
  \item{b}{}
  \item{bs}{}
  \item{e}{}
  \item{Rd}{}
  \item{k}{}
  \item{f}{}
  \item{Gamma.star}{}
}
\details{\deqn{pc = (a.b * (pa - ps) + a * (ps - pi) + pi * (bs + al) - Delta * pa  - (e * Rd / k + f * Gamma.star)) / (bs + al - b)}}
\value{pc}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





