\name{f.val.calc.gm.point.simple}
\alias{f.val.calc.gm.point.simple}
\title{gm point simple, internal leaf (mesophyll) conductance calculated for every D value ignoring decarboxylation effects}

\usage{f.val.calc.gm.point.simple(b, bs, al, A, pa, Di, D.obs)}
\arguments{
  \item{b}{}
  \item{bs}{}
  \item{al}{}
  \item{A}{}
  \item{pa}{}
  \item{Di}{}
  \item{D.obs}{}
}
\details{\deqn{gm.point.simple = (( b - bs - al) * A / pa) / (Di - D.obs)}}
\value{gm.point.simple}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





