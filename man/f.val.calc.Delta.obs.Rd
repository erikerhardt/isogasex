\name{f.val.calc.Delta.obs}
\alias{f.val.calc.Delta.obs}
\title{Delta, observed discrimination}

\usage{f.val.calc.Delta.obs(reference.delta.e, chamber.delta.o, xi)}
\arguments{
  \item{reference.delta.e}{}
  \item{chamber.delta.o}{}
  \item{xi}{}
}
\details{\deqn{temp.xidode = xi * (chamber.delta.o - reference.delta.e)}
\deqn{Delta.obs = temp.xidode / (1 + chamber.delta.o - temp.xidode)}}
\value{Delta.obs}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





