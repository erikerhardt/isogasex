\name{f.val.calc.delta.13C.Assim}
\alias{f.val.calc.delta.13C.Assim}
\title{delta13C Assimilated, isotopic composition of assimilated sugars}
\description{d13CA (do-Dobs)/(Dobs+1)  isotopic composition of assimilated sugars
(Chris did this differently for d13Cs in his paper see following:
I think this could be calculated for modelling using the equivalent of de instead of do
i.e. when the leaf modelled is not in a chamber so the air above a leaf is equivalent to the bulk atmosphere)}
\usage{f.val.calc.delta.13C.Assim(chamber.delta.o, Delta.obs)}
\arguments{
  \item{chamber.delta.o}{}
  \item{Delta.obs}{}
}
\details{\deqn{delta.13C.Assim = (chamber.delta.o - Delta.obs) / (Delta.obs + 1)}}
\value{delta.13C.Assim}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





