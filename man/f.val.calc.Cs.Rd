\name{f.val.calc.Cs}
\alias{f.val.calc.Cs}
\title{Cs, ppm CO2 concentration at the leaf surface, cs calculated from eq 40 Ball 1987 Ch 20 Stomatal Function, eds Zeiger, Farquhar, Cowan}

\usage{f.val.calc.Cs(gbc, E, Co, A)}
\arguments{
  \item{gbc}{}
  \item{E}{}
  \item{Co}{}
  \item{A}{}
}
\details{\deqn{Cs = (((gbc - (E / 2000)) * Co) - A) / (gbc + (E / 2000))}}
\value{Cs}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





