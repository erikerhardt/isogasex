\name{f.val.calc.corrected}
\alias{f.val.calc.corrected}
\title{corrected (Corrected 12C, Corrected 13C)}

\usage{f.val.calc.corrected(obs.12C, obs.13C, gain.12C, gain.13C, offset.12C, 
    offset.13C)}
\arguments{
  \item{obs.12C}{}
  \item{obs.13C}{}
  \item{gain.12C}{}
  \item{gain.13C}{}
  \item{offset.12C}{}
  \item{offset.13C}{}
}
\details{\deqn{corrected = (obs * gain) + offset;}}
\value{corrected 12C and 13C}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





