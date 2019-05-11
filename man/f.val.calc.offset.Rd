\name{f.val.calc.offset}
\alias{f.val.calc.offset}
\title{offset (12C Offset, 13C Offset) using hi tank}

\usage{f.val.calc.offset(true.value.hi.12C, true.value.hi.13C, gain.12C, 
    gain.13C, cal.tank.hi.12C, cal.tank.hi.13C)}
\arguments{
  \item{true.value.hi.12C}{}
  \item{true.value.hi.13C}{}
  \item{gain.12C}{}
  \item{gain.13C}{}
  \item{cal.tank.hi.12C}{}
  \item{cal.tank.hi.13C}{}
}
\details{\deqn{offset = true.value.hi - (gain * cal.tank.hi)}}
\value{offset for 12C and 13C}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





