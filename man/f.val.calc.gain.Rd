\name{f.val.calc.gain}
\alias{f.val.calc.gain}
\title{gain (12C Gain, 13C Gain)}

\usage{f.val.calc.gain(true.value.hi.12C, true.value.hi.13C, true.value.lo.12C, 
    true.value.lo.13C, cal.tank.hi.12C, cal.tank.hi.13C, cal.tank.lo.12C, 
    cal.tank.lo.13C)}
\arguments{
  \item{true.value.hi.12C}{}
  \item{true.value.hi.13C}{}
  \item{true.value.lo.12C}{}
  \item{true.value.lo.13C}{}
  \item{cal.tank.hi.12C}{}
  \item{cal.tank.hi.13C}{}
  \item{cal.tank.lo.12C}{}
  \item{cal.tank.lo.13C}{}
}
\details{\deqn{gain = (true.value.hi - true.value.lo) / (cal.tank.hi - cal.tank.lo)}}
\value{gain for 12C and 13C}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





