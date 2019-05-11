\name{fix.missing.TDL.values}
\alias{fix.missing.TDL.values}
\title{Replace NA and NAN values in TDL$data by previous value for that variable}

\usage{fix.missing.TDL.values(TDL, bad.ind)}
\arguments{
  \item{TDL}{}
  \item{bad.ind}{}
}
\details{Look at each field.

If not finite, then it's a bad value to replace.

Replace bad value with previous value in the file.}
\value{TDL}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





