\name{read.TDL}
\alias{read.TDL}
\title{read TDL file}

\usage{read.TDL(TDL.fn, sw)}
\arguments{
  \item{TDL.fn}{}
  \item{sw}{}
}
\details{See \code{\link{val.TDL.Licor.variables}} for expected column names.

Create \code{TDL} to hold data.

Read TDL file

Report times where cycle times when TDL program was not running 2/9/2012 1:18AM.

Remove non-cycle times from head of TDL file.

Keep all points (including where TDL program was not running in middle).

Keep all the time values available for Licor/TDL time merge.}
\value{TDL}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





