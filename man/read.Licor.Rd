\name{read.Licor}
\alias{read.Licor}
\title{read Licor file}

\usage{read.Licor(Licor.fn, Licor.TDL.time.offset.seconds, sw)}
\arguments{
  \item{Licor.fn}{}
  \item{Licor.TDL.time.offset.seconds}{}
  \item{sw}{}
}
\details{See \code{\link{val.TDL.Licor.variables}} for expected column names.

Create \code{Licor} to hold data.

Read Licor file

Find the row where the column headers and data begin.

Look for "Obs" as the first row (used to look for $STARTOFDATA$, but not in every version of Licor file).

If any extra lines in Licor file, remove those lines and fix the Obs and HHMMSS columns.

Date (day) of run, fix Thursday (Thr to Thu) representation.}
\value{Licor}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





