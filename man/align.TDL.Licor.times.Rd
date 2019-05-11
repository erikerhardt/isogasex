\name{align.TDL.Licor.times}
\alias{align.TDL.Licor.times}
\title{find the overlapping time window of the TDL and Licor files and return the start/end indices of each file}

\usage{align.TDL.Licor.times(TDL.time, TDL.n, Licor.time, Licor.n, val.timewindow, 
    sw)}
\arguments{
  \item{TDL.time}{}
  \item{TDL.n}{}
  \item{Licor.time}{}
  \item{Licor.n}{}
  \item{val.timewindow}{}
  \item{sw}{}
}
\details{Unused sites (comment):
When sites are specified in TDL file that are not in the template file (because more than 4 sites are used),
then there are NAs for times for used sites.  In this case, the matching times above may be different
from the min/max times below.  In this case, we should use the matching times (that is, always use the matching times)
otherwise errors result from NA times in \code{\link{time.window.TDL.Licor.interp}}.
The logic is: use the widest matching times within the timewindow.

Indicate first and last times for TDL and Licor.

Report first and last times.

Indicate the time window specified.

Set TDL and Licor times to NA outside the time window.

Indicate the first and last matching TDL and Licor times in time window.

Check that TDL and Licor first and last times are identical.

Indices for first and last times in TDL and Licor files.

Use narrower timewindow start and end times, if specified.}
\value{TDL.Licor.times}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





