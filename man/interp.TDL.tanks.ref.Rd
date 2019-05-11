\name{interp.TDL.tanks.ref}
\alias{interp.TDL.tanks.ref}
\title{Interpolate TDL tank and reference values}

\usage{interp.TDL.tanks.ref(TDL, TDL.cycle, plot.format.list, output.fn.prefix, 
    sw)}
\arguments{
  \item{TDL}{}
  \item{TDL.cycle}{}
  \item{plot.format.list}{}
  \item{output.fn.prefix}{}
  \item{sw}{}
}
\details{Create TDL$interp to hold interpolated values between measurement cycles and observed values within measurement cycles

List of sites (hi, low, ref) to perform smooth interpolation (1).

For those sites with interp=1, perform smooth interpolation using \code{smooth.spline}.

Cubic spline interpolation using \code{smooth.spline} with \code{nknots}=\code{df}=number of cycles for current site.

Extrapolate the first/last cal tank to the first/last time values for the run.

Fill within block times with data

List of sites (hi, low, ref) to perform "last mean value" flat interpolation (2).

For those sites with interp=2, perform last mean value flat interpolation.

Plot the interpolated values.}
\value{TDL}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





