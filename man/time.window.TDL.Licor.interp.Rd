\name{time.window.TDL.Licor.interp}
\alias{time.window.TDL.Licor.interp}
\title{keep only the overlapping time window of TDL and Licor files}
\description{interpolate the Licor measurements via cubic splines to TDL faster sampling rate
assign variables names within TDL and Licor.interp}
\usage{\method{time}{window.TDL.Licor.interp}(TDL, Licor, TDL.Licor.times, sw)}
\arguments{
  \item{TDL}{}
  \item{Licor}{}
  \item{TDL.Licor.times}{}
  \item{sw}{}
}
\details{BEGIN If use TDL

- Indices to keep based on TDL/Licor time window overlap.

- TDL summary ind to keep.

- Reduce TDL summary.

- Reduce TDL interp.

END

BEGIN If use Licor

- Indices to keep based on TDL/Licor time window overlap.

- Use \code{zoo} to merge the times for TDL and Licor.

Create \code{Licor.interp} to hold interpolated Licor values at resolution of TDL observations.

populate with observed values in right locations

These indices have actual Licor values, the others are interpolated.

Some Licor values maybe shouldn't be interpolated --

If all values are NA, then consider column as a missing column and don't interp.

Cubic interpolation of Licor values to TDL sampling time points using \code{nknots}=\code{df}=
Cubic spline interpolation using \code{smooth.spline} with \code{nknots}=\code{df}=number of Licor times matching TDL times.

END

Put TDL and Licor.interp together to return.}
\value{TDL.and.Licor.interp}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





