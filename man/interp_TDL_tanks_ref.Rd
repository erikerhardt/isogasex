% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interp_TDL_tanks_ref.R
\name{interp_TDL_tanks_ref}
\alias{interp_TDL_tanks_ref}
\title{Interpolate TDL tank and reference values}
\usage{
interp_TDL_tanks_ref(TDL, TDL_cycle, plot_format_list, output_fn_prefix,
  sw)
}
\arguments{
\item{TDL}{xxxPARAMxxx}

\item{TDL_cycle}{xxxPARAMxxx}

\item{plot_format_list}{xxxPARAMxxx}

\item{output_fn_prefix}{xxxPARAMxxx}

\item{sw}{xxxPARAMxxx}
}
\value{
TDL xxxRETURNxxx
}
\description{
Create TDL$interp to hold interpolated values between measurement cycles and observed values within measurement cycles
}
\details{
List of sites (hi, low, ref) to perform smooth interpolation (1).

For those sites with interp=1, perform smooth interpolation using \code{smooth_spline}.

Cubic spline interpolation using \code{smooth_spline} with \code{nknots}=\code{df}=number of cycles for current site.

Extrapolate the first/last cal tank to the first/last time values for the run.

Fill within block times with data

List of sites (hi, low, ref) to perform "last mean value" flat interpolation (2).

For those sites with interp=2, perform last mean value flat interpolation.

Plot the interpolated values.
}
