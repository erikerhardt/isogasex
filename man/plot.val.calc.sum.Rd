\name{plot.val.calc.sum}
\alias{plot.val.calc.sum}
\title{This function plots all the calculated values}
\description{most plots are not created when not using Licor file}
\usage{\method{plot}{val.calc.sum}(val.TDL, val.Licor, val.const, val.temp, plot.format.list, 
    output.fn.prefix, sw)}
\arguments{
  \item{val.TDL}{}
  \item{val.Licor}{}
  \item{val.const}{}
  \item{val.temp}{}
  \item{plot.format.list}{}
  \item{output.fn.prefix}{}
  \item{sw}{}
}
\details{Plots many variables by calling the \code{plot.*} functions.

Plot active gains, offsets, and corrected magnitudes. \code{\link{plot.gain.offset}}

Plot corrected 12CO2 and 13CO2, total, and difference for reference and sample. \code{\link{plot.corrected.total.diff.xi}}

Plot A photosynthesis. \code{\link{plot.A.photosynthesis}}

Plot Licor Temp Light values. \code{\link{plot.Licor.temp.light}}

Plot Licor uin, xin, La, Atm.press values 7/21/2012. \code{\link{plot.Licor.flow.press}}

Plot delta, Delta, and p. \code{\link{plot.delta.Delta.p}}

Plot Ca Cs, CO2 concentrations above the leaf and at the leaf surface. \code{\link{plot.Ca.Cs}}

Plot Pa Ps, partial pressure of CO2 above the leaf and at the leaf surface. \code{\link{plot.pa.ps}}

Plot gbc, gsc, gtc: boundary layer, stomatal, and total conductance for CO2. \code{\link{plot.gbc.gsc.gtc}}

Plot Ci, Pi, CO2 concentration and partial pressure of CO2 in the substomatal cavities. \code{\link{plot.Ci.pi}}

Plot Totalpi_pa, ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions. \code{\link{plot.pi_pa}}

Plot Delta.i, predicted discrimination. \code{\link{plot.Delta.i}}

Plot gm point simple and complex, internal leaf (mesophyll) conductance calculated for every D value ignoring and estimating decarboxylation effects. \code{\link{plot.gm}}

Plot pc, total partial pressure of CO2 at the site of carboxylation. \code{\link{plot.pc.total}}

Plot pc, simple and complex. \code{\link{plot.pc.simple.complex}}

Plot Cc, ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol. \code{\link{plot.Cc.total}}}
\value{NULL}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





