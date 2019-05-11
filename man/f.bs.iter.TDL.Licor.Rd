\name{f.bs.iter.TDL.Licor}
\alias{f.bs.iter.TDL.Licor}
\title{one BS resample of TDL and Licor means}

\usage{f.bs.iter.TDL.Licor(val.obs.TDL, val.sum.TDL, val.obs.Licor, 
    val.sum.Licor, var.interp)}
\arguments{
  \item{val.obs.TDL}{}
  \item{val.sum.TDL}{}
  \item{val.obs.Licor}{}
  \item{val.sum.Licor}{}
  \item{var.interp}{}
}
\details{create val.bs.sum for calculated observed values to use in NP bootstrap

TDL using \code{\link{f.np.bs.mean}}

Licor
XXX Still need to determine what to do about BS for Licor data}
\value{val.bs.sum}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





