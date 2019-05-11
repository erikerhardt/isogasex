\name{f.bs.save.TDL.Licor}
\alias{f.bs.save.TDL.Licor}
\title{save BS resample of TDL and Licor means}

\usage{f.bs.save.TDL.Licor(val.bs.sum, val.bs, i.bs, R.bootstrap, sw)}
\arguments{
  \item{val.bs.sum}{}
  \item{val.bs}{}
  \item{i.bs}{}
  \item{R.bootstrap}{}
  \item{sw}{}
}
\details{create val.bs$TDL and $Licor for calculated observed values to use in NP bootstrap

Initialize values to zeros.

If using TDL, copy val.bs.sum$TDL$* to val.bs$TDL$*

If using Licor, copy val.bs.sum$Licor$* to val.bs$Licor$*}
\value{val.bs}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





