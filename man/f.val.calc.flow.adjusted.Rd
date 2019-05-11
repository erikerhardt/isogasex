\name{f.val.calc.flow.adjusted}
\alias{f.val.calc.flow.adjusted}
\title{Adjusted flow}
\description{Deprecated (0.1-20, 9/5/2012)

This is no longer used since the correction is not required.

LI6400 flow includes water in the entering air but the TDL removes this water before measuring and this effectively reduces the flow (use H2OR to correct it).}
\usage{f.val.calc.flow.adjusted(flow, H2OR)}
\arguments{
  \item{flow}{}
  \item{H2OR}{}
}
\details{\deqn{flow.adjusted <- flow * (1 - (H2OR / 1000))}}
\value{flow.adjusted}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





