\name{last.TDL.index.for.means}
\alias{last.TDL.index.for.means}
\title{Determine the last index for admissible values to calculate means}

\usage{last.TDL.index.for.means(TDL, TDL.cycle)}
\arguments{
  \item{TDL}{}
  \item{TDL.cycle}{}
}
\details{Rind indices just before where site change occurs.

Error checking which sites are defined in template/TDL file but not the other.

Determine sites in TDL file not in template file.

Remove indices in TDL file not defined in the template (do not calculate for these).

Determine sites in template file not in TDL file.

Make sure the last TDL.cycle$table[,"last.n.obs"] are the same site as temp.last.ind.

If chamber, then go back and keep calculating index blocks.

For chamber, skip the first TDL.cycle$first.n.last.skip.chamber number of blocks.
Note that these are in reverse order, so the last rows in TDL$last.list are the first cycles.
Check that the last TDL.cycle$first.n.last.skip.chamber number of blocks are all chambers
and if so, then remove these cycles.

Sort by index.}
\value{TDL}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





