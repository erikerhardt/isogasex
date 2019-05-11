\name{assign.variables}
\alias{assign.variables}
\title{Assign variables from input file to variable names}

\usage{assign.variables(D, path)}
\arguments{
  \item{D}{}
  \item{path}{}
}
\details{initialize lists to hold all data/output

Template version

switches to use and filenames for TDL and Licor files

Prefix for output filenames

TIME WINDOW SECTION

OUTPUT SECTION

plot formats

interpolation:  1 = smooth.spline() like tanks
2 = use last reference mean until next measurement (horizontal interp) - for abrupt ref gas changes

Bootstrap

TDL sampling information

CONSTANTS SECTION

True calibration tank values

Which values to use

constants in formulas not given names

Calculation information

create a list to all returned parameters}
\value{VARIABLES list of all inputs from Excel template}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





