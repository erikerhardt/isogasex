\name{tdllicor}
\alias{tdllicor}
\title{Reads TDL and Licor files, aligns them, calculates quantities of interest with bootstrap intervals.}
\description{Edit \file{tdllicor_templateX.xls} and input parameters and TDL/Licor filenames. Run \code{\link{tdllicor}}. See output in \file{./out} directory.
Edit tdllicor_templateX.xls and input parameters and TDL/Licor filenames.
Run tdllicor().
See output in ./out directory.}
\usage{tdllicor(input.fn, path = getwd())}
\arguments{
  \item{input.fn}{The Excel workbook name.  A modified version of tdllicor_templateX.xls}
  \item{path}{Directory where TDL and Licor data are to be read from, and where ./out directory for results are to be written to.}
}
\details{SECTION Preamble
Set version.
Display header.

Save original working directory, change to data dir.
(This is a little hokey.)  Change to data dir, then to out dir, then back to original at end.

Create output directory.

SECTION Read data.

Input Excel workbook. \code{\link{get.data}}

Assign variables. \code{\link{assign.variables}}

Create output directory (prefix) and update the process_info.txt.

Check that template version matches version of tdllicor.

Set random seed for samples.

Read TDL and Licor data.  \code{\link{read.TDL}} and \code{\link{read.Licor}}

SECTION TDL Plot and interpolate.

BEGIN If using TDL:

_ Print lines with missing values in TDL file and fix them. \code{\link{fix.missing.TDL.values}}

_ Extract cycle timing, write to file. \code{\link{extract.cycle.timing}}

_ Determine the last index for admissible values to calculate means. \code{\link{last.TDL.index.for.means}}

_ Plot each cycle of TDL measurements (visual diagnostics). \code{\link{plot.data.cycles}}

_ Calculate the mean and variance for the TDL data based on last measurements. \code{\link{calc.mean.TDL}}

_ Interpolate TDL tank and reference values. \code{\link{interp.TDL.tanks.ref}}

_ Summary values for TDL interpolated tanks and reference. \code{\link{calc.mean.TDL.interp.tanks.ref}}

_ Calculate mean Variance for interp values for Par BS of tanks and reference.

END

SECTION Align TDL and Licor

Determine overlapping time window of TDL and Licor measurements. \code{\link{align.TDL.Licor.times}}

Reduce data to overlapping time window and interp Licor values to TDL timepoints. \code{\link{time.window.TDL.Licor.interp}}

Plot each cycle of TDL measurements (visual diagnostics). \code{\link{plot.data.cycles}}

Calculate the mean and variance for the Licor data based on last measurements. \code{\link{calc.mean.Licor}}

SECTION Mean Calculations

Assign TDL and Licor values to val variable names, raw and summarized values. \code{\link{val.TDL.Licor.variables}}

summary values (mean). \code{\link{f.val.calc.all.driver}}

All time point values. \code{\link{f.val.calc.all.driver}}

SECTION Write out results

Summary files

TDL file. \code{\link{write.summary.TDL.file}}

Licor file. \code{\link{write.summary.Licor.file}}

Summary Calculation file. \code{\link{write.summary.Calc.file}}

All Calculation file. \code{\link{write.all.Calc.file}}

plot all the calculated values. \code{\link{plot.val.calc.sum}}

SECTION Bootstrap

BEGIN Bootstrap Calculations

_ Generate NP and Par BS samples for observed values.

. \code{\link{f.init.bs.matrix}}

. \code{\link{f.bs.iter.TDL.Licor}}

. \code{\link{f.bs.save.TDL.Licor}}

. \code{\link{f.val.calc.all.driver}}

. \code{\link{f.val.bs.matrix}}

_ calculate central 95\% intervals.

_ create CI for each calculated value. \code{\link{f.val.bs.CI}}

_ plot all variables with bs values, mean value, and CI intervals. \code{\link{f.plot.CI.individuals.driver}}

_ write out central 95\% interval, SD,

_ Summary files. \code{\link{write.CI.TDL.file}}

_ TDL file

_ Licor file. \code{\link{write.CI.Licor.file}}

_ Calculation file. \code{\link{write.CI.Calc.file}}

END

Save workspace.

SECTION Wrap it up.
Move selected plot files into subdirectories. \code{\link{move.plot.files}}

Copy template file to out dir.

Change back to original dir.

Complete.}
\value{NULL}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





