% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_val_calc_delta_13C_Resp.R
\name{f_val_calc_delta_13C_Resp}
\alias{f_val_calc_delta_13C_Resp}
\title{delta13C Respired, isotopic composition of respired CO2}
\usage{
f_val_calc_delta_13C_Resp(reference_delta_e, chamber_delta_o, p)
}
\arguments{
\item{reference_delta_e}{xxxPARAMxxx}

\item{chamber_delta_o}{xxxPARAMxxx}

\item{p}{xxxPARAMxxx}
}
\value{
delta_13C_Resp xxxRETURNxxx
}
\description{
\deqn{delta_13C_Resp = (chamber_delta_o - reference_delta_e * (1 - p)) / p}
}
