\name{val.TDL.Licor.variables}
\alias{val.TDL.Licor.variables}
\title{Assign TDL and Licor values to val variable names, raw and summarized values}

\usage{val.TDL.Licor.variables(TDL, Licor, TDL.cycle, val, sw)}
\arguments{
  \item{TDL}{}
  \item{Licor}{}
  \item{TDL.cycle}{}
  \item{val}{}
  \item{sw}{}
}
\details{Function to assign TDL and Licor values to variable names,
summaries and some smart way for individual observations (later for NPBS).
If the variable has values, use those, if not then populate with NAs.

These are the columns expected from the Licor and TDL.
Any columns with headers not listed here will not be used.

EXPECTED column names for Licor:
\preformatted{
(in tdllicor)     (column from Licor)
Obs               Obs
HHMMSS            HHMMSS
FTime             FTime
A                 Photo
E                 Trmmol
temp.air          Tair
temp.leaf         Tleaf
temp.block        TBlk
Ce                CO2R
Co                CO2S
xin               H2OR
xout              H2OS
rh.ref            RH_R
rh.sam            RH_S
uin               Flow
par.int           PARi
par.ext           PARo
Atm.press         Press
CsMch             CsMch
HsMch             HsMch
StableF           StableF
Status            Status
gsc               Cond
Ci                Ci
VPD               VpdL
La                Area
StmRat            StmRat
gbw               BLCond
VpdA              VpdA
Ci.Ca             Ci.Ca
pi                Ci_Pa
uc_20_mV          uc_20_mV
uc_21_mV          uc_21_mV
U_S               X.U.S.
Trans             Trans
CndCO2            CndCO2
Ref_mV            Ref_mV
xTemp1            xTemp1
xTemp2            xTemp2
TChamAir          TChamAir
}

EXPECTED column names for TDL:
\preformatted{
(in tdllicor)       (column from TDL)
TIMESTAMP           TIMESTAMP
RECORD              RECORD
PrevSite            PrevSite
SiteOutput          SiteOutput
StartSeqFlag        StartSeqFlag
SeqActiveFlag       SeqActiveFlag
SiteCount           SiteCount
conc12CO2           ConcA
conc13CO2           ConcB
ConcC               ConcC
TGAStatus           TGAStatus
TGApressure         TGApressure
MassFlow1           MassFlow1
Pressure1           Pressure1
MassFlow2           MassFlow2
Pressure2           Pressure2
PressureProMan      PressureProMan
}

Conventions, put everything in \code{val}.
Observed values from TDL or Licor  prefix: \code{val$obs$*}

Summarized values from TDL or Licor  prefix: \code{val$sum$*}}
\value{val}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





