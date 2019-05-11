\name{f.val.calc.all.driver}
\alias{f.val.calc.all.driver}
\title{This function calls all the \code{f.val.calc.*} files}

\usage{f.val.calc.all.driver(val.TDL, val.Licor, val.const, sw)}
\arguments{
  \item{val.TDL}{}
  \item{val.Licor}{}
  \item{val.const}{}
  \item{sw}{}
}
\details{SECTION Licor data assign variables

SECTION Concentrations.

Calibration tank concentrations

Calibration tank gain. \code{\link{f.val.calc.gain}}

Calibration tank offset. \code{\link{f.val.calc.offset}}

Corrected reference values. \code{\link{f.val.calc.corrected}}

Corrected chamber values. \code{\link{f.val.calc.corrected}}

Total reference and chamber values. \code{\link{f.val.calc.total.mol.fraction.CO2}}

xi. \code{\link{f.val.calc.xi}}

SECTION A Photosynthesis

A, TDL photosynthesis. \code{\link{f.val.calc.TDL.A.photosynthesis}}

12A, TDL photosynthesis. \code{\link{f.val.calc.TDL.A.photosynthesis}}

13A, TDL photosynthesis. \code{\link{f.val.calc.TDL.A.photosynthesis}}

Total A LI6400  Photo LI6400 header, these values listed because I will likely use them in a plot

Delta from ratios in and out?, (Re/Ro)-1, is this really the same? \code{\link{f.val.calc.Delta.from.ratios.in.out}}

D from A ratio, (Ro/(13A/12A))-1, should be the same as Dobs above. \code{\link{f.val.calc.Delta.from.A.ratio}}

Select TDL or Licor based on switch

SECTION Delta

delta reference and chamber values. \code{\link{f.val.calc.delta.proportion}}

delta diff

Delta discrim

Dobs observed discrimination. \code{\link{f.val.calc.Delta.obs}}

Dobs per mil  1000*Dobs. \code{\link{f.val.calc.Delta.obs.permil}}

delta13C Assimilated, isotopic composition of assimilated sugars. \code{\link{f.val.calc.delta.13C.Assim}}

p (Co - Ce) / Co. \code{\link{f.val.calc.p}}

delta13C Respired, isotopic composition of respired CO2. \code{\link{f.val.calc.delta.13C.Resp}}

SECTION g conductance

gbw BLcond  boundary layer conductance for water, Blcond is LI 6400 header

gbc BLcond/1.37 boundary layer conductance for CO2

gsw cond  stomatal conductance for water, cond is LI6400 header

gsc gsw/1.6 stomatal conductance for CO2

gtc, total (stomatal and boundary layer) conductance for CO2

SECTION Cx and px (concentrations and pressures)

Ca = Co, ppm CO2 concentration above the leaf = concentration leaving the leaf chamber = ambient CO2 concentration

Cs, ppm CO2 concentration at the leaf surface, cs calculated from eq 40 Ball 1987 Ch 20 Stomatal Function, eds Zeiger, Farquhar, Cowan. \code{\link{f.val.calc.Cs}}

pa, partial pressure of CO2 above the leaf, Press is the atmospheric pressure value from the LI6400. \code{\link{f.val.calc.pp}}

ps, partial pressure of CO2 at the leaf surface, Press is the atmospheric pressure value from the LI6400. \code{\link{f.val.calc.pp}}
NB: same formula as pa, but using Cs instead of Co

Ci, ppm CO2 concentration in the sub-stomatal cavities, ci calculated from eq 35 Ball 1987 Ch 20 Stomatal Function, eds Zeiger, Farquhar, Cowan. \code{\link{f.val.calc.Cs}}
NB: same formula as Cs, but using gtc instead of gbc

pi, partial pressure of CO2 in the substomatal cavities, Press is the atmospheric pressure value from the LI6400. \code{\link{f.val.calc.pp}}
NB: same formula as pa, but using Ci instead of Co

pi/pa total pi/pa or total Ci/Ca  ratio of substomatal CO2 partial pressure to CO2 partial pressure above leaf = Ci/Ca ratio of mol fractions

SECTION Delta.i predictions

Delta.i simple for gm, predicted discrimination including boundary layer effects but not decarboxylation effects. \code{\link{f.val.calc.Delta.i.simple.for.gm}}

Delta.i simple for modeling  a + (b-a) pi/pa predicted discrimination including boundary layer effects but using b adjustments to approximate effects of gm and decarboxylations. \code{\link{f.val.calc.Delta.i.simple.for.modeling}}

Delta.i complex for gm  CHECK DERIVATION  predicted discrimination including boundary layer effects AND decarboxylation effects. \code{\link{f.val.calc.Delta.i.complex.for.gm}}

Delta.i simple for gm-Dobs   (Di simple for gm)-(Dobs per mil)

Delta.i complex for gm-Dobs  (Di complex for gm)-(Dobs per mil)

SECTION gm mesophyll conductance

gm point simple, internal leaf (mesophyll) conductance calculated for every D value ignoring decarboxylation effects. \code{\link{f.val.calc.gm.point.simple}}

gm point complex, internal leaf (mesophyll) conductance calculated for every D value estimating decarboxylation effects. \code{\link{f.val.calc.gm.point.complex}}

select one of the calculated gm values to use

SECTION pc

pc using gm,  total partial pressure of CO2 at the site of carboxylation, Press is the atmospheric pressure value from the LI6400. \code{\link{f.val.calc.pc.using.gm}}

pc using simple D for gm, includes boundary layer. \code{\link{f.val.calc.pc.using.simple.Delta.for.gm}}

pc using simple D for modeling. \code{\link{f.val.calc.pc.using.simple.Delta.for.modeling}}

pc using complex D, no decarboxylation  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-Dpa]/(bs+al-b)  is this different from two up? They give different values but both may not be derived properly. \code{\link{f.val.calc.pc.using.complex.Delta.no.decarboxylation}}

pc using complex D, full model  [ab(pa-ps)+a(ps-pi)+pi(bs+al)-(eRd/k+fG*)-Dpa]/(bs+al-b)  includes boundary layer and decarboxylation effects. \code{\link{f.val.calc.pc.using.complex.Delta.full.model}}

select one of the calculated pc values to use

Cc  (pc*10^6)/(Press*1000)  ppm CO2 concentration at the site of carboxylation, generally meaning inside the chloroplast and ignoring PEPC in cytosol. \code{\link{f.val.calc.pp}}
NB: same formula as pa, but using pc instead of Co}
\value{val.temp}

\author{Erik B. Erhardt <erike@stat.unm.edu> and David T. Hanson <dthanson@unm.edu>}





