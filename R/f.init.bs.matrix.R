f.init.bs.matrix <-
function# init bs values to zero
###
(n
###
, R.bootstrap
###
)
{
  ##details<<
  ## Create bs for calculated bootstrap values to estimate error, initializing to zeros.
  bs  <- as.list(new.env()); # for calculated bootstrap values to estimate error
  zeros <- matrix(0, nrow=n, ncol=R.bootstrap);
  bs$gain.12C                                                <- zeros;
  bs$gain.13C                                                <- zeros;
  bs$offset.12C                                              <- zeros;
  bs$offset.13C                                              <- zeros;
  bs$reference.12Ce                                          <- zeros;
  bs$reference.13Ce                                          <- zeros;
  bs$chamber.12Co                                            <- zeros;
  bs$chamber.13Co                                            <- zeros;
  bs$reference.TotalCe                                       <- zeros;
  bs$chamber.TotalCo                                         <- zeros;
  bs$chamber.reference.Total.diff.CeCo                       <- zeros;
  bs$chamber.reference.12.diff.CeCo                          <- zeros;
  bs$chamber.reference.13.diff.CeCo                          <- zeros;
  bs$xi                                                      <- zeros;
  #bs$flow.adjusted                                           <- zeros;  # 9/5/2012
  bs$TDL.A.photosynthesis                                    <- zeros;
  bs$TDL.12A.photosynthesis                                  <- zeros;
  bs$TDL.13A.photosynthesis                                  <- zeros;
  bs$Licor.A.photosynthesis                                  <- zeros;
  bs$Delta.from.ratios.in.out                                <- zeros;
  bs$Delta.from.A.ratio                                      <- zeros;
  bs$VPD                                                     <- zeros;
  bs$E.transpiration                                         <- zeros;
  bs$leaf.temp                                               <- zeros;
  bs$air.temp                                                <- zeros;
  bs$light.in                                                <- zeros;
  bs$light.out                                               <- zeros;
  bs$reference.delta.e                                       <- zeros;
  bs$chamber.delta.o                                         <- zeros;
  bs$chamber.reference.delta.diff.CoCe                       <- zeros;
  bs$Delta.obs                                               <- zeros;
  bs$Delta.obs.permil                                        <- zeros;
  bs$delta.13C.Assim                                         <- zeros;
  bs$p                                                       <- zeros;
  bs$delta.13C.Resp                                          <- zeros;
  bs$chamber.TotalCa                                         <- zeros;
  bs$chamber.12Ca                                            <- zeros;
  bs$chamber.13Ca                                            <- zeros;
  bs$chamber.TotalCs                                         <- zeros;
  bs$chamber.12Cs                                            <- zeros;
  bs$chamber.13Cs                                            <- zeros;
  bs$chamber.Totalpa                                         <- zeros;
  bs$chamber.12pa                                            <- zeros;
  bs$chamber.13pa                                            <- zeros;
  bs$chamber.Totalps                                         <- zeros;
  bs$chamber.12ps                                            <- zeros;
  bs$chamber.13ps                                            <- zeros;
  bs$chamber.Totalgbw                                        <- zeros;
  bs$chamber.Totalgbc                                        <- zeros;
  bs$chamber.12gbc                                           <- zeros;
  bs$chamber.13gbc                                           <- zeros;
  bs$chamber.Totalgsw                                        <- zeros;
  bs$chamber.Totalgsc                                        <- zeros;
  bs$chamber.12gsc                                           <- zeros;
  bs$chamber.13gsc                                           <- zeros;
  bs$chamber.Totalgtc                                        <- zeros;
  bs$chamber.12gtc                                           <- zeros;
  bs$chamber.13gtc                                           <- zeros;
  bs$chamber.TotalCi                                         <- zeros;
  bs$chamber.12Ci                                            <- zeros;
  bs$chamber.13Ci                                            <- zeros;
  bs$chamber.Totalpi                                         <- zeros;
  bs$chamber.12pi                                            <- zeros;
  bs$chamber.13pi                                            <- zeros;
  bs$chamber.Totalpi_pa                                      <- zeros;
  bs$chamber.Delta.i.simple.for.gm                           <- zeros;
  bs$chamber.Delta.i.simple.for.modeling                     <- zeros;
  bs$chamber.Delta.i.complex.for.gm                          <- zeros;
  bs$chamber.Delta.i.simple.for.gm_Delta.obs                 <- zeros;
  bs$chamber.Delta.i.complex.for.gm_Delta.obs                <- zeros;
  bs$chamber.Totalgm.point.simple                            <- zeros;
  bs$chamber.12gm.point.simple                               <- zeros;
  bs$chamber.13gm.point.simple                               <- zeros;
  bs$chamber.Totalgm.point.complex                           <- zeros;
  bs$chamber.Totalgm.to.use                                  <- zeros;
  bs$chamber.Totalpc.using.gm                                <- zeros;
  bs$chamber.12pc.using.gm                                   <- zeros;
  bs$chamber.13pc.using.gm                                   <- zeros;
  bs$chamber.Totalpc.using.simple.Delta.for.gm               <- zeros;
  bs$chamber.Totalpc.using.simple.Delta.for.modeling         <- zeros;
  bs$chamber.Totalpc.using.complex.Delta.no.decarboxylation  <- zeros;
  bs$chamber.Totalpc.using.complex.Delta.full.model          <- zeros;
  bs$chamber.Totalpc.to.use                                  <- zeros;
  bs$chamber.TotalCc                                         <- zeros;
  bs$chamber.12Cc                                            <- zeros;
  bs$chamber.13Cc                                            <- zeros;

  return( bs );
  ### bs
}

