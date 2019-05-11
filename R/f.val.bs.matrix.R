f.val.bs.matrix <-
function# update values for each bs iterate
###
(bs
###
, val.calc.bs.temp
###
, i.bs
###
)
{

  ##details<<
  ## Move current value of \code{val.calc.bs.temp} into \code{bs$*} after checked with \code{\link{f.val.bs.matrix.check}}.

  bs$gain.12C                                              [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$gain.12C                                              );
  bs$gain.13C                                              [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$gain.13C                                              );
  bs$offset.12C                                            [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$offset.12C                                            );
  bs$offset.13C                                            [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$offset.13C                                            );
  bs$reference.12Ce                                        [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$reference.12Ce                                        );
  bs$reference.13Ce                                        [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$reference.13Ce                                        );
  bs$chamber.12Co                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12Co                                          );
  bs$chamber.13Co                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13Co                                          );
  bs$reference.TotalCe                                     [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$reference.TotalCe                                     );
  bs$chamber.TotalCo                                       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.TotalCo                                       );
  bs$chamber.reference.Total.diff.CeCo                     [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.reference.Total.diff.CeCo                     );
  bs$chamber.reference.12.diff.CeCo                        [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.reference.12.diff.CeCo                        );
  bs$chamber.reference.13.diff.CeCo                        [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.reference.13.diff.CeCo                        );
  bs$xi                                                    [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$xi                                                    );
  #bs$flow.adjusted                                         [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$flow.adjusted                                         ); # 9/5/2012
  bs$TDL.A.photosynthesis                                  [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$TDL.A.photosynthesis                                  );
  bs$TDL.12A.photosynthesis                                [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$TDL.12A.photosynthesis                                );
  bs$TDL.13A.photosynthesis                                [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$TDL.13A.photosynthesis                                );
  bs$Licor.A.photosynthesis                                [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$Licor.A.photosynthesis                                );
  bs$Delta.from.ratios.in.out                              [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$Delta.from.ratios.in.out                              );
  bs$Delta.from.A.ratio                                    [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$Delta.from.A.ratio                                    );
  bs$VPD                                                   [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$VPD                                                   );
  bs$E.transpiration                                       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$E.transpiration                                       );
  bs$leaf.temp                                             [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$leaf.temp                                             );
  bs$air.temp                                              [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$air.temp                                              );
  bs$light.in                                              [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$light.in                                              );
  bs$light.out                                             [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$light.out                                             );
  bs$reference.delta.e                                     [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$reference.delta.e                                     );
  bs$chamber.delta.o                                       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.delta.o                                       );
  bs$chamber.reference.delta.diff.CoCe                     [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.reference.delta.diff.CoCe                     );
  bs$Delta.obs                                             [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$Delta.obs                                             );
  bs$Delta.obs.permil                                      [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$Delta.obs.permil                                      );
  bs$delta.13C.Assim                                       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$delta.13C.Assim                                       );
  bs$p                                                     [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$p                                                     );
  bs$delta.13C.Resp                                        [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$delta.13C.Resp                                        );
  bs$chamber.TotalCa                                       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.TotalCa                                       );
  bs$chamber.12Ca                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12Ca                                          );
  bs$chamber.13Ca                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13Ca                                          );
  bs$chamber.TotalCs                                       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.TotalCs                                       );
  bs$chamber.12Cs                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12Cs                                          );
  bs$chamber.13Cs                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13Cs                                          );
  bs$chamber.Totalpa                                       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalpa                                       );
  bs$chamber.12pa                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12pa                                          );
  bs$chamber.13pa                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13pa                                          );
  bs$chamber.Totalps                                       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalps                                       );
  bs$chamber.12ps                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12ps                                          );
  bs$chamber.13ps                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13ps                                          );
  bs$chamber.Totalgbw                                      [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalgbw                                      );
  bs$chamber.Totalgbc                                      [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalgbc                                      );
  bs$chamber.12gbc                                         [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12gbc                                         );
  bs$chamber.13gbc                                         [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13gbc                                         );
  bs$chamber.Totalgsw                                      [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalgsw                                      );
  bs$chamber.Totalgsc                                      [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalgsc                                      );
  bs$chamber.12gsc                                         [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12gsc                                         );
  bs$chamber.13gsc                                         [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13gsc                                         );
  bs$chamber.Totalgtc                                      [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalgtc                                      );
  bs$chamber.12gtc                                         [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12gtc                                         );
  bs$chamber.13gtc                                         [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13gtc                                         );
  bs$chamber.TotalCi                                       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.TotalCi                                       );
  bs$chamber.12Ci                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12Ci                                          );
  bs$chamber.13Ci                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13Ci                                          );
  bs$chamber.Totalpi                                       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalpi                                       );
  bs$chamber.12pi                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12pi                                          );
  bs$chamber.13pi                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13pi                                          );
  bs$chamber.Totalpi_pa                                    [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalpi_pa                                    );
  bs$chamber.Delta.i.simple.for.gm                         [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Delta.i.simple.for.gm                         );
  bs$chamber.Delta.i.simple.for.modeling                   [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Delta.i.simple.for.modeling                   );
  bs$chamber.Delta.i.complex.for.gm                        [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Delta.i.complex.for.gm                        );
  bs$chamber.Delta.i.simple.for.gm_Delta.obs               [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Delta.i.simple.for.gm_Delta.obs               );
  bs$chamber.Delta.i.complex.for.gm_Delta.obs              [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Delta.i.complex.for.gm_Delta.obs              );
  bs$chamber.Totalgm.point.simple                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalgm.point.simple                          );
  bs$chamber.12gm.point.simple                             [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12gm.point.simple                             );
  bs$chamber.13gm.point.simple                             [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13gm.point.simple                             );
  bs$chamber.Totalgm.point.complex                         [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalgm.point.complex                         );
  bs$chamber.Totalgm.to.use                                [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalgm.to.use                                );
  bs$chamber.Totalpc.using.gm                              [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalpc.using.gm                              );
  bs$chamber.12pc.using.gm                                 [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12pc.using.gm                                 );
  bs$chamber.13pc.using.gm                                 [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13pc.using.gm                                 );
  bs$chamber.Totalpc.using.simple.Delta.for.gm             [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalpc.using.simple.Delta.for.gm             );
  bs$chamber.Totalpc.using.simple.Delta.for.modeling       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalpc.using.simple.Delta.for.modeling       );
  bs$chamber.Totalpc.using.complex.Delta.no.decarboxylation[,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalpc.using.complex.Delta.no.decarboxylation);
  bs$chamber.Totalpc.using.complex.Delta.full.model        [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalpc.using.complex.Delta.full.model        );
  bs$chamber.Totalpc.to.use                                [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.Totalpc.to.use                                );
  bs$chamber.TotalCc                                       [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.TotalCc                                       );
  bs$chamber.12Cc                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.12Cc                                          );
  bs$chamber.13Cc                                          [,i.bs]  <- f.val.bs.matrix.check(val.calc.bs.temp$chamber.13Cc                                          );

  return( bs );
  ### bs
}

