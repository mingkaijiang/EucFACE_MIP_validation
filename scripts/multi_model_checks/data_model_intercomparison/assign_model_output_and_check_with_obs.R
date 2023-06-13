assign_model_output_and_check_with_obs <- function (obsDF, outDF,
                                                    modDF.sum, treatment) {
    
    
    ###########################################################################
    ### GPP
    outDF$C_GDAYP[outDF$Variable=="GPP"] <- ifelse(modDF.sum$GPP.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                       (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$GPP.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                       (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="GPP"] <- ifelse(modDF.sum$GPP.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$GPP.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="GPP"] <- ifelse(modDF.sum$GPP.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$GPP.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="GPP"] <- ifelse(modDF.sum$GPP.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$GPP.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="GPP"] <- ifelse(modDF.sum$GPP.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$GPP.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="GPP"] <- ifelse(modDF.sum$GPP.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$GPP.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="GPP"] <- ifelse(modDF.sum$GPP.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$GPP.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$GPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    ### NPP
    outDF$C_GDAYP[outDF$Variable=="NPP"] <- ifelse(modDF.sum$NPP.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NPP.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="NPP"] <- ifelse(modDF.sum$NPP.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NPP.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="NPP"] <- ifelse(modDF.sum$NPP.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NPP.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="NPP"] <- ifelse(modDF.sum$NPP.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NPP.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="NPP"] <- ifelse(modDF.sum$NPP.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NPP.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="NPP"] <- ifelse(modDF.sum$NPP.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NPP.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="NPP"] <- ifelse(modDF.sum$NPP.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NPP.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="NPP"] <- ifelse(modDF.sum$NPP.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NPP.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="NPP"] <- ifelse(modDF.sum$NPP.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$NPP.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$NPP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### NEP
    outDF$C_GDAYP[outDF$Variable=="NEP"] <- ifelse(modDF.sum$NEP.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NEP.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="NEP"] <- ifelse(modDF.sum$NEP.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NEP.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="NEP"] <- ifelse(modDF.sum$NEP.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NEP.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="NEP"] <- ifelse(modDF.sum$NEP.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NEP.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="NEP"] <- ifelse(modDF.sum$NEP.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NEP.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="NEP"] <- ifelse(modDF.sum$NEP.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NEP.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="NEP"] <- ifelse(modDF.sum$NEP.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NEP.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="NEP"] <- ifelse(modDF.sum$NEP.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$NEP.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$I_MM[outDF$Variable=="NEP"] <- ifelse(modDF.sum$NEP.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$NEP.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$NEP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### RHET
    outDF$C_GDAYP[outDF$Variable=="RHET"] <- ifelse(modDF.sum$RHET.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$RHET.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="RHET"] <- ifelse(modDF.sum$RHET.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$RHET.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$B_CABLP[outDF$Variable=="RHET"] <- ifelse(modDF.sum$RHET.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$RHET.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="RHET"] <- ifelse(modDF.sum$RHET.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$RHET.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="RHET"] <- ifelse(modDF.sum$RHET.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$RHET.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$F_QUINC[outDF$Variable=="RHET"] <- ifelse(modDF.sum$RHET.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$RHET.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="RHET"] <- ifelse(modDF.sum$RHET.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$RHET.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="RHET"] <- ifelse(modDF.sum$RHET.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$RHET.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                          (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$I_MM[outDF$Variable=="RHET"] <- ifelse(modDF.sum$RHET.mean[modDF.sum$ModName=="I_MM"]<=
                                                        (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$RHET.mean[modDF.sum$ModName=="I_MM"]>=
                                                        (obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$RHET[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### BP
    outDF$C_GDAYP[outDF$Variable=="BP"] <- ifelse(modDF.sum$BP.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$BP.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="BP"] <- ifelse(modDF.sum$BP.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$BP.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$B_CABLP[outDF$Variable=="BP"] <- ifelse(modDF.sum$BP.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$BP.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="BP"] <- ifelse(modDF.sum$BP.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$BP.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="BP"] <- ifelse(modDF.sum$BP.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$BP.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$F_QUINC[outDF$Variable=="BP"] <- ifelse(modDF.sum$BP.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$BP.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="BP"] <- ifelse(modDF.sum$BP.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$BP.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="BP"] <- ifelse(modDF.sum$BP.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$BP.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                        (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$I_MM[outDF$Variable=="BP"] <- ifelse(modDF.sum$BP.mean[modDF.sum$ModName=="I_MM"]<=
                                                      (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$BP.mean[modDF.sum$ModName=="I_MM"]>=
                                                      (obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$BP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    
    ### deltaCL
    outDF$C_GDAYP[outDF$Variable=="deltaCL"] <- ifelse(modDF.sum$deltaCL.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCL.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCL"] <- ifelse(modDF.sum$deltaCL.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCL.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCL"] <- ifelse(modDF.sum$deltaCL.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCL.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCL"] <- ifelse(modDF.sum$deltaCL.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCL.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCL"] <- ifelse(modDF.sum$deltaCL.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCL.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCL"] <- ifelse(modDF.sum$deltaCL.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCL.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCL"] <- ifelse(modDF.sum$deltaCL.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCL.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCL"] <- ifelse(modDF.sum$deltaCL.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCL.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                             (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    
    outDF$I_MM[outDF$Variable=="deltaCL"] <- ifelse(modDF.sum$deltaCL.mean[modDF.sum$ModName=="I_MM"]<=
                                                           (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$deltaCL.mean[modDF.sum$ModName=="I_MM"]>=
                                                           (obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$deltaCL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    ### deltaCW
    outDF$C_GDAYP[outDF$Variable=="deltaCW"] <- ifelse(modDF.sum$deltaCW.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCW.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCW"] <- ifelse(modDF.sum$deltaCW.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCW.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCW"] <- ifelse(modDF.sum$deltaCW.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCW.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCW"] <- ifelse(modDF.sum$deltaCW.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCW.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCW"] <- ifelse(modDF.sum$deltaCW.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCW.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCW"] <- ifelse(modDF.sum$deltaCW.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCW.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCW"] <- ifelse(modDF.sum$deltaCW.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCW.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCW"] <- ifelse(modDF.sum$deltaCW.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaCW.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                             (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaCW"] <- ifelse(modDF.sum$deltaCW.mean[modDF.sum$ModName=="I_MM"]<=
                                                           (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$deltaCW.mean[modDF.sum$ModName=="I_MM"]>=
                                                           (obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    ### deltaCFR
    outDF$C_GDAYP[outDF$Variable=="deltaCFR"] <- ifelse(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCFR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCFR"] <- ifelse(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCFR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCFR"] <- ifelse(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCFR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCFR"] <- ifelse(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCFR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCFR"] <- ifelse(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCFR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCFR"] <- ifelse(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCFR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCFR"] <- ifelse(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCFR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCFR"] <- ifelse(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCFR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                              (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaCFR"] <- ifelse(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="I_MM"]<=
                                                            (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$deltaCFR.mean[modDF.sum$ModName=="I_MM"]>=
                                                            (obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    ### deltaCCR
    outDF$C_GDAYP[outDF$Variable=="deltaCCR"] <- ifelse(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCCR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCCR"] <- ifelse(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCCR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCCR"] <- ifelse(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCCR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCCR"] <- ifelse(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCCR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCCR"] <- ifelse(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCCR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCCR"] <- ifelse(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCCR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCCR"] <- ifelse(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCCR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCCR"] <- ifelse(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                              modDF.sum$deltaCCR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                              (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                   obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                          1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaCCR"] <- ifelse(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="I_MM"]<=
                                                            (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$deltaCCR.mean[modDF.sum$ModName=="I_MM"]>=
                                                            (obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    ### deltaCFLITA
    outDF$C_GDAYP[outDF$Variable=="deltaCFLITA"] <- ifelse(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                                 modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                             1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCFLITA"] <- ifelse(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                                 modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                             1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCFLITA"] <- ifelse(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                                 modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                             1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCFLITA"] <- ifelse(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                                 modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                             1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCFLITA"] <- ifelse(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                                 modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                             1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCFLITA"] <- ifelse(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                                 modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                             1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCFLITA"] <- ifelse(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                                 modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                             1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCFLITA"] <- ifelse(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                                 modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                                 (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                      obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                             1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaCFLITA"] <- ifelse(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="I_MM"]<=
                                                               (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                    obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                               modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="I_MM"]>=
                                                               (obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                    obsDF$deltaCFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    ### deltaCMIC
    #outDF$C_GDAYP[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="C_GDAYP"]<=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                           modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="C_GDAYP"]>=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                       1, 0)
    #
    #outDF$A_ELMV1[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="A_ELMV1"]<=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                           modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="A_ELMV1"]>=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                       1, 0)
    #
    #outDF$B_CABLP[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="B_CABLP"]<=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                           modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="B_CABLP"]>=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                       1, 0)
    #
    #
    #outDF$D_LPJGP[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="D_LPJGP"]<=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                           modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="D_LPJGP"]>=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                       1, 0)
    #
    #outDF$E_OCHDP[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="E_OCHDP"]<=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                           modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="E_OCHDP"]>=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                       1, 0)
    #
    #outDF$F_QUINC[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="F_QUINC"]<=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                           modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="F_QUINC"]>=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                       1, 0)
    #
    #outDF$G_OCHDX[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="G_OCHDX"]<=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                           modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="G_OCHDX"]>=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                       1, 0)
    #
    #outDF$H_QUJSM[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="H_QUJSM"]<=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                           modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="H_QUJSM"]>=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                       1, 0)
    
    ### CGL
    outDF$C_GDAYP[outDF$Variable=="CGL"] <- ifelse(modDF.sum$CGL.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGL.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CGL"] <- ifelse(modDF.sum$CGL.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGL.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CGL"] <- ifelse(modDF.sum$CGL.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGL.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CGL"] <- ifelse(modDF.sum$CGL.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGL.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CGL"] <- ifelse(modDF.sum$CGL.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGL.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CGL"] <- ifelse(modDF.sum$CGL.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGL.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CGL"] <- ifelse(modDF.sum$CGL.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGL.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CGL"] <- ifelse(modDF.sum$CGL.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGL.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="CGL"] <- ifelse(modDF.sum$CGL.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$CGL.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$CGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CGW
    outDF$C_GDAYP[outDF$Variable=="CGW"] <- ifelse(modDF.sum$CGW.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGW.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CGW"] <- ifelse(modDF.sum$CGW.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGW.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CGW"] <- ifelse(modDF.sum$CGW.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGW.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CGW"] <- ifelse(modDF.sum$CGW.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGW.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CGW"] <- ifelse(modDF.sum$CGW.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGW.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CGW"] <- ifelse(modDF.sum$CGW.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGW.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CGW"] <- ifelse(modDF.sum$CGW.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGW.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CGW"] <- ifelse(modDF.sum$CGW.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CGW.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="CGW"] <- ifelse(modDF.sum$CGW.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$CGW.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$CGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CGFR
    outDF$C_GDAYP[outDF$Variable=="CGFR"] <- ifelse(modDF.sum$CGFR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGFR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CGFR"] <- ifelse(modDF.sum$CGFR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGFR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CGFR"] <- ifelse(modDF.sum$CGFR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGFR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CGFR"] <- ifelse(modDF.sum$CGFR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGFR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CGFR"] <- ifelse(modDF.sum$CGFR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGFR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CGFR"] <- ifelse(modDF.sum$CGFR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGFR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CGFR"] <- ifelse(modDF.sum$CGFR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGFR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CGFR"] <- ifelse(modDF.sum$CGFR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGFR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                          (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$I_MM[outDF$Variable=="CGFR"] <- ifelse(modDF.sum$CGFR.mean[modDF.sum$ModName=="I_MM"]<=
                                                        (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CGFR.mean[modDF.sum$ModName=="I_MM"]>=
                                                        (obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### CGCR
    outDF$C_GDAYP[outDF$Variable=="CGCR"] <- ifelse(modDF.sum$CGCR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGCR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CGCR"] <- ifelse(modDF.sum$CGCR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGCR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CGCR"] <- ifelse(modDF.sum$CGCR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGCR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CGCR"] <- ifelse(modDF.sum$CGCR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGCR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CGCR"] <- ifelse(modDF.sum$CGCR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGCR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CGCR"] <- ifelse(modDF.sum$CGCR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGCR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CGCR"] <- ifelse(modDF.sum$CGCR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGCR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CGCR"] <- ifelse(modDF.sum$CGCR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CGCR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                          (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$I_MM[outDF$Variable=="CGCR"] <- ifelse(modDF.sum$CGCR.mean[modDF.sum$ModName=="I_MM"]<=
                                                        (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CGCR.mean[modDF.sum$ModName=="I_MM"]>=
                                                        (obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### LAI
    outDF$C_GDAYP[outDF$Variable=="LAI"] <- ifelse(modDF.sum$LAI.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$LAI.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="LAI"] <- ifelse(modDF.sum$LAI.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$LAI.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="LAI"] <- ifelse(modDF.sum$LAI.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$LAI.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="LAI"] <- ifelse(modDF.sum$LAI.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$LAI.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="LAI"] <- ifelse(modDF.sum$LAI.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$LAI.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="LAI"] <- ifelse(modDF.sum$LAI.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$LAI.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="LAI"] <- ifelse(modDF.sum$LAI.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$LAI.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="LAI"] <- ifelse(modDF.sum$LAI.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$LAI.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$I_MM[outDF$Variable=="LAI"] <- ifelse(modDF.sum$LAI.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$LAI.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$LAI[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    ### CL
    outDF$C_GDAYP[outDF$Variable=="CL"] <- ifelse(modDF.sum$CL.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CL.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CL"] <- ifelse(modDF.sum$CL.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CL.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CL"] <- ifelse(modDF.sum$CL.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CL.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CL"] <- ifelse(modDF.sum$CL.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CL.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CL"] <- ifelse(modDF.sum$CL.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CL.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CL"] <- ifelse(modDF.sum$CL.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CL.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CL"] <- ifelse(modDF.sum$CL.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CL.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CL"] <- ifelse(modDF.sum$CL.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CL.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                        (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$I_MM[outDF$Variable=="CL"] <- ifelse(modDF.sum$CL.mean[modDF.sum$ModName=="I_MM"]<=
                                                      (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$CL.mean[modDF.sum$ModName=="I_MM"]>=
                                                      (obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$CL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    ### CW
    outDF$C_GDAYP[outDF$Variable=="CW"] <- ifelse(modDF.sum$CW.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CW.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CW"] <- ifelse(modDF.sum$CW.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CW.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CW"] <- ifelse(modDF.sum$CW.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CW.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CW"] <- ifelse(modDF.sum$CW.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CW.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CW"] <- ifelse(modDF.sum$CW.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CW.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CW"] <- ifelse(modDF.sum$CW.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CW.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CW"] <- ifelse(modDF.sum$CW.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CW.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CW"] <- ifelse(modDF.sum$CW.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CW.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                        (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$I_MM[outDF$Variable=="CW"] <- ifelse(modDF.sum$CW.mean[modDF.sum$ModName=="I_MM"]<=
                                                      (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$CW.mean[modDF.sum$ModName=="I_MM"]>=
                                                      (obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$CW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    ### CFR
    outDF$C_GDAYP[outDF$Variable=="CFR"] <- ifelse(modDF.sum$CFR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CFR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CFR"] <- ifelse(modDF.sum$CFR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CFR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CFR"] <- ifelse(modDF.sum$CFR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CFR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CFR"] <- ifelse(modDF.sum$CFR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CFR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CFR"] <- ifelse(modDF.sum$CFR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CFR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CFR"] <- ifelse(modDF.sum$CFR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CFR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CFR"] <- ifelse(modDF.sum$CFR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CFR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CFR"] <- ifelse(modDF.sum$CFR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CFR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="CFR"] <- ifelse(modDF.sum$CFR.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$CFR.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$CFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CCR
    outDF$C_GDAYP[outDF$Variable=="CCR"] <- ifelse(modDF.sum$CCR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CCR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CCR"] <- ifelse(modDF.sum$CCR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CCR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CCR"] <- ifelse(modDF.sum$CCR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CCR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CCR"] <- ifelse(modDF.sum$CCR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CCR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CCR"] <- ifelse(modDF.sum$CCR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CCR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CCR"] <- ifelse(modDF.sum$CCR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CCR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CCR"] <- ifelse(modDF.sum$CCR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CCR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CCR"] <- ifelse(modDF.sum$CCR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CCR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="CCR"] <- ifelse(modDF.sum$CCR.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$CCR.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$CCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CFLITA
    outDF$C_GDAYP[outDF$Variable=="CFLITA"] <- ifelse(modDF.sum$CFLITA.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CFLITA.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CFLITA"] <- ifelse(modDF.sum$CFLITA.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CFLITA.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CFLITA"] <- ifelse(modDF.sum$CFLITA.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CFLITA.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CFLITA"] <- ifelse(modDF.sum$CFLITA.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CFLITA.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CFLITA"] <- ifelse(modDF.sum$CFLITA.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CFLITA.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CFLITA"] <- ifelse(modDF.sum$CFLITA.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CFLITA.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CFLITA"] <- ifelse(modDF.sum$CFLITA.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CFLITA.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CFLITA"] <- ifelse(modDF.sum$CFLITA.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CFLITA.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                            (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$I_MM[outDF$Variable=="CFLITA"] <- ifelse(modDF.sum$CFLITA.mean[modDF.sum$ModName=="I_MM"]<=
                                                          (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CFLITA.mean[modDF.sum$ModName=="I_MM"]>=
                                                          (obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    ### CMIC
    #outDF$C_GDAYP[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="C_GDAYP"]<=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                      modDF.sum$CMIC.mean[modDF.sum$ModName=="C_GDAYP"]>=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                  1, 0)
    #
    #outDF$A_ELMV1[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="A_ELMV1"]<=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                      modDF.sum$CMIC.mean[modDF.sum$ModName=="A_ELMV1"]>=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                  1, 0)
    #
    #outDF$B_CABLP[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="B_CABLP"]<=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                      modDF.sum$CMIC.mean[modDF.sum$ModName=="B_CABLP"]>=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                  1, 0)
    #
    #
    #outDF$D_LPJGP[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="D_LPJGP"]<=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                      modDF.sum$CMIC.mean[modDF.sum$ModName=="D_LPJGP"]>=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                  1, 0)
    #
    #outDF$E_OCHDP[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="E_OCHDP"]<=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                      modDF.sum$CMIC.mean[modDF.sum$ModName=="E_OCHDP"]>=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                  1, 0)
    #
    #outDF$F_QUINC[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="F_QUINC"]<=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                      modDF.sum$CMIC.mean[modDF.sum$ModName=="F_QUINC"]>=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                  1, 0)
    #
    #outDF$G_OCHDX[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="G_OCHDX"]<=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                      modDF.sum$CMIC.mean[modDF.sum$ModName=="G_OCHDX"]>=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                  1, 0)
    #
    #outDF$H_QUJSM[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="H_QUJSM"]<=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                      modDF.sum$CMIC.mean[modDF.sum$ModName=="H_QUJSM"]>=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                  1, 0)
    
    ### CSOIL
    outDF$C_GDAYP[outDF$Variable=="CSOIL"] <- ifelse(modDF.sum$CSOIL.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$CSOIL.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CSOIL"] <- ifelse(modDF.sum$CSOIL.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$CSOIL.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CSOIL"] <- ifelse(modDF.sum$CSOIL.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$CSOIL.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CSOIL"] <- ifelse(modDF.sum$CSOIL.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$CSOIL.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CSOIL"] <- ifelse(modDF.sum$CSOIL.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$CSOIL.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CSOIL"] <- ifelse(modDF.sum$CSOIL.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$CSOIL.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CSOIL"] <- ifelse(modDF.sum$CSOIL.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$CSOIL.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CSOIL"] <- ifelse(modDF.sum$CSOIL.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$CSOIL.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                           (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$I_MM[outDF$Variable=="CSOIL"] <- ifelse(modDF.sum$CSOIL.mean[modDF.sum$ModName=="I_MM"]<=
                                                         (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CSOIL.mean[modDF.sum$ModName=="I_MM"]>=
                                                         (obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    ### PL
    outDF$C_GDAYP[outDF$Variable=="PL"] <- ifelse(modDF.sum$PL.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PL.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PL"] <- ifelse(modDF.sum$PL.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PL.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PL"] <- ifelse(modDF.sum$PL.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PL.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PL"] <- ifelse(modDF.sum$PL.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PL.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PL"] <- ifelse(modDF.sum$PL.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PL.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PL"] <- ifelse(modDF.sum$PL.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PL.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PL"] <- ifelse(modDF.sum$PL.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PL.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PL"] <- ifelse(modDF.sum$PL.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PL.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                        (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$I_MM[outDF$Variable=="PL"] <- ifelse(modDF.sum$PL.mean[modDF.sum$ModName=="I_MM"]<=
                                                      (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$PL.mean[modDF.sum$ModName=="I_MM"]>=
                                                      (obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$PL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    ### PW
    outDF$C_GDAYP[outDF$Variable=="PW"] <- ifelse(modDF.sum$PW.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PW.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PW"] <- ifelse(modDF.sum$PW.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PW.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PW"] <- ifelse(modDF.sum$PW.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PW.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PW"] <- ifelse(modDF.sum$PW.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PW.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PW"] <- ifelse(modDF.sum$PW.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PW.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PW"] <- ifelse(modDF.sum$PW.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PW.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PW"] <- ifelse(modDF.sum$PW.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PW.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PW"] <- ifelse(modDF.sum$PW.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PW.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                        (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$I_MM[outDF$Variable=="PW"] <- ifelse(modDF.sum$PW.mean[modDF.sum$ModName=="I_MM"]<=
                                                      (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$PW.mean[modDF.sum$ModName=="I_MM"]>=
                                                      (obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$PW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    ### PFR
    outDF$C_GDAYP[outDF$Variable=="PFR"] <- ifelse(modDF.sum$PFR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PFR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PFR"] <- ifelse(modDF.sum$PFR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PFR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PFR"] <- ifelse(modDF.sum$PFR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PFR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PFR"] <- ifelse(modDF.sum$PFR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PFR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PFR"] <- ifelse(modDF.sum$PFR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PFR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PFR"] <- ifelse(modDF.sum$PFR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PFR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PFR"] <- ifelse(modDF.sum$PFR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PFR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PFR"] <- ifelse(modDF.sum$PFR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PFR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="PFR"] <- ifelse(modDF.sum$PFR.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$PFR.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$PFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PCR
    outDF$C_GDAYP[outDF$Variable=="PCR"] <- ifelse(modDF.sum$PCR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PCR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PCR"] <- ifelse(modDF.sum$PCR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PCR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PCR"] <- ifelse(modDF.sum$PCR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PCR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PCR"] <- ifelse(modDF.sum$PCR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PCR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PCR"] <- ifelse(modDF.sum$PCR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PCR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PCR"] <- ifelse(modDF.sum$PCR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PCR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PCR"] <- ifelse(modDF.sum$PCR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PCR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PCR"] <- ifelse(modDF.sum$PCR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PCR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="PCR"] <- ifelse(modDF.sum$PCR.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$PCR.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$PCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PFLITA
    outDF$C_GDAYP[outDF$Variable=="PFLITA"] <- ifelse(modDF.sum$PFLITA.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFLITA.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PFLITA"] <- ifelse(modDF.sum$PFLITA.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFLITA.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PFLITA"] <- ifelse(modDF.sum$PFLITA.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFLITA.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PFLITA"] <- ifelse(modDF.sum$PFLITA.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFLITA.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PFLITA"] <- ifelse(modDF.sum$PFLITA.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFLITA.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PFLITA"] <- ifelse(modDF.sum$PFLITA.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFLITA.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PFLITA"] <- ifelse(modDF.sum$PFLITA.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFLITA.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PFLITA"] <- ifelse(modDF.sum$PFLITA.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFLITA.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                            (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$I_MM[outDF$Variable=="PFLITA"] <- ifelse(modDF.sum$PFLITA.mean[modDF.sum$ModName=="I_MM"]<=
                                                          (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PFLITA.mean[modDF.sum$ModName=="I_MM"]>=
                                                          (obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PFLITA[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    ### PSOIL
    outDF$C_GDAYP[outDF$Variable=="PSOIL"] <- ifelse(modDF.sum$PSOIL.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PSOIL.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PSOIL"] <- ifelse(modDF.sum$PSOIL.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PSOIL.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PSOIL"] <- ifelse(modDF.sum$PSOIL.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PSOIL.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PSOIL"] <- ifelse(modDF.sum$PSOIL.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PSOIL.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PSOIL"] <- ifelse(modDF.sum$PSOIL.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PSOIL.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PSOIL"] <- ifelse(modDF.sum$PSOIL.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PSOIL.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PSOIL"] <- ifelse(modDF.sum$PSOIL.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PSOIL.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PSOIL"] <- ifelse(modDF.sum$PSOIL.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PSOIL.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                           (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$I_MM[outDF$Variable=="PSOIL"] <- ifelse(modDF.sum$PSOIL.mean[modDF.sum$ModName=="I_MM"]<=
                                                         (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PSOIL.mean[modDF.sum$ModName=="I_MM"]>=
                                                         (obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    ### PPORG
    outDF$C_GDAYP[outDF$Variable=="PPORG"] <- ifelse(modDF.sum$PPORG.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPORG.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PPORG"] <- ifelse(modDF.sum$PPORG.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPORG.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PPORG"] <- ifelse(modDF.sum$PPORG.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPORG.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PPORG"] <- ifelse(modDF.sum$PPORG.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPORG.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PPORG"] <- ifelse(modDF.sum$PPORG.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPORG.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PPORG"] <- ifelse(modDF.sum$PPORG.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPORG.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PPORG"] <- ifelse(modDF.sum$PPORG.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPORG.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PPORG"] <- ifelse(modDF.sum$PPORG.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPORG.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                           (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$I_MM[outDF$Variable=="PPORG"] <- ifelse(modDF.sum$PPORG.mean[modDF.sum$ModName=="I_MM"]<=
                                                         (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PPORG.mean[modDF.sum$ModName=="I_MM"]>=
                                                         (obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PPORG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    ### PPMIN
    outDF$C_GDAYP[outDF$Variable=="PPMIN"] <- ifelse(modDF.sum$PPMIN.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPMIN.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PPMIN"] <- ifelse(modDF.sum$PPMIN.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPMIN.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PPMIN"] <- ifelse(modDF.sum$PPMIN.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPMIN.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PPMIN"] <- ifelse(modDF.sum$PPMIN.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPMIN.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PPMIN"] <- ifelse(modDF.sum$PPMIN.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPMIN.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PPMIN"] <- ifelse(modDF.sum$PPMIN.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPMIN.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PPMIN"] <- ifelse(modDF.sum$PPMIN.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPMIN.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PPMIN"] <- ifelse(modDF.sum$PPMIN.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PPMIN.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                           (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$I_MM[outDF$Variable=="PPMIN"] <- ifelse(modDF.sum$PPMIN.mean[modDF.sum$ModName=="I_MM"]<=
                                                         (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PPMIN.mean[modDF.sum$ModName=="I_MM"]>=
                                                         (obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PPMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    ### PDEM
    outDF$C_GDAYP[outDF$Variable=="PDEM"] <- ifelse(modDF.sum$PDEM.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PDEM.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PDEM"] <- ifelse(modDF.sum$PDEM.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PDEM.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PDEM"] <- ifelse(modDF.sum$PDEM.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PDEM.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PDEM"] <- ifelse(modDF.sum$PDEM.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PDEM.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PDEM"] <- ifelse(modDF.sum$PDEM.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PDEM.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PDEM"] <- ifelse(modDF.sum$PDEM.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PDEM.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PDEM"] <- ifelse(modDF.sum$PDEM.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PDEM.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PDEM"] <- ifelse(modDF.sum$PDEM.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PDEM.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                          (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$I_MM[outDF$Variable=="PDEM"] <- ifelse(modDF.sum$PDEM.mean[modDF.sum$ModName=="I_MM"]<=
                                                        (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PDEM.mean[modDF.sum$ModName=="I_MM"]>=
                                                        (obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PDEM[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### PGL
    outDF$C_GDAYP[outDF$Variable=="PGL"] <- ifelse(modDF.sum$PGL.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGL.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PGL"] <- ifelse(modDF.sum$PGL.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGL.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PGL"] <- ifelse(modDF.sum$PGL.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGL.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PGL"] <- ifelse(modDF.sum$PGL.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGL.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PGL"] <- ifelse(modDF.sum$PGL.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGL.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PGL"] <- ifelse(modDF.sum$PGL.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGL.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PGL"] <- ifelse(modDF.sum$PGL.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGL.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PGL"] <- ifelse(modDF.sum$PGL.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGL.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="PGL"] <- ifelse(modDF.sum$PGL.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$PGL.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$PGL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PGW
    outDF$C_GDAYP[outDF$Variable=="PGW"] <- ifelse(modDF.sum$PGW.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGW.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PGW"] <- ifelse(modDF.sum$PGW.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGW.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PGW"] <- ifelse(modDF.sum$PGW.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGW.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PGW"] <- ifelse(modDF.sum$PGW.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGW.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PGW"] <- ifelse(modDF.sum$PGW.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGW.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PGW"] <- ifelse(modDF.sum$PGW.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGW.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PGW"] <- ifelse(modDF.sum$PGW.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGW.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PGW"] <- ifelse(modDF.sum$PGW.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PGW.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="PGW"] <- ifelse(modDF.sum$PGW.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$PGW.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$PGW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PGFR
    outDF$C_GDAYP[outDF$Variable=="PGFR"] <- ifelse(modDF.sum$PGFR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGFR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PGFR"] <- ifelse(modDF.sum$PGFR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGFR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PGFR"] <- ifelse(modDF.sum$PGFR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGFR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PGFR"] <- ifelse(modDF.sum$PGFR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGFR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PGFR"] <- ifelse(modDF.sum$PGFR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGFR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PGFR"] <- ifelse(modDF.sum$PGFR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGFR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PGFR"] <- ifelse(modDF.sum$PGFR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGFR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PGFR"] <- ifelse(modDF.sum$PGFR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGFR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                          (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$I_MM[outDF$Variable=="PGFR"] <- ifelse(modDF.sum$PGFR.mean[modDF.sum$ModName=="I_MM"]<=
                                                        (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PGFR.mean[modDF.sum$ModName=="I_MM"]>=
                                                        (obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PGFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### PGCR
    outDF$C_GDAYP[outDF$Variable=="PGCR"] <- ifelse(modDF.sum$PGCR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGCR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PGCR"] <- ifelse(modDF.sum$PGCR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGCR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PGCR"] <- ifelse(modDF.sum$PGCR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGCR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PGCR"] <- ifelse(modDF.sum$PGCR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGCR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PGCR"] <- ifelse(modDF.sum$PGCR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGCR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PGCR"] <- ifelse(modDF.sum$PGCR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGCR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PGCR"] <- ifelse(modDF.sum$PGCR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGCR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PGCR"] <- ifelse(modDF.sum$PGCR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PGCR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                          (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$I_MM[outDF$Variable=="PGCR"] <- ifelse(modDF.sum$PGCR.mean[modDF.sum$ModName=="I_MM"]<=
                                                        (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PGCR.mean[modDF.sum$ModName=="I_MM"]>=
                                                        (obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PGCR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### PLITIN
    outDF$C_GDAYP[outDF$Variable=="PLITIN"] <- ifelse(modDF.sum$PLITIN.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLITIN.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PLITIN"] <- ifelse(modDF.sum$PLITIN.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLITIN.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PLITIN"] <- ifelse(modDF.sum$PLITIN.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLITIN.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PLITIN"] <- ifelse(modDF.sum$PLITIN.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLITIN.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PLITIN"] <- ifelse(modDF.sum$PLITIN.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLITIN.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PLITIN"] <- ifelse(modDF.sum$PLITIN.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLITIN.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PLITIN"] <- ifelse(modDF.sum$PLITIN.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLITIN.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PLITIN"] <- ifelse(modDF.sum$PLITIN.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLITIN.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                            (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$I_MM[outDF$Variable=="PLITIN"] <- ifelse(modDF.sum$PLITIN.mean[modDF.sum$ModName=="I_MM"]<=
                                                          (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PLITIN.mean[modDF.sum$ModName=="I_MM"]>=
                                                          (obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PLITIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    ### PWLIN
    outDF$C_GDAYP[outDF$Variable=="PWLIN"] <- ifelse(modDF.sum$PWLIN.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PWLIN.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PWLIN"] <- ifelse(modDF.sum$PWLIN.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PWLIN.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PWLIN"] <- ifelse(modDF.sum$PWLIN.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PWLIN.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PWLIN"] <- ifelse(modDF.sum$PWLIN.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PWLIN.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PWLIN"] <- ifelse(modDF.sum$PWLIN.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PWLIN.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PWLIN"] <- ifelse(modDF.sum$PWLIN.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PWLIN.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PWLIN"] <- ifelse(modDF.sum$PWLIN.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PWLIN.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PWLIN"] <- ifelse(modDF.sum$PWLIN.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PWLIN.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                           (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    outDF$I_MM[outDF$Variable=="PWLIN"] <- ifelse(modDF.sum$PWLIN.mean[modDF.sum$ModName=="I_MM"]<=
                                                         (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PWLIN.mean[modDF.sum$ModName=="I_MM"]>=
                                                         (obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PWLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    ### PFRLIN
    outDF$C_GDAYP[outDF$Variable=="PFRLIN"] <- ifelse(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFRLIN.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PFRLIN"] <- ifelse(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFRLIN.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PFRLIN"] <- ifelse(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFRLIN.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PFRLIN"] <- ifelse(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFRLIN.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PFRLIN"] <- ifelse(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFRLIN.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PFRLIN"] <- ifelse(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFRLIN.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PFRLIN"] <- ifelse(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFRLIN.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PFRLIN"] <- ifelse(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PFRLIN.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                            (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$I_MM[outDF$Variable=="PFRLIN"] <- ifelse(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="I_MM"]<=
                                                          (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PFRLIN.mean[modDF.sum$ModName=="I_MM"]>=
                                                          (obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PFRLIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    ### PLAB
    outDF$C_GDAYP[outDF$Variable=="PLAB"] <- ifelse(modDF.sum$PLAB.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PLAB.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PLAB"] <- ifelse(modDF.sum$PLAB.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PLAB.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PLAB"] <- ifelse(modDF.sum$PLAB.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PLAB.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PLAB"] <- ifelse(modDF.sum$PLAB.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PLAB.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PLAB"] <- ifelse(modDF.sum$PLAB.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PLAB.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PLAB"] <- ifelse(modDF.sum$PLAB.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PLAB.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PLAB"] <- ifelse(modDF.sum$PLAB.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PLAB.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PLAB"] <- ifelse(modDF.sum$PLAB.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PLAB.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                          (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$I_MM[outDF$Variable=="PLAB"] <- ifelse(modDF.sum$PLAB.mean[modDF.sum$ModName=="I_MM"]<=
                                                        (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PLAB.mean[modDF.sum$ModName=="I_MM"]>=
                                                        (obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PLAB[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### PSEC
    outDF$C_GDAYP[outDF$Variable=="PLAB"] <- ifelse(modDF.sum$PSEC.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PSEC.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PSEC"] <- ifelse(modDF.sum$PSEC.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PSEC.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PSEC"] <- ifelse(modDF.sum$PSEC.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PSEC.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PSEC"] <- ifelse(modDF.sum$PSEC.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PSEC.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PSEC"] <- ifelse(modDF.sum$PSEC.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PSEC.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PSEC"] <- ifelse(modDF.sum$PSEC.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PSEC.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PSEC"] <- ifelse(modDF.sum$PSEC.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PSEC.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PSEC"] <- ifelse(modDF.sum$PSEC.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PSEC.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                          (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$I_MM[outDF$Variable=="PSEC"] <- ifelse(modDF.sum$PSEC.mean[modDF.sum$ModName=="I_MM"]<=
                                                        (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PSEC.mean[modDF.sum$ModName=="I_MM"]>=
                                                        (obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PSEC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### POCC
    outDF$C_GDAYP[outDF$Variable=="POCC"] <- ifelse(modDF.sum$POCC.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$POCC.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="POCC"] <- ifelse(modDF.sum$POCC.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$POCC.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$B_CABLP[outDF$Variable=="POCC"] <- ifelse(modDF.sum$POCC.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$POCC.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="POCC"] <- ifelse(modDF.sum$POCC.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$POCC.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="POCC"] <- ifelse(modDF.sum$POCC.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$POCC.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$F_QUINC[outDF$Variable=="POCC"] <- ifelse(modDF.sum$POCC.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$POCC.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="POCC"] <- ifelse(modDF.sum$POCC.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$POCC.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="POCC"] <- ifelse(modDF.sum$POCC.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$POCC.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                          (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$I_MM[outDF$Variable=="POCC"] <- ifelse(modDF.sum$POCC.mean[modDF.sum$ModName=="I_MM"]<=
                                                        (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$POCC.mean[modDF.sum$ModName=="I_MM"]>=
                                                        (obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$POCC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### PUP
    outDF$C_GDAYP[outDF$Variable=="PUP"] <- ifelse(modDF.sum$PUP.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUP.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PUP"] <- ifelse(modDF.sum$PUP.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUP.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PUP"] <- ifelse(modDF.sum$PUP.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUP.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PUP"] <- ifelse(modDF.sum$PUP.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUP.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PUP"] <- ifelse(modDF.sum$PUP.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUP.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PUP"] <- ifelse(modDF.sum$PUP.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUP.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PUP"] <- ifelse(modDF.sum$PUP.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUP.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PUP"] <- ifelse(modDF.sum$PUP.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUP.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$I_MM[outDF$Variable=="PUP"] <- ifelse(modDF.sum$PUP.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$PUP.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$PUP[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PRETR
    outDF$C_GDAYP[outDF$Variable=="PRETR"] <- ifelse(modDF.sum$PRETR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PRETR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PRETR"] <- ifelse(modDF.sum$PRETR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PRETR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PRETR"] <- ifelse(modDF.sum$PRETR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PRETR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PRETR"] <- ifelse(modDF.sum$PRETR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PRETR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PRETR"] <- ifelse(modDF.sum$PRETR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PRETR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PRETR"] <- ifelse(modDF.sum$PRETR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PRETR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PRETR"] <- ifelse(modDF.sum$PRETR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PRETR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PRETR"] <- ifelse(modDF.sum$PRETR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$PRETR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                           (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$I_MM[outDF$Variable=="PRETR"] <- ifelse(modDF.sum$PRETR.mean[modDF.sum$ModName=="I_MM"]<=
                                                         (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PRETR.mean[modDF.sum$ModName=="I_MM"]>=
                                                         (obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PRETR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    ### PMIN
    outDF$C_GDAYP[outDF$Variable=="PMIN"] <- ifelse(modDF.sum$PMIN.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PMIN.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PMIN"] <- ifelse(modDF.sum$PMIN.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PMIN.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PMIN"] <- ifelse(modDF.sum$PMIN.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PMIN.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PMIN"] <- ifelse(modDF.sum$PMIN.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PMIN.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PMIN"] <- ifelse(modDF.sum$PMIN.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PMIN.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PMIN"] <- ifelse(modDF.sum$PMIN.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PMIN.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PMIN"] <- ifelse(modDF.sum$PMIN.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PMIN.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PMIN"] <- ifelse(modDF.sum$PMIN.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PMIN.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                          (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$I_MM[outDF$Variable=="PMIN"] <- ifelse(modDF.sum$PMIN.mean[modDF.sum$ModName=="I_MM"]<=
                                                        (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PMIN.mean[modDF.sum$ModName=="I_MM"]>=
                                                        (obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PMIN[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### PLEACH
    outDF$C_GDAYP[outDF$Variable=="PLEACH"] <- ifelse(modDF.sum$PLEACH.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLEACH.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PLEACH"] <- ifelse(modDF.sum$PLEACH.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLEACH.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PLEACH"] <- ifelse(modDF.sum$PLEACH.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLEACH.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PLEACH"] <- ifelse(modDF.sum$PLEACH.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLEACH.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PLEACH"] <- ifelse(modDF.sum$PLEACH.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLEACH.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PLEACH"] <- ifelse(modDF.sum$PLEACH.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLEACH.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PLEACH"] <- ifelse(modDF.sum$PLEACH.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLEACH.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PLEACH"] <- ifelse(modDF.sum$PLEACH.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$PLEACH.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                            (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$I_MM[outDF$Variable=="PLEACH"] <- ifelse(modDF.sum$PLEACH.mean[modDF.sum$ModName=="I_MM"]<=
                                                          (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$PLEACH.mean[modDF.sum$ModName=="I_MM"]>=
                                                          (obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$PLEACH[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    ### PUE
    outDF$C_GDAYP[outDF$Variable=="PUE"] <- ifelse(modDF.sum$PUE.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUE.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PUE"] <- ifelse(modDF.sum$PUE.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUE.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PUE"] <- ifelse(modDF.sum$PUE.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUE.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PUE"] <- ifelse(modDF.sum$PUE.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUE.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PUE"] <- ifelse(modDF.sum$PUE.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUE.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PUE"] <- ifelse(modDF.sum$PUE.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUE.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PUE"] <- ifelse(modDF.sum$PUE.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUE.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PUE"] <- ifelse(modDF.sum$PUE.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$PUE.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="PUE"] <- ifelse(modDF.sum$PUE.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$PUE.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$PUE[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CPL
    outDF$C_GDAYP[outDF$Variable=="CPL"] <- ifelse(modDF.sum$CPL.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPL.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPL"] <- ifelse(modDF.sum$CPL.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPL.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPL"] <- ifelse(modDF.sum$CPL.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPL.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPL"] <- ifelse(modDF.sum$CPL.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPL.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPL"] <- ifelse(modDF.sum$CPL.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPL.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPL"] <- ifelse(modDF.sum$CPL.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPL.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPL"] <- ifelse(modDF.sum$CPL.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPL.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPL"] <- ifelse(modDF.sum$CPL.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPL.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="CPL"] <- ifelse(modDF.sum$CPL.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$CPL.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$CPL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CPW
    outDF$C_GDAYP[outDF$Variable=="CPW"] <- ifelse(modDF.sum$CPW.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPW.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPW"] <- ifelse(modDF.sum$CPW.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPW.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPW"] <- ifelse(modDF.sum$CPW.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPW.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPW"] <- ifelse(modDF.sum$CPW.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPW.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPW"] <- ifelse(modDF.sum$CPW.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPW.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPW"] <- ifelse(modDF.sum$CPW.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPW.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPW"] <- ifelse(modDF.sum$CPW.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPW.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPW"] <- ifelse(modDF.sum$CPW.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                         modDF.sum$CPW.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                         (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                              obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$I_MM[outDF$Variable=="CPW"] <- ifelse(modDF.sum$CPW.mean[modDF.sum$ModName=="I_MM"]<=
                                                       (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$CPW.mean[modDF.sum$ModName=="I_MM"]>=
                                                       (obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$CPW[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CPFR
    outDF$C_GDAYP[outDF$Variable=="CPFR"] <- ifelse(modDF.sum$CPFR.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CPFR.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPFR"] <- ifelse(modDF.sum$CPFR.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CPFR.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPFR"] <- ifelse(modDF.sum$CPFR.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CPFR.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPFR"] <- ifelse(modDF.sum$CPFR.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CPFR.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPFR"] <- ifelse(modDF.sum$CPFR.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CPFR.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPFR"] <- ifelse(modDF.sum$CPFR.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CPFR.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPFR"] <- ifelse(modDF.sum$CPFR.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CPFR.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPFR"] <- ifelse(modDF.sum$CPFR.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CPFR.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                          (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    outDF$I_MM[outDF$Variable=="CPFR"] <- ifelse(modDF.sum$CPFR.mean[modDF.sum$ModName=="I_MM"]<=
                                                        (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$CPFR.mean[modDF.sum$ModName=="I_MM"]>=
                                                        (obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$CPFR[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### CPSOIL
    outDF$C_GDAYP[outDF$Variable=="CPSOIL"] <- ifelse(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPSOIL.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPSOIL"] <- ifelse(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPSOIL.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPSOIL"] <- ifelse(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPSOIL.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPSOIL"] <- ifelse(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPSOIL.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPSOIL"] <- ifelse(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPSOIL.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPSOIL"] <- ifelse(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPSOIL.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPSOIL"] <- ifelse(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPSOIL.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPSOIL"] <- ifelse(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPSOIL.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                            (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$I_MM[outDF$Variable=="CPSOIL"] <- ifelse(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="I_MM"]<=
                                                          (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CPSOIL.mean[modDF.sum$ModName=="I_MM"]>=
                                                          (obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CPSOIL[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    ### CPFLIT
    outDF$C_GDAYP[outDF$Variable=="CPFLIT"] <- ifelse(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPFLIT.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPFLIT"] <- ifelse(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPFLIT.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPFLIT"] <- ifelse(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPFLIT.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPFLIT"] <- ifelse(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPFLIT.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPFLIT"] <- ifelse(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPFLIT.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPFLIT"] <- ifelse(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPFLIT.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPFLIT"] <- ifelse(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPFLIT.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPFLIT"] <- ifelse(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                            modDF.sum$CPFLIT.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                            (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                 obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    outDF$I_MM[outDF$Variable=="CPFLIT"] <- ifelse(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="I_MM"]<=
                                                          (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CPFLIT.mean[modDF.sum$ModName=="I_MM"]>=
                                                          (obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CPFLIT[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    ### GPP use
    outDF$C_GDAYP[outDF$Variable=="GPP_use"] <- ifelse(modDF.sum$GPP_use.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$GPP_use.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="GPP_use"] <- ifelse(modDF.sum$GPP_use.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$GPP_use.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="GPP_use"] <- ifelse(modDF.sum$GPP_use.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$GPP_use.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="GPP_use"] <- ifelse(modDF.sum$GPP_use.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$GPP_use.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="GPP_use"] <- ifelse(modDF.sum$GPP_use.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$GPP.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="GPP_use"] <- ifelse(modDF.sum$GPP_use.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$GPP_use.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="GPP_use"] <- ifelse(modDF.sum$GPP_use.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$GPP_use.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP_use"] <- ifelse(modDF.sum$GPP_use.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                       modDF.sum$GPP_use.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                       (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                            obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="GPP_use"] <- ifelse(modDF.sum$GPP_use.mean[modDF.sum$ModName=="I_MM"]<=
                                                           (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                           modDF.sum$GPP_use.mean[modDF.sum$ModName=="I_MM"]>=
                                                           (obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                obsDF$GPP_use[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    ### deltaPVEG
    outDF$C_GDAYP[outDF$Variable=="deltaPVEG"] <- ifelse(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaPVEG"] <- ifelse(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaPVEG"] <- ifelse(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaPVEG"] <- ifelse(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaPVEG"] <- ifelse(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaPVEG"] <- ifelse(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaPVEG"] <- ifelse(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaPVEG"] <- ifelse(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                      modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                      (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                           obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaPVEG"] <- ifelse(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="I_MM"]<=
                                                   (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                        obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                   modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="I_MM"]>=
                                                   (obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                        obsDF$deltaPVEG[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                               1, 0)
    
    
    
    
    ### CMIC
    outDF$C_GDAYP[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CMIC.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CMIC.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CMIC.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CMIC.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CMIC.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CMIC.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CMIC.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CMIC.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                             (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$I_MM[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="I_MM"]<=
                                                          (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CMIC.mean[modDF.sum$ModName=="I_MM"]>=
                                                          (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    
    
    ### PMIC
    outDF$C_GDAYP[outDF$Variable=="PMIC"] <- ifelse(modDF.sum$PMIC.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PMIC.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PMIC"] <- ifelse(modDF.sum$PMIC.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PMIC.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PMIC"] <- ifelse(modDF.sum$PMIC.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PMIC.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PMIC"] <- ifelse(modDF.sum$PMIC.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PMIC.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PMIC"] <- ifelse(modDF.sum$PMIC.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PMIC.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PMIC"] <- ifelse(modDF.sum$PMIC.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PMIC.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PMIC"] <- ifelse(modDF.sum$PMIC.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PMIC.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PMIC"] <- ifelse(modDF.sum$PMIC.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$PMIC.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                        (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$I_MM[outDF$Variable=="PMIC"] <- ifelse(modDF.sum$PMIC.mean[modDF.sum$ModName=="I_MM"]<=
                                                     (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                          obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                     modDF.sum$PMIC.mean[modDF.sum$ModName=="I_MM"]>=
                                                     (obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                          obsDF$PMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                 1, 0)
    
    
    
    ### deltadeltaCMIC
    outDF$C_GDAYP[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                        modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                        (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                             obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="I_MM"]<=
                                                     (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                          obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                     modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="I_MM"]>=
                                                     (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                          obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                 1, 0)
    
    
    
    ### deltaPMIC
    outDF$C_GDAYP[outDF$Variable=="deltaPMIC"] <- ifelse(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaPMIC"] <- ifelse(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaPMIC"] <- ifelse(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaPMIC"] <- ifelse(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaPMIC"] <- ifelse(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaPMIC"] <- ifelse(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaPMIC"] <- ifelse(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaPMIC"] <- ifelse(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                             (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaPMIC"] <- ifelse(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="I_MM"]<=
                                                          (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="I_MM"]>=
                                                          (obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$deltaPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    
    ### CPMIC
    outDF$C_GDAYP[outDF$Variable=="CPMIC"] <- ifelse(modDF.sum$CPMIC.mean[modDF.sum$ModName=="C_GDAYP"]<=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CPMIC.mean[modDF.sum$ModName=="C_GDAYP"]>=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPMIC"] <- ifelse(modDF.sum$CPMIC.mean[modDF.sum$ModName=="A_ELMV1"]<=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CPMIC.mean[modDF.sum$ModName=="A_ELMV1"]>=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPMIC"] <- ifelse(modDF.sum$CPMIC.mean[modDF.sum$ModName=="B_CABLP"]<=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CPMIC.mean[modDF.sum$ModName=="B_CABLP"]>=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPMIC"] <- ifelse(modDF.sum$CPMIC.mean[modDF.sum$ModName=="D_LPJGP"]<=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CPMIC.mean[modDF.sum$ModName=="D_LPJGP"]>=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPMIC"] <- ifelse(modDF.sum$CPMIC.mean[modDF.sum$ModName=="E_OCHDP"]<=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CPMIC.mean[modDF.sum$ModName=="E_OCHDP"]>=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPMIC"] <- ifelse(modDF.sum$CPMIC.mean[modDF.sum$ModName=="F_QUINC"]<=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CPMIC.mean[modDF.sum$ModName=="F_QUINC"]>=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPMIC"] <- ifelse(modDF.sum$CPMIC.mean[modDF.sum$ModName=="G_OCHDX"]<=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CPMIC.mean[modDF.sum$ModName=="G_OCHDX"]>=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPMIC"] <- ifelse(modDF.sum$CPMIC.mean[modDF.sum$ModName=="H_QUJSM"]<=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                             modDF.sum$CPMIC.mean[modDF.sum$ModName=="H_QUJSM"]>=
                                                             (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                                  obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                         1, 0)
    
    outDF$I_MM[outDF$Variable=="CPMIC"] <- ifelse(modDF.sum$CPMIC.mean[modDF.sum$ModName=="I_MM"]<=
                                                          (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
                                                               obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
                                                          modDF.sum$CPMIC.mean[modDF.sum$ModName=="I_MM"]>=
                                                          (obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
                                                               obsDF$CPMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    return(outDF)
    
}