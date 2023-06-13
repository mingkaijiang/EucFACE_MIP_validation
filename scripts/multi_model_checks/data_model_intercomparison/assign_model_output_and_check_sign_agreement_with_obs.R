assign_model_output_and_check_sign_agreement_with_obs <- function (obsDF, outDF,
                                                                   modDF.sum, treatment) {
    
    
    ###########################################################################
    ### GPP
    outDF$C_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                         sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### NPP
    outDF$C_GDAYP[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### NEP
    outDF$C_GDAYP[outDF$Variable=="NEP"] <- ifelse(sign(modDF.sum$NEP.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="NEP"] <- ifelse(sign(modDF.sum$NEP.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="NEP"] <- ifelse(sign(modDF.sum$NEP.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="NEP"] <- ifelse(sign(modDF.sum$NEP.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="NEP"] <- ifelse(sign(modDF.sum$NEP.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="NEP"] <- ifelse(sign(modDF.sum$NEP.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="NEP"] <- ifelse(sign(modDF.sum$NEP.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="NEP"] <- ifelse(sign(modDF.sum$NEP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="NEP"] <- ifelse(sign(modDF.sum$NEP.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$NEP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### RHET
    outDF$C_GDAYP[outDF$Variable=="RHET"] <- ifelse(sign(modDF.sum$RHET.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="RHET"] <- ifelse(sign(modDF.sum$RHET.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="RHET"] <- ifelse(sign(modDF.sum$RHET.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="RHET"] <- ifelse(sign(modDF.sum$RHET.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="RHET"] <- ifelse(sign(modDF.sum$RHET.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="RHET"] <- ifelse(sign(modDF.sum$RHET.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="RHET"] <- ifelse(sign(modDF.sum$RHET.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="RHET"] <- ifelse(sign(modDF.sum$RHET.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="RHET"] <- ifelse(sign(modDF.sum$RHET.mean[modDF.sum$ModName=="I_MM"])==
                                                        sign(obsDF$RHET[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### BP
    outDF$C_GDAYP[outDF$Variable=="BP"] <- ifelse(sign(modDF.sum$BP.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="BP"] <- ifelse(sign(modDF.sum$BP.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="BP"] <- ifelse(sign(modDF.sum$BP.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="BP"] <- ifelse(sign(modDF.sum$BP.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="BP"] <- ifelse(sign(modDF.sum$BP.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="BP"] <- ifelse(sign(modDF.sum$BP.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="BP"] <- ifelse(sign(modDF.sum$BP.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="BP"] <- ifelse(sign(modDF.sum$BP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="BP"] <- ifelse(sign(modDF.sum$BP.mean[modDF.sum$ModName=="I_MM"])==
                                                      sign(obsDF$BP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    ### deltaCL
    outDF$C_GDAYP[outDF$Variable=="deltaCL"] <- ifelse(sign(modDF.sum$deltaCL.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCL"] <- ifelse(sign(modDF.sum$deltaCL.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCL"] <- ifelse(sign(modDF.sum$deltaCL.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCL"] <- ifelse(sign(modDF.sum$deltaCL.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCL"] <- ifelse(sign(modDF.sum$deltaCL.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCL"] <- ifelse(sign(modDF.sum$deltaCL.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCL"] <- ifelse(sign(modDF.sum$deltaCL.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCL"] <- ifelse(sign(modDF.sum$deltaCL.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$I_MM[outDF$Variable=="deltaCL"] <- ifelse(sign(modDF.sum$deltaCL.mean[modDF.sum$ModName=="I_MM"])==
                                                           sign(obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    ### deltaCW
    outDF$C_GDAYP[outDF$Variable=="deltaCW"] <- ifelse(sign(modDF.sum$deltaCW.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCW"] <- ifelse(sign(modDF.sum$deltaCW.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCW"] <- ifelse(sign(modDF.sum$deltaCW.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCW"] <- ifelse(sign(modDF.sum$deltaCW.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCW"] <- ifelse(sign(modDF.sum$deltaCW.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCW"] <- ifelse(sign(modDF.sum$deltaCW.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCW"] <- ifelse(sign(modDF.sum$deltaCW.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCW"] <- ifelse(sign(modDF.sum$deltaCW.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaCW"] <- ifelse(sign(modDF.sum$deltaCW.mean[modDF.sum$ModName=="I_MM"])==
                                                           sign(obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    ### deltaCFR
    outDF$C_GDAYP[outDF$Variable=="deltaCFR"] <- ifelse(sign(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCFR"] <- ifelse(sign(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCFR"] <- ifelse(sign(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCFR"] <- ifelse(sign(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCFR"] <- ifelse(sign(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCFR"] <- ifelse(sign(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCFR"] <- ifelse(sign(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCFR"] <- ifelse(sign(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaCFR"] <- ifelse(sign(modDF.sum$deltaCFR.mean[modDF.sum$ModName=="I_MM"])==
                                                            sign(obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    ### deltaCCR
    outDF$C_GDAYP[outDF$Variable=="deltaCCR"] <- ifelse(sign(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCCR"] <- ifelse(sign(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCCR"] <- ifelse(sign(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCCR"] <- ifelse(sign(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCCR"] <- ifelse(sign(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCCR"] <- ifelse(sign(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCCR"] <- ifelse(sign(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCCR"] <- ifelse(sign(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaCCR"] <- ifelse(sign(modDF.sum$deltaCCR.mean[modDF.sum$ModName=="I_MM"])==
                                                            sign(obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    
    ### deltaCFLITA
    outDF$C_GDAYP[outDF$Variable=="deltaCFLITA"] <- ifelse(sign(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCFLITA"] <- ifelse(sign(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCFLITA"] <- ifelse(sign(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCFLITA"] <- ifelse(sign(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCFLITA"] <- ifelse(sign(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCFLITA"] <- ifelse(sign(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCFLITA"] <- ifelse(sign(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCFLITA"] <- ifelse(sign(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaCFLITA"] <- ifelse(sign(modDF.sum$deltaCFLITA.mean[modDF.sum$ModName=="I_MM"])==
                                                               sign(obsDF$deltaCFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    ### deltaCMIC
    outDF$C_GDAYP[outDF$Variable=="deltaCMIC"] <- ifelse(sign(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                               sign(obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaCMIC"] <- ifelse(sign(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                               sign(obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaCMIC"] <- ifelse(sign(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="B_CABLP"])==
                                                               sign(obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaCMIC"] <- ifelse(sign(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                               sign(obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaCMIC"] <- ifelse(sign(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                               sign(obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaCMIC"] <- ifelse(sign(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="F_QUINC"])==
                                                               sign(obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaCMIC"] <- ifelse(sign(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                               sign(obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaCMIC"] <- ifelse(sign(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                               sign(obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaCMIC"] <- ifelse(sign(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="I_MM"])==
                                                            sign(obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    ### CGL
    outDF$C_GDAYP[outDF$Variable=="CGL"] <- ifelse(sign(modDF.sum$CGL.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CGL"] <- ifelse(sign(modDF.sum$CGL.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CGL"] <- ifelse(sign(modDF.sum$CGL.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CGL"] <- ifelse(sign(modDF.sum$CGL.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CGL"] <- ifelse(sign(modDF.sum$CGL.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CGL"] <- ifelse(sign(modDF.sum$CGL.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CGL"] <- ifelse(sign(modDF.sum$CGL.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CGL"] <- ifelse(sign(modDF.sum$CGL.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CGL"] <- ifelse(sign(modDF.sum$CGL.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$CGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CGW
    outDF$C_GDAYP[outDF$Variable=="CGW"] <- ifelse(sign(modDF.sum$CGW.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CGW"] <- ifelse(sign(modDF.sum$CGW.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CGW"] <- ifelse(sign(modDF.sum$CGW.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CGW"] <- ifelse(sign(modDF.sum$CGW.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CGW"] <- ifelse(sign(modDF.sum$CGW.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CGW"] <- ifelse(sign(modDF.sum$CGW.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CGW"] <- ifelse(sign(modDF.sum$CGW.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CGW"] <- ifelse(sign(modDF.sum$CGW.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CGW"] <- ifelse(sign(modDF.sum$CGW.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$CGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CGFR
    outDF$C_GDAYP[outDF$Variable=="CGFR"] <- ifelse(sign(modDF.sum$CGFR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CGFR"] <- ifelse(sign(modDF.sum$CGFR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CGFR"] <- ifelse(sign(modDF.sum$CGFR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CGFR"] <- ifelse(sign(modDF.sum$CGFR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CGFR"] <- ifelse(sign(modDF.sum$CGFR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CGFR"] <- ifelse(sign(modDF.sum$CGFR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CGFR"] <- ifelse(sign(modDF.sum$CGFR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CGFR"] <- ifelse(sign(modDF.sum$CGFR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CGFR"] <- ifelse(sign(modDF.sum$CGFR.mean[modDF.sum$ModName=="I_MM"])==
                                                        sign(obsDF$CGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### CGCR
    outDF$C_GDAYP[outDF$Variable=="CGCR"] <- ifelse(sign(modDF.sum$CGCR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CGCR"] <- ifelse(sign(modDF.sum$CGCR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CGCR"] <- ifelse(sign(modDF.sum$CGCR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CGCR"] <- ifelse(sign(modDF.sum$CGCR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CGCR"] <- ifelse(sign(modDF.sum$CGCR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CGCR"] <- ifelse(sign(modDF.sum$CGCR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CGCR"] <- ifelse(sign(modDF.sum$CGCR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CGCR"] <- ifelse(sign(modDF.sum$CGCR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CGCR"] <- ifelse(sign(modDF.sum$CGCR.mean[modDF.sum$ModName=="I_MM"])==
                                                        sign(obsDF$CGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### LAI
    outDF$C_GDAYP[outDF$Variable=="LAI"] <- ifelse(sign(modDF.sum$LAI.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="LAI"] <- ifelse(sign(modDF.sum$LAI.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="LAI"] <- ifelse(sign(modDF.sum$LAI.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="LAI"] <- ifelse(sign(modDF.sum$LAI.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="LAI"] <- ifelse(sign(modDF.sum$LAI.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="LAI"] <- ifelse(sign(modDF.sum$LAI.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="LAI"] <- ifelse(sign(modDF.sum$LAI.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="LAI"] <- ifelse(sign(modDF.sum$LAI.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="LAI"] <- ifelse(sign(modDF.sum$LAI.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$LAI[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CL
    outDF$C_GDAYP[outDF$Variable=="CL"] <- ifelse(sign(modDF.sum$CL.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CL"] <- ifelse(sign(modDF.sum$CL.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CL"] <- ifelse(sign(modDF.sum$CL.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CL"] <- ifelse(sign(modDF.sum$CL.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CL"] <- ifelse(sign(modDF.sum$CL.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CL"] <- ifelse(sign(modDF.sum$CL.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CL"] <- ifelse(sign(modDF.sum$CL.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CL"] <- ifelse(sign(modDF.sum$CL.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CL"] <- ifelse(sign(modDF.sum$CL.mean[modDF.sum$ModName=="I_MM"])==
                                                      sign(obsDF$CL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    
    ### CW
    outDF$C_GDAYP[outDF$Variable=="CW"] <- ifelse(sign(modDF.sum$CW.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CW"] <- ifelse(sign(modDF.sum$CW.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CW"] <- ifelse(sign(modDF.sum$CW.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CW"] <- ifelse(sign(modDF.sum$CW.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CW"] <- ifelse(sign(modDF.sum$CW.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CW"] <- ifelse(sign(modDF.sum$CW.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CW"] <- ifelse(sign(modDF.sum$CW.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CW"] <- ifelse(sign(modDF.sum$CW.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CW"] <- ifelse(sign(modDF.sum$CW.mean[modDF.sum$ModName=="I_MM"])==
                                                      sign(obsDF$CW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    
    ### CFR
    outDF$C_GDAYP[outDF$Variable=="CFR"] <- ifelse(sign(modDF.sum$CFR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CFR"] <- ifelse(sign(modDF.sum$CFR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CFR"] <- ifelse(sign(modDF.sum$CFR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CFR"] <- ifelse(sign(modDF.sum$CFR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CFR"] <- ifelse(sign(modDF.sum$CFR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CFR"] <- ifelse(sign(modDF.sum$CFR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CFR"] <- ifelse(sign(modDF.sum$CFR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CFR"] <- ifelse(sign(modDF.sum$CFR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CFR"] <- ifelse(sign(modDF.sum$CFR.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$CFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    ### CCR
    outDF$C_GDAYP[outDF$Variable=="CCR"] <- ifelse(sign(modDF.sum$CCR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CCR"] <- ifelse(sign(modDF.sum$CCR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CCR"] <- ifelse(sign(modDF.sum$CCR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CCR"] <- ifelse(sign(modDF.sum$CCR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CCR"] <- ifelse(sign(modDF.sum$CCR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CCR"] <- ifelse(sign(modDF.sum$CCR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CCR"] <- ifelse(sign(modDF.sum$CCR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CCR"] <- ifelse(sign(modDF.sum$CCR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CCR"] <- ifelse(sign(modDF.sum$CCR.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$CCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CFLITA
    outDF$C_GDAYP[outDF$Variable=="CFLITA"] <- ifelse(sign(modDF.sum$CFLITA.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CFLITA"] <- ifelse(sign(modDF.sum$CFLITA.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CFLITA"] <- ifelse(sign(modDF.sum$CFLITA.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CFLITA"] <- ifelse(sign(modDF.sum$CFLITA.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CFLITA"] <- ifelse(sign(modDF.sum$CFLITA.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CFLITA"] <- ifelse(sign(modDF.sum$CFLITA.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CFLITA"] <- ifelse(sign(modDF.sum$CFLITA.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CFLITA"] <- ifelse(sign(modDF.sum$CFLITA.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CFLITA"] <- ifelse(sign(modDF.sum$CFLITA.mean[modDF.sum$ModName=="I_MM"])==
                                                          sign(obsDF$CFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    ### CMIC
    outDF$C_GDAYP[outDF$Variable=="CMIC"] <- ifelse(sign(modDF.sum$CMIC.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                               sign(obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CMIC"] <- ifelse(sign(modDF.sum$CMIC.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                               sign(obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CMIC"] <- ifelse(sign(modDF.sum$CMIC.mean[modDF.sum$ModName=="B_CABLP"])==
                                                               sign(obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CMIC"] <- ifelse(sign(modDF.sum$CMIC.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                               sign(obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CMIC"] <- ifelse(sign(modDF.sum$CMIC.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                               sign(obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CMIC"] <- ifelse(sign(modDF.sum$CMIC.mean[modDF.sum$ModName=="F_QUINC"])==
                                                               sign(obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CMIC"] <- ifelse(sign(modDF.sum$CMIC.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                               sign(obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CMIC"] <- ifelse(sign(modDF.sum$CMIC.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                               sign(obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$I_MM[outDF$Variable=="CMIC"] <- ifelse(sign(modDF.sum$CMIC.mean[modDF.sum$ModName=="I_MM"])==
                                                            sign(obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    ### CSOIL
    outDF$C_GDAYP[outDF$Variable=="CSOIL"] <- ifelse(sign(modDF.sum$CSOIL.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CSOIL"] <- ifelse(sign(modDF.sum$CSOIL.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CSOIL"] <- ifelse(sign(modDF.sum$CSOIL.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CSOIL"] <- ifelse(sign(modDF.sum$CSOIL.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CSOIL"] <- ifelse(sign(modDF.sum$CSOIL.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CSOIL"] <- ifelse(sign(modDF.sum$CSOIL.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CSOIL"] <- ifelse(sign(modDF.sum$CSOIL.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CSOIL"] <- ifelse(sign(modDF.sum$CSOIL.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CSOIL"] <- ifelse(sign(modDF.sum$CSOIL.mean[modDF.sum$ModName=="I_MM"])==
                                                         sign(obsDF$CSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    
    ### PL
    outDF$C_GDAYP[outDF$Variable=="PL"] <- ifelse(sign(modDF.sum$PL.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PL"] <- ifelse(sign(modDF.sum$PL.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PL"] <- ifelse(sign(modDF.sum$PL.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PL"] <- ifelse(sign(modDF.sum$PL.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PL"] <- ifelse(sign(modDF.sum$PL.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PL"] <- ifelse(sign(modDF.sum$PL.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PL"] <- ifelse(sign(modDF.sum$PL.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PL"] <- ifelse(sign(modDF.sum$PL.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PL"] <- ifelse(sign(modDF.sum$PL.mean[modDF.sum$ModName=="I_MM"])==
                                                      sign(obsDF$PL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    
    
    ### PW
    outDF$C_GDAYP[outDF$Variable=="PW"] <- ifelse(sign(modDF.sum$PW.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PW"] <- ifelse(sign(modDF.sum$PW.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PW"] <- ifelse(sign(modDF.sum$PW.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PW"] <- ifelse(sign(modDF.sum$PW.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PW"] <- ifelse(sign(modDF.sum$PW.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PW"] <- ifelse(sign(modDF.sum$PW.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PW"] <- ifelse(sign(modDF.sum$PW.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PW"] <- ifelse(sign(modDF.sum$PW.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PW"] <- ifelse(sign(modDF.sum$PW.mean[modDF.sum$ModName=="I_MM"])==
                                                      sign(obsDF$PW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    
    ### PFR
    outDF$C_GDAYP[outDF$Variable=="PFR"] <- ifelse(sign(modDF.sum$PFR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PFR"] <- ifelse(sign(modDF.sum$PFR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PFR"] <- ifelse(sign(modDF.sum$PFR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PFR"] <- ifelse(sign(modDF.sum$PFR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PFR"] <- ifelse(sign(modDF.sum$PFR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PFR"] <- ifelse(sign(modDF.sum$PFR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PFR"] <- ifelse(sign(modDF.sum$PFR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PFR"] <- ifelse(sign(modDF.sum$PFR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PFR"] <- ifelse(sign(modDF.sum$PFR.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$PFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    
    ### PCR
    outDF$C_GDAYP[outDF$Variable=="PCR"] <- ifelse(sign(modDF.sum$PCR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PCR"] <- ifelse(sign(modDF.sum$PCR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PCR"] <- ifelse(sign(modDF.sum$PCR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PCR"] <- ifelse(sign(modDF.sum$PCR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PCR"] <- ifelse(sign(modDF.sum$PCR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PCR"] <- ifelse(sign(modDF.sum$PCR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PCR"] <- ifelse(sign(modDF.sum$PCR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PCR"] <- ifelse(sign(modDF.sum$PCR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PCR"] <- ifelse(sign(modDF.sum$PCR.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$PCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    
    ### PFLITA
    outDF$C_GDAYP[outDF$Variable=="PFLITA"] <- ifelse(sign(modDF.sum$PFLITA.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PFLITA"] <- ifelse(sign(modDF.sum$PFLITA.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PFLITA"] <- ifelse(sign(modDF.sum$PFLITA.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PFLITA"] <- ifelse(sign(modDF.sum$PFLITA.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PFLITA"] <- ifelse(sign(modDF.sum$PFLITA.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PFLITA"] <- ifelse(sign(modDF.sum$PFLITA.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PFLITA"] <- ifelse(sign(modDF.sum$PFLITA.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PFLITA"] <- ifelse(sign(modDF.sum$PFLITA.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PFLITA"] <- ifelse(sign(modDF.sum$PFLITA.mean[modDF.sum$ModName=="I_MM"])==
                                                          sign(obsDF$PFLITA[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    
    ### PSOIL
    outDF$C_GDAYP[outDF$Variable=="PSOIL"] <- ifelse(sign(modDF.sum$PSOIL.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PSOIL"] <- ifelse(sign(modDF.sum$PSOIL.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PSOIL"] <- ifelse(sign(modDF.sum$PSOIL.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PSOIL"] <- ifelse(sign(modDF.sum$PSOIL.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PSOIL"] <- ifelse(sign(modDF.sum$PSOIL.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PSOIL"] <- ifelse(sign(modDF.sum$PSOIL.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PSOIL"] <- ifelse(sign(modDF.sum$PSOIL.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PSOIL"] <- ifelse(sign(modDF.sum$PSOIL.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PSOIL"] <- ifelse(sign(modDF.sum$PSOIL.mean[modDF.sum$ModName=="I_MM"])==
                                                         sign(obsDF$PSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    
    ### PPORG
    outDF$C_GDAYP[outDF$Variable=="PPORG"] <- ifelse(sign(modDF.sum$PPORG.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PPORG"] <- ifelse(sign(modDF.sum$PPORG.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PPORG"] <- ifelse(sign(modDF.sum$PPORG.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PPORG"] <- ifelse(sign(modDF.sum$PPORG.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PPORG"] <- ifelse(sign(modDF.sum$PPORG.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PPORG"] <- ifelse(sign(modDF.sum$PPORG.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PPORG"] <- ifelse(sign(modDF.sum$PPORG.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PPORG"] <- ifelse(sign(modDF.sum$PPORG.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PPORG"] <- ifelse(sign(modDF.sum$PPORG.mean[modDF.sum$ModName=="I_MM"])==
                                                         sign(obsDF$PPORG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    
    ### PPMIN
    outDF$C_GDAYP[outDF$Variable=="PPMIN"] <- ifelse(sign(modDF.sum$PPMIN.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PPMIN"] <- ifelse(sign(modDF.sum$PPMIN.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PPMIN"] <- ifelse(sign(modDF.sum$PPMIN.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PPMIN"] <- ifelse(sign(modDF.sum$PPMIN.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PPMIN"] <- ifelse(sign(modDF.sum$PPMIN.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PPMIN"] <- ifelse(sign(modDF.sum$PPMIN.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PPMIN"] <- ifelse(sign(modDF.sum$PPMIN.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PPMIN"] <- ifelse(sign(modDF.sum$PPMIN.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PPMIN"] <- ifelse(sign(modDF.sum$PPMIN.mean[modDF.sum$ModName=="I_MM"])==
                                                         sign(obsDF$PPMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    
    ### PDEM
    outDF$C_GDAYP[outDF$Variable=="PDEM"] <- ifelse(sign(modDF.sum$PDEM.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PDEM"] <- ifelse(sign(modDF.sum$PDEM.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PDEM"] <- ifelse(sign(modDF.sum$PDEM.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PDEM"] <- ifelse(sign(modDF.sum$PDEM.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PDEM"] <- ifelse(sign(modDF.sum$PDEM.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PDEM"] <- ifelse(sign(modDF.sum$PDEM.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PDEM"] <- ifelse(sign(modDF.sum$PDEM.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PDEM"] <- ifelse(sign(modDF.sum$PDEM.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PDEM"] <- ifelse(sign(modDF.sum$PDEM.mean[modDF.sum$ModName=="I_MM"])==
                                                        sign(obsDF$PDEM[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    
    ### PGL
    outDF$C_GDAYP[outDF$Variable=="PGL"] <- ifelse(sign(modDF.sum$PGL.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PGL"] <- ifelse(sign(modDF.sum$PGL.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PGL"] <- ifelse(sign(modDF.sum$PGL.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PGL"] <- ifelse(sign(modDF.sum$PGL.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PGL"] <- ifelse(sign(modDF.sum$PGL.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PGL"] <- ifelse(sign(modDF.sum$PGL.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PGL"] <- ifelse(sign(modDF.sum$PGL.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PGL"] <- ifelse(sign(modDF.sum$PGL.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PGL"] <- ifelse(sign(modDF.sum$PGL.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$PGL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    
    ### PGW
    outDF$C_GDAYP[outDF$Variable=="PGW"] <- ifelse(sign(modDF.sum$PGW.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PGW"] <- ifelse(sign(modDF.sum$PGW.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PGW"] <- ifelse(sign(modDF.sum$PGW.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PGW"] <- ifelse(sign(modDF.sum$PGW.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PGW"] <- ifelse(sign(modDF.sum$PGW.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PGW"] <- ifelse(sign(modDF.sum$PGW.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PGW"] <- ifelse(sign(modDF.sum$PGW.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PGW"] <- ifelse(sign(modDF.sum$PGW.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PGW"] <- ifelse(sign(modDF.sum$PGW.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$PGW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    
    ### PGFR
    outDF$C_GDAYP[outDF$Variable=="PGFR"] <- ifelse(sign(modDF.sum$PGFR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PGFR"] <- ifelse(sign(modDF.sum$PGFR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PGFR"] <- ifelse(sign(modDF.sum$PGFR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PGFR"] <- ifelse(sign(modDF.sum$PGFR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PGFR"] <- ifelse(sign(modDF.sum$PGFR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PGFR"] <- ifelse(sign(modDF.sum$PGFR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PGFR"] <- ifelse(sign(modDF.sum$PGFR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PGFR"] <- ifelse(sign(modDF.sum$PGFR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PGFR"] <- ifelse(sign(modDF.sum$PGFR.mean[modDF.sum$ModName=="I_MM"])==
                                                        sign(obsDF$PGFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    
    ### PGCR
    outDF$C_GDAYP[outDF$Variable=="PGCR"] <- ifelse(sign(modDF.sum$PGCR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PGCR"] <- ifelse(sign(modDF.sum$PGCR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PGCR"] <- ifelse(sign(modDF.sum$PGCR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PGCR"] <- ifelse(sign(modDF.sum$PGCR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PGCR"] <- ifelse(sign(modDF.sum$PGCR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PGCR"] <- ifelse(sign(modDF.sum$PGCR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PGCR"] <- ifelse(sign(modDF.sum$PGCR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PGCR"] <- ifelse(sign(modDF.sum$PGCR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PGCR"] <- ifelse(sign(modDF.sum$PGCR.mean[modDF.sum$ModName=="I_MM"])==
                                                        sign(obsDF$PGCR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    
    ### PLITIN
    outDF$C_GDAYP[outDF$Variable=="PLITIN"] <- ifelse(sign(modDF.sum$PLITIN.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PLITIN"] <- ifelse(sign(modDF.sum$PLITIN.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PLITIN"] <- ifelse(sign(modDF.sum$PLITIN.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PLITIN"] <- ifelse(sign(modDF.sum$PLITIN.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PLITIN"] <- ifelse(sign(modDF.sum$PLITIN.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PLITIN"] <- ifelse(sign(modDF.sum$PLITIN.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PLITIN"] <- ifelse(sign(modDF.sum$PLITIN.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PLITIN"] <- ifelse(sign(modDF.sum$PLITIN.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PLITIN"] <- ifelse(sign(modDF.sum$PLITIN.mean[modDF.sum$ModName=="I_MM"])==
                                                          sign(obsDF$PLITIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    
    ### PWLIN
    outDF$C_GDAYP[outDF$Variable=="PWLIN"] <- ifelse(sign(modDF.sum$PWLIN.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PWLIN"] <- ifelse(sign(modDF.sum$PWLIN.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PWLIN"] <- ifelse(sign(modDF.sum$PWLIN.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PWLIN"] <- ifelse(sign(modDF.sum$PWLIN.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PWLIN"] <- ifelse(sign(modDF.sum$PWLIN.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PWLIN"] <- ifelse(sign(modDF.sum$PWLIN.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PWLIN"] <- ifelse(sign(modDF.sum$PWLIN.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PWLIN"] <- ifelse(sign(modDF.sum$PWLIN.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PWLIN"] <- ifelse(sign(modDF.sum$PWLIN.mean[modDF.sum$ModName=="I_MM"])==
                                                         sign(obsDF$PWLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    
    ### PFRLIN
    outDF$C_GDAYP[outDF$Variable=="PFRLIN"] <- ifelse(sign(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PFRLIN"] <- ifelse(sign(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PFRLIN"] <- ifelse(sign(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PFRLIN"] <- ifelse(sign(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PFRLIN"] <- ifelse(sign(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PFRLIN"] <- ifelse(sign(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PFRLIN"] <- ifelse(sign(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PFRLIN"] <- ifelse(sign(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PFRLIN"] <- ifelse(sign(modDF.sum$PFRLIN.mean[modDF.sum$ModName=="I_MM"])==
                                                          sign(obsDF$PFRLIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    
    ### PLAB
    outDF$C_GDAYP[outDF$Variable=="PLAB"] <- ifelse(sign(modDF.sum$PLAB.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PLAB"] <- ifelse(sign(modDF.sum$PLAB.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PLAB"] <- ifelse(sign(modDF.sum$PLAB.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PLAB"] <- ifelse(sign(modDF.sum$PLAB.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PLAB"] <- ifelse(sign(modDF.sum$PLAB.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PLAB"] <- ifelse(sign(modDF.sum$PLAB.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PLAB"] <- ifelse(sign(modDF.sum$PLAB.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PLAB"] <- ifelse(sign(modDF.sum$PLAB.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PLAB"] <- ifelse(sign(modDF.sum$PLAB.mean[modDF.sum$ModName=="I_MM"])==
                                                        sign(obsDF$PLAB[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    
    ### PSEC
    outDF$C_GDAYP[outDF$Variable=="PSEC"] <- ifelse(sign(modDF.sum$PSEC.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PSEC"] <- ifelse(sign(modDF.sum$PSEC.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PSEC"] <- ifelse(sign(modDF.sum$PSEC.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PSEC"] <- ifelse(sign(modDF.sum$PSEC.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PSEC"] <- ifelse(sign(modDF.sum$PSEC.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PSEC"] <- ifelse(sign(modDF.sum$PSEC.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PSEC"] <- ifelse(sign(modDF.sum$PSEC.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PSEC"] <- ifelse(sign(modDF.sum$PSEC.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PSEC"] <- ifelse(sign(modDF.sum$PSEC.mean[modDF.sum$ModName=="I_MM"])==
                                                        sign(obsDF$PSEC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    
    ### POCC
    outDF$C_GDAYP[outDF$Variable=="POCC"] <- ifelse(sign(modDF.sum$POCC.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="POCC"] <- ifelse(sign(modDF.sum$POCC.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="POCC"] <- ifelse(sign(modDF.sum$POCC.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="POCC"] <- ifelse(sign(modDF.sum$POCC.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="POCC"] <- ifelse(sign(modDF.sum$POCC.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="POCC"] <- ifelse(sign(modDF.sum$POCC.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="POCC"] <- ifelse(sign(modDF.sum$POCC.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="POCC"] <- ifelse(sign(modDF.sum$POCC.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="POCC"] <- ifelse(sign(modDF.sum$POCC.mean[modDF.sum$ModName=="I_MM"])==
                                                        sign(obsDF$POCC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### PUP
    outDF$C_GDAYP[outDF$Variable=="PUP"] <- ifelse(sign(modDF.sum$PUP.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PUP"] <- ifelse(sign(modDF.sum$PUP.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PUP"] <- ifelse(sign(modDF.sum$PUP.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PUP"] <- ifelse(sign(modDF.sum$PUP.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PUP"] <- ifelse(sign(modDF.sum$PUP.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PUP"] <- ifelse(sign(modDF.sum$PUP.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PUP"] <- ifelse(sign(modDF.sum$PUP.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PUP"] <- ifelse(sign(modDF.sum$PUP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PUP"] <- ifelse(sign(modDF.sum$PUP.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$PUP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    
    ### PRETR
    outDF$C_GDAYP[outDF$Variable=="PRETR"] <- ifelse(sign(modDF.sum$PRETR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PRETR"] <- ifelse(sign(modDF.sum$PRETR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PRETR"] <- ifelse(sign(modDF.sum$PRETR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PRETR"] <- ifelse(sign(modDF.sum$PRETR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PRETR"] <- ifelse(sign(modDF.sum$PRETR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PRETR"] <- ifelse(sign(modDF.sum$PRETR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PRETR"] <- ifelse(sign(modDF.sum$PRETR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PRETR"] <- ifelse(sign(modDF.sum$PRETR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PRETR"] <- ifelse(sign(modDF.sum$PRETR.mean[modDF.sum$ModName=="I_MM"])==
                                                         sign(obsDF$PRETR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    
    ### PMIN
    outDF$C_GDAYP[outDF$Variable=="PMIN"] <- ifelse(sign(modDF.sum$PMIN.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PMIN"] <- ifelse(sign(modDF.sum$PMIN.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PMIN"] <- ifelse(sign(modDF.sum$PMIN.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PMIN"] <- ifelse(sign(modDF.sum$PMIN.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PMIN"] <- ifelse(sign(modDF.sum$PMIN.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PMIN"] <- ifelse(sign(modDF.sum$PMIN.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PMIN"] <- ifelse(sign(modDF.sum$PMIN.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PMIN"] <- ifelse(sign(modDF.sum$PMIN.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PMIN"] <- ifelse(sign(modDF.sum$PMIN.mean[modDF.sum$ModName=="I_MM"])==
                                                        sign(obsDF$PMIN[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    
    
    ### PLEACH
    outDF$C_GDAYP[outDF$Variable=="PLEACH"] <- ifelse(sign(modDF.sum$PLEACH.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PLEACH"] <- ifelse(sign(modDF.sum$PLEACH.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PLEACH"] <- ifelse(sign(modDF.sum$PLEACH.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PLEACH"] <- ifelse(sign(modDF.sum$PLEACH.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PLEACH"] <- ifelse(sign(modDF.sum$PLEACH.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PLEACH"] <- ifelse(sign(modDF.sum$PLEACH.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PLEACH"] <- ifelse(sign(modDF.sum$PLEACH.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PLEACH"] <- ifelse(sign(modDF.sum$PLEACH.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PLEACH"] <- ifelse(sign(modDF.sum$PLEACH.mean[modDF.sum$ModName=="I_MM"])==
                                                          sign(obsDF$PLEACH[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    
    ### PUE
    outDF$C_GDAYP[outDF$Variable=="PUE"] <- ifelse(sign(modDF.sum$PUE.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PUE"] <- ifelse(sign(modDF.sum$PUE.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PUE"] <- ifelse(sign(modDF.sum$PUE.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PUE"] <- ifelse(sign(modDF.sum$PUE.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PUE"] <- ifelse(sign(modDF.sum$PUE.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PUE"] <- ifelse(sign(modDF.sum$PUE.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PUE"] <- ifelse(sign(modDF.sum$PUE.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PUE"] <- ifelse(sign(modDF.sum$PUE.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="PUE"] <- ifelse(sign(modDF.sum$PUE.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$PUE[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    ### CPL
    outDF$C_GDAYP[outDF$Variable=="CPL"] <- ifelse(sign(modDF.sum$CPL.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPL"] <- ifelse(sign(modDF.sum$CPL.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPL"] <- ifelse(sign(modDF.sum$CPL.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPL"] <- ifelse(sign(modDF.sum$CPL.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPL"] <- ifelse(sign(modDF.sum$CPL.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPL"] <- ifelse(sign(modDF.sum$CPL.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPL"] <- ifelse(sign(modDF.sum$CPL.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPL"] <- ifelse(sign(modDF.sum$CPL.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CPL"] <- ifelse(sign(modDF.sum$CPL.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$CPL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    
    ### CPW
    outDF$C_GDAYP[outDF$Variable=="CPW"] <- ifelse(sign(modDF.sum$CPW.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPW"] <- ifelse(sign(modDF.sum$CPW.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPW"] <- ifelse(sign(modDF.sum$CPW.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPW"] <- ifelse(sign(modDF.sum$CPW.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPW"] <- ifelse(sign(modDF.sum$CPW.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPW"] <- ifelse(sign(modDF.sum$CPW.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPW"] <- ifelse(sign(modDF.sum$CPW.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPW"] <- ifelse(sign(modDF.sum$CPW.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CPW"] <- ifelse(sign(modDF.sum$CPW.mean[modDF.sum$ModName=="I_MM"])==
                                                       sign(obsDF$CPW[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    ### CPFR
    outDF$C_GDAYP[outDF$Variable=="CPFR"] <- ifelse(sign(modDF.sum$CPFR.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPFR"] <- ifelse(sign(modDF.sum$CPFR.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPFR"] <- ifelse(sign(modDF.sum$CPFR.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPFR"] <- ifelse(sign(modDF.sum$CPFR.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPFR"] <- ifelse(sign(modDF.sum$CPFR.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPFR"] <- ifelse(sign(modDF.sum$CPFR.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPFR"] <- ifelse(sign(modDF.sum$CPFR.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPFR"] <- ifelse(sign(modDF.sum$CPFR.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CPFR"] <- ifelse(sign(modDF.sum$CPFR.mean[modDF.sum$ModName=="I_MM"])==
                                                        sign(obsDF$CPFR[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                    1, 0)
    
    ### CPSOIL
    outDF$C_GDAYP[outDF$Variable=="CPSOIL"] <- ifelse(sign(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPSOIL"] <- ifelse(sign(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPSOIL"] <- ifelse(sign(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPSOIL"] <- ifelse(sign(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPSOIL"] <- ifelse(sign(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPSOIL"] <- ifelse(sign(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPSOIL"] <- ifelse(sign(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPSOIL"] <- ifelse(sign(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CPSOIL"] <- ifelse(sign(modDF.sum$CPSOIL.mean[modDF.sum$ModName=="I_MM"])==
                                                          sign(obsDF$CPSOIL[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    
    ### CPFLIT
    outDF$C_GDAYP[outDF$Variable=="CPFLIT"] <- ifelse(sign(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                       sign(obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPFLIT"] <- ifelse(sign(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                       sign(obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPFLIT"] <- ifelse(sign(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="B_CABLP"])==
                                                       sign(obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPFLIT"] <- ifelse(sign(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                       sign(obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPFLIT"] <- ifelse(sign(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                       sign(obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPFLIT"] <- ifelse(sign(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="F_QUINC"])==
                                                       sign(obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPFLIT"] <- ifelse(sign(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                       sign(obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPFLIT"] <- ifelse(sign(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$I_MM[outDF$Variable=="CPFLIT"] <- ifelse(sign(modDF.sum$CPFLIT.mean[modDF.sum$ModName=="I_MM"])==
                                                          sign(obsDF$CPFLIT[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                      1, 0)
    
    
    
    ### GPP use
    outDF$C_GDAYP[outDF$Variable=="GPP_use"] <- ifelse(sign(modDF.sum$GPP_use.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                           sign(obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="GPP_use"] <- ifelse(sign(modDF.sum$GPP_use.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                           sign(obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$B_CABLP[outDF$Variable=="GPP_use"] <- ifelse(sign(modDF.sum$GPP_use.mean[modDF.sum$ModName=="B_CABLP"])==
                                                           sign(obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="GPP_use"] <- ifelse(sign(modDF.sum$GPP_use.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                           sign(obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="GPP_use"] <- ifelse(sign(modDF.sum$GPP_use.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                           sign(obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$F_QUINC[outDF$Variable=="GPP_use"] <- ifelse(sign(modDF.sum$GPP_use.mean[modDF.sum$ModName=="F_QUINC"])==
                                                           sign(obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="GPP_use"] <- ifelse(sign(modDF.sum$GPP_use.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                           sign(obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP_use"] <- ifelse(sign(modDF.sum$GPP_use.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                           sign(obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    outDF$I_MM[outDF$Variable=="GPP_use"] <- ifelse(sign(modDF.sum$GPP_use.mean[modDF.sum$ModName=="I_MM"])==
                                                           sign(obsDF$GPP_use[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                       1, 0)
    
    
    ### deltaPVEG
    outDF$C_GDAYP[outDF$Variable=="deltaPVEG"] <- ifelse(sign(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                      sign(obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaPVEG"] <- ifelse(sign(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                      sign(obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaPVEG"] <- ifelse(sign(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="B_CABLP"])==
                                                      sign(obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaPVEG"] <- ifelse(sign(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                      sign(obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaPVEG"] <- ifelse(sign(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                      sign(obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaPVEG"] <- ifelse(sign(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="F_QUINC"])==
                                                      sign(obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaPVEG"] <- ifelse(sign(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                      sign(obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaPVEG"] <- ifelse(sign(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                      sign(obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                  1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaPVEG"] <- ifelse(sign(modDF.sum$deltaPVEG.mean[modDF.sum$ModName=="I_MM"])==
                                                   sign(obsDF$deltaPVEG[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                               1, 0)
    
    
    
    
    ### deltaPMIC
    outDF$C_GDAYP[outDF$Variable=="deltaPMIC"] <- ifelse(sign(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                               sign(obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="deltaPMIC"] <- ifelse(sign(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                               sign(obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$B_CABLP[outDF$Variable=="deltaPMIC"] <- ifelse(sign(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="B_CABLP"])==
                                                               sign(obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="deltaPMIC"] <- ifelse(sign(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                               sign(obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="deltaPMIC"] <- ifelse(sign(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                               sign(obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$F_QUINC[outDF$Variable=="deltaPMIC"] <- ifelse(sign(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="F_QUINC"])==
                                                               sign(obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="deltaPMIC"] <- ifelse(sign(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                               sign(obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="deltaPMIC"] <- ifelse(sign(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                               sign(obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$I_MM[outDF$Variable=="deltaPMIC"] <- ifelse(sign(modDF.sum$deltaPMIC.mean[modDF.sum$ModName=="I_MM"])==
                                                            sign(obsDF$deltaPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    
    
    
    
    ### PMIC
    outDF$C_GDAYP[outDF$Variable=="PMIC"] <- ifelse(sign(modDF.sum$PMIC.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                               sign(obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="PMIC"] <- ifelse(sign(modDF.sum$PMIC.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                               sign(obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$B_CABLP[outDF$Variable=="PMIC"] <- ifelse(sign(modDF.sum$PMIC.mean[modDF.sum$ModName=="B_CABLP"])==
                                                               sign(obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="PMIC"] <- ifelse(sign(modDF.sum$PMIC.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                               sign(obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="PMIC"] <- ifelse(sign(modDF.sum$PMIC.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                               sign(obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$F_QUINC[outDF$Variable=="PMIC"] <- ifelse(sign(modDF.sum$PMIC.mean[modDF.sum$ModName=="F_QUINC"])==
                                                               sign(obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="PMIC"] <- ifelse(sign(modDF.sum$PMIC.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                               sign(obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="PMIC"] <- ifelse(sign(modDF.sum$PMIC.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                               sign(obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$I_MM[outDF$Variable=="PMIC"] <- ifelse(sign(modDF.sum$PMIC.mean[modDF.sum$ModName=="I_MM"])==
                                                            sign(obsDF$PMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    
    ### CPMIC
    outDF$C_GDAYP[outDF$Variable=="CPMIC"] <- ifelse(sign(modDF.sum$CPMIC.mean[modDF.sum$ModName=="C_GDAYP"])==
                                                               sign(obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$A_ELMV1[outDF$Variable=="CPMIC"] <- ifelse(sign(modDF.sum$CPMIC.mean[modDF.sum$ModName=="A_ELMV1"])==
                                                               sign(obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$B_CABLP[outDF$Variable=="CPMIC"] <- ifelse(sign(modDF.sum$CPMIC.mean[modDF.sum$ModName=="B_CABLP"])==
                                                               sign(obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    
    outDF$D_LPJGP[outDF$Variable=="CPMIC"] <- ifelse(sign(modDF.sum$CPMIC.mean[modDF.sum$ModName=="D_LPJGP"])==
                                                               sign(obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$E_OCHDP[outDF$Variable=="CPMIC"] <- ifelse(sign(modDF.sum$CPMIC.mean[modDF.sum$ModName=="E_OCHDP"])==
                                                               sign(obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$F_QUINC[outDF$Variable=="CPMIC"] <- ifelse(sign(modDF.sum$CPMIC.mean[modDF.sum$ModName=="F_QUINC"])==
                                                               sign(obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$G_OCHDX[outDF$Variable=="CPMIC"] <- ifelse(sign(modDF.sum$CPMIC.mean[modDF.sum$ModName=="G_OCHDX"])==
                                                               sign(obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="CPMIC"] <- ifelse(sign(modDF.sum$CPMIC.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                               sign(obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                           1, 0)
    
    outDF$I_MM[outDF$Variable=="CPMIC"] <- ifelse(sign(modDF.sum$CPMIC.mean[modDF.sum$ModName=="I_MM"])==
                                                            sign(obsDF$CPMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                        1, 0)
    
    return(outDF)
    
}