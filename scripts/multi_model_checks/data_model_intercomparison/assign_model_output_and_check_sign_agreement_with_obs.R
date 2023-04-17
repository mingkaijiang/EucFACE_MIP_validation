assign_model_output_and_check_sign_agreement_with_obs <- function (obsDF, outDF,
                                                                   modDF.sum, treatment) {
    
    
    ###########################################################################
    ### GPP
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                         sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                     1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    ### NPP
    outDF$A_GDAYP[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="NPP"] <- ifelse(sign(modDF.sum$NPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$NPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### NEP
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### RHET
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### BP
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### deltaCL
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### deltaCW
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### deltaCFR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### deltaCCR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### deltaCFLITA
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### deltaCMIC
    #outDF$A_GDAYP[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="A_GDAYP"]<=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                           modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="A_GDAYP"]>=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                       1, 0)
    #
    #outDF$B_ELMV1[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="B_ELMV1"]<=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                           modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="B_ELMV1"]>=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                       1, 0)
    #
    #outDF$C_CABLP[outDF$Variable=="deltaCMIC"] <- ifelse(modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="C_CABLP"]<=
    #                                                           (obsDF$deltaCMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                                obsDF$deltaCMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                           modDF.sum$deltaCMIC.mean[modDF.sum$ModName=="C_CABLP"]>=
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
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CGW
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CGFR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CGCR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### LAI
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CL
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CW
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CFR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CCR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CFLITA
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CMIC
    #outDF$A_GDAYP[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="A_GDAYP"]<=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                      modDF.sum$CMIC.mean[modDF.sum$ModName=="A_GDAYP"]>=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                  1, 0)
    #
    #outDF$B_ELMV1[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="B_ELMV1"]<=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                      modDF.sum$CMIC.mean[modDF.sum$ModName=="B_ELMV1"]>=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]-
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment]),
    #                                                  1, 0)
    #
    #outDF$C_CABLP[outDF$Variable=="CMIC"] <- ifelse(modDF.sum$CMIC.mean[modDF.sum$ModName=="C_CABLP"]<=
    #                                                      (obsDF$CMIC[obsDF$Group=="mean"&obsDF$Trt==treatment]+
    #                                                           obsDF$CMIC[obsDF$Group=="sd"&obsDF$Trt==treatment])&
    #                                                      modDF.sum$CMIC.mean[modDF.sum$ModName=="C_CABLP"]>=
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
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PL
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PW
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PFR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PCR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PFLITA
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PSOIL
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PPORG
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PPMIN
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PDEM
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PGL
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PGW
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PGFR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PGCR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PLITIN
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PWLIN
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PFRLIN
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PLAB
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PSEC
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### POCC
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PUP
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PRETR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PMIN
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PLEACH
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### PUE
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CPL
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CPW
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CPFR
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CPSOIL
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    ### CPFLIT
    outDF$A_GDAYP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="A_GDAYP"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$B_ELMV1[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="B_ELMV1"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$C_CABLP[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="C_CABLP"])==
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
    
    outDF$G_OCHDX[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="G_OCHDXs"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    outDF$H_QUJSM[outDF$Variable=="GPP"] <- ifelse(sign(modDF.sum$GPP.mean[modDF.sum$ModName=="H_QUJSM"])==
                                                       sign(obsDF$GPP[obsDF$Group=="mean"&obsDF$Trt==treatment]),
                                                   1, 0)
    
    
    return(outDF)
    
}