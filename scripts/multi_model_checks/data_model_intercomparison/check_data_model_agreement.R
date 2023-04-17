check_data_model_agreement <- function (scenario, eucDF) {
    
    
    ##################################################################
    #### Set up basics
    ### setting out path to store the files
    ### this is only valid for variable climate
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario, "/")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### import obsDF
    obsDF <- eucDF
    
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_ELE_annual.rds"))
    
    d <- dim(ambDF)[2]
    
    ### remove N models
    ambDF <- ambDF[ambDF$ModName!="I_GDAYN",]
    ambDF <- ambDF[ambDF$ModName!="J_LPJGN",]
    eleDF <- eleDF[eleDF$ModName!="I_GDAYN",]
    eleDF <- eleDF[eleDF$ModName!="J_LPJGN",]
    
    #### calculate 4-yr means in the simulation datasets
    ambDF <- subset(ambDF, YEAR>2012 & YEAR<2017)
    eleDF <- subset(eleDF, YEAR>2012 & YEAR<2017)
    
    ### add more variables
    ## biomass production
    ambDF$BP <- rowSums(ambDF[,c("deltaCL","deltaCW","deltaCFR","deltaCCR")], na.rm=T)
    eleDF$BP <- rowSums(eleDF[,c("deltaCL","deltaCW","deltaCFR","deltaCCR")], na.rm=T)
    
    #obsDF$BP <- NA
    #obsDF$BP[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] <- obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] 
    #
    #obsDF$BP[obsDF$Group=="mean"&obsDF$Trt=="eCO2"] <- obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt=="eCO2"] +
    #    obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt=="eCO2"] +
    #    obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt=="eCO2"] +
    #    obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt=="eCO2"]
    #
    #obsDF$BP[obsDF$Group=="sd"&obsDF$Trt=="aCO2"] <- obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt=="aCO2"] 
    #
    #obsDF$BP[obsDF$Group=="sd"&obsDF$Trt=="eCO2"] <- obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt=="eCO2"] +
    #    obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt=="eCO2"] +
    #    obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt=="eCO2"] +
    #    obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt=="eCO2"]
    #
    
    
    
    ## PDEM
    ambDF$PDEM <- rowMeans(ambDF[,c("PGL","PGW","PGFR","PGCR")], na.rm=T)
    eleDF$PDEM <- rowMeans(eleDF[,c("PGL","PGW","PGFR","PGCR")], na.rm=T)
    #obsDF$PDEM <- with(obsDF, PGL+PGW+PGFR+PGCR)
    
    ## PUE
    ambDF$PUE <- with(ambDF, PUP/NPP)
    eleDF$PUE <- with(eleDF, PUP/NPP)
    #obsDF$PUE <- with(obsDF, PUP/NPP)
    
    
    ## CPL
    ambDF$CPL <- with(ambDF, CL/PL)
    eleDF$CPL <- with(eleDF, CL/PL)
    #obsDF$CPL <- with(obsDF, CL/PL)
    
    ambDF$CPW <- with(ambDF, CW/PW)
    eleDF$CPW <- with(eleDF, CW/PW)
    #obsDF$CPW <- with(obsDF, CW/PW)
    
    ambDF$CPFR <- with(ambDF, CFR/PFR)
    eleDF$CPFR <- with(eleDF, CFR/PFR)
    #obsDF$CPFR <- with(obsDF, CFR/PFR)
    
    ambDF$CPSOIL <- with(ambDF, CSOIL/PSOIL)
    eleDF$CPSOIL <- with(eleDF, CSOIL/PSOIL)
    #obsDF$CPSOIL <- with(obsDF, CSOIL/PSOIL)
    
    ambDF$CPFLIT <- with(ambDF, CFLITA/PFLITA)
    eleDF$CPFLIT <- with(eleDF, CFLITA/PFLITA)
    #obsDF$CPFLIT <- with(obsDF, CFLITA/PFLITA)
    
    ### PRETR
    ambDF$PRETR <- with(ambDF, PLRETR+PWRETR+PCRRETR+PFRRETR)
    eleDF$PRETR <- with(eleDF, PLRETR+PWRETR+PCRRETR+PFRRETR)
    #obsDF$PRETR <- with(obsDF, PLRETR+PWRETR+PCRETR+PFRETR)
    
    
    
    
    ### calculate difference
    annDF.diff <- ambDF
    annDF.diff[,3:d] <- eleDF[,3:d]-ambDF[,3:d]
    
    annDF.pct.diff <- ambDF
    annDF.pct.diff[,3:d] <- (eleDF[,3:d]-ambDF[,3:d])/ambDF[,3:d] * 100.0
    
    
    ### calculate means
    ambDF.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                           data=ambDF,
                           keep.names=T, na.rm=T)
    
    eleDF.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                           data=eleDF,
                           keep.names=T, na.rm=T)
    
    annDF.diff.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                                    data=annDF.diff,
                                    keep.names=T, na.rm=T)
    
    annDF.pct.diff.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                                    data=annDF.pct.diff,
                                    keep.names=T, na.rm=T)
    
    
    ### get the list of models
    mod.list <- unique(ambDF.sum$ModName)
    nmod <- length(mod.list)
    
    
    
    ### prepare outDF
    data.variable.list <- c("GPP", "NPP", "NEP", "RHET", 
                            "BP", 
                            "deltaCL", "deltaCW", "deltaCFR", "deltaCCR",
                            "deltaCFLITA", "deltaCMIC", 
                            "CGL", "CGW", "CGFR", "CGCR", 
                            "LAI", 
                            "CL", "CW", "CFR", "CCR", "CFLITA", "CMIC", "CSOIL",
                            "PL", "PW", "PFR", "PCR", "PFLITA", "PSOIL", "PPORG", "PPMIN",
                            "PDEM", "PGL", "PGW", "PGFR", "PGCR", 
                            "PLITIN", "PWLIN", "PFRLIN",
                            "PLAB", "PSEC", "POCC", "PUP", "PRETR", "PMIN", "PLEACH", 
                            "PUE", 
                            "CPL", "CPW", "CPFR", "CPSOIL", "CPFLIT")
    
    
    outDF1 <- data.frame("Variable"=data.variable.list,
                         "A_GDAYP"=NA, "B_ELMV1"=NA,
                         "C_CABLP"=NA, "D_LPJGP"=NA,
                         "E_OCHDP"=NA, "F_QUINC"=NA,
                         "G_OCHDX"=NA, "H_QUJSM"=NA)
    
    outDF4 <- outDF3 <- outDF2 <- outDF1
    
    ### assign model output and check with obsDF
    ## aCO2
    outDF1 <- assign_model_output_and_check_with_obs(obsDF=obsDF, outDF=outDF1,
                                                     modDF.sum=ambDF.sum, treatment="aCO2")
    
    
    ## eCO2
    outDF2 <- assign_model_output_and_check_with_obs(obsDF=obsDF, outDF=outDF2,
                                                     modDF.sum=eleDF.sum, treatment="eCO2")
    
    ## eCO2 - aCO2
    outDF3 <- assign_model_output_and_check_with_obs(obsDF=obsDF, outDF=outDF3,
                                                     modDF.sum=annDF.diff.sum, treatment="diff")
    
    ## sign of CO2 response
    outDF4 <- assign_model_output_and_check_sign_agreement_with_obs(obsDF=obsDF, outDF=outDF4,
                                                                    modDF.sum=annDF.diff.sum, treatment="diff")
    
    
    
    
    
}