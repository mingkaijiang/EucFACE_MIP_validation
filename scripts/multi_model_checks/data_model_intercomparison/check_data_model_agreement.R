check_data_model_agreement <- function (scenario, eucDF, rev.sd) {
    
    
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
    
    
    ### revise sd to rev.sd times the original value
    num.col <- ncol(obsDF)
    obsDF[obsDF$Group=="sd"&obsDF$Trt=="aCO2", 3:num.col] <- obsDF[obsDF$Group=="sd"&obsDF$Trt=="aCO2", 3:num.col] * rev.sd
    obsDF[obsDF$Group=="sd"&obsDF$Trt=="diff", 3:num.col] <- obsDF[obsDF$Group=="sd"&obsDF$Trt=="diff", 3:num.col] * rev.sd
    
    
    ### read in annual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_ELE_annual.rds"))
    
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
    ambDF$deltaCVEG <- rowSums(ambDF[,c("deltaCL","deltaCW","deltaCFR","deltaCCR","deltaCSTOR")], na.rm=T)
    eleDF$deltaCVEG <- rowSums(eleDF[,c("deltaCL","deltaCW","deltaCFR","deltaCCR","deltaCSTOR")], na.rm=T)
    
    ## biomass production
    ambDF$deltaPVEG <- rowSums(ambDF[,c("deltaPL","deltaPW","deltaPFR","deltaPCR","deltaPSTOR")], na.rm=T)
    eleDF$deltaPVEG <- rowSums(eleDF[,c("deltaPL","deltaPW","deltaPFR","deltaPCR","deltaPSTOR")], na.rm=T)
    
    
    ### add microbial pools, fluxes and CP ratios
    # CMIC and PMIC
    ambDF$CMIC <- NA
    eleDF$CMIC <- NA
    
    ambDF$PMIC <- NA
    eleDF$PMIC <- NA
    
    # deltaCMIC and deltaPMIC
    ambDF$deltaCMIC <- NA
    eleDF$deltaCMIC <- NA
    
    ambDF$deltaPMIC <- NA
    eleDF$deltaPMIC <- NA
    
    # CPMIC
    ambDF$CPMIC <- NA
    eleDF$CPMIC <- NA
    
    
    ### read in microbial dataset
    ### because we are only interested in the general variable responses. 
    tDF1 <- readRDS(paste0("output/MIP_output/processed_simulation/microbial_models/MIP_OBS_", scenario, "_AMB_annual.rds"))
    tDF2 <- readRDS(paste0("output/MIP_output/processed_simulation/microbial_models/MIP_OBS_", scenario, "_ELE_annual.rds"))
    
    ### select model output
    tDF1 <- subset(tDF1, ModName%in%c("E_OCHDP", "F_QUINC", 
                                        "G_OCHDX", "H_QUJSM"))
    
    
    tDF2 <- subset(tDF2, ModName%in%c("E_OCHDP", "F_QUINC", 
                                        "G_OCHDX", "H_QUJSM"))
    
    ### just to plot NMIC and PMIC pool and their changes in aCO2 and eCO2
    tDF1$Trt <- "amb"
    tDF2$Trt <- "ele"
    
    myDF <- rbind(tDF1, tDF2)
    
    myDF <- myDF[,c("ModName", "YEAR", "Trt",
                    "CMIC10", "CMIC30", "CMIC60",
                    "NMIC10", 
                    "PMIC10", "PMIC30", "PMIC60",
                    "CMICR", "NMICR", "PMICR",
                    "deltaCMIC10", "deltaCMIC30", "deltaCMIC60",
                    "deltaPMIC10", "deltaPMIC30", "deltaPMIC60",
                    "deltaCMICR",  "deltaPMICR")]
    
    myDF$CMIC10 <- ifelse(is.na(myDF$CMIC10), myDF$CMICR, myDF$CMIC10)
    myDF$NMIC10 <- ifelse(is.na(myDF$NMIC10), myDF$NMICR, myDF$NMIC10)
    myDF$PMIC10 <- ifelse(is.na(myDF$PMIC10), myDF$PMICR, myDF$PMIC10)
    
    myDF$deltaCMIC10 <- ifelse(is.na(myDF$deltaCMIC10), myDF$deltaCMICR, myDF$deltaCMIC10)
    myDF$deltaPMIC10 <- ifelse(is.na(myDF$deltaPMIC10), myDF$deltaPMICR, myDF$deltaPMIC10)
    

    #tDF3 <- summaryBy(CMIC10+NMIC10+PMIC10+deltaCMIC10+deltaPMIC10~ModName+Trt,
    #                   FUN=mean, data=myDF, na.rm=T, keep.names=T)
    #
    #tDF3 <- tDF3[tDF3$ModName%in%c("G_OCHDX","H_QUJSM"),]
    
    ### add microbial simulation output
    for (i in c("G_OCHDX", "H_QUJSM")) {
        for (k in 2013:2016) {
            ambDF$CMIC[ambDF$ModName==i&ambDF$YEAR==k] <- myDF$CMIC10[myDF$ModName==i&myDF$YEAR==k&myDF$Trt=="amb"]
            eleDF$CMIC[eleDF$ModName==i&eleDF$YEAR==k] <- myDF$CMIC10[myDF$ModName==i&myDF$YEAR==k&myDF$Trt=="ele"]
            
            ambDF$PMIC[ambDF$ModName==i&ambDF$YEAR==k] <- myDF$PMIC10[myDF$ModName==i&myDF$YEAR==k&myDF$Trt=="amb"]
            eleDF$PMIC[eleDF$ModName==i&eleDF$YEAR==k] <- myDF$PMIC10[myDF$ModName==i&myDF$YEAR==k&myDF$Trt=="ele"]
            
            ambDF$deltaCMIC[ambDF$ModName==i&ambDF$YEAR==k] <- myDF$deltaCMIC10[myDF$ModName==i&myDF$YEAR==k&myDF$Trt=="amb"]
            eleDF$deltaCMIC[eleDF$ModName==i&eleDF$YEAR==k] <- myDF$deltaCMIC10[myDF$ModName==i&myDF$YEAR==k&myDF$Trt=="ele"]
            
            ambDF$deltaPMIC[ambDF$ModName==i&ambDF$YEAR==k] <- myDF$deltaPMIC10[myDF$ModName==i&myDF$YEAR==k&myDF$Trt=="amb"]
            eleDF$deltaPMIC[eleDF$ModName==i&eleDF$YEAR==k] <- myDF$deltaPMIC10[myDF$ModName==i&myDF$YEAR==k&myDF$Trt=="ele"]
        }
    }
    
    
    ## CP ratios
    ambDF$CPMIC <- with(ambDF, CMIC/PMIC)
    eleDF$CPMIC <- with(eleDF, CMIC/PMIC)
    
    
    #obsDF$deltaCVEG <- NA
    #obsDF$deltaCVEG[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] <- obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] 
    #
    #obsDF$deltaCVEG[obsDF$Group=="mean"&obsDF$Trt=="eCO2"] <- obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt=="eCO2"] +
    #    obsDF$deltaCW[obsDF$Group=="mean"&obsDF$Trt=="eCO2"] +
    #    obsDF$deltaCFR[obsDF$Group=="mean"&obsDF$Trt=="eCO2"] +
    #    obsDF$deltaCCR[obsDF$Group=="mean"&obsDF$Trt=="eCO2"]
    #
    #obsDF$deltaCVEG[obsDF$Group=="sd"&obsDF$Trt=="aCO2"] <- obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCW[obsDF$Group=="sd"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCFR[obsDF$Group=="sd"&obsDF$Trt=="aCO2"] +
    #    obsDF$deltaCCR[obsDF$Group=="sd"&obsDF$Trt=="aCO2"] 
    #
    #obsDF$deltaCVEG[obsDF$Group=="sd"&obsDF$Trt=="eCO2"] <- obsDF$deltaCL[obsDF$Group=="mean"&obsDF$Trt=="eCO2"] +
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
    
    
    ### GPP use efficiency
    ambDF$GPP_use <- with(ambDF, GPP/PGL)
    eleDF$GPP_use <- with(eleDF, GPP/PGL)
    
    
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
    
    
    ### get new dimension
    d <- dim(ambDF)[2]
    
    
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
    
    
    
    ### calculate multi-model mean
    ## ambDF
    n <- (dim(ambDF.sum)[2]-1)/2+1
    tmpDF <- ambDF.sum[ambDF.sum$ModName=="C_GDAYP",]
    tmpDF$ModName <- "I_MM"
    subDF <- ambDF.sum[,1:n]
    subDF$MM <- "I_MM"
    
    tmpDF1 <- summaryBy(.~MM, data=subDF, FUN=c(mean,sd),
                        keep.names=T, na.rm=T)

    names(tmpDF1) <- names(ambDF.sum)
    ambDF.sum <- rbind(ambDF.sum, tmpDF1)
    
    
    
    ## eleDF
    n <- (dim(eleDF.sum)[2]-1)/2+1
    tmpDF <- eleDF.sum[eleDF.sum$ModName=="C_GDAYP",]
    tmpDF$ModName <- "I_MM"
    subDF <- eleDF.sum[,1:n]
    subDF$MM <- "I_MM"
    
    tmpDF1 <- summaryBy(.~MM, data=subDF, FUN=c(mean,sd),
                        keep.names=T, na.rm=T)
    
    names(tmpDF1) <- names(eleDF.sum)
    eleDF.sum <- rbind(eleDF.sum, tmpDF1)
    
    
    ## annDF.diff.sum
    n <- (dim(annDF.diff.sum)[2]-1)/2+1
    tmpDF <- annDF.diff.sum[annDF.diff.sum$ModName=="C_GDAYP",]
    tmpDF$ModName <- "I_MM"
    subDF <- annDF.diff.sum[,1:n]
    subDF$MM <- "I_MM"
    
    tmpDF1 <- summaryBy(.~MM, data=subDF, FUN=c(mean,sd),
                        keep.names=T, na.rm=T)
    
    names(tmpDF1) <- names(annDF.diff.sum)
    annDF.diff.sum <- rbind(annDF.diff.sum, tmpDF1)
    
    
    ## pctDF.diff.sum
    n <- (dim(annDF.pct.diff.sum)[2]-1)/2+1
    tmpDF <- annDF.pct.diff.sum[annDF.pct.diff.sum$ModName=="C_GDAYP",]
    tmpDF$ModName <- "I_MM"
    subDF <- annDF.pct.diff.sum[,1:n]
    subDF$MM <- "I_MM"
    
    tmpDF1 <- summaryBy(.~MM, data=subDF, FUN=c(mean,sd),
                        keep.names=T, na.rm=T)
    
    names(tmpDF1) <- names(annDF.pct.diff.sum)
    annDF.pct.diff.sum <- rbind(annDF.pct.diff.sum, tmpDF1)
    
    
    
    
    ### get the list of models
    mod.list <- unique(ambDF.sum$ModName)
    nmod <- length(mod.list)
    
    
    
    ### prepare outDF
    data.variable.list <- c("GPP", "NPP", "NEP", "RHET", 
                            "deltaCVEG", 
                            "deltaCL", "deltaCW", "deltaCFR", "deltaCCR",
                            "deltaCFLITA", "deltaCMIC", 
                            "CGL", "CGW", "CGFR", "CGCR", 
                            "LAI", 
                            "CL", "CW", "CFR", "CCR", "CFLITA", "CMIC", "CSOIL",
                            "PL", "PW", "PFR", "PCR", "PFLITA", "PSOIL", "PPORG", "PPMIN",
                            "PMIC",
                            "PDEM", "PGL", "PGW", "PGFR", "PGCR", 
                            "deltaPVEG", 
                            "PLITIN", "PWLIN", "PFRLIN",
                            "PLAB", "PSEC", "POCC", "PUP", "PRETR", "PMIN", "PLEACH", 
                            "PUE", "GPP_use",
                            "CPL", "CPW", "CPFR", "CPSOIL", "CPFLIT", "CPMIC")
    
    
    outDF1 <- data.frame("Variable"=data.variable.list,
                         "A_ELMV1"=NA, "B_CABLP"=NA, 
                         "C_GDAYP"=NA, "D_LPJGP"=NA,
                         "E_OCHDP"=NA, "F_QUINC"=NA,
                         "G_OCHDX"=NA, "H_QUJSM"=NA,
                         "I_MM"=NA)
    
    outDF4 <- outDF3 <- outDF2 <- outDF1
    
    ### assign model output and check with obsDF
    ## aCO2
    outDF1 <- assign_model_output_and_check_with_obs(obsDF=obsDF, outDF=outDF1,
                                                     modDF.sum=ambDF.sum, treatment="aCO2")
    
    
    ## eCO2
    outDF2 <- assign_model_output_and_check_with_obs(obsDF=obsDF, outDF=outDF2,
                                                     modDF.sum=eleDF.sum, treatment="eCO2")
    
    ## eCO2 - aCO2 magnitude
    outDF3 <- assign_model_output_and_check_with_obs(obsDF=obsDF, outDF=outDF3,
                                                     modDF.sum=annDF.diff.sum, treatment="diff")
    
    ## sign of CO2 response
    outDF4 <- assign_model_output_and_check_sign_agreement_with_obs(obsDF=obsDF, outDF=outDF4,
                                                                    modDF.sum=annDF.diff.sum, treatment="diff")
    
    
    ### prepare plot labels
    y.limits.list <- c("CPSOIL", "CPMIC", "CPFLIT", "CPFR", "CPW", "CPL", 
                       "PUE",  "GPP_use",
                       "POCC", "PSEC", "PLAB",
                       "deltaPVEG",
                       "PLEACH", "PMIN", "PRETR", "PUP", "PDEM",
                       "PFRLIN", "PWLIN", "PLITIN", 
                       "PGCR", "PGFR", "PGW", "PGL", 
                       "PPMIN", "PPORG", "PSOIL", "PMIC",
                       "PFLITA", "PCR", "PFR", "PW", "PL", 
                       "CSOIL", "CMIC", 
                       "CFLITA","CCR", "CFR", "CW", "CL", 
                       "LAI", 
                       "CGCR", "CGFR", "CGW", "CGL", 
                       #"deltaCL", "deltaCW", "deltaCFR", "deltaCCR",
                       #"deltaCFLITA", "deltaCMIC", 
                       "RHET", "NEP", "deltaCVEG", "NPP", "GPP")

    y.labels.list <- c("GPP"="GPP",
                      "NPP"="NPP", 
                      "deltaCVEG"=expression(Delta*C[veg]), 
                      "NEP"="NEP", 
                      "RHET"=expression(R[het]), 
                      #"deltaCL", "deltaCW", "deltaCFR", "deltaCCR",
                      #"deltaCFLITA", "deltaCMIC", 
                      "CGL"=expression(NPP[leaf]), 
                      "CGW"=expression(NPP[wood]), 
                      "CGFR"=expression(NPP[froot]), 
                      "CGCR"=expression(NPP[croot]), 
                      "LAI"="LAI", 
                      "CL"=expression(C[leaf]), 
                      "CW"=expression(C[wood]), 
                      "CFR"=expression(C[froot]), 
                      "CCR"=expression(C[croot]), 
                      "CFLITA"=expression(C[leaflit]), 
                      "CMIC"=expression(C[mic]), 
                      "CSOIL"=expression(C[soil]),
                      "PL"=expression(P[leaf]), 
                      "PW"=expression(P[wood]), 
                      "PFR"=expression(P[froot]), 
                      "PCR"=expression(P[croot]), 
                      "PFLITA"=expression(P[leaflit]), 
                      "PMIC"=expression(P[mic]),
                      "PSOIL"=expression(P[soil]), 
                      "PPORG"=expression(P[org]), 
                      "PPMIN"=expression(P[inorg]),
                      "PDEM"=expression(P[dem]), 
                      "PUP"=expression(P[upt]),
                      "PRETR"=expression(P[retr]), 
                      "PMIN"=expression(P[net]), 
                      "PLEACH"=expression(P[leach]),
                      "deltaPVEG"=expression(Delta * P[veg]),
                      "PGL"=expression(P[gleaf]), 
                      "PGW"=expression(P[gwood]), 
                      "PGFR"=expression(P[gfroot]), 
                      "PGCR"=expression(P[gcroot]), 
                      "PLITIN"=expression(P[leaflit]), 
                      "PWLIN"=expression(P[woodlit]), 
                      "PFRLIN"=expression(P[frootlit]),
                      "PLAB"=expression(P[lab]), 
                      "PSEC"=expression(P[sec]), 
                      "POCC"=expression(P[occ]), 
                      "PUE"=expression(PUE[NPP]), 
                      "GPP_use"=expression(PUE[GPP]),
                      "CPL"=expression(CP[leaf]), 
                      "CPW"=expression(CP[wood]), 
                      "CPFR"=expression(CP[froot]), 
                      "CPMIC"=expression(CP[mic]),
                      "CPSOIL"=expression(CP[soil]), 
                      "CPFLIT"=expression(CP[leaflit]))
    
    
    
    
    ### melt
    plotDF1 <- reshape2::melt(outDF1, id.vars=c("Variable"),
                              variable.name="ModName")
    
    plotDF3 <- reshape2::melt(outDF3, id.vars=c("Variable"),
                              variable.name="ModName")
    
    plotDF4 <- reshape2::melt(outDF4, id.vars=c("Variable"),
                              variable.name="ModName")
    
    
    
    plotDF1 <- plotDF1[plotDF1$Variable%in%y.limits.list,]
    
    plotDF3 <- plotDF3[plotDF3$Variable%in%y.limits.list,]
    
    plotDF4 <- plotDF4[plotDF4$Variable%in%y.limits.list,]
    
    
    ### as character
    plotDF1$value <- as.character(plotDF1$value)
    plotDF3$value <- as.character(plotDF3$value)
    plotDF4$value <- as.character(plotDF4$value)
    
    plotDF5 <- merge(plotDF1, plotDF3, by=c("Variable", "ModName"))
    plotDF5 <- merge(plotDF5, plotDF4, by=c("Variable", "ModName"))
    colnames(plotDF5) <- c("Variable", "ModName", "aCO2", "CO2_magnitude", "CO2_sign")
    
    ### prepare subsets
    y.limits.sub.list1 <- c("CSOIL", "CMIC",
                            "CFLITA", 
                            "CFR", "CW", "CL", 
                            "LAI",
                            "CGFR", "CGW", "CGL", 
                            "RHET", "deltaCVEG", "NPP", "GPP", "NEP")
    
    y.limits.sub.list3 <- c("deltaPVEG",
                            "PLEACH", 
                            "PLITIN", 
                            "PGFR", "PGW", "PGL", 
                            "PMIN", "PRETR", "PUP", "PDEM")
    
    y.limits.sub.list4 <- c("PLAB",
                            "PPMIN", "PPORG", "PSOIL", "PMIC",
                            "PFLITA", 
                            "PFR", "PW", "PL")
    
    
    y.limits.sub.list5 <- c("PUE", 
                            "GPP_use",
                            "CPSOIL", "CPMIC",
                            "CPFLIT", "CPFR", "CPW", "CPL")
    
    
    ### prepare to count the number of variables making correct predictions
    plotDF5$count2 <- ifelse(plotDF5$CO2_magnitude==1&plotDF5$CO2_sign==1,1,0)
    plotDF5$count3 <- ifelse(plotDF5$CO2_magnitude==1&plotDF5$CO2_sign==1&plotDF5$aCO2==1,1,0)
    plotDF5$xlab <- rep(1:9)
    
    ### subset the data
    subDF1.1 <- plotDF1[plotDF1$Variable%in%y.limits.sub.list1,]
    subDF1.3 <- plotDF1[plotDF1$Variable%in%y.limits.sub.list3,]
    subDF1.4 <- plotDF1[plotDF1$Variable%in%y.limits.sub.list4,]
    subDF1.5 <- plotDF1[plotDF1$Variable%in%y.limits.sub.list5,]
    
    
    subDF5.1 <- plotDF5[plotDF5$Variable%in%y.limits.sub.list1,]
    subDF5.3 <- plotDF5[plotDF5$Variable%in%y.limits.sub.list3,]
    subDF5.4 <- plotDF5[plotDF5$Variable%in%y.limits.sub.list4,]
    subDF5.5 <- plotDF5[plotDF5$Variable%in%y.limits.sub.list5,]
    
    
    ### calculate the number of variables that reproduce the data
    sumDF1 <- summaryBy(count2+count3~ModName+xlab, data=subDF5.1, 
                        na.rm=T, keep.names=T, FUN=sum)

    sumDF3 <- summaryBy(count2+count3~ModName+xlab, data=subDF5.3, 
                        na.rm=T, keep.names=T, FUN=sum)
    
    sumDF4 <- summaryBy(count2+count3~ModName+xlab, data=subDF5.4, 
                        na.rm=T, keep.names=T, FUN=sum)
    
    sumDF5 <- summaryBy(count2+count3~ModName+xlab, data=subDF5.5, 
                        na.rm=T, keep.names=T, FUN=sum)
    
    
    ### prepare ylabDF
    ylabDF1 <- data.frame("Variable"=y.limits.sub.list1,
                          "count"=NA)
       
    ylabDF3 <- data.frame("Variable"=y.limits.sub.list3,
                          "count"=NA)
    
    ylabDF4 <- data.frame("Variable"=y.limits.sub.list4,
                          "count"=NA)
    
    ylabDF5 <- data.frame("Variable"=y.limits.sub.list5,
                          "count"=NA)
        
        
    for (i in y.limits.sub.list1) {
        ## ignore MM
        tmpDF <- subDF5.1[subDF5.1$ModName!="I_MM",]
        ylabDF1$count[ylabDF1$Variable==i] <- sum(tmpDF$count2[tmpDF$Variable==i], na.rm=T)
    }
    
    for (i in y.limits.sub.list3) {
        ## ignore MM
        tmpDF <- subDF5.3[subDF5.3$ModName!="I_MM",]
        ylabDF3$count[ylabDF3$Variable==i] <- sum(tmpDF$count2[tmpDF$Variable==i], na.rm=T)
    }
    
    for (i in y.limits.sub.list4) {
        ## ignore MM
        tmpDF <- subDF5.4[subDF5.4$ModName!="I_MM",]
        ylabDF4$count[ylabDF4$Variable==i] <- sum(tmpDF$count2[tmpDF$Variable==i], na.rm=T)
    }
    
    for (i in y.limits.sub.list5) {
        ## ignore MM
        tmpDF <- subDF5.5[subDF5.5$ModName!="I_MM",]
        ylabDF5$count[ylabDF5$Variable==i] <- sum(tmpDF$count2[tmpDF$Variable==i], na.rm=T)
    }
    
    
    ### revise y label to continuous variable
    ylabDF1$ylab <- c(1:dim(ylabDF1)[1])
    ylabDF3$ylab <- c(1:dim(ylabDF3)[1])
    ylabDF4$ylab <- c(1:dim(ylabDF4)[1])
    ylabDF5$ylab <- c(1:dim(ylabDF5)[1])
    
    ### same
    for (i in y.limits.sub.list1) {
        subDF5.1$ylab[subDF5.1$Variable==i] <- ylabDF1$ylab[ylabDF1$Variable==i]
    }
    
    for (i in y.limits.sub.list3) {
        subDF5.3$ylab[subDF5.3$Variable==i] <- ylabDF3$ylab[ylabDF3$Variable==i]
    }
    
    for (i in y.limits.sub.list4) {
        subDF5.4$ylab[subDF5.4$Variable==i] <- ylabDF4$ylab[ylabDF4$Variable==i]
    }
    
    for (i in y.limits.sub.list5) {
        subDF5.5$ylab[subDF5.5$Variable==i] <- ylabDF5$ylab[ylabDF5$Variable==i]
    }
        
        
    
    
    ##################################################################    
    ### plot labelling
    
    model.labels <- c("A_ELMV1" = "ELMV1",
                      "B_CABLP" = "CABLP",
                      "C_GDAYP" = "GDAYP",
                      "D_LPJGP" = "LPJGP",
                      "E_OCHDP" = "OCDHP",
                      "F_QUINC" = "QUINC",
                      "G_OCHDX" = "OCHDX",
                      "H_QUJSM" = "QUJSM",
                      "I_MM" = expression(bold("M-M")))
    
    x.labels <- c("ELMV1",
                  "CABLP",
                  "GDAYP",
                  "LPJGP",
                  "OCDHP",
                  "QUINC",
                  "OCHDX",
                  "QUJSM",
                  expression(bold("M-M")))
    
    
    
    y.labels.list1 <- rev(c( "NEP",
                             "GPP",
                            "NPP", 
                            expression(Delta*C[veg]), 
                            expression(R[het]), 
                            expression(NPP[leaf]), 
                            expression(NPP[wood]), 
                            expression(NPP[froot]), 
                            "LAI", 
                            expression(C[leaf]), 
                            expression(C[wood]), 
                            expression(C[froot]), 
                            expression(C[leaflit]), 
                            expression(C[mic]),
                            expression(C[soil])))
    
    
    y.labels.list3 <- rev(c(expression(P[dem]), 
                            expression(P[upt]),
                            expression(P[retr]), 
                            expression(P[net]), 
                            expression(P[gleaf]), 
                            expression(P[gwood]), 
                            expression(P[gfroot]), 
                            expression(P[leaflit]), 
                            expression(P[leach]),
                            expression(Delta * P[veg])))
    
    y.labels.list4 <- rev(c(expression(P[leaf]), 
                            expression(P[wood]), 
                            expression(P[froot]), 
                            expression(P[leaflit]), 
                            expression(P[mic]),
                            expression(P[soil]), 
                            expression(P[org]), 
                            expression(P[inorg]),
                            expression(P[lab])))
    
    
    y.labels.list5 <- rev(c(expression(CP[leaf]), 
                            expression(CP[wood]), 
                            expression(CP[froot]), 
                            expression(CP[leaflit]),
                            expression(CP[mic]),
                            expression(CP[soil]),
                            expression(PUE[GPP]),
                            expression(PUE[NPP])))
    
    
    ##################################################################
    ### make plot
    
    p1 <- ggplot() + 
        #geom_tile(data = subDF5.1, aes(x=ModName, y=Variable, fill=aCO2), color="white",
        #          width=1)+
        #geom_point(data=subDF5.1, aes(x=ModName, y=Variable, pch=CO2_magnitude, color=CO2_sign),
        #           size=4)+
        geom_tile(data = subDF5.1, aes(x=xlab, y=ylab, fill=aCO2), color="white",
                  width=1)+
        geom_point(data=subDF5.1, aes(x=xlab, y=ylab, size=CO2_magnitude, pch=CO2_sign))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x.bottom=element_text(size=16,angle = 45, 
                                              vjust = 1, hjust = 1),
              axis.text.x.top=element_text(size=16,angle = 0,
                                           vjust = 0.5, hjust = 0.5),
              axis.title.x=element_text(size=16),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=16),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("")+
        xlab("")+
        #scale_x_discrete(limit=mod.list,label=model.labels)+
        #scale_y_discrete(limit=y.limits.sub.list1,label=y.labels.list)+
        scale_x_continuous(breaks=c(1:9),label=x.labels,
                           sec.axis = dup_axis(labels=sumDF1$count2))+
        scale_y_continuous(breaks=c(1:dim(ylabDF1)[1]),label=y.labels.list1,
                           sec.axis = dup_axis(labels=ylabDF1$count))+
        #scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
        #                   values=c("0"=21,
        #                            "1"=19),
        #                   labels=c("0"="Outside data SD range",
        #                            "1"="Inside data SD range"))+
        #scale_size_manual(name=expression("Predicted " * CO[2] * " response sign"),
        #                   values=c("0"=1,
        #                            "1"=4),
        #                   labels=c("0"="Inconsistent with data",
        #                            "1"="Consistent with data"))+
        scale_shape_manual(name=expression("Predicted " * CO[2] * " response sign"),
                           values=c("0"=21,
                                    "1"=19),
                           labels=c("0"="Inconsistent with data",
                                    "1"="Consistent with data"))+
        scale_size_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                          values=c("0"=1,
                                   "1"=4),
                          labels=c("0"="Outside data SD range",
                                   "1"="Inside data SD range"))+
        scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
                          values=c("0"="#DFF8D5",
                                   "1"="#00D971"),
                          labels=c("0"="Outside data SD range",
                                   "1"="Inside data SD range"))+
        coord_fixed()
    
    #plot(p1)
    
    
    
    p1_1 <- ggplot() + 
        geom_tile(data = subDF5.1, aes(x=xlab, y=ylab, fill=CO2_sign), color="white",
                  width=1)+
        geom_point(data=subDF5.1, aes(x=xlab, y=ylab, pch=CO2_magnitude),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x.bottom=element_text(size=16,angle = 45, 
                                              vjust = 1, hjust = 1),
              axis.text.x.top=element_text(size=16,angle = 0,
                                           vjust = 0.5, hjust = 0.5),
              axis.title.x=element_text(size=16),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=16),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("")+
        xlab("")+
        scale_x_continuous(breaks=c(1:9),label=x.labels,
                           sec.axis = dup_axis(labels=sumDF1$count2))+
        scale_y_continuous(breaks=c(1:dim(ylabDF1)[1]),label=y.labels.list1,
                           sec.axis = dup_axis(labels=ylabDF1$count))+
        #scale_y_discrete(limit=y.limits.sub.list1,label=y.labels.list)+
        scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                           values=c("0"=21,
                                    "1"=19),
                           labels=c("0"="Outside data SD range",
                                    "1"="Within data SD range"))+
        scale_fill_manual(name=expression("Predicted " * CO[2] * " sign"),
                          values=c("0"="#DFF8D5",
                                   "1"="#00D971"),#"darkgreen"),
                          #values=c("0"="#E8D5B5", #"burlywood1",
                          #         "1"="#00C1FE"),#"darkgreen"),
                          labels=c("0"="Inconsistent with data",
                                   "1"="Consistent with data"))+
        coord_fixed()
    
    #plot(p1_1)
    
    
    
    p3 <- ggplot() + 
        geom_tile(data = subDF5.3, aes(x=xlab, y=ylab, fill=aCO2), color="white",
                  width=1)+
        geom_point(data=subDF5.3, aes(x=xlab, y=ylab, size=CO2_magnitude, pch=CO2_sign))+
        #geom_tile(data = subDF5.3, aes(x=ModName, y=Variable, fill=aCO2), color="white",
        #          width=1)+
        #geom_point(data=subDF5.3, aes(x=ModName, y=Variable, pch=CO2_magnitude, color=CO2_sign),
        #           size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x.bottom=element_text(size=16,angle = 45, 
                                              vjust = 1, hjust = 1),
              axis.text.x.top=element_text(size=16,angle = 0,
                                           vjust = 0.5, hjust = 0.5),
              axis.title.x=element_text(size=16),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=16),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("")+
        xlab("")+
        scale_x_continuous(breaks=c(1:9),label=x.labels,
                           sec.axis = dup_axis(labels=sumDF3$count2))+
        scale_y_continuous(breaks=c(1:dim(ylabDF3)[1]),label=y.labels.list3,
                           sec.axis = dup_axis(labels=ylabDF3$count))+
        #scale_x_discrete(limit=mod.list,label=model.labels)+
        #scale_y_discrete(limit=y.limits.sub.list3,label=y.labels.list)+
        #scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
        #                   values=c("0"=19,
        #                            "1"=17),
        #                   labels=c("0"="Outside data SD range",
        #                            "1"="Inside data SD range"))+
        #scale_color_manual(name=expression("Predicted " * CO[2] * " response sign"),
        #                   values=c("0"="brown",
        #                            "1"="black"),
        #                   labels=c("0"="Inconsistent with data",
        #                            "1"="Consistent with data"))+
        #scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
        #                  values=c("0"="#DFF8D5",
        #                           "1"="#00D971"),
        #                  labels=c("0"="Outside data range",
        #                           "1"="Inside data range"))+
    scale_shape_manual(name=expression("Predicted " * CO[2] * " response sign"),
                       values=c("0"=21,
                                "1"=19),
                       labels=c("0"="Inconsistent with data",
                                "1"="Consistent with data"))+
        scale_size_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                          values=c("0"=1,
                                   "1"=4),
                          labels=c("0"="Outside data SD range",
                                   "1"="Inside data SD range"))+
        scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
                          values=c("0"="#DFF8D5",
                                   "1"="#00D971"),
                          labels=c("0"="Outside data SD range",
                                   "1"="Inside data SD range"))+
        coord_fixed()
    
    
    p3_1 <- ggplot() + 
        geom_tile(data = subDF5.3, aes(x=xlab, y=ylab, fill=CO2_sign), color="white",
                  width=1)+
        geom_point(data=subDF5.3, aes(x=xlab, y=ylab, pch=CO2_magnitude),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x.bottom=element_text(size=16,angle = 45, 
                                              vjust = 1, hjust = 1),
              axis.text.x.top=element_text(size=16,angle = 0,
                                           vjust = 0.5, hjust = 0.5),
              axis.title.x=element_text(size=16),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=16),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("")+
        xlab("")+
        #scale_x_discrete(limit=mod.list,label=model.labels)+
        scale_x_continuous(breaks=c(1:9),label=x.labels,
                           sec.axis = dup_axis(labels=sumDF3$count2))+
        #scale_y_discrete(limit=y.limits.sub.list3,label=y.labels.list)+
        scale_y_continuous(breaks=c(1:dim(ylabDF3)[1]),label=y.labels.list3,
                           sec.axis = dup_axis(labels=ylabDF3$count))+
        scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                           values=c("0"=21,
                                    "1"=19),
                           labels=c("0"="Outside data SD range",
                                    "1"="Within data SD range"))+
        scale_fill_manual(name=expression("Predicted " * CO[2] * " sign"),
                          values=c("0"="#DFF8D5",
                                   "1"="#00D971"),#"darkgreen"),
                          labels=c("0"="Inconsistent with data",
                                   "1"="Consistent with data"))+
        coord_fixed()
    
    
    p4 <- ggplot() + 
        geom_tile(data = subDF5.4, aes(x=xlab, y=ylab, fill=aCO2), color="white",
                  width=1)+
        geom_point(data=subDF5.4, aes(x=xlab, y=ylab, size=CO2_magnitude, pch=CO2_sign))+
        #geom_tile(data = subDF5.4, aes(x=ModName, y=Variable, fill=aCO2), color="white",
        #          width=1)+
        #geom_point(data=subDF5.4, aes(x=ModName, y=Variable, pch=CO2_magnitude, color=CO2_sign),
        #           size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x.bottom=element_text(size=16,angle = 45, 
                                              vjust = 1, hjust = 1),
              axis.text.x.top=element_text(size=16,angle = 0,
                                           vjust = 0.5, hjust = 0.5),
              axis.title.x=element_text(size=16),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=16),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("")+
        xlab("")+
        scale_x_continuous(breaks=c(1:9),label=x.labels,
                           sec.axis = dup_axis(labels=sumDF4$count2))+
        scale_y_continuous(breaks=c(1:dim(ylabDF4)[1]),label=y.labels.list4,
                           sec.axis = dup_axis(labels=ylabDF4$count))+
        #scale_x_discrete(limit=mod.list,label=model.labels)+
        #scale_y_discrete(limit=y.limits.sub.list4,label=y.labels.list)+
        #scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
        #                   values=c("0"=19,
        #                            "1"=17),
        #                   labels=c("0"="Outside data SD range",
        #                            "1"="Inside data SD range"))+
        #scale_color_manual(name=expression("Predicted " * CO[2] * " response sign"),
        #                   values=c("0"="brown",
        #                            "1"="black"),
        #                   labels=c("0"="Inconsistent with data",
        #                            "1"="Consistent with data"))+
        #scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
        #                  values=c("0"="#DFF8D5",
        #                           "1"="#00D971"),
        #                  labels=c("0"="Outside data range",
        #                           "1"="Inside data range"))+
    scale_shape_manual(name=expression("Predicted " * CO[2] * " response sign"),
                       values=c("0"=21,
                                "1"=19),
                       labels=c("0"="Inconsistent with data",
                                "1"="Consistent with data"))+
        scale_size_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                          values=c("0"=1,
                                   "1"=4),
                          labels=c("0"="Outside data SD range",
                                   "1"="Inside data SD range"))+
        scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
                          values=c("0"="#DFF8D5",
                                   "1"="#00D971"),
                          labels=c("0"="Outside data SD range",
                                   "1"="Inside data SD range"))+
        coord_fixed()
    
    
    p4_1 <- ggplot() + 
        geom_tile(data = subDF5.4, aes(x=xlab, y=ylab, fill=CO2_sign), color="white",
                  width=1)+
        geom_point(data=subDF5.4, aes(x=xlab, y=ylab, pch=CO2_magnitude),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x.bottom=element_text(size=16,angle = 45, 
                                              vjust = 1, hjust = 1),
              axis.text.x.top=element_text(size=16,angle = 0,
                                           vjust = 0.5, hjust = 0.5),
              axis.title.x=element_text(size=16),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=16),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("")+
        xlab("")+
        #scale_x_discrete(limit=mod.list,label=model.labels)+
        scale_x_continuous(breaks=c(1:9),label=x.labels,
                           sec.axis = dup_axis(labels=sumDF4$count2))+
        #scale_y_discrete(limit=y.limits.sub.list4,label=y.labels.list)+
        scale_y_continuous(breaks=c(1:dim(ylabDF4)[1]),label=y.labels.list4,
                           sec.axis = dup_axis(labels=ylabDF4$count))+
        scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                           values=c("0"=21,
                                    "1"=19),
                           labels=c("0"="Outside data SD range",
                                    "1"="Within data SD range"))+
        scale_fill_manual(name=expression("Predicted " * CO[2] * " sign"),
                          values=c("0"="#DFF8D5",
                                   "1"="#00D971"),#"darkgreen"),
                          labels=c("0"="Inconsistent with data",
                                   "1"="Consistent with data"))+
        coord_fixed()
    
    
    
    p5 <- ggplot() + 
        geom_tile(data = subDF5.5, aes(x=xlab, y=ylab, fill=aCO2), color="white",
                  width=1)+
        geom_point(data=subDF5.5, aes(x=xlab, y=ylab, size=CO2_magnitude, pch=CO2_sign))+
        #geom_tile(data = subDF5.5, aes(x=ModName, y=Variable, fill=aCO2), color="white",
        #          width=1)+
        #geom_point(data=subDF5.5, aes(x=ModName, y=Variable, pch=CO2_magnitude, color=CO2_sign),
        #           size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x.bottom=element_text(size=16,angle = 45, 
                                              vjust = 1, hjust = 1),
              axis.text.x.top=element_text(size=16,angle = 0,
                                           vjust = 0.5, hjust = 0.5),
              axis.title.x=element_text(size=16),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=16),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("")+
        xlab("")+
        scale_x_continuous(breaks=c(1:9),label=x.labels,
                           sec.axis = dup_axis(labels=sumDF5$count2))+
        scale_y_continuous(breaks=c(1:dim(ylabDF5)[1]),label=y.labels.list5,
                           sec.axis = dup_axis(labels=ylabDF5$count))+
        #scale_x_discrete(limit=mod.list,label=model.labels)+
        #scale_y_discrete(limit=y.limits.sub.list5,label=y.labels.list)+
        #scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
        #                   values=c("0"=19,
        #                            "1"=17),
        #                   labels=c("0"="Outside data SD range",
        #                            "1"="Inside data SD range"))+
        #scale_color_manual(name=expression("Predicted " * CO[2] * " response sign"),
        #                   values=c("0"="brown",
        #                            "1"="black"),
        #                   labels=c("0"="Inconsistent with data",
        #                            "1"="Consistent with data"))+
        #scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
        #                  values=c("0"="#DFF8D5",
        #                           "1"="#00D971"),
        #                  labels=c("0"="Outside data range",
        #                           "1"="Inside data range"))+
    scale_shape_manual(name=expression("Predicted " * CO[2] * " response sign"),
                       values=c("0"=21,
                                "1"=19),
                       labels=c("0"="Inconsistent with data",
                                "1"="Consistent with data"))+
        scale_size_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                          values=c("0"=1,
                                   "1"=4),
                          labels=c("0"="Outside data SD range",
                                   "1"="Inside data SD range"))+
        scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
                          values=c("0"="#DFF8D5",
                                   "1"="#00D971"),
                          labels=c("0"="Outside data SD range",
                                   "1"="Inside data SD range"))+
        coord_fixed()
    
    
    p5_1 <- ggplot() + 
        geom_tile(data = subDF5.5, aes(x=xlab, y=ylab, fill=CO2_sign), color="white",
                  width=1)+
        geom_point(data=subDF5.5, aes(x=xlab, y=ylab, pch=CO2_magnitude),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x.bottom=element_text(size=16,angle = 45, 
                                              vjust = 1, hjust = 1),
              axis.text.x.top=element_text(size=16,angle = 0,
                                           vjust = 0.5, hjust = 0.5),
              axis.title.x=element_text(size=16),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=16),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("")+
        xlab("")+
        #scale_x_discrete(limit=mod.list,label=model.labels)+
        scale_x_continuous(breaks=c(1:9),label=x.labels,
                           sec.axis = dup_axis(labels=sumDF5$count2))+
        #scale_y_discrete(limit=y.limits.sub.list5,label=y.labels.list)+
        scale_y_continuous(breaks=c(1:dim(ylabDF5)[1]),label=y.labels.list5,
                           sec.axis = dup_axis(labels=ylabDF5$count))+
        scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                           values=c("0"=21,
                                    "1"=19),
                           labels=c("0"="Outside data SD range",
                                    "1"="Within data SD range"))+
        scale_fill_manual(name=expression("Predicted " * CO[2] * " sign"),
                          values=c("0"="#DFF8D5",
                                   "1"="#00D971"),#"darkgreen"),
                          labels=c("0"="Inconsistent with data",
                                   "1"="Consistent with data"))+
        coord_fixed()
    
    
    
    #plots_left_column <- plot_grid(p1, p2,
    #                               labels=c("a", "b"), label_x=0.1, label_y=0.95,
    #                               label_size=24,
    #                               ncol=1)
    
    legend_bottom_row <- get_legend(p1 + theme(legend.position="right",
                                               legend.box = 'horizontal',
                                               legend.box.just = 'left'))
    
    plots_top_row <- plot_grid(p1, p3, p4, p5, 
                               labels=c("", ""), label_x=0.1, label_y=0.95,
                               label_size=24,
                               ncol=4)
    
    
    pdf(paste0(out.dir, "/multi-model_comparison_agreement_with_data.pdf"), 
        width=20, height=10)
    plot_grid(plots_top_row, legend_bottom_row, rel_heights=c(1,0.2),
              ncol=1)
    dev.off()
    
    
    
    legend_bottom_row <- get_legend(p1_1 + theme(legend.position="right",
                                               legend.box = 'horizontal',
                                               legend.box.just = 'left'))
    
    plots_top_row <- plot_grid(p1_1, p3_1, p4_1, p5_1, 
                               labels=c("", ""), label_x=0.1, label_y=0.95,
                               label_size=24,
                               ncol=4)
    
    
    pdf(paste0(out.dir, "/multi-model_comparison_agreement_with_data2.pdf"), 
        width=20, height=10)
    plot_grid(plots_top_row, legend_bottom_row, rel_heights=c(1,0.2),
              ncol=1)
    dev.off()
    
    
    
    ##################################################################
    
    
    ##################################################################
    
    
}