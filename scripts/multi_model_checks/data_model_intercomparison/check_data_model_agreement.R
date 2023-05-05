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
    
    
    ### revise sd to 1.5 times the original value
    num.col <- ncol(obsDF)
    obsDF[obsDF$Group=="sd"&obsDF$Trt=="aCO2", 3:num.col] <- obsDF[obsDF$Group=="sd"&obsDF$Trt=="aCO2", 3:num.col] * rev.sd
    obsDF[obsDF$Group=="sd"&obsDF$Trt=="diff", 3:num.col] <- obsDF[obsDF$Group=="sd"&obsDF$Trt=="diff", 3:num.col] * rev.sd
    
    
    ### read in annual datasets
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
    tmpDF <- ambDF.sum[ambDF.sum$ModName=="A_GDAYP",]
    tmpDF$ModName <- "I_MM"
    subDF <- ambDF.sum[,1:n]
    subDF$MM <- "I_MM"
    
    tmpDF1 <- summaryBy(.~MM, data=subDF, FUN=c(mean,sd),
                        keep.names=T, na.rm=T)

    names(tmpDF1) <- names(ambDF.sum)
    ambDF.sum <- rbind(ambDF.sum, tmpDF1)
    
    
    
    ## eleDF
    n <- (dim(eleDF.sum)[2]-1)/2+1
    tmpDF <- eleDF.sum[eleDF.sum$ModName=="A_GDAYP",]
    tmpDF$ModName <- "I_MM"
    subDF <- eleDF.sum[,1:n]
    subDF$MM <- "I_MM"
    
    tmpDF1 <- summaryBy(.~MM, data=subDF, FUN=c(mean,sd),
                        keep.names=T, na.rm=T)
    
    names(tmpDF1) <- names(eleDF.sum)
    eleDF.sum <- rbind(eleDF.sum, tmpDF1)
    
    
    ## annDF.diff.sum
    n <- (dim(annDF.diff.sum)[2]-1)/2+1
    tmpDF <- annDF.diff.sum[annDF.diff.sum$ModName=="A_GDAYP",]
    tmpDF$ModName <- "I_MM"
    subDF <- annDF.diff.sum[,1:n]
    subDF$MM <- "I_MM"
    
    tmpDF1 <- summaryBy(.~MM, data=subDF, FUN=c(mean,sd),
                        keep.names=T, na.rm=T)
    
    names(tmpDF1) <- names(annDF.diff.sum)
    annDF.diff.sum <- rbind(annDF.diff.sum, tmpDF1)
    
    
    ## pctDF.diff.sum
    n <- (dim(annDF.pct.diff.sum)[2]-1)/2+1
    tmpDF <- annDF.pct.diff.sum[annDF.pct.diff.sum$ModName=="A_GDAYP",]
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
                            "PUE", "GPP_use",
                            "CPL", "CPW", "CPFR", "CPSOIL", "CPFLIT")
    
    
    outDF1 <- data.frame("Variable"=data.variable.list,
                         "A_GDAYP"=NA, "B_ELMV1"=NA,
                         "C_CABLP"=NA, "D_LPJGP"=NA,
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
    y.limits.list <- c("CPSOIL", "CPFLIT", "CPFR", "CPW", "CPL", 
                       "PUE",  "GPP_use",
                       "POCC", "PSEC", "PLAB",
                       "PLEACH", "PMIN", "PRETR", "PUP", "PDEM",
                       "PFRLIN", "PWLIN", "PLITIN", 
                       "PGCR", "PGFR", "PGW", "PGL", 
                       "PPMIN", "PPORG", "PSOIL", "PFLITA", "PCR", "PFR", "PW", "PL", 
                       "CSOIL", #"CMIC", 
                       "CFLITA","CCR", "CFR", "CW", "CL", 
                       "LAI", 
                       "CGCR", "CGFR", "CGW", "CGL", 
                       #"deltaCL", "deltaCW", "deltaCFR", "deltaCCR",
                       #"deltaCFLITA", "deltaCMIC", 
                       "RHET", "NEP", "BP", "NPP", "GPP")

    y.labels.list <- c("GPP"="GPP",
                      "NPP"="NPP", 
                      "BP"="BP", 
                      "NEP"="NEP", 
                      "RHET"=expression(R[het]), 
                      #"deltaCL", "deltaCW", "deltaCFR", "deltaCCR",
                      #"deltaCFLITA", "deltaCMIC", 
                      "CGL"=expression(BP[leaf]), 
                      "CGW"=expression(BP[wood]), 
                      "CGFR"=expression(BP[froot]), 
                      "CGCR"=expression(BP[croot]), 
                      "LAI"="LAI", 
                      "CL"=expression(C[leaf]), 
                      "CW"=expression(C[wood]), 
                      "CFR"=expression(C[froot]), 
                      "CCR"=expression(C[croot]), 
                      "CFLITA"=expression(C[leaflit]), #"CMIC", 
                      "CSOIL"=expression(C[soil]),
                      "PL"=expression(P[leaf]), 
                      "PW"=expression(P[wood]), 
                      "PFR"=expression(P[froot]), 
                      "PCR"=expression(P[croot]), 
                      "PFLITA"=expression(P[leaflit]), 
                      "PSOIL"=expression(P[soil]), 
                      "PPORG"=expression(P[org]), 
                      "PPMIN"=expression(P[inorg]),
                      "PDEM"=expression(P[dem]), 
                      "PUP"=expression(P[upt]),
                      "PRETR"=expression(P[retr]), 
                      "PMIN"=expression(P[net]), 
                      "PLEACH"=expression(P[leach]),
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
    
    plotDF5 <- merge(plotDF3, plotDF4, by=c("Variable", "ModName"))
    
    
    ### prepare subsets
    y.limits.sub.list1 <- c("CSOIL", "CFLITA", #"CCR", 
                            "CFR", "CW", "CL", 
                            "LAI",
                            #"CGCR", 
                            "CGFR", "CGW", "CGL", 
                            "RHET", "NEP", "BP", "NPP", "GPP")
    
    #y.limits.sub.list2 <- c()
    
    y.limits.sub.list3 <- c("PUE", 
                            "GPP_use",
                            "PLEACH", 
                            "PFRLIN", #"PWLIN", "PLITIN", 
                            #"PGCR", 
                            "PGFR", "PGW", "PGL", "PMIN", "PRETR", "PUP", "PDEM")
    
    y.limits.sub.list4 <- c(#"POCC", "PSEC", 
                            "PLAB",
                            "PPMIN", "PPORG", "PSOIL", "PFLITA", #"PCR", 
                            "PFR", "PW", "PL")
    
    
    y.limits.sub.list5 <- c("CPSOIL", "CPFLIT", "CPFR", "CPW", "CPL")
    
    
    subDF1.1 <- plotDF1[plotDF1$Variable%in%y.limits.sub.list1,]
    #subDF1.2 <- plotDF1[plotDF1$Variable%in%y.limits.sub.list2,]
    subDF1.3 <- plotDF1[plotDF1$Variable%in%y.limits.sub.list3,]
    subDF1.4 <- plotDF1[plotDF1$Variable%in%y.limits.sub.list4,]
    subDF1.5 <- plotDF1[plotDF1$Variable%in%y.limits.sub.list5,]
    
    
    subDF5.1 <- plotDF5[plotDF5$Variable%in%y.limits.sub.list1,]
    #subDF5.2 <- plotDF5[plotDF5$Variable%in%y.limits.sub.list2,]
    subDF5.3 <- plotDF5[plotDF5$Variable%in%y.limits.sub.list3,]
    subDF5.4 <- plotDF5[plotDF5$Variable%in%y.limits.sub.list4,]
    subDF5.5 <- plotDF5[plotDF5$Variable%in%y.limits.sub.list5,]
    
    
    model.labels <- c("A_GDAYP" = "GDAYP",
                      "B_ELMV1" = "ELMV1",
                      "C_CABLP" = "CABLP",
                      "D_LPJGP" = "LPJGP",
                      "E_OCHDP" = "OCDHP",
                      "F_QUINC" = "QUINC",
                      "G_OCHDX" = "OCHDX",
                      "H_QUJSM" = "QUJSM",
                      "I_MM" = "MM")
    
    
    ##################################################################
    ### make plot
    
    p1 <- ggplot() + 
        geom_tile(data = subDF1.1, aes(x=ModName, y=Variable, fill=value), color="white",
                  width=1)+
        geom_point(data=subDF5.1, aes(x=ModName, y=Variable, pch=value.x, color=value.y),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=16,angle = 45, 
                                       vjust = 1, hjust = 1),
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
        scale_x_discrete(limit=mod.list,label=model.labels)+
        scale_y_discrete(limit=y.limits.sub.list1,label=y.labels.list)+
        scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                           values=c("0"=19,
                                    "1"=17),
                           labels=c("0"="Outside data SD range",
                                    "1"="Inside data SD range"))+
        scale_color_manual(name=expression("Predicted " * CO[2] * " response sign"),
                           values=c("0"="brown",
                                    "1"="black"),
                           labels=c("0"="Inconsistent with data",
                                    "1"="Consistent with data"))+
        scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
                          values=c("0"="burlywood1",
                                   "1"="green3"),
                          labels=c("0"="Outside data SD range",
                                   "1"="Inside data SD range"))+
        coord_fixed()
    
    
    #p2 <- ggplot() + 
    #    geom_tile(data = subDF1.2, aes(x=ModName, y=Variable, fill=value), color="white",
    #              width=1)+
    #    geom_point(data=subDF5.2, aes(x=ModName, y=Variable, pch=value.x, color=value.y),
    #               size=4)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_text(size=16,angle = 45, 
    #                                   vjust = 1, hjust = 1),
    #          axis.title.x=element_text(size=16),
    #          axis.text.y=element_text(size=16),
    #          axis.title.y=element_text(size=16),
    #          legend.text=element_text(size=16),
    #          legend.title=element_text(size=16),
    #          panel.grid.major=element_blank(),
    #          legend.position="none",
    #          legend.box = 'vertical',
    #          legend.box.just = 'left',
    #          plot.title = element_text(size=16, face="bold.italic", 
    #                                    hjust = 0.5))+
    #    ylab("")+
    #    xlab("")+
    #    scale_x_discrete(limit=mod.list,label=model.labels)+
    #    scale_y_discrete(limit=y.limits.sub.list2,label=y.labels.list)+
    #    scale_shape_manual(name=expression("Predicted " * CO[2] * " response sign"),
    #                       values=c("0"=19,
    #                                "1"=17),
    #                       labels=c("0"="Inconsistent with data",
    #                                "1"="Consistent with data"))+
    #    scale_color_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
    #                       values=c("0"="brown",
    #                                "1"="black"),
    #                       labels=c("0"="Outside data range",
    #                                "1"="Inside data range"))+
    #    scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
    #                      values=c("0"="burlywood1",
    #                               "1"="green3"),
    #                      labels=c("0"="Outside data range",
    #                               "1"="Inside data range"))
    
    
    p3 <- ggplot() + 
        geom_tile(data = subDF1.3, aes(x=ModName, y=Variable, fill=value), color="white",
                  width=1)+
        geom_point(data=subDF5.3, aes(x=ModName, y=Variable, pch=value.x, color=value.y),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=16,angle = 45, 
                                       vjust = 1, hjust = 1),
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
        scale_x_discrete(limit=mod.list,label=model.labels)+
        scale_y_discrete(limit=y.limits.sub.list3,label=y.labels.list)+
        scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                           values=c("0"=19,
                                    "1"=17),
                           labels=c("0"="Outside data SD range",
                                    "1"="Inside data SD range"))+
        scale_color_manual(name=expression("Predicted " * CO[2] * " response sign"),
                           values=c("0"="brown",
                                    "1"="black"),
                           labels=c("0"="Inconsistent with data",
                                    "1"="Consistent with data"))+
        scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
                          values=c("0"="burlywood1",
                                   "1"="green3"),
                          labels=c("0"="Outside data range",
                                   "1"="Inside data range"))+
        coord_fixed()
    
    p4 <- ggplot() + 
        geom_tile(data = subDF1.4, aes(x=ModName, y=Variable, fill=value), color="white",
                  width=1)+
        geom_point(data=subDF5.4, aes(x=ModName, y=Variable, pch=value.x, color=value.y),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=16,angle = 45, 
                                       vjust = 1, hjust = 1),
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
        scale_x_discrete(limit=mod.list,label=model.labels)+
        scale_y_discrete(limit=y.limits.sub.list4,label=y.labels.list)+
        scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                           values=c("0"=19,
                                    "1"=17),
                           labels=c("0"="Outside data SD range",
                                    "1"="Inside data SD range"))+
        scale_color_manual(name=expression("Predicted " * CO[2] * " response sign"),
                           values=c("0"="brown",
                                    "1"="black"),
                           labels=c("0"="Inconsistent with data",
                                    "1"="Consistent with data"))+
        scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
                          values=c("0"="burlywood1",
                                   "1"="green3"),
                          labels=c("0"="Outside data range",
                                   "1"="Inside data range"))+
        coord_fixed()
    
    
    
    p5 <- ggplot() + 
        geom_tile(data = subDF1.5, aes(x=ModName, y=Variable, fill=value), color="white",
                  width=1)+
        geom_point(data=subDF5.5, aes(x=ModName, y=Variable, pch=value.x, color=value.y),
                   size=4)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=16,angle = 45, 
                                       vjust = 1, hjust = 1),
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
        scale_x_discrete(limit=mod.list,label=model.labels)+
        scale_y_discrete(limit=y.limits.sub.list5,label=y.labels.list)+
        scale_shape_manual(name=expression("Predicted " * CO[2] * " response magnitude"),
                           values=c("0"=19,
                                    "1"=17),
                           labels=c("0"="Outside data SD range",
                                    "1"="Inside data SD range"))+
        scale_color_manual(name=expression("Predicted " * CO[2] * " response sign"),
                           values=c("0"="brown",
                                    "1"="black"),
                           labels=c("0"="Inconsistent with data",
                                    "1"="Consistent with data"))+
        scale_fill_manual(name=expression("Predicted " * aCO[2] * " value"),
                          values=c("0"="burlywood1",
                                   "1"="green3"),
                          labels=c("0"="Outside data range",
                                   "1"="Inside data range"))+
        coord_fixed()
    
    
    
    #plots_left_column <- plot_grid(p1, p2,
    #                               labels=c("a", "b"), label_x=0.1, label_y=0.95,
    #                               label_size=24,
    #                               ncol=1)
    
    legend_bottom_row <- get_legend(p1 + theme(legend.position="right",
                                               legend.box = 'vertical',
                                               legend.box.just = 'left'))
    
    plots_right_column <- plot_grid(p5, legend_bottom_row,
                                   labels=c("e", ""), label_x=0.1, label_y=0.95,
                                   label_size=24,
                                   ncol=1)
    
    
    pdf(paste0(out.dir, "/multi-model_comparison_agreement_with_data.pdf"), 
        width=16, height=10)
    plot_grid(p1, p3, p4, plots_right_column,
              ncol=4)
    dev.off()
    
    
    ##################################################################
    
    
    ##################################################################
    
    
}