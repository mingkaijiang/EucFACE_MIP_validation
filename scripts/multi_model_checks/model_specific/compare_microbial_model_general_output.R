compare_microbial_model_general_output <- function(scenario) {
    
    
    ##################################################################
    ### Note:
    ### the default models and the microbial models have different
    ### variable output. The microbial models have more variables to
    ### capture microbial dynamics. 
    ### The key to explore here is the C-nutrient interaction modulated by
    ### microbial processes. 
    ### So we need to report nutrient variables. 
    ### We will need additional figure on unique variables to microbial model
    ### to understand their C-nutrient dynamics more. 
    
    
    ##################################################################
    #### Set up basics
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario, "/")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    
    ### read in prepared microbial model
    ### here we don't need to read in the model-specific variables
    ### because we are only interested in the general variable responses. 
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_ELE_annual.rds"))
    
    
    ### select GDAYN, GDAYP, LPJGN, LPJGP model output
    ambDF <- subset(ambDF, ModName%in%c("E_OCHDP", "F_QUINC", 
                                        "G_OCHDX", "H_QUJSM"))
    
    
    eleDF <- subset(eleDF, ModName%in%c("E_OCHDP", "F_QUINC", 
                                        "G_OCHDX", "H_QUJSM"))
    
    
    #### calculate 4-yr means in the simulation datasets
    ambDF <- subset(ambDF, YEAR>2012 & YEAR<2017)
    eleDF <- subset(eleDF, YEAR>2012 & YEAR<2017)
    
    d <- dim(ambDF)[2]
    
    ### calculate the effect of P limitation as difference of advanced model - basic model
    diffDF <- ambDF[ambDF$ModName%in%c("E_OCHDP", "F_QUINC"),]
    
    diffDF$ModName <- gsub("E_OCHDP", "ORCHIDEE", diffDF$ModName)
    diffDF$ModName <- gsub("F_QUINC", "QUINCY", diffDF$ModName)
    
    diffDF[diffDF$ModName=="ORCHIDEE",3:d] <- ambDF[ambDF$ModName=="G_OCHDX",3:d] - ambDF[ambDF$ModName=="E_OCHDP",3:d]
    diffDF[diffDF$ModName=="QUINCY",3:d] <- ambDF[ambDF$ModName=="H_QUJSM",3:d] - ambDF[ambDF$ModName=="F_QUINC",3:d]
    
    
    ### calculate the effect of P limitation as % difference of (CNP - CN)/CN
    pctdiffDF <- diffDF
    
    pctdiffDF[pctdiffDF$ModName=="ORCHIDEE",3:d] <- diffDF[diffDF$ModName=="ORCHIDEE",3:d]/ambDF[ambDF$ModName=="E_OCHDP",3:d] * 100.0
    pctdiffDF[pctdiffDF$ModName=="QUINCY",3:d] <- diffDF[diffDF$ModName=="QUINCY",3:d]/ambDF[ambDF$ModName=="F_QUINC",3:d] * 100.0
    
    
    ### calculate the effect of CO2 effect in real magnitude, in both versions of model
    co2DF <- ambDF
    co2DF[,3:d] <- eleDF[,3:d]-ambDF[,3:d]
    
    
    ### calculate the effect of CO2 effect in %, in both versions of model
    pctco2DF <- co2DF
    pctco2DF[,3:d] <- co2DF[,3:d]/ambDF[,3:d]*100.0
    
    
    ### calculate multi-year means and sds across datasets
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    tmpDF <- rbind(ambDF, eleDF)
    
    myDF1 <- summaryBy(.~ModName+Trt, FUN=c(mean, sd), data=tmpDF,
                       keep.names=T, na.rm=T)
    
    
    myDF2 <- summaryBy(.~ModName, FUN=c(mean, sd), data=diffDF,
                       keep.names=T, na.rm=T)
    
    
    myDF3 <- summaryBy(.~ModName, FUN=c(mean, sd), data=pctdiffDF,
                       keep.names=T, na.rm=T)
    
    
    myDF4 <- summaryBy(.~ModName, FUN=c(mean, sd), data=co2DF,
                       keep.names=T, na.rm=T)
    
    
    myDF5 <- summaryBy(.~ModName, FUN=c(mean, sd), data=pctco2DF,
                       keep.names=T, na.rm=T)
    
    
    ##################################################################
    #### Plotting
    mod.list1 <- c("E_OCHDP", "F_QUINC", "G_OCHDX", "H_QUJSM")
    mod.list2 <- c("ORCHIDEE", "QUINCY")
    
    
    ##################################################################
    ### prepare vegetation biomass datasets
    vegDF1 <- data.frame(rep(c("CL", "CW", "CFR", "CCR", "CSTOR", "Total"), 8), 
                         rep(mod.list1, each=12), 
                         rep(c("amb", "ele"), each = 6), NA, NA)
    colnames(vegDF1) <- c("Variable", 
                         "Model",
                         "Trt",
                         "meanvalue",
                         "sdvalue")
    
    for (i in c("amb", "ele")) {
        vegDF1$meanvalue[vegDF1$Variable=="CL"&vegDF1$Trt==i] <- myDF1$CL.mean[myDF1$Trt==i]
        vegDF1$sdvalue[vegDF1$Variable=="CL"&vegDF1$Trt==i] <- myDF1$CL.sd[myDF1$Trt==i]
        
        vegDF1$meanvalue[vegDF1$Variable=="CW"&vegDF1$Trt==i] <- myDF1$CW.mean[myDF1$Trt==i]
        vegDF1$sdvalue[vegDF1$Variable=="CW"&vegDF1$Trt==i] <- myDF1$CW.sd[myDF1$Trt==i]
        
        vegDF1$meanvalue[vegDF1$Variable=="CCR"&vegDF1$Trt==i] <- myDF1$CCR.mean[myDF1$Trt==i]
        vegDF1$sdvalue[vegDF1$Variable=="CCR"&vegDF1$Trt==i] <- myDF1$CCR.sd[myDF1$Trt==i]
        
        vegDF1$meanvalue[vegDF1$Variable=="CFR"&vegDF1$Trt==i] <- myDF1$CFR.mean[myDF1$Trt==i]
        vegDF1$sdvalue[vegDF1$Variable=="CFR"&vegDF1$Trt==i] <- myDF1$CFR.sd[myDF1$Trt==i]
        
        vegDF1$meanvalue[vegDF1$Variable=="CSTOR"&vegDF1$Trt==i] <- myDF1$CSTOR.mean[myDF1$Trt==i]
        vegDF1$sdvalue[vegDF1$Variable=="CSTOR"&vegDF1$Trt==i] <- myDF1$CSTOR.sd[myDF1$Trt==i]
    }
    
    for (i in c("amb", "ele")) {
        for (j in mod.list1) {
            
            ### calculate means
            v1 <- sum(vegDF1$meanvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable%in%c("CL", "CW", "CCR", "CFR", "CSTOR")],
                      na.rm=T)
            
            ### calculate sd
            v2 <- sqrt(sum(vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CL"]^2,
                            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CW"]^2,
                            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CFR"]^2,
                            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CCR"]^2, 
                            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CSTOR"]^2, na.rm=T)/3)
            
            ### assign values
            vegDF1$meanvalue[vegDF1$Model==j&vegDF1$Variable=="Total"&vegDF1$Trt==i] <- v1
            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Variable=="Total"&vegDF1$Trt==i] <- v2
            
        }
    }
    
    plotDF1 <- subset(vegDF1, Variable%in%c("CL", "CW", "CFR", "CCR", "CSTOR") & Trt=="amb")
    plotDF2 <- subset(vegDF1, Variable%in%c("Total") & Trt=="amb")
    
    val1 <- round((plotDF2$meanvalue[plotDF2$Model=="G_OCHDX"]-plotDF2$meanvalue[plotDF2$Model=="E_OCHDP"])/plotDF2$meanvalue[plotDF2$Model=="E_OCHDP"]*100, 1)
    val2 <- round((plotDF2$meanvalue[plotDF2$Model=="H_QUJSM"]-plotDF2$meanvalue[plotDF2$Model=="F_QUINC"])/plotDF2$meanvalue[plotDF2$Model=="F_QUINC"]*100, 1)
    
    
    ### Plotting C pools in ambient CO2
    p1 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position="stack", col="black") +
        geom_errorbar(data=plotDF2, 
                      aes(x=Model, ymin=meanvalue-sdvalue, ymax=meanvalue+sdvalue), 
                      position="dodge", width=0.2, col="black") +
        geom_point(data=plotDF2, 
                   aes(x=Model, y=meanvalue), 
                   position="dodge", col="black", size=2, fill="white", pch=21) +
        #annotate("text", x=2, y=plotDF2$meanvalue[plotDF2$Model=="G_OCHDX"]*1.15, 
        #         label=(paste0(val1, "%")), size=10)+
        #annotate("text", x=4, y=plotDF2$meanvalue[plotDF2$Model=="H_QUJSM"]*1.15, 
        #         label=(paste0(val2, "%")), size=10)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(C[veg] * " pools (g C " * m^2*")")))+
        scale_x_discrete(limit=c("E_OCHDP","G_OCHDX", 
                                 "F_QUINC","H_QUJSM"),
                         label=c("OCHDP","OCHDX", 
                                 "QUINC","QUJSM"))+
        xlab("")+
        scale_fill_manual(name=expression(C[veg]),
                          values=c("CL"=cbbPalette[2],
                                   "CW"=cbbPalette[3],
                                   "CFR"=cbbPalette[4],
                                   "CCR"=cbbPalette[7],
                                   "CSTOR"=cbbPalette[8]),
                          labels=c("CL"=expression(C[leaf]), 
                                   "CW"=expression(C[wood]), 
                                   "CFR"=expression(C[froot]), 
                                   "CCR"=expression(C[croot]),
                                   "CSTOR"=expression(C[store])))+
        guides(fill = guide_legend(override.aes=list(fill=c("CL"=cbbPalette[2],
                                                            "CW"=cbbPalette[3],
                                                            "CFR"=cbbPalette[4],
                                                            "CCR"=cbbPalette[7],
                                                            "CSTOR"=cbbPalette[8])),
                                   nrow=5, byrow=F))+
        scale_alpha_manual(values=c("E_OCHDP" = 0.3, 
                                    "F_QUINC" = 0.3,
                                    "G_OCHDX" = 1.0, 
                                    "H_QUJSM" = 1.0),
                           label=c("OCHDP","QUINC",
                                   "OCHDX","QUJSM")); p1
    
    ### calculate CO2 pct response difference
    plotDF3 <- plotDF2
    plotDF3$meanvalue <- vegDF1$meanvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="ele"]/vegDF1$meanvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="amb"]
    plotDF3$sdvalue <- NA #sqrt((vegDF1$sdvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="ele"]^2 + vegDF1$sdvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="amb"]^2)/2)
    
    val1 <- round((plotDF3$meanvalue[plotDF3$Model=="G_OCHDX"]-plotDF3$meanvalue[plotDF3$Model=="E_OCHDP"])/plotDF3$meanvalue[plotDF3$Model=="E_OCHDP"]*100, 1)
    val2 <- round((plotDF3$meanvalue[plotDF3$Model=="H_QUJSM"]-plotDF3$meanvalue[plotDF3$Model=="F_QUINC"])/plotDF3$meanvalue[plotDF3$Model=="F_QUINC"]*100, 1)
    

    ### Plotting C pools in CO2 pct response
    p2 <- ggplot(data=plotDF3, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Model, alpha=Model), 
                 position="stack", col="black") +
        #annotate("text", x=2, y=plotDF3$meanvalue[plotDF3$Model=="B_GDAYP"]*1.01, 
        #         label=(paste0(val1, "%")), size=10)+
        #annotate("text", x=4, y=plotDF3$meanvalue[plotDF3$Model=="C_LPJGP"]*1.01, 
        #         label=(paste0(val2, "%")), size=10)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(C[veg] * " " * CO[2] *" response ratio")))+
        scale_x_discrete(limit=c("E_OCHDP","G_OCHDX", 
                                 "F_QUINC","H_QUJSM"),
                         label=c("OCHDP","OCHDX", 
                                 "QUINC","QUJSM"))+
        xlab("")+
        scale_alpha_manual(values=c("E_OCHDP" = 0.3, 
                                    "F_QUINC" = 0.3,
                                    "G_OCHDX" = 1.0, 
                                    "H_QUJSM" = 1.0),
                           label=c("OCHDP","QUINC",
                                   "OCHDX","QUJSM"))+
        scale_fill_manual(values=c("E_OCHDP" = "purple", "F_QUINC" = "orange",
                                   "G_OCHDX" = "purple", "H_QUJSM" = "orange"),
                          label=c("OCHDP","QUINC",
                                  "OCHDX","QUJSM"))+
        coord_cartesian(ylim=c(1,1.1)); p2
    
    
    ##################################################################
    ### normalized vegetation C pool responses
    norDF1 <- calculate_normalized_vegetation_pool_response(inDF=tmpDF,
                                                            pcycle=F)
    
    norDF <- calculate_normalized_delta_vegetation_pool_response(inDF=tmpDF,
                                                                 pcycle=F)
    
    myDF1 <- norDF$absDF
    
    ### prepare vegetation biomass datasets
    vegDF1 <- data.frame(rep(c("CL", "CW", "CFR", "CCR", "CSTOR", "CVEG"), 8), 
                         rep(mod.list1, each=12), 
                         rep(c("amb", "ele"), each = 6), NA, NA)
    colnames(vegDF1) <- c("Variable", 
                          "Model",
                          "Trt",
                          "meanvalue",
                          "sdvalue")
    
    for (i in c("amb", "ele")) {
        for (j in mod.list1) {
            vegDF1$meanvalue[vegDF1$Variable=="CL"&vegDF1$Trt==i&vegDF1$Model==j] <- myDF1$CL[myDF1$Trt==i&myDF1$ModName==j]
            
            vegDF1$meanvalue[vegDF1$Variable=="CW"&vegDF1$Trt==i&vegDF1$Model==j] <- myDF1$CW[myDF1$Trt==i&myDF1$ModName==j]
            
            vegDF1$meanvalue[vegDF1$Variable=="CCR"&vegDF1$Trt==i&vegDF1$Model==j] <- myDF1$CCR[myDF1$Trt==i&myDF1$ModName==j]
            
            vegDF1$meanvalue[vegDF1$Variable=="CFR"&vegDF1$Trt==i&vegDF1$Model==j] <- myDF1$CFR[myDF1$Trt==i&myDF1$ModName==j]
            
            vegDF1$meanvalue[vegDF1$Variable=="CSTOR"&vegDF1$Trt==i&vegDF1$Model==j] <- myDF1$CSTOR[myDF1$Trt==i&myDF1$ModName==j]
            
            vegDF1$meanvalue[vegDF1$Variable=="CVEG"&vegDF1$Trt==i&vegDF1$Model==j] <- myDF1$CVEG[myDF1$Trt==i&myDF1$ModName==j]
            
        }
    }
    
    
    plotDF1 <- subset(vegDF1, Variable%in%c("CL", "CW", "CFR", "CCR", "CSTOR") & Trt=="amb")
    plotDF2 <- subset(vegDF1, Variable%in%c("CVEG") & Trt=="amb")
    
    ### add sd for CVEG
    tDF <- tmpDF[,c("ModName", "YEAR", "Trt",
                    "deltaCL", "deltaCW", "deltaCFR", "deltaCCR", "deltaCSTOR")]
    
    tDF <- tDF[tDF$Trt=="amb",]
    
    tDF$CVEG <- rowSums(tDF[,4:8], na.rm=T)
    
    smDF <- summaryBy(CVEG~ModName, FUN=c(mean,sd),
                      na.rm=T, data=tDF, keep.names=T)
    colnames(smDF) <- c("Model", "meanvalue", "sdvalue")
    
    for(i in mod.list1) {
        plotDF2$sdvalue[plotDF2$Model==i] <- smDF$sdvalue[smDF$Model==i]
    }
    
    val1 <- round((plotDF2$meanvalue[plotDF2$Model=="G_OCHDX"]-plotDF2$meanvalue[plotDF2$Model=="E_OCHDP"])/plotDF2$meanvalue[plotDF2$Model=="E_OCHDP"]*100, 1)
    val2 <- round((plotDF2$meanvalue[plotDF2$Model=="H_QUJSM"]-plotDF2$meanvalue[plotDF2$Model=="F_QUINC"])/plotDF2$meanvalue[plotDF2$Model=="F_QUINC"]*100, 1)
    
    
    ### Plotting C pools in ambient CO2
    p3 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position="stack", col="black") +
        #annotate("text", x=2, y=plotDF2$meanvalue[plotDF2$Model=="B_GDAYP"]*3, 
        #         label=(paste0(val1, "%")), size=10)+
        #annotate("text", x=4, y=plotDF2$meanvalue[plotDF2$Model=="C_LPJGP"]*1.2, 
        #         label=(paste0(val2, "%")), size=10)+
        geom_errorbar(data=plotDF2, aes(x=Model,
                                        ymin=meanvalue-sdvalue,
                                        ymax=meanvalue+sdvalue),
                      position="dodge", width=0.5)+
        geom_point(data=plotDF2, aes(x=Model, y=meanvalue), col="black", pch=21, fill="white")+
        theme_linedraw() +
        geom_vline(xintercept=2.5, lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(Delta * C[veg] * " (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("E_OCHDP","G_OCHDX", 
                                 "F_QUINC","H_QUJSM"),
                         label=c("OCHDP","OCHDX", 
                                 "QUINC","QUJSM"))+
        xlab("")+
        scale_fill_manual(name=expression(C[veg]),
                          values=c("CL"=cbbPalette[2],
                                   "CW"=cbbPalette[3],
                                   "CFR"=cbbPalette[4],
                                   "CCR"=cbbPalette[7],
                                   "CSTOR"=cbbPalette[8]),
                          labels=c("CL"=expression(C[leaf]), 
                                   "CW"=expression(C[wood]), 
                                   "CFR"=expression(C[froot]), 
                                   "CCR"=expression(C[croot]),
                                   "CSTOR"=expression(C[store])))+
        guides(fill = guide_legend(override.aes=list(fill=c("CL"=cbbPalette[2],
                                                            "CW"=cbbPalette[3],
                                                            "CFR"=cbbPalette[4],
                                                            "CCR"=cbbPalette[7],
                                                            "CSTOR"=cbbPalette[8])),
                                   nrow=1, byrow=F))+
        scale_alpha_manual(values=c("E_OCHDP" = 0.3, 
                                    "F_QUINC" = 0.3,
                                    "G_OCHDX" = 1.0, 
                                    "H_QUJSM" = 1.0),
                           label=c("OCHDP","QUINC",
                                   "OCHDX","QUJSM")); p3
    
    
    ### calculate CO2 pct response difference
    plotDF3 <- plotDF2
    #plotDF3$meanvalue <- vegDF1$meanvalue[vegDF1$Variable=="CVEG"&vegDF1$Trt=="ele"]/vegDF1$meanvalue[vegDF1$Variable=="CVEG"&vegDF1$Trt=="amb"]
    
    ### add sd for CVEG
    tDF <- tmpDF[,c("ModName", "YEAR", "Trt",
                    "deltaCL", "deltaCW", "deltaCFR", "deltaCCR", "deltaCSTOR")]
    
    tDF$CVEG <- rowSums(tDF[,4:8], na.rm=T)
    
    tDF1 <- tDF[tDF$Trt=="amb",]
    tDF1$CVEG2 <- tDF$CVEG[tDF$Trt=="ele"]
    
    tDF1$abs <- with(tDF1, CVEG2-CVEG)
    tDF1$pct <- with(tDF1, (CVEG2-CVEG)/abs(CVEG) * 100)
    
    smDF <- summaryBy(abs+pct~ModName, FUN=c(mean,sd),
                      na.rm=T, data=tDF1, keep.names=T)
    smDF <- smDF[,c("ModName", "abs.mean", "abs.sd")]
    colnames(smDF) <- c("Model", "meanvalue", "sdvalue")
    
    for(i in mod.list1) {
        plotDF3$meanvalue[plotDF3$Model==i] <- smDF$meanvalue[smDF$Model==i]
        plotDF3$sdvalue[plotDF3$Model==i] <- smDF$sdvalue[smDF$Model==i]
    }
    
    val1 <- round((plotDF3$meanvalue[plotDF3$Model=="B_GDAYP"]-plotDF3$meanvalue[plotDF3$Model=="I_GDAYN"])/plotDF3$meanvalue[plotDF3$Model=="I_GDAYN"]*100, 1)
    val2 <- round((plotDF3$meanvalue[plotDF3$Model=="C_LPJGP"]-plotDF3$meanvalue[plotDF3$Model=="J_LPJGN"])/plotDF3$meanvalue[plotDF3$Model=="J_LPJGN"]*100, 1)
    
    
    ### Plotting C pools in CO2 pct response
    p4 <- ggplot(data=plotDF3, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Model, alpha=Model), 
                 position="stack", col="black") +
        #annotate("text", x=2, y=plotDF3$meanvalue[plotDF3$Model=="B_GDAYP"]*1.01, 
        #         label=(paste0(val1, "%")), size=10)+
        #annotate("text", x=4, y=plotDF3$meanvalue[plotDF3$Model=="C_LPJGP"]*1.01, 
        #         label=(paste0(val2, "%")), size=10)+
        theme_linedraw() +
        geom_errorbar(data=plotDF3, aes(x=Model,
                                        ymin=meanvalue-sdvalue,
                                        ymax=meanvalue+sdvalue),
                      position="dodge", width=0.5)+
        geom_point(data=plotDF3, aes(x=Model, y=meanvalue), col="black", pch=21, fill="white")+
        geom_vline(xintercept=2.5, lty=2)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(Delta * C[veg] * " " * CO[2] * " effect (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("E_OCHDP","G_OCHDX", 
                                 "F_QUINC","H_QUJSM"),
                         label=c("OCHDP","OCHDX", 
                                 "QUINC","QUJSM"))+
        xlab("")+
        scale_alpha_manual(values=c("E_OCHDP" = 0.3, 
                                    "F_QUINC" = 0.3,
                                    "G_OCHDX" = 1.0, 
                                    "H_QUJSM" = 1.0),
                           label=c("OCHDP","QUINC",
                                   "OCHDX","QUJSM"))+
        scale_fill_manual(values=c("E_OCHDP" = "purple", "F_QUINC" = "orange",
                                   "G_OCHDX" = "purple", "H_QUJSM" = "orange"),
                          label=c("OCHDP","QUINC",
                                  "OCHDX","QUJSM")); p4
    
    
    
    
    
    ##################################################################
    #### major C fluxes
    myDF1 <- summaryBy(.~ModName+Trt, FUN=c(mean, sd), data=tmpDF,
                       keep.names=T, na.rm=T)
    
    fluxDF1 <- data.frame(rep(c("GPP", "NPP", "RAU"), 8), 
                          rep(mod.list1, each = 6), 
                          rep(c("amb", "ele"), each = 3),
                          NA, NA)
    colnames(fluxDF1) <- c("Variable", 
                           "Model",
                           "Trt",
                           "meanvalue",
                           "sdvalue")
    
    for (i in c("amb", "ele")) {
        #fluxDF1$meanvalue[fluxDF1$Variable=="NEP"&fluxDF1$Trt==i] <- myDF1$NEP.mean[myDF1$Trt==i]
        #fluxDF1$sdvalue[fluxDF1$Variable=="NEP"&fluxDF1$Trt==i] <- myDF1$NEP.sd[myDF1$Trt==i]
        
        fluxDF1$meanvalue[fluxDF1$Variable=="GPP"&fluxDF1$Trt==i] <- myDF1$GPP.mean[myDF1$Trt==i]
        fluxDF1$sdvalue[fluxDF1$Variable=="GPP"&fluxDF1$Trt==i] <- myDF1$GPP.sd[myDF1$Trt==i]
        
        fluxDF1$meanvalue[fluxDF1$Variable=="NPP"&fluxDF1$Trt==i] <- myDF1$NPP.mean[myDF1$Trt==i]
        fluxDF1$sdvalue[fluxDF1$Variable=="NPP"&fluxDF1$Trt==i] <- myDF1$NPP.sd[myDF1$Trt==i]
        
        fluxDF1$meanvalue[fluxDF1$Variable=="RAU"&fluxDF1$Trt==i] <- myDF1$RAU.mean[myDF1$Trt==i]
        fluxDF1$sdvalue[fluxDF1$Variable=="RAU"&fluxDF1$Trt==i] <- myDF1$RAU.sd[myDF1$Trt==i]
    }
    
    plotDF1 <- fluxDF1[fluxDF1$Trt=="amb",]
    
    ### plotting GPP, NPP, and RAU
    p5 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position=position_dodge(), col="black") +
        geom_errorbar(aes(x=Model, 
                          ymin=meanvalue-sdvalue, 
                          ymax=meanvalue+sdvalue), 
                      position=position_dodge()) +
        geom_vline(xintercept=2.5, lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Carbon fluxes (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("E_OCHDP","G_OCHDX", 
                                 "F_QUINC","H_QUJSM"),
                         label=c("OCHDP","OCHDX", 
                                 "QUINC","QUJSM"))+
        xlab("")+
        scale_fill_manual(name=expression(C[flux]),
                          values=c("GPP"="purple",
                                   "NPP"="green",
                                   "RAU"="red"),
                          labels=c("GPP", "NPP", "RAU"))+
        scale_alpha_manual(name="Model",
                           values=c("E_OCHDP" = 0.3, 
                                    "F_QUINC" = 0.3,
                                    "G_OCHDX" = 1.0, 
                                    "H_QUJSM" = 1.0),
                           label=c("OCHDP","QUINC",
                                   "OCHDX","QUJSM"))+
        #guides(fill = guide_legend(override.aes=list(fill=c("GPP"="purple",
        #                                                    "NPP"="green",
        #                                                    "RAU"="red")),
        #                           nrow=1, byrow=F))+
        guides(fill=guide_legend(expression(C[flux])), alpha = FALSE); p5
    
    
    ### pct CO2 effect
    plotDF2 <- plotDF1
    plotDF2$Trt <- NULL
    
    #plotDF2$meanvalue[plotDF2$Variable=="NEP"] <- myDF5$NEP.mean
    #plotDF2$sdvalue[plotDF2$Variable=="NEP"] <- myDF5$NEP.sd
    
    plotDF2$meanvalue[plotDF2$Variable=="GPP"] <- myDF5$GPP.mean
    plotDF2$sdvalue[plotDF2$Variable=="GPP"] <- myDF5$GPP.sd
    
    plotDF2$meanvalue[plotDF2$Variable=="NPP"] <- myDF5$NPP.mean
    plotDF2$sdvalue[plotDF2$Variable=="NPP"] <- myDF5$NPP.sd
    
    plotDF2$meanvalue[plotDF2$Variable=="RAU"] <- myDF5$RAU.mean
    plotDF2$sdvalue[plotDF2$Variable=="RAU"] <- myDF5$RAU.sd
    
    ### plotting GPP, NPP, and RAU
    p6 <- ggplot(data=plotDF2, 
                 aes(Model, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position=position_dodge(), col="black") +
        geom_errorbar(aes(x=Model, 
                          ymin=meanvalue-sdvalue, 
                          ymax=meanvalue+sdvalue), 
                      position=position_dodge()) +
        geom_vline(xintercept=2.5, lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Carbon fluxes " * CO[2] *" response (%)")))+
        scale_x_discrete(limit=c("E_OCHDP","G_OCHDX", 
                                 "F_QUINC","H_QUJSM"),
                         label=c("OCHDP","OCHDX", 
                                 "QUINC","QUJSM"))+
        xlab("")+
        scale_fill_manual(name=expression(C[flux]),
                          values=c("GPP"="purple",
                                   "NPP"="green",
                                   "RAU"="red"),
                          labels=c("GPP", "NPP", "RAU"))+
        guides(fill = guide_legend(override.aes=list(fill=c("GPP"="purple",
                                                            "NPP"="green",
                                                            "RAU"="red")),
                                   nrow=1, byrow=F))+
        scale_alpha_manual(name="Model",
                           values=c("E_OCHDP" = 0.3, 
                                    "F_QUINC" = 0.3,
                                    "G_OCHDX" = 1.0, 
                                    "H_QUJSM" = 1.0),
                           label=c("OCHDP","QUINC",
                                   "OCHDX","QUJSM"))
    

    #gg.gap::gg.gap(plot=p4,
    #               segments=list(c(-200,-100),c(400,600)),
    #               ylim=c(-1300,700),
    #               tick_width = c(200,100,50))
    #gg.gap::add.legend(plot = p4,
    #                   margin = c(top = 1, right = 700, 
    #                              bottom = 450, left = 1))
    #
    
    legend_top_row <- get_legend(p3 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    legend_bottom_row <- get_legend(p5 + theme(legend.position="bottom",
                                               legend.box = 'horizontal',
                                               legend.box.just = 'left'))
    
    plots_top_row <- plot_grid(p3, p4, 
                               labels=c("(a)", "(b)"),
                               ncol=2, align="vh", axis = "l",
                               label_x=c(0.86,0.16), label_y=0.95,
                               label_size = 18)
    
    
    plots_bottom_row <- plot_grid(p5, p6, 
                                  labels=c("(c)", "(d)"),
                                  ncol=2, align="vh", axis = "l",
                                  label_x=c(0.86,0.16), label_y=0.95,
                                  label_size = 18)
    
    
    #pdf(paste0(out.dir, "/MIP_microbial_model_carbon_combined.pdf"), 
    #    width=10, height=12)
    #plot_grid(plots_top_row,
    #          legend_top_row,
    #          plots_bottom_row,
    #          legend_bottom_row,
    #          ncol=1, rel_heights=c(1,0.2,1,0.2))
    #
    #dev.off()
    
    
    
    
    
    
    
    
    
    
    
    
    #### Nutrient cycling
    ## P uptake, P mineralization flux, P biochemical mineralization flux
    ## P leaching flux
    ## delta PLAB
    ## delta CSOIL
    # N uptake, P mineralization flux, N leaching flux
    # delta NPORG
    
    pfluxDF <- data.frame(rep(c("PUP", "PMIN", "PBIOCHMIN", "PLEACH", "PMINTOT"), 8), 
                             rep(mod.list1, each=10), 
                             rep(c("amb", "ele"), each = 5), NA, NA)
    colnames(pfluxDF) <- c("Variable", 
                          "Model",
                          "Trt",
                          "meanvalue",
                          "sdvalue")
    
    for (i in c("amb", "ele")) {
        pfluxDF$meanvalue[pfluxDF$Variable=="PUP"&pfluxDF$Trt==i] <- myDF1$PUP.mean[myDF1$Trt==i]
        pfluxDF$sdvalue[pfluxDF$Variable=="PUP"&pfluxDF$Trt==i] <- myDF1$PUP.sd[myDF1$Trt==i]
        
        pfluxDF$meanvalue[pfluxDF$Variable=="PMIN"&pfluxDF$Trt==i] <- myDF1$PMIN.mean[myDF1$Trt==i]
        pfluxDF$sdvalue[pfluxDF$Variable=="PMIN"&pfluxDF$Trt==i] <- myDF1$PMIN.sd[myDF1$Trt==i]
        
        pfluxDF$meanvalue[pfluxDF$Variable=="PBIOCHMIN"&pfluxDF$Trt==i] <- myDF1$PBIOCHMIN.mean[myDF1$Trt==i]
        pfluxDF$sdvalue[pfluxDF$Variable=="PBIOCHMIN"&pfluxDF$Trt==i] <- myDF1$PBIOCHMIN.sd[myDF1$Trt==i]
        
        pfluxDF$meanvalue[pfluxDF$Variable=="PLEACH"&pfluxDF$Trt==i] <- myDF1$PLEACH.mean[myDF1$Trt==i]
        pfluxDF$sdvalue[pfluxDF$Variable=="PLEACH"&pfluxDF$Trt==i] <- myDF1$PLEACH.sd[myDF1$Trt==i]

    }
    
    
    ### calculate total P mineralization flux
    ### sum of PMIN and PBIOCHMIN
    for (i in c("amb", "ele")) {
        for (j in mod.list1) {
            
            ### calculate means
            v1 <- sum(pfluxDF$meanvalue[pfluxDF$Model==j&pfluxDF$Trt==i&pfluxDF$Variable%in%c("PMIN", "PBIOCHMIN")],
                      na.rm=T)
            
            ### calculate sd
            v2 <- sqrt(sum(pfluxDF$sdvalue[pfluxDF$Model==j&pfluxDF$Trt==i&pfluxDF$Variable=="PMIN"]^2,
                           pfluxDF$sdvalue[pfluxDF$Model==j&pfluxDF$Trt==i&pfluxDF$Variable=="PBIOCHMIN"]^2, na.rm=T)/2)
            
            ### assign values
            pfluxDF$meanvalue[pfluxDF$Model==j&pfluxDF$Variable=="PMINTOT"&pfluxDF$Trt==i] <- v1
            pfluxDF$sdvalue[pfluxDF$Model==j&pfluxDF$Variable=="PMINTOT"&pfluxDF$Trt==i] <- v2
            
        }
    }
    
    
    
    plotDF1 <- subset(pfluxDF, Variable%in%c("PUP"))
    plotDF2 <- subset(pfluxDF, Variable%in%c("PMINTOT"))
    plotDF3 <- subset(pfluxDF, Variable%in%c("PLEACH"))
    
    
    ### plot PUP
    p1 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Model, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(aes(x=Model, ymin=meanvalue-sdvalue, 
                          ymax=meanvalue+sdvalue), width=0.4,
                      position=position_dodge(width=0.9)) +
        theme_linedraw() +
        ylim(0, 1.25)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(P[uptake] * " (g P " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("E_OCHDP","G_OCHDX", 
                                 "F_QUINC","H_QUJSM"),
                         label=c("OCHDP","OCHDX", 
                                 "QUINC","QUJSM"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values),
                          labels=c(model.labels))+
        guides(alpha=guide_legend("Treatment"), fill = FALSE); p1
    
    
    ### plot PMINTOT
    p2 <- ggplot(data=plotDF2, 
                 aes(Model, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Model, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(aes(x=Model, ymin=meanvalue-sdvalue, 
                          ymax=meanvalue+sdvalue), width=0.4,
                      position=position_dodge(width=0.9)) +
        theme_linedraw() +
        ylim(0, 1.25)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(P[mineralization] * " (g P " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("E_OCHDP","G_OCHDX", 
                                 "F_QUINC","H_QUJSM"),
                         label=c("OCHDP","OCHDX", 
                                 "QUINC","QUJSM"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values),
                          labels=c(model.labels))+
        guides(alpha=guide_legend("Treatment"), fill = FALSE); p2
    
    
    ### plot PLEACH
    #p3 <- ggplot(data=plotDF3, 
    #             aes(Model, meanvalue, group=Trt)) +
    #    geom_bar(stat = "identity", aes(fill=Model, alpha=Trt), 
    #             position=position_dodge(), col="black") +
    #    geom_errorbar(aes(x=Model, ymin=meanvalue-sdvalue, 
    #                      ymax=meanvalue+sdvalue), width=0.4,
    #                  position=position_dodge(width=0.9)) +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=14),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="right",
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          plot.title = element_text(size=14, face="bold.italic", 
    #                                    hjust = 0.5))+
    #    ylab(expression(paste(P[leach] * " (g P " * m^2 * " " * yr^-1 * ")")))+
    #    scale_x_discrete(limit=c("E_OCHDP","G_OCHDX", 
    #                             "F_QUINC","H_QUJSM"),
    #                     label=c("OCHDP","OCHDX", 
    #                             "QUINC","QUJSM"))+
    #    xlab("")+
    #    scale_alpha_manual(name="Treatment",
    #                       values=c("amb" = 0.3, 
    #                                "ele" = 1.0),
    #                       label=c("AMB", "ELE"))+
    #    scale_fill_manual(name="Model",
    #                      values=c(col.values),
    #                      labels=c(model.labels))+
    #    guides(alpha=guide_legend("Treatment"), fill = FALSE); p3
    
    
    ### N flux
    # N uptake, N mineralization flux, N leaching flux

    nfluxDF <- data.frame(rep(c("NUP", "NMIN", "NLEACH"), 8), 
                          rep(mod.list1, each=6), 
                          rep(c("amb", "ele"), each = 3), NA, NA)
    colnames(nfluxDF) <- c("Variable", 
                           "Model",
                           "Trt",
                           "meanvalue",
                           "sdvalue")
    
    for (i in c("amb", "ele")) {
        nfluxDF$meanvalue[nfluxDF$Variable=="NUP"&nfluxDF$Trt==i] <- myDF1$NUP.mean[myDF1$Trt==i]
        nfluxDF$sdvalue[nfluxDF$Variable=="NUP"&nfluxDF$Trt==i] <- myDF1$NUP.sd[myDF1$Trt==i]
        
        nfluxDF$meanvalue[nfluxDF$Variable=="NMIN"&nfluxDF$Trt==i] <- myDF1$NMIN.mean[myDF1$Trt==i]
        nfluxDF$sdvalue[nfluxDF$Variable=="NMIN"&nfluxDF$Trt==i] <- myDF1$NMIN.sd[myDF1$Trt==i]
        
        nfluxDF$meanvalue[nfluxDF$Variable=="NLEACH"&nfluxDF$Trt==i] <- myDF1$NLEACH.mean[myDF1$Trt==i]
        nfluxDF$sdvalue[nfluxDF$Variable=="NLEACH"&nfluxDF$Trt==i] <- myDF1$NLEACH.sd[myDF1$Trt==i]
        
    }
    
    
    plotDF1 <- subset(nfluxDF, Variable%in%c("NUP"))
    plotDF2 <- subset(nfluxDF, Variable%in%c("NMIN"))
    plotDF3 <- subset(nfluxDF, Variable%in%c("NLEACH"))
    
    
    ### plot PUP
    p3 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Model, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(aes(x=Model, ymin=meanvalue-sdvalue, 
                          ymax=meanvalue+sdvalue), width=0.4,
                      position=position_dodge(width=0.9)) +
        theme_linedraw() +
        ylim(0, 12.0)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(N[uptake] * " (g N " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("E_OCHDP","G_OCHDX", 
                                 "F_QUINC","H_QUJSM"),
                         label=c("OCHDP","OCHDX", 
                                 "QUINC","QUJSM"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values),
                          labels=c(model.labels))+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)
    
    
    ### plot PMINTOT
    p4 <- ggplot(data=plotDF2, 
                 aes(Model, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Model, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(aes(x=Model, ymin=meanvalue-sdvalue, 
                          ymax=meanvalue+sdvalue), width=0.4,
                      position=position_dodge(width=0.9)) +
        theme_linedraw() +
        ylim(0, 12.0)+
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(N[mineralization] * " (g N " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("E_OCHDP","G_OCHDX", 
                                 "F_QUINC","H_QUJSM"),
                         label=c("OCHDP","OCHDX", 
                                 "QUINC","QUJSM"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values),
                          labels=c(model.labels))+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)
    
    
    
    legend_top_row <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    plots_top_row <- plot_grid(p1, p2, p3, p4, 
                               labels=c("(a)", "(b)", "(c)", "(d)"),
                               ncol=2, align="vh", axis = "l",
                               label_x=c(0.86,0.16), label_y=0.95,
                               label_size = 18)
    
    
    
    #pdf(paste0(out.dir, "/MIP_microbial_model_nutrient_combined.pdf"), 
    #    width=10, height=12)
    #plot_grid(plots_top_row,
    #          legend_top_row,
    #          ncol=1, rel_heights=c(1,0.2))
    #
    #dev.off()
    
    ### It looks like that it is more important to show the
    ### CO2 response of these nutrient fluxes. So from this
    ### point forward, need to make script to plot all the
    ### relevant CO2 response variables. 
    
    
    
    
    ### Plotting C pools in CO2 pct response
    
    
### end    
}
