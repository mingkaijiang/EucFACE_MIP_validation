compare_CNP_and_CN_model_output <- function(scenario) {
    
    
    ##################################################################
    #### Set up basics
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario, "/")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_ELE_annual.rds"))
    
    ### select GDAYN, GDAYP, LPJGN, LPJGP model output
    ambDF <- subset(ambDF, ModName%in%c("I_GDAYN", "C_GDAYP",
                                        "J_LPJGN", "D_LPJGP"))
    
    
    eleDF <- subset(eleDF, ModName%in%c("I_GDAYN", "C_GDAYP",
                                        "J_LPJGN", "D_LPJGP"))
    
    
    #### calculate 4-yr means in the simulation datasets
    #ambDF <- subset(ambDF, YEAR>2012 & YEAR<2017)
    #eleDF <- subset(eleDF, YEAR>2012 & YEAR<2017)
    
    d <- dim(ambDF)[2]
    
    ### calculate the effect of P limitation as difference of CNP - CN
    diffDF <- ambDF[ambDF$ModName%in%c("I_GDAYN", "J_LPJGN"),]
    
    diffDF$ModName <- gsub("I_GDAYN", "GDAY", diffDF$ModName)
    diffDF$ModName <- gsub("J_LPJGN", "LPJG", diffDF$ModName)
    
    diffDF[diffDF$ModName=="GDAY",3:d] <- ambDF[ambDF$ModName=="C_GDAYP",3:d] - ambDF[ambDF$ModName=="I_GDAYN",3:d]
    diffDF[diffDF$ModName=="LPJG",3:d] <- ambDF[ambDF$ModName=="D_LPJGP",3:d] - ambDF[ambDF$ModName=="J_LPJGN",3:d]
    
    
    ### calculate the effect of P limitation as % difference of (CNP - CN)/CN
    pctdiffDF <- diffDF
    
    pctdiffDF[pctdiffDF$ModName=="GDAY",3:d] <- diffDF[diffDF$ModName=="GDAY",3:d]/ambDF[ambDF$ModName=="I_GDAYN",3:d] * 100.0
    pctdiffDF[pctdiffDF$ModName=="LPJG",3:d] <- diffDF[diffDF$ModName=="LPJG",3:d]/ambDF[ambDF$ModName=="J_LPJGN",3:d] * 100.0
    
    
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
    mod.list1 <- c("C_GDAYP", "D_LPJGP", "I_GDAYN", "J_LPJGN")
    mod.list2 <- c("GDAY", "LPJG")
    
    
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
    
    val1 <- round((plotDF2$meanvalue[plotDF2$Model=="C_GDAYP"]-plotDF2$meanvalue[plotDF2$Model=="I_GDAYN"])/plotDF2$meanvalue[plotDF2$Model=="I_GDAYN"]*100, 1)
    val2 <- round((plotDF2$meanvalue[plotDF2$Model=="D_LPJGP"]-plotDF2$meanvalue[plotDF2$Model=="J_LPJGN"])/plotDF2$meanvalue[plotDF2$Model=="J_LPJGN"]*100, 1)
    
    
    ### Plotting C pools in ambient CO2
    p1 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position="stack", col="black") +
        #geom_errorbar(data=plotDF2, 
        #              aes(x=Model, ymin=meanvalue-sdvalue, ymax=meanvalue+sdvalue), 
        #              position="dodge", width=0.2, col="black") +
        geom_point(data=plotDF2, 
                      aes(x=Model, y=meanvalue), 
                      position="dodge", col="black", size=2, fill="white", pch=21) +
        geom_vline(xintercept=2.5, lty=2)+
        #annotate("text", x=2, y=plotDF2$meanvalue[plotDF2$Model=="C_GDAYP"]*1.15, 
        #         label=(paste0(val1, "%")), size=10)+
        #annotate("text", x=4, y=plotDF2$meanvalue[plotDF2$Model=="D_LPJGP"]*1.15, 
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
              legend.box.just = 'none',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(C[veg] * " pools (g C " * m^2*")")))+
        scale_x_discrete(limit=c("I_GDAYN","C_GDAYP", 
                                 "J_LPJGN","D_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
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
        scale_alpha_manual(values=c("C_GDAYP" = 1.0, 
                                    "D_LPJGP" = 1.0,
                                    "I_GDAYN" = 0.3, 
                                    "J_LPJGN" = 0.3),
                           label=c("GDAYP","LPJGP", 
                                   "GDAYN","LPJGN"))
    
    
    ### calculate CO2 pct response difference
    plotDF3 <- plotDF2
    plotDF3$meanvalue <- (vegDF1$meanvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="ele"]/vegDF1$meanvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="amb"]-1)*100
    plotDF3$sdvalue <- sqrt((vegDF1$sdvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="ele"]^2 + vegDF1$sdvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="amb"]^2+
                                 + vegDF1$sdvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="amb"]^2)/3)/vegDF1$meanvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="amb"]
    
    val1 <- round((plotDF3$meanvalue[plotDF3$Model=="C_GDAYP"]-plotDF3$meanvalue[plotDF3$Model=="I_GDAYN"])/plotDF3$meanvalue[plotDF3$Model=="I_GDAYN"]*100, 1)
    val2 <- round((plotDF3$meanvalue[plotDF3$Model=="D_LPJGP"]-plotDF3$meanvalue[plotDF3$Model=="J_LPJGN"])/plotDF3$meanvalue[plotDF3$Model=="J_LPJGN"]*100, 1)
    
    
    ### Plotting C pools in CO2 pct response
    p2 <- ggplot(data=plotDF3, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(alpha=Model), fill="#505050", 
                 position="stack", col="black") +
        #geom_errorbar(aes(x=Model, ymin=meanvalue-sdvalue, ymax=meanvalue+sdvalue), 
        #              position="dodge", width=0.2, col="black") +
        #annotate("text", x=2, y=plotDF3$meanvalue[plotDF3$Model=="C_GDAYP"]*1.01, 
        #         label=(paste0(val1, "%")), size=10)+
        #annotate("text", x=4, y=plotDF3$meanvalue[plotDF3$Model=="D_LPJGP"]*1.01, 
        #         label=(paste0(val2, "%")), size=10)+
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
        ylab(expression(paste(CO[2] *" effect on " * C[veg] * " (%)")))+
        scale_x_discrete(limit=c("I_GDAYN","C_GDAYP", 
                                 "J_LPJGN","D_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("C_GDAYP" = 1.0, 
                                    "D_LPJGP" = 1.0,
                                    "I_GDAYN" = 0.3, 
                                    "J_LPJGN" = 0.3),
                           label=c("GDAYP","LPJGP", 
                                   "GDAYN","LPJGN"))+
        #scale_fill_manual(values=c("C_GDAYP" = "grey", "D_LPJGP" = "grey",
        #                           "I_GDAYN" = "white", "J_LPJGN" = "white"),
        #                  label=c("GDAYP","LPJGP", 
        #                          "GDAYN","LPJGN"))+
        coord_cartesian(ylim=c(0,5))
    
    
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
    
    val1 <- round((plotDF2$meanvalue[plotDF2$Model=="C_GDAYP"]-plotDF2$meanvalue[plotDF2$Model=="I_GDAYN"])/plotDF2$meanvalue[plotDF2$Model=="I_GDAYN"]*100, 1)
    val2 <- round((plotDF2$meanvalue[plotDF2$Model=="D_LPJGP"]-plotDF2$meanvalue[plotDF2$Model=="J_LPJGN"])/plotDF2$meanvalue[plotDF2$Model=="J_LPJGN"]*100, 1)
    
    
    ### Plotting C pools in ambient CO2
    p3 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position="stack", col="black") +
        #annotate("text", x=2, y=plotDF2$meanvalue[plotDF2$Model=="B_GDAYP"]*3, 
        #         label=(paste0(val1, "%")), size=10)+
        #annotate("text", x=4, y=plotDF2$meanvalue[plotDF2$Model=="C_LPJGP"]*1.2, 
        #         label=(paste0(val2, "%")), size=10)+
        geom_point(data=plotDF2, aes(x=Model, y=meanvalue), fill="white", pch=21, col="black")+
        #geom_errorbar(data=plotDF2, aes(x=Model,
        #                             ymin=meanvalue-sdvalue,
        #                             ymax=meanvalue+sdvalue),
        #              position="dodge", width=0.5)+
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
        scale_x_discrete(limit=c("I_GDAYN","C_GDAYP", 
                                 "J_LPJGN","D_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
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
        scale_alpha_manual(values=c("C_GDAYP" = 1.0, 
                                    "D_LPJGP" = 1.0,
                                    "I_GDAYN" = 0.3, 
                                    "J_LPJGN" = 0.3),
                           label=c("GDAYP","LPJGP", 
                                   "GDAYN","LPJGN")); p3
    
    
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
    
    val1 <- round((plotDF3$meanvalue[plotDF3$Model=="C_GDAYP"]-plotDF3$meanvalue[plotDF3$Model=="I_GDAYN"])/plotDF3$meanvalue[plotDF3$Model=="I_GDAYN"]*100, 1)
    val2 <- round((plotDF3$meanvalue[plotDF3$Model=="D_LPJGP"]-plotDF3$meanvalue[plotDF3$Model=="J_LPJGN"])/plotDF3$meanvalue[plotDF3$Model=="J_LPJGN"]*100, 1)
    
    
    ### Plotting C pools in CO2 pct response
    p4 <- ggplot(data=plotDF3, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(alpha=Model), fill="#505050", 
                 position="stack", col="black") +
        #annotate("text", x=2, y=plotDF3$meanvalue[plotDF3$Model=="B_GDAYP"]*1.01, 
        #         label=(paste0(val1, "%")), size=10)+
        #annotate("text", x=4, y=plotDF3$meanvalue[plotDF3$Model=="C_LPJGP"]*1.01, 
        #         label=(paste0(val2, "%")), size=10)+
        theme_linedraw() +
        #geom_errorbar(data=plotDF3, aes(x=Model,
        #                                ymin=meanvalue-sdvalue,
        #                                ymax=meanvalue+sdvalue),
        #              position="dodge", width=0.5)+
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
        ylab(expression(paste(CO[2] * " effect on " * Delta * C[veg] * " (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("I_GDAYN","C_GDAYP", 
                                 "J_LPJGN","D_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("C_GDAYP" = 1.0, 
                                    "D_LPJGP" = 1.0,
                                    "I_GDAYN" = 0.3, 
                                    "J_LPJGN" = 0.3),
                           label=c("GDAYP","LPJGP", 
                                   "GDAYN","LPJGN"))#+
        #scale_fill_manual(values=c("C_GDAYP" = "grey", "D_LPJGP" = "grey",
        #                           "I_GDAYN" = "white", "J_LPJGN" = "white"),
        #                  label=c("GDAYP","LPJGP", 
        #                          "GDAYN","LPJGN"));p4
    
    
    
    

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
        #geom_errorbar(aes(x=Model, 
        #                  ymin=meanvalue-sdvalue, 
        #                  ymax=meanvalue+sdvalue), 
        #              position=position_dodge()) +
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
        scale_x_discrete(limit=c("I_GDAYN","C_GDAYP", 
                                 "J_LPJGN","D_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_fill_manual(name=expression(C[flux]),
                          values=c("GPP"="purple",
                                   "NPP"="green",
                                   "RAU"="red"),
                          labels=c("GPP", "NPP", 
                                   expression(R[auto])))+
        scale_alpha_manual(name="Model",
                           values=c("C_GDAYP" = 1.0, 
                                    "D_LPJGP" = 1.0,
                                    "I_GDAYN" = 0.3, 
                                    "J_LPJGN" = 0.3),
                           label=c("GDAYP","LPJGP", 
                                   "GDAYN","LPJGN"))+
        #guides(fill = guide_legend(override.aes=list(fill=c("GPP"="purple",
        #                                                    "NPP"="green",
        #                                                    "RAU"="red")),
        #                           nrow=1, byrow=F))+
        guides(fill=guide_legend(expression(C[flux])), alpha = FALSE)
    
    
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
        #geom_errorbar(aes(x=Model, 
        #                  ymin=meanvalue-sdvalue, 
        #                  ymax=meanvalue+sdvalue), 
        #              position=position_dodge()) +
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
        scale_x_discrete(limit=c("I_GDAYN","C_GDAYP", 
                                 "J_LPJGN","D_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_fill_manual(name=expression(C[flux]),
                          values=c("GPP"="purple",
                                   "NPP"="green",
                                   "RAU"="red"),
                          labels=c("GPP", "NPP", 
                                   expression(R[auto])))+
        guides(fill = guide_legend(override.aes=list(fill=c("GPP"="purple",
                                                            "NPP"="green",
                                                            "RAU"="red")),
                                   nrow=1, byrow=F))+
        scale_alpha_manual(name="Model",
                           values=c("C_GDAYP" = 1.0, 
                                    "D_LPJGP" = 1.0,
                                    "I_GDAYN" = 0.3, 
                                    "J_LPJGN" = 0.3),
                           label=c("GDAYP","LPJGP", 
                                   "GDAYN","LPJGN"))
    
    
    #gg.gap::gg.gap(plot=p4,
    #               segments=list(c(-200,-100),c(400,600)),
    #               ylim=c(-1300,700),
    #               tick_width = c(200,100,50))
    #gg.gap::add.legend(plot = p4,
    #                   margin = c(top = 1, right = 700, 
    #                              bottom = 450, left = 1))
    #
    
    legend_bottom_row <- get_legend(p3 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    legend_top_row <- get_legend(p5 + theme(legend.position="bottom",
                                               legend.box = 'horizontal',
                                               legend.box.just = 'left'))
    
    legend_middle_row <- get_legend(p3 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    plots_middle_row <- plot_grid(p1, p2, 
                               labels=c("C", "D"),
                               ncol=2, align="vh", axis = "l",
                               label_x=0.92, label_y=0.95,
                               label_size = 18)
    
    plots_bottom_row <- plot_grid(p3, p4, 
                                labels=c("E", "F"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.92, label_y=0.95,
                                label_size = 18)
    
    
    plots_top_row <- plot_grid(p5, p6, 
                                labels=c("A", "B"),
                                ncol=2, align="vh", axis = "l",
                                label_x=0.92, label_y=0.95,
                                label_size = 18)
    
    
    pdf(paste0(out.dir, "/MIP_CN_vs_CNP_combined.pdf"), 
        width=10, height=12)
    #grid.arrange(p3, p4, p5, p6,
    #             ncol = 2)
    plot_grid(plots_top_row,
              legend_top_row,
              plots_middle_row,
              legend_middle_row,
              plots_bottom_row,
              legend_bottom_row,
              ncol=1, rel_heights=c(1,0.2,1,0.2,1,0.2))
    
    dev.off()
    
    
    
    
    
    
    
 ### end    
}
