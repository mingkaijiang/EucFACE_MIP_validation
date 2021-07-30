compare_CNP_and_CN_model_output <- function() {
    
    
    ##################################################################
    #### Set up basics
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/obs_fix_output")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0(out.dir, "/MIP_obs_fix_amb_annual.rds"))
    eleDF <- readRDS(paste0(out.dir, "/MIP_obs_fix_ele_annual.rds"))
    
    ### select GDAYN, GDAYP, LPJGN, LPJGP model output
    ambDF <- subset(ambDF, ModName%in%c("I_GDAYN", "B_GDAYP",
                                        "J_LPJGN", "C_LPJGP"))
    
    
    eleDF <- subset(eleDF, ModName%in%c("I_GDAYN", "B_GDAYP",
                                        "J_LPJGN", "C_LPJGP"))
    
    
    #### calculate 4-yr means in the simulation datasets
    #ambDF <- subset(ambDF, YEAR>2012 & YEAR<2017)
    #eleDF <- subset(eleDF, YEAR>2012 & YEAR<2017)
    
    d <- dim(ambDF)[2]
    
    ### calculate the effect of P limitation as difference of CNP - CN
    diffDF <- ambDF[ambDF$ModName%in%c("I_GDAYN", "J_LPJGN"),]
    
    diffDF$ModName <- gsub("I_GDAYN", "GDAY", diffDF$ModName)
    diffDF$ModName <- gsub("J_LPJGN", "LPJG", diffDF$ModName)
    
    diffDF[diffDF$ModName=="GDAY",3:d] <- ambDF[ambDF$ModName=="B_GDAYP",3:d] - ambDF[ambDF$ModName=="I_GDAYN",3:d]
    diffDF[diffDF$ModName=="LPJG",3:d] <- ambDF[ambDF$ModName=="C_LPJGP",3:d] - ambDF[ambDF$ModName=="J_LPJGN",3:d]
    
    
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
    mod.list1 <- c("B_GDAYP", "C_LPJGP", "I_GDAYN", "J_LPJGN")
    mod.list2 <- c("GDAY", "LPJG")
    
    
    ##################################################################
    ### prepare vegetation biomass datasets
    vegDF1 <- data.frame(rep(c("CL", "CW", "CFR", "CCR", "Total"), 8), 
                         rep(mod.list1, each=10), 
                         rep(c("amb", "ele"), each = 5), NA, NA)
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
    }
    
    for (i in c("amb", "ele")) {
        for (j in mod.list1) {
            
            ### calculate means
            v1 <- sum(vegDF1$meanvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable%in%c("CL", "CW", "CCR", "CFR")],
                      na.rm=T)
            
            ### calculate sd
            v2 <- sqrt(sum(vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CL"]^2,
                            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CW"]^2,
                            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CFR"]^2,
                            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CCR"]^2, na.rm=T)/3)
            
            ### assign values
            vegDF1$meanvalue[vegDF1$Model==j&vegDF1$Variable=="Total"&vegDF1$Trt==i] <- v1
            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Variable=="Total"&vegDF1$Trt==i] <- v2
            
        }
    }
    
    plotDF1 <- subset(vegDF1, Variable%in%c("CL", "CW", "CFR", "CCR") & Trt=="amb")
    plotDF2 <- subset(vegDF1, Variable%in%c("Total") & Trt=="amb")
    
    val1 <- round((plotDF2$meanvalue[plotDF2$Model=="B_GDAYP"]-plotDF2$meanvalue[plotDF2$Model=="I_GDAYN"])/plotDF2$meanvalue[plotDF2$Model=="I_GDAYN"]*100, 1)
    val2 <- round((plotDF2$meanvalue[plotDF2$Model=="C_LPJGP"]-plotDF2$meanvalue[plotDF2$Model=="J_LPJGN"])/plotDF2$meanvalue[plotDF2$Model=="J_LPJGN"]*100, 1)
    
    
    ### Plotting C pools in ambient CO2
    p1 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position="stack", col="black") +
        annotate("text", x=2, y=plotDF2$meanvalue[plotDF2$Model=="B_GDAYP"]*1.15, 
                 label=(paste0(val1, "%")), size=10)+
        annotate("text", x=4, y=plotDF2$meanvalue[plotDF2$Model=="C_LPJGP"]*1.15, 
                 label=(paste0(val2, "%")), size=10)+
        #geom_errorbar(data=plotDF2,
        #              aes(x=Model, 
        #                  ymin=meanvalue-sdvalue, 
        #                  ymax=meanvalue+sdvalue), 
        #              position="dodge", width=1.0) +
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
        ylab(expression(paste("Vegetation carbon pools (g C " * m^2*")")))+
        scale_x_discrete(limit=c("I_GDAYN","B_GDAYP", 
                                 "J_LPJGN","C_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("I_GDAYN" = 0.3, 
                                    "B_GDAYP" = 1.0, 
                                    "J_LPJGN" = 0.3, 
                                    "C_LPJGP" = 1.0),
                           label=c("GDAYN","GDAYP", 
                                   "LPJGN","LPJGP")); p1
    
    
    ### calculate CO2 pct response difference
    plotDF3 <- plotDF2
    plotDF3$meanvalue <- vegDF1$meanvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="ele"]/vegDF1$meanvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="amb"]
    plotDF3$sdvalue <- NA #sqrt((vegDF1$sdvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="ele"]^2 + vegDF1$sdvalue[vegDF1$Variable=="Total"&vegDF1$Trt=="amb"]^2)/2)
    
    val1 <- round((plotDF3$meanvalue[plotDF3$Model=="B_GDAYP"]-plotDF3$meanvalue[plotDF3$Model=="I_GDAYN"])/plotDF3$meanvalue[plotDF3$Model=="I_GDAYN"]*100, 1)
    val2 <- round((plotDF3$meanvalue[plotDF3$Model=="C_LPJGP"]-plotDF3$meanvalue[plotDF3$Model=="J_LPJGN"])/plotDF3$meanvalue[plotDF3$Model=="J_LPJGN"]*100, 1)
    
    
    ### Plotting C pools in CO2 pct response
    p2 <- ggplot(data=plotDF3, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Model, alpha=Model), 
                 position="stack", col="black") +
        annotate("text", x=2, y=plotDF3$meanvalue[plotDF3$Model=="B_GDAYP"]*1.01, 
                 label=(paste0(val1, "%")), size=10)+
        annotate("text", x=4, y=plotDF3$meanvalue[plotDF3$Model=="C_LPJGP"]*1.01, 
                 label=(paste0(val2, "%")), size=10)+
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
        ylab(expression(paste("Vegetation carbon " * CO[2] *" response ratio")))+
        scale_x_discrete(limit=c("I_GDAYN","B_GDAYP", 
                                 "J_LPJGN","C_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("I_GDAYN" = 0.3, 
                                    "B_GDAYP" = 1.0, 
                                    "J_LPJGN" = 0.3, 
                                    "C_LPJGP" = 1.0),
                           label=c("GDAYN","GDAYP", 
                                   "LPJGN","LPJGP"))+
        scale_fill_manual(values=c("I_GDAYN" = "purple", "B_GDAYP" = "purple",
                                   "J_LPJGN" = "orange", "C_LPJGP" = "orange"),
                          label=c("GDAYN","GDAYP", 
                                  "LPJGN","LPJGP"))+
        coord_cartesian(ylim=c(1,1.1)); p2
    
    
    ##################################################################
    ### normalized vegetation C pool responses
    norDF1 <- calculate_normalized_vegetation_pool_response(inDF=tmpDF,
                                                           pcycle=F)
    
    norDF <- calculate_normalized_delta_vegetation_pool_response(inDF=tmpDF,
                                                                 pcycle=F)
    
    myDF1 <- norDF$absDF
    
    ### prepare vegetation biomass datasets
    vegDF1 <- data.frame(rep(c("CL", "CW", "CFR", "CCR", "CVEG"), 8), 
                         rep(mod.list1, each=10), 
                         rep(c("amb", "ele"), each = 5), NA, NA)
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
            
            vegDF1$meanvalue[vegDF1$Variable=="CVEG"&vegDF1$Trt==i&vegDF1$Model==j] <- myDF1$CVEG[myDF1$Trt==i&myDF1$ModName==j]
            
        }
    }
    
    
    plotDF1 <- subset(vegDF1, Variable%in%c("CL", "CW", "CFR", "CCR") & Trt=="amb")
    plotDF2 <- subset(vegDF1, Variable%in%c("CVEG") & Trt=="amb")
    
    val1 <- round((plotDF2$meanvalue[plotDF2$Model=="B_GDAYP"]-plotDF2$meanvalue[plotDF2$Model=="I_GDAYN"])/plotDF2$meanvalue[plotDF2$Model=="I_GDAYN"]*100, 1)
    val2 <- round((plotDF2$meanvalue[plotDF2$Model=="C_LPJGP"]-plotDF2$meanvalue[plotDF2$Model=="J_LPJGN"])/plotDF2$meanvalue[plotDF2$Model=="J_LPJGN"]*100, 1)
    
    
    ### Plotting C pools in ambient CO2
    p1 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position="stack", col="black") +
        annotate("text", x=2, y=plotDF2$meanvalue[plotDF2$Model=="B_GDAYP"]*3, 
                 label=(paste0(val1, "%")), size=10)+
        annotate("text", x=4, y=plotDF2$meanvalue[plotDF2$Model=="C_LPJGP"]*1.2, 
                 label=(paste0(val2, "%")), size=10)+
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
        ylab(expression(paste(Delta * C[veg] * " (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("I_GDAYN","B_GDAYP", 
                                 "J_LPJGN","C_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("I_GDAYN" = 0.3, 
                                    "B_GDAYP" = 1.0, 
                                    "J_LPJGN" = 0.3, 
                                    "C_LPJGP" = 1.0),
                           label=c("GDAYN","GDAYP", 
                                   "LPJGN","LPJGP")); p1
    
    
    ### calculate CO2 pct response difference
    plotDF3 <- plotDF2
    plotDF3$meanvalue <- vegDF1$meanvalue[vegDF1$Variable=="CVEG"&vegDF1$Trt=="ele"]/vegDF1$meanvalue[vegDF1$Variable=="CVEG"&vegDF1$Trt=="amb"]
    plotDF3$sdvalue <- NA 
    
    val1 <- round((plotDF3$meanvalue[plotDF3$Model=="B_GDAYP"]-plotDF3$meanvalue[plotDF3$Model=="I_GDAYN"])/plotDF3$meanvalue[plotDF3$Model=="I_GDAYN"]*100, 1)
    val2 <- round((plotDF3$meanvalue[plotDF3$Model=="C_LPJGP"]-plotDF3$meanvalue[plotDF3$Model=="J_LPJGN"])/plotDF3$meanvalue[plotDF3$Model=="J_LPJGN"]*100, 1)
    
    
    ### Plotting C pools in CO2 pct response
    p2 <- ggplot(data=plotDF3, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Model, alpha=Model), 
                 position="stack", col="black") +
        annotate("text", x=2, y=plotDF3$meanvalue[plotDF3$Model=="B_GDAYP"]*1.01, 
                 label=(paste0(val1, "%")), size=10)+
        annotate("text", x=4, y=plotDF3$meanvalue[plotDF3$Model=="C_LPJGP"]*1.01, 
                 label=(paste0(val2, "%")), size=10)+
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
        ylab(expression(paste(Delta * C[veg] * " " * CO[2] *" response ratio")))+
        scale_x_discrete(limit=c("I_GDAYN","B_GDAYP", 
                                 "J_LPJGN","C_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("I_GDAYN" = 0.3, 
                                    "B_GDAYP" = 1.0, 
                                    "J_LPJGN" = 0.3, 
                                    "C_LPJGP" = 1.0),
                           label=c("GDAYN","GDAYP", 
                                   "LPJGN","LPJGP"))+
        scale_fill_manual(values=c("I_GDAYN" = "purple", "B_GDAYP" = "purple",
                                   "J_LPJGN" = "orange", "C_LPJGP" = "orange"),
                          label=c("GDAYN","GDAYP", 
                                  "LPJGN","LPJGP"))+
        coord_cartesian(ylim=c(1,3)); p2
    
    
    
    

    ##################################################################
    #### major C fluxes
    fluxDF1 <- data.frame(rep(c("NEP", "GPP", "NPP", "RAU"), 8), 
                        rep(mod.list1, each = 8), 
                        rep(c("amb", "ele"), each = 4),
                        NA, NA)
    colnames(fluxDF1) <- c("Variable", 
                         "Model",
                         "Trt",
                         "meanvalue",
                         "sdvalue")
    
    for (i in c("amb", "ele")) {
        fluxDF1$meanvalue[fluxDF1$Variable=="NEP"&fluxDF1$Trt==i] <- myDF1$NEP.mean[myDF1$Trt==i]
        fluxDF1$sdvalue[fluxDF1$Variable=="NEP"&fluxDF1$Trt==i] <- myDF1$NEP.sd[myDF1$Trt==i]
        
        fluxDF1$meanvalue[fluxDF1$Variable=="GPP"&fluxDF1$Trt==i] <- myDF1$GPP.mean[myDF1$Trt==i]
        fluxDF1$sdvalue[fluxDF1$Variable=="GPP"&fluxDF1$Trt==i] <- myDF1$GPP.sd[myDF1$Trt==i]
        
        fluxDF1$meanvalue[fluxDF1$Variable=="NPP"&fluxDF1$Trt==i] <- myDF1$NPP.mean[myDF1$Trt==i]
        fluxDF1$sdvalue[fluxDF1$Variable=="NPP"&fluxDF1$Trt==i] <- myDF1$NPP.sd[myDF1$Trt==i]
        
        fluxDF1$meanvalue[fluxDF1$Variable=="RAU"&fluxDF1$Trt==i] <- myDF1$RAU.mean[myDF1$Trt==i]
        fluxDF1$sdvalue[fluxDF1$Variable=="RAU"&fluxDF1$Trt==i] <- myDF1$RAU.sd[myDF1$Trt==i]
    }
    
    plotDF1 <- fluxDF1[fluxDF1$Trt=="amb",]
    
    ### plotting GPP, NPP, and RAU
    p3 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position=position_dodge(), col="black") +
        geom_errorbar(aes(x=Model, 
                          ymin=meanvalue-sdvalue, 
                          ymax=meanvalue+sdvalue), 
                      position=position_dodge()) +
        geom_vline(xintercept=2.5)+
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
        ylab(expression(paste("Carbon fluxes (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("I_GDAYN","B_GDAYP", 
                                 "J_LPJGN","C_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("I_GDAYN" = 0.3, 
                                    "B_GDAYP" = 1.0, 
                                    "J_LPJGN" = 0.3, 
                                    "C_LPJGP" = 1.0),
                           label=c("GDAYN","GDAYP", 
                                   "LPJGN","LPJGP")); p3
    
    
    ### pct CO2 effect
    plotDF2 <- plotDF1
    plotDF2$Trt <- NULL
    
    plotDF2$meanvalue[plotDF2$Variable=="NEP"] <- myDF5$NEP.mean
    plotDF2$sdvalue[plotDF2$Variable=="NEP"] <- myDF5$NEP.sd
    
    plotDF2$meanvalue[plotDF2$Variable=="GPP"] <- myDF5$GPP.mean
    plotDF2$sdvalue[plotDF2$Variable=="GPP"] <- myDF5$GPP.sd
    
    plotDF2$meanvalue[plotDF2$Variable=="NPP"] <- myDF5$NPP.mean
    plotDF2$sdvalue[plotDF2$Variable=="NPP"] <- myDF5$NPP.sd
    
    plotDF2$meanvalue[plotDF2$Variable=="RAU"] <- myDF5$RAU.mean
    plotDF2$sdvalue[plotDF2$Variable=="RAU"] <- myDF5$RAU.sd

    ### plotting GPP, NPP, and RAU
    p4 <- ggplot(data=plotDF2, 
                 aes(Model, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position=position_dodge(), col="black") +
        geom_errorbar(aes(x=Model, 
                          ymin=meanvalue-sdvalue, 
                          ymax=meanvalue+sdvalue), 
                      position=position_dodge()) +
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
        ylab(expression(paste("Carbon fluxes " * CO[2] *" response ratio")))+
        scale_x_discrete(limit=c("I_GDAYN","B_GDAYP", 
                                 "J_LPJGN","C_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("I_GDAYN" = 0.3, 
                                    "B_GDAYP" = 1.0, 
                                    "J_LPJGN" = 0.3, 
                                    "C_LPJGP" = 1.0),
                           label=c("GDAYN","GDAYP", 
                                   "LPJGN","LPJGP")); p4
    
    gg.gap::gg.gap(plot=p4,
                   segments=list(c(-200,-100),c(400,600)),
                   ylim=c(-1300,700),
                   tick_width = c(200,100,50))
    gg.gap::add.legend(plot = p4,
                       margin = c(top = 1, right = 700, 
                                  bottom = 450, left = 1))
    
    
    
    
    
    
    
    
    
    
    ##################################################################
    #### N pools
    ### prepare vegetation biomass datasets
    vegDF2 <- data.frame(rep(c("NL", "NW", "NFR", "NCR", "Total"), 8), 
                         rep(mod.list1, each=10), 
                         rep(c("amb", "ele"), each = 5), NA, NA)
    colnames(vegDF2) <- c("Variable", 
                          "Model",
                          "Trt",
                          "meanvalue",
                          "sdvalue")
    
    for (i in c("amb", "ele")) {
        vegDF2$meanvalue[vegDF2$Variable=="NL"&vegDF2$Trt==i] <- myDF1$NL.mean[myDF1$Trt==i]
        vegDF2$sdvalue[vegDF2$Variable=="NL"&vegDF2$Trt==i] <- myDF1$NL.sd[myDF1$Trt==i]
        
        vegDF2$meanvalue[vegDF2$Variable=="NW"&vegDF2$Trt==i] <- myDF1$NW.mean[myDF1$Trt==i]
        vegDF2$sdvalue[vegDF2$Variable=="NW"&vegDF2$Trt==i] <- myDF1$NW.sd[myDF1$Trt==i]
        
        vegDF2$meanvalue[vegDF2$Variable=="NCR"&vegDF2$Trt==i] <- myDF1$NCR.mean[myDF1$Trt==i]
        vegDF2$sdvalue[vegDF2$Variable=="NCR"&vegDF2$Trt==i] <- myDF1$NCR.sd[myDF1$Trt==i]
        
        vegDF2$meanvalue[vegDF2$Variable=="NFR"&vegDF2$Trt==i] <- myDF1$NFR.mean[myDF1$Trt==i]
        vegDF2$sdvalue[vegDF2$Variable=="NFR"&vegDF2$Trt==i] <- myDF1$NFR.sd[myDF1$Trt==i]
    }
    
    for (i in c("amb", "ele")) {
        for (j in mod.list1) {
            
            ### calculate means
            v1 <- sum(vegDF2$meanvalue[vegDF2$Model==j&vegDF2$Trt==i&vegDF2$Variable%in%c("NL", "NW", "NCR", "NFR")],
                      na.rm=T)
            
            ### calculate sd
            v2 <- sqrt(sum(vegDF2$sdvalue[vegDF2$Model==j&vegDF2$Trt==i&vegDF2$Variable=="NL"]^2,
                           vegDF2$sdvalue[vegDF2$Model==j&vegDF2$Trt==i&vegDF2$Variable=="NW"]^2,
                           vegDF2$sdvalue[vegDF2$Model==j&vegDF2$Trt==i&vegDF2$Variable=="NFR"]^2,
                           vegDF2$sdvalue[vegDF2$Model==j&vegDF2$Trt==i&vegDF2$Variable=="NCR"]^2, na.rm=T)/3)
            
            ### assign values
            vegDF2$meanvalue[vegDF2$Model==j&vegDF2$Variable=="Total"&vegDF2$Trt==i] <- v1
            vegDF2$sdvalue[vegDF2$Model==j&vegDF2$Variable=="Total"&vegDF2$Trt==i] <- v2
            
        }
    }
    
    plotDF1 <- subset(vegDF2, Variable%in%c("NL", "NW", "NFR", "NCR") & Trt=="amb")
    plotDF2 <- subset(vegDF2, Variable%in%c("Total") & Trt=="amb")
    
    val1 <- round((plotDF2$meanvalue[plotDF2$Model=="B_GDAYP"]-plotDF2$meanvalue[plotDF2$Model=="I_GDAYN"])/plotDF2$meanvalue[plotDF2$Model=="I_GDAYN"]*100, 1)
    val2 <- round((plotDF2$meanvalue[plotDF2$Model=="C_LPJGP"]-plotDF2$meanvalue[plotDF2$Model=="J_LPJGN"])/plotDF2$meanvalue[plotDF2$Model=="J_LPJGN"]*100, 1)
    
    
    ### Plotting C pools in ambient CO2
    p5 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position="stack", col="black") +
        annotate("text", x=2, y=plotDF2$meanvalue[plotDF2$Model=="GDAYP"]*1.15, 
                 label=(paste0(val1, "%")), size=10)+
        annotate("text", x=4, y=plotDF2$meanvalue[plotDF2$Model=="LPJGP"]*1.15, 
                 label=(paste0(val2, "%")), size=10)+
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
        ylab(expression(paste("Vegetation nitrogen pools (g N " * m^2*")")))+
        scale_x_discrete(limit=c("I_GDAYN","B_GDAYP", 
                                 "J_LPJGN","C_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("I_GDAYN" = 0.3, 
                                    "B_GDAYP" = 1.0, 
                                    "J_LPJGN" = 0.3, 
                                    "C_LPJGP" = 1.0),
                           label=c("GDAYN","GDAYP", 
                                   "LPJGN","LPJGP")); p5
    
    
    ### calculate CO2 pct response difference
    plotDF3 <- plotDF2
    plotDF3$meanvalue <- vegDF2$meanvalue[vegDF2$Variable=="Total"&vegDF2$Trt=="ele"]/vegDF2$meanvalue[vegDF2$Variable=="Total"&vegDF2$Trt=="amb"]
    plotDF3$sdvalue <- NA
    
    
    ### Plotting C pools in CO2 pct response
    p6 <- ggplot(data=plotDF3, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Model, alpha=Model), 
                 position="stack", col="black") +
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
        ylab(expression(paste("Vegetation nitrogen " * CO[2] *" response ratio")))+
        scale_x_discrete(limit=c("I_GDAYN","B_GDAYP", 
                                 "J_LPJGN","C_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("I_GDAYN" = 0.3, 
                                    "B_GDAYP" = 1.0, 
                                    "J_LPJGN" = 0.3, 
                                    "C_LPJGP" = 1.0),
                           label=c("GDAYN","GDAYP", 
                                   "LPJGN","LPJGP"))+
        scale_fill_manual(values=c("I_GDAYN" = "purple", "B_GDAYP" = "purple",
                                   "J_LPJGN" = "orange", "C_LPJGP" = "orange"),
                          label=c("GDAYN","GDAYP", 
                                  "LPJGN","LPJGP"))+
        coord_cartesian(ylim=c(0.9,1.1)); p6
    
    
    
    ##################################################################
    #### major N fluxes
    fluxDF2 <- data.frame(rep(c("NGL", "NLRETR", "NUP", "NMIN"), 8), 
                          rep(mod.list1, each = 8), 
                          rep(c("amb", "ele"), each = 4),
                          NA, NA)
    colnames(fluxDF2) <- c("Variable", 
                           "Model",
                           "Trt",
                           "meanvalue",
                           "sdvalue")
    
    for (i in c("amb", "ele")) {
        fluxDF2$meanvalue[fluxDF2$Variable=="NGL"&fluxDF2$Trt==i] <- myDF1$NGL.mean[myDF1$Trt==i]
        fluxDF2$sdvalue[fluxDF2$Variable=="NGL"&fluxDF2$Trt==i] <- myDF1$NGL.sd[myDF1$Trt==i]
        
        fluxDF2$meanvalue[fluxDF2$Variable=="NLRETR"&fluxDF2$Trt==i] <- myDF1$NLRETR.mean[myDF1$Trt==i]
        fluxDF2$sdvalue[fluxDF2$Variable=="NLRETR"&fluxDF2$Trt==i] <- myDF1$NLRETR.sd[myDF1$Trt==i]
        
        fluxDF2$meanvalue[fluxDF2$Variable=="NUP"&fluxDF2$Trt==i] <- myDF1$NUP.mean[myDF1$Trt==i]
        fluxDF2$sdvalue[fluxDF2$Variable=="NUP"&fluxDF2$Trt==i] <- myDF1$NUP.sd[myDF1$Trt==i]
        
        fluxDF2$meanvalue[fluxDF2$Variable=="NMIN"&fluxDF2$Trt==i] <- myDF1$NMIN.mean[myDF1$Trt==i]
        fluxDF2$sdvalue[fluxDF2$Variable=="NMIN"&fluxDF2$Trt==i] <- myDF1$NMIN.sd[myDF1$Trt==i]
    }
    
    plotDF1 <- fluxDF2[fluxDF2$Trt=="amb",]
    
    ### plotting NLRETR, NUP, and NMIN
    p7 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position=position_dodge(), col="black") +
        geom_errorbar(aes(x=Model, 
                          ymin=meanvalue-sdvalue, 
                          ymax=meanvalue+sdvalue), 
                      position=position_dodge()) +
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
        ylab(expression(paste("Nitrogen fluxes (g N " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("I_GDAYN","B_GDAYP", 
                                 "J_LPJGN","C_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("I_GDAYN" = 0.3, 
                                    "B_GDAYP" = 1.0, 
                                    "J_LPJGN" = 0.3, 
                                    "C_LPJGP" = 1.0),
                           label=c("GDAYN","GDAYP", 
                                   "LPJGN","LPJGP")); p7
    
    
    ### pct CO2 effect
    plotDF2 <- plotDF1
    plotDF2$Trt <- NULL
    
    plotDF2$meanvalue[plotDF2$Variable=="NGL"] <- myDF5$NGL.mean
    plotDF2$sdvalue[plotDF2$Variable=="NGL"] <- myDF5$NGL.sd
    
    plotDF2$meanvalue[plotDF2$Variable=="NLRETR"] <- myDF5$NLRETR.mean
    plotDF2$sdvalue[plotDF2$Variable=="NLRETR"] <- myDF5$NLRETR.sd
    
    plotDF2$meanvalue[plotDF2$Variable=="NUP"] <- myDF5$NUP.mean
    plotDF2$sdvalue[plotDF2$Variable=="NUP"] <- myDF5$NUP.sd
    
    plotDF2$meanvalue[plotDF2$Variable=="NMIN"] <- myDF5$NMIN.mean
    plotDF2$sdvalue[plotDF2$Variable=="NMIN"] <- myDF5$NMIN.sd
    
    
    ### subset
    plotDF2 <- subset(plotDF2, Variable%in%c("NGL", "NUP", "NMIN"))
    
    ### plotting 
    p8 <- ggplot(data=plotDF2, 
                 aes(Model, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position=position_dodge(), col="black") +
        geom_errorbar(aes(x=Model, 
                          ymin=meanvalue-sdvalue, 
                          ymax=meanvalue+sdvalue), 
                      position=position_dodge()) +
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
        ylab(expression(paste("Nitrogen fluxes " * CO[2] *" response ratio")))+
        scale_x_discrete(limit=c("I_GDAYN","B_GDAYP", 
                                 "J_LPJGN","C_LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("I_GDAYN" = 0.3, 
                                    "B_GDAYP" = 1.0, 
                                    "J_LPJGN" = 0.3, 
                                    "C_LPJGP" = 1.0),
                           label=c("GDAYN","GDAYP", 
                                   "LPJGN","LPJGP")); p8
    
    pdf(paste0(out.dir, '/MIP_CNP_vs_CN_model_comparisons.pdf',sep=''),width=12,height=8)
    for (i in 1:8) {
        print(get(paste("p",i,sep="")))
    }
    dev.off()
    
    
    
### end    
}