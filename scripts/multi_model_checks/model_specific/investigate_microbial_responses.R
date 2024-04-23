investigate_microbial_responses <- function (scenario, eucDF) {
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario, "/")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    
    ### read in prepared microbial model
    ### here we don't need to read in the model-specific variables
    ### because we are only interested in the general variable responses. 
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/microbial_models/MIP_OBS_", scenario, "_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/microbial_models/MIP_OBS_", scenario, "_ELE_annual.rds"))
    
    ### select model output
    ambDF <- subset(ambDF, ModName%in%c("E_OCHDP", "F_QUINC", 
                                        "G_OCHDX", "H_QUJSM"))
    
    
    eleDF <- subset(eleDF, ModName%in%c("E_OCHDP", "F_QUINC", 
                                        "G_OCHDX", "H_QUJSM"))
    
    ### just to plot NMIC and PMIC pool and their changes in aCO2 and eCO2
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    
    myDF <- rbind(ambDF, eleDF)
    
    myDF <- myDF[,c("ModName", "YEAR", "Trt",
                    "CMIC10", "CMIC30", "CMIC60",
                    #"CMOC", "CTMOC",
                    "NMIC10", #"NMIC30", "NMIC60",
                    #"NMOC", "NTMOC",
                    "PMIC10", "PMIC30", "PMIC60",
                    #"PMOC", "PTMOC")]
                    "CMICR", "NMICR", "PMICR",
                    "deltaCMIC10", "deltaCMIC30", "deltaCMIC60",
                    "deltaPMIC10", "deltaPMIC30", "deltaPMIC60",
                    "deltaCMICR",  "deltaPMICR")]
    
    myDF$CMIC10 <- ifelse(is.na(myDF$CMIC10), myDF$CMICR, myDF$CMIC10)
    myDF$NMIC10 <- ifelse(is.na(myDF$NMIC10), myDF$NMICR, myDF$NMIC10)
    myDF$PMIC10 <- ifelse(is.na(myDF$PMIC10), myDF$PMICR, myDF$PMIC10)
    
    myDF$deltaCMIC10 <- ifelse(is.na(myDF$deltaCMIC10), myDF$deltaCMICR, myDF$deltaCMIC10)
    myDF$deltaPMIC10 <- ifelse(is.na(myDF$deltaPMIC10), myDF$deltaPMICR, myDF$deltaPMIC10)
    
    
    
    sumDF <- summaryBy(CMIC10+NMIC10+PMIC10+deltaCMIC10+deltaPMIC10~ModName+Trt,
                       FUN=c(mean, sd), data=myDF, na.rm=T, keep.names=T)
    
    sumDF <- sumDF[sumDF$ModName%in%c("G_OCHDX","H_QUJSM"),]
    
    
    ### melt
    mtDF <- reshape::melt(sumDF, id.vars=c("ModName", "Trt"))
    mtDF$Variable <- gsub("\\..*", "", mtDF$variable)
    mtDF$Category <- gsub("^.*\\.", "", mtDF$variable)
    mtDF$variable <- NULL
    
    mtDF1 <- mtDF[mtDF$Category=="mean",]
    mtDF2 <- mtDF[mtDF$Category=="sd",]
    mtDF <- merge(mtDF1, mtDF2, by=c("ModName", "Trt", "Variable"))
    names(mtDF)[names(mtDF)=="value.x"] <- "meanvalue"
    names(mtDF)[names(mtDF)=="value.y"] <- "sdvalue"
    
    plotDF <- mtDF[,c("ModName", "Trt", "Variable", "meanvalue", "sdvalue")]
    
    ### make all sd values in models zero
    plotDF$sdvalue <- NA
    
    
    
    ### add data
    tmpDF <- data.frame("ModName"=rep("OBS", 6),
                        "Trt"=rep(c("amb", "ele"), each=3),
                        "Variable"=rep(c("CMIC10", "NMIC10", "PMIC10"), 2),
                        "meanvalue"=NA,
                        "sdvalue"=NA)
    
    tmpDF$meanvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="CMIC10"] <- eucDF$CMIC[eucDF$Group=="mean"&eucDF$Trt=="aCO2"]
    tmpDF$meanvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="CMIC10"] <- eucDF$CMIC[eucDF$Group=="mean"&eucDF$Trt=="eCO2"]
    tmpDF$sdvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="CMIC10"] <- eucDF$CMIC[eucDF$Group=="sd"&eucDF$Trt=="aCO2"]
    tmpDF$sdvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="CMIC10"] <- eucDF$CMIC[eucDF$Group=="sd"&eucDF$Trt=="eCO2"]
    
    tmpDF$meanvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="NMIC10"] <- 7.69
    tmpDF$meanvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="NMIC10"] <- 6.73
    tmpDF$sdvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="NMIC10"] <- 5.1
    tmpDF$sdvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="NMIC10"] <- 4.39
    
    tmpDF$meanvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="PMIC10"] <- eucDF$PMIC[eucDF$Group=="mean"&eucDF$Trt=="aCO2"]
    tmpDF$meanvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="PMIC10"] <- eucDF$PMIC[eucDF$Group=="mean"&eucDF$Trt=="eCO2"]
    tmpDF$sdvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="PMIC10"] <- eucDF$PMIC[eucDF$Group=="sd"&eucDF$Trt=="aCO2"]
    tmpDF$sdvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="PMIC10"] <- eucDF$PMIC[eucDF$Group=="sd"&eucDF$Trt=="eCO2"]
    
    plotDF <- rbind(plotDF, tmpDF)
    
    
    
    ### add data
    tmpDF <- data.frame("ModName"=rep("OBS", 4),
                        "Trt"=rep(c("amb", "ele"), each=2),
                        "Variable"=rep(c("deltaCMIC10", "deltaPMIC10"), 2),
                        "meanvalue"=NA,
                        "sdvalue"=NA)
    
    tmpDF$meanvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="deltaCMIC10"] <- eucDF$deltaCMIC[eucDF$Group=="mean"&eucDF$Trt=="aCO2"]
    tmpDF$meanvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="deltaCMIC10"] <- eucDF$deltaCMIC[eucDF$Group=="mean"&eucDF$Trt=="eCO2"]
    tmpDF$sdvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="deltaCMIC10"] <- eucDF$deltaCMIC[eucDF$Group=="sd"&eucDF$Trt=="aCO2"]
    tmpDF$sdvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="deltaCMIC10"] <- eucDF$deltaCMIC[eucDF$Group=="sd"&eucDF$Trt=="eCO2"]
    
    
    tmpDF$meanvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="deltaPMIC10"] <- eucDF$deltaPMIC[eucDF$Group=="mean"&eucDF$Trt=="aCO2"]
    tmpDF$meanvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="deltaPMIC10"] <- eucDF$deltaPMIC[eucDF$Group=="mean"&eucDF$Trt=="eCO2"]
    tmpDF$sdvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="deltaPMIC10"] <- eucDF$deltaPMIC[eucDF$Group=="sd"&eucDF$Trt=="aCO2"]
    tmpDF$sdvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="deltaPMIC10"] <- eucDF$deltaPMIC[eucDF$Group=="sd"&eucDF$Trt=="eCO2"]
    
    plotDF <- rbind(plotDF, tmpDF)
    
    
    ### calculate CO2 effect in absolute and percentage
    tmpDF2 <- plotDF
    tmpDF2$Trt[tmpDF2$Trt=="amb"] <- "diff"
    tmpDF2$Trt[tmpDF2$Trt=="ele"] <- "pct_diff"
    
    for (i in c("OBS", "G_OCHDX", "H_QUJSM")) {
        for (j in c("CMIC10", "NMIC10", "PMIC10",
                    "deltaCMIC10", "deltaPMIC10")) {
            tmpDF2$meanvalue[tmpDF2$ModName==i&tmpDF2$Trt=="diff"&tmpDF2$Variable==j] <- plotDF$meanvalue[plotDF$ModName==i&plotDF$Trt=="ele"&plotDF$Variable==j]-plotDF$meanvalue[plotDF$ModName==i&plotDF$Trt=="amb"&plotDF$Variable==j]
            
            tmpDF2$sdvalue[tmpDF2$ModName==i&tmpDF2$Trt=="diff"&tmpDF2$Variable==j] <- sqrt(sum(c(plotDF$sdvalue[plotDF$ModName==i&plotDF$Trt=="ele"&plotDF$Variable==j]^2,
                                                                                                  plotDF$sdvalue[plotDF$ModName==i&plotDF$Trt=="amb"&plotDF$Variable==j]^2), na.rm=T)/2)
            
            tmpDF2$meanvalue[tmpDF2$ModName==i&tmpDF2$Trt=="pct_diff"&tmpDF2$Variable==j] <- (plotDF$meanvalue[plotDF$ModName==i&plotDF$Trt=="ele"&plotDF$Variable==j]-
                                                                                                  plotDF$meanvalue[plotDF$ModName==i&plotDF$Trt=="amb"&plotDF$Variable==j])/abs(plotDF$meanvalue[plotDF$ModName==i&plotDF$Trt=="amb"&plotDF$Variable==j])*100
            
            tmpDF2$sdvalue[tmpDF2$ModName==i&tmpDF2$Trt=="pct_diff"&tmpDF2$Variable==j] <- sqrt(sum(c(plotDF$sdvalue[plotDF$ModName==i&plotDF$Trt=="ele"&plotDF$Variable==j]^2,
                                                                                                  plotDF$sdvalue[plotDF$ModName==i&plotDF$Trt=="amb"&plotDF$Variable==j]^2,
                                                                                                  plotDF$sdvalue[plotDF$ModName==i&plotDF$Trt=="amb"&plotDF$Variable==j]^2), na.rm=T)/3)/abs(plotDF$meanvalue[plotDF$ModName==i&plotDF$Trt=="amb"&plotDF$Variable==j])*100
        }
    }
    
    ### revise obs
    tmpDF2$meanvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="diff"&tmpDF2$Variable=="CMIC"] <- eucDF$CMIC[eucDF$Group=="mean"&eucDF$Trt=="diff"]
    tmpDF2$sdvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="diff"&tmpDF2$Variable=="CMIC"] <- eucDF$CMIC[eucDF$Group=="sd"&eucDF$Trt=="diff"]
    
    tmpDF2$meanvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="pct_diff"&tmpDF2$Variable=="CMIC"] <- eucDF$CMIC[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    tmpDF2$sdvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="pct_diff"&tmpDF2$Variable=="CMIC"] <- eucDF$CMIC[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    
    tmpDF2$meanvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="diff"&tmpDF2$Variable=="PMIC"] <- eucDF$PMIC[eucDF$Group=="mean"&eucDF$Trt=="diff"]
    tmpDF2$sdvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="diff"&tmpDF2$Variable=="PMIC"] <- eucDF$PMIC[eucDF$Group=="sd"&eucDF$Trt=="diff"]
    
    tmpDF2$meanvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="pct_diff"&tmpDF2$Variable=="PMIC"] <- eucDF$PMIC[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    tmpDF2$sdvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="pct_diff"&tmpDF2$Variable=="PMIC"] <- eucDF$PMIC[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    
    ## delta
    tmpDF2$meanvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="diff"&tmpDF2$Variable=="deltaCMIC10"] <- eucDF$deltaCMIC[eucDF$Group=="mean"&eucDF$Trt=="diff"]
    tmpDF2$sdvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="diff"&tmpDF2$Variable=="deltaCMIC10"] <- eucDF$deltaCMIC[eucDF$Group=="sd"&eucDF$Trt=="diff"]
    
    tmpDF2$meanvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="pct_diff"&tmpDF2$Variable=="deltaCMIC10"] <- eucDF$deltaCMIC[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    tmpDF2$sdvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="pct_diff"&tmpDF2$Variable=="deltaCMIC10"] <- eucDF$deltaCMIC[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    
    tmpDF2$meanvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="diff"&tmpDF2$Variable=="deltaPMIC10"] <- eucDF$deltaPMIC[eucDF$Group=="mean"&eucDF$Trt=="diff"]
    tmpDF2$sdvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="diff"&tmpDF2$Variable=="deltaPMIC10"] <- eucDF$deltaPMIC[eucDF$Group=="sd"&eucDF$Trt=="diff"]
    
    tmpDF2$meanvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="pct_diff"&tmpDF2$Variable=="deltaPMIC10"] <- eucDF$deltaPMIC[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    tmpDF2$sdvalue[tmpDF2$ModName=="OBS"&tmpDF2$Trt=="pct_diff"&tmpDF2$Variable=="deltaPMIC10"] <- eucDF$deltaPMIC[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    
    ### plot
    ### Cmic
    plotDF1 <- plotDF[plotDF$Variable%in%c("CMIC10"),]
    plotDF2 <- tmpDF2[tmpDF2$Variable%in%c("CMIC10")&tmpDF2$Trt=="pct_diff",]
    plotDF3 <- plotDF[plotDF$Variable%in%c("deltaCMIC10"),]
    plotDF4 <- tmpDF2[tmpDF2$Variable%in%c("deltaCMIC10")&tmpDF2$Trt=="pct_diff",]
    
    p1 <- ggplot(data=plotDF1, 
                 aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=meanvalue-sdvalue,
                                             ymax=meanvalue+sdvalue), 
                      position=position_dodge(width=0.8), width=0.5,
                      col="black") +
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
        ylab(expression(paste(C[mic] * " (g C " * m^2*")")))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, "black"),
                          labels=c(model.labels, 
                                   "OBS" = "OBS"))+
        ylim(0, 200)
        
    
    p1_2 <- ggplot(data=plotDF2, 
                   aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=meanvalue-sdvalue,
                                             ymax=meanvalue+sdvalue), 
                      position=position_dodge(width=0.8), width=0.5,
                      col="black") +
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
        ylab(expression(CO[2] * "effect (%)"))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, "black"),
                          labels=c(model.labels, 
                                   "OBS" = "OBS"))+
        ylim(-30, 10)
    
    
    p1_3 <- ggplot(data=plotDF3, 
                 aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=meanvalue-sdvalue,
                                             ymax=meanvalue+sdvalue), 
                      position=position_dodge(width=0.8), width=0.5,
                      col="black") +
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
        ylab(expression(paste(Delta * C[mic] * " (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, "black"),
                          labels=c(model.labels, 
                                   "OBS" = "OBS"))
    
    
    
    p1_4 <- ggplot(data=plotDF4, 
                   aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=meanvalue-sdvalue,
                                             ymax=meanvalue+sdvalue), 
                      position=position_dodge(width=0.8), width=0.5,
                      col="black") +
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
        ylab(expression(CO[2] * "effect (%)"))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, "black"),
                          labels=c(model.labels, 
                                   "OBS" = "OBS"))
    
    
    
    ### Nmic
    plotDF1 <- plotDF[plotDF$Variable%in%c("NMIC10"),]
    plotDF2 <- tmpDF2[tmpDF2$Variable%in%c("NMIC10")&tmpDF2$Trt=="pct_diff",]
    
    
    
    p2 <- ggplot(data=plotDF1, 
                 aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=meanvalue-sdvalue,
                                             ymax=meanvalue+sdvalue), 
                      position=position_dodge(width=0.8), width=0.5,
                      col="black") +
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
        ylab(expression(paste(N[mic] * " (g N " * m^2*")")))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, "black"),
                          labels=c(model.labels, 
                                   "OBS" = "OBS"))
    
    
    p2_2 <- ggplot(data=plotDF2, 
                   aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=meanvalue-sdvalue,
                                             ymax=meanvalue+sdvalue), 
                      position=position_dodge(width=0.8), width=0.5,
                      col="black") +
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
        ylab(expression(CO[2] * "effect (%)"))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, "black"),
                          labels=c(model.labels, 
                                   "OBS" = "OBS"))
    
    
    
    ### Pmic
    plotDF1 <- plotDF[plotDF$Variable%in%c("PMIC10"),]
    plotDF2 <- tmpDF2[tmpDF2$Variable%in%c("PMIC10")&tmpDF2$Trt=="pct_diff",]
    plotDF3 <- plotDF[plotDF$Variable%in%c("deltaPMIC10"),]
    plotDF4 <- tmpDF2[tmpDF2$Variable%in%c("deltaPMIC10")&tmpDF2$Trt=="pct_diff",]
    
    
    
    p3 <- ggplot(data=plotDF1, 
                 aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=meanvalue-sdvalue,
                                             ymax=meanvalue+sdvalue), 
                      position=position_dodge(width=0.8), width=0.5,
                      col="black") +
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
        ylab(expression(paste(P[mic] * " (g P " * m^2*")")))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, "black"),
                          labels=c(model.labels, 
                                   "OBS" = "OBS"))
    
    
    p3_2 <- ggplot(data=plotDF2, 
                   aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=meanvalue-sdvalue,
                                             ymax=meanvalue+sdvalue), 
                      position=position_dodge(width=0.8), width=0.5,
                      col="black") +
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
        ylab(expression(CO[2] * "effect (%)"))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, "black"),
                          labels=c(model.labels, 
                                   "OBS" = "OBS"))+
        ylim(-40, 20)
    
    p3_3 <- ggplot(data=plotDF3, 
                   aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=meanvalue-sdvalue,
                                             ymax=meanvalue+sdvalue), 
                      position=position_dodge(width=0.8), width=0.5,
                      col="black") +
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
        ylab(expression(paste(Delta * P[mic] * " (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, "black"),
                          labels=c(model.labels, 
                                   "OBS" = "OBS"))
    
    
    
    p3_4 <- ggplot(data=plotDF4, 
                   aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=meanvalue-sdvalue,
                                             ymax=meanvalue+sdvalue), 
                      position=position_dodge(width=0.8), width=0.5,
                      col="black") +
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
        ylab(expression(CO[2] * "effect (%)"))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, "black"),
                          labels=c(model.labels, 
                                   "OBS" = "OBS"))
    
    
    
    
    pdf(paste0(out.dir, "/two_microbial_model_microbial_pools.pdf"), 
        width=10, height=8)
    plot_grid(p1, p1_2,
              p3, p3_2,
              labels=c("A", "B", "C", "D"), label_x=0.18, label_y=0.97,
              label_size=24,
              ncol=2)
    
    #grid.arrange(p1, p1_2, #p1_3, p1_4, 
    #             #p2, p2_2,
    #             p3, p3_2, #p3_3, p3_4, 
    #             nrow=2, ncol=2)
    dev.off()
    
    
}