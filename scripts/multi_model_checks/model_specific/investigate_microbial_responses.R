investigate_microbial_responses <- function (scenario, compare.to.obs) {
    
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
                    "NMICR", "PMICR")]
    
    myDF$NMIC10 <- ifelse(is.na(myDF$NMIC10), myDF$NMICR, myDF$NMIC10)
    myDF$PMIC10 <- ifelse(is.na(myDF$PMIC10), myDF$PMICR, myDF$PMIC10)
    
    
    
    sumDF <- summaryBy(CMIC10+NMIC10+PMIC10~ModName+Trt,
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
    
    if (compare.to.obs==T) {
        tmpDF <- data.frame("ModName"=rep("OBS", 6),
                            "Trt"=rep(c("amb", "ele"), each=3),
                            "Variable"=rep(c("CMIC10", "NMIC10", "PMIC10"), 2),
                            "meanvalue"=NA,
                            "sdvalue"=NA)
        
        tmpDF$meanvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="CMIC10"] <- 63.76
        tmpDF$meanvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="CMIC10"] <- 60.69
        tmpDF$sdvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="CMIC10"] <- 5.3
        tmpDF$sdvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="CMIC10"] <- 3.6
        
        tmpDF$meanvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="NMIC10"] <- 7.69
        tmpDF$meanvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="NMIC10"] <- 6.73
        tmpDF$sdvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="NMIC10"] <- 5.1
        tmpDF$sdvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="NMIC10"] <- 4.39
        
        tmpDF$meanvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="PMIC10"] <- 3.43
        tmpDF$meanvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="PMIC10"] <- 2.86
        tmpDF$sdvalue[tmpDF$Trt=="amb"&tmpDF$Variable=="PMIC10"] <- 0.88
        tmpDF$sdvalue[tmpDF$Trt=="ele"&tmpDF$Variable=="PMIC10"] <- 0.24    
        
        plotDF <- rbind(plotDF, tmpDF)
    }
    
    
    
    
    ### plot
    ### Cmic
    plotDF1 <- plotDF[plotDF$Variable%in%c("CMIC10"),]
    
    p1 <- ggplot(data=plotDF1, 
                 aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
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
        ylab(expression(paste(C[microbe] * " pools (g C " * m^2*")")))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        #coord_cartesian(ylim=p1.y.range)+
        scale_fill_manual(name="",
                          values=c("amb"="blue3",
                                   "ele"="red2"),
                          labels=c("amb", "ele")); p1
    
    
    ### Nmic
    plotDF2 <- plotDF[plotDF$Variable%in%c("NMIC10"),]
    
    p2 <- ggplot(data=plotDF2, 
                 aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
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
        ylab(expression(paste(N[microbe] * " pools (g N " * m^2*")")))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        #coord_cartesian(ylim=p1.y.range)+
        scale_fill_manual(name="",
                          values=c("amb"="blue3",
                                   "ele"="red2"),
                          labels=c("amb", "ele")); p2
    
    
    ### Pmic
    plotDF3 <- plotDF[plotDF$Variable%in%c("PMIC10"),]
    
    p3 <- ggplot(data=plotDF3, 
                 aes(ModName, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
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
        ylab(expression(paste(P[microbe] * " pools (g P " * m^2*")")))+
        scale_x_discrete(limit=c("G_OCHDX","H_QUJSM", "OBS"),
                         label=c("G_OCHDX"= "OCHDX",
                                 "H_QUJSM"= "QUJSM",
                                 "OBS"="OBS"))+
        xlab("")+
        #coord_cartesian(ylim=p1.y.range)+
        scale_fill_manual(name="",
                          values=c("amb"="blue3",
                                   "ele"="red2"),
                          labels=c("amb", "ele"))
    
    
    
    
    
    pdf(paste0(out.dir, "/two_microbial_model_microbial_pools.pdf"), 
        width=4, height=8)
    grid.arrange(p1, p2, p3, nrow=3, ncol=1)
    dev.off()
    
    
}