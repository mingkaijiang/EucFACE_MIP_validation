investigate_QUJSM_microbial_responses <- function (scenario) {
    
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
    ambDF <- subset(ambDF, ModName%in%c("F_QUINC", 
                                        "H_QUJSM"))
    
    
    eleDF <- subset(eleDF, ModName%in%c("F_QUINC", 
                                        "H_QUJSM"))
    
    ### just to plot NMIC and PMIC pool and their changes in aCO2 and eCO2
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    
    myDF <- rbind(ambDF, eleDF)
    
    myDF <- myDF[,c("ModName", "YEAR", "Trt",
                    "CMIC10", "CMIC30", "CMIC60",
                    "CMOC", "CTMOC",
                    "NMIC10", "NMIC30", "NMIC60",
                    "NMOC", "NTMOC",
                    "PMIC10", "PMIC30", "PMIC60",
                    "PMOC", "PTMOC")]
    
    sumDF <- summaryBy(CMIC10+CMIC30+CMIC60+
                       CMOC+CTMOC+
                       NMIC10+NMIC30+NMIC60+
                       NMOC+NTMOC+
                       PMIC10+PMIC30+PMIC60+
                       PMOC+PTMOC~ModName+Trt,
                       FUN=c(mean, sd), data=myDF, na.rm=T, keep.names=T)
    
    sumDF <- sumDF[sumDF$ModName=="H_QUJSM",]
    
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
    
    
    ### y.range
    if (scenario == "FIX") {
        p1.y.range <- c(0, 15)
        p2.y.range <- c(1.8, 2.2)
    } else {
        p1.y.range <- c(0, 18)
        p2.y.range <- c(2.0, 2.5)
    }
    
    
    
    ### plot
    plotDF1 <- plotDF[plotDF$Variable%in%c("CMIC10", "CMIC30", "CMIC60"),]
    
    p1 <- ggplot(data=plotDF1, 
                 aes(Variable, meanvalue, group=Trt)) +
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
        #scale_x_discrete(limit=c("amb","ele"),
        #                 label=c("amb"= "AMB",
        #                         "ele"= "ELE"))+
        xlab("")+
        #coord_cartesian(ylim=p1.y.range)+
        scale_fill_manual(name="",
                          values=c("amb"="blue3",
                                   "ele"="red2"),
                          labels=c("amb", "ele")); p1
        
    
    ### Nmic
    plotDF2 <- plotDF[plotDF$Variable%in%c("NMIC10", "NMIC30", "NMIC60"),]
    
    p2 <- ggplot(data=plotDF2, 
                 aes(Variable, meanvalue, group=Trt)) +
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
        ylab(expression(paste(N[microbe] * " pools (g C " * m^2*")")))+
        #scale_x_discrete(limit=c("amb","ele"),
        #                 label=c("amb"= "AMB",
        #                         "ele"= "ELE"))+
        xlab("")+
        #coord_cartesian(ylim=p1.y.range)+
        scale_fill_manual(name="",
                          values=c("amb"="blue3",
                                   "ele"="red2"),
                          labels=c("amb", "ele")); p2

    
    ### Pmic
    plotDF3 <- plotDF[plotDF$Variable%in%c("PMIC10", "PMIC30", "PMIC60"),]
    
    p3 <- ggplot(data=plotDF3, 
                 aes(Variable, meanvalue, group=Trt)) +
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
        ylab(expression(paste(P[microbe] * " pools (g C " * m^2*")")))+
        #scale_x_discrete(limit=c("amb","ele"),
        #                 label=c("amb"= "AMB",
        #                         "ele"= "ELE"))+
        xlab("")+
        #coord_cartesian(ylim=p1.y.range)+
        scale_fill_manual(name="",
                          values=c("amb"="blue3",
                                   "ele"="red2"),
                          labels=c("amb", "ele")); p3
    
    
    
    ### Cmoc
    plotDF4 <- plotDF[plotDF$Variable%in%c("CMOC", "CTMOC"),]
    
    p4 <- ggplot(data=plotDF4, 
                 aes(Variable, meanvalue, group=Trt)) +
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
        ylab(expression(paste("Mineral associated C" * " pools (g C " * m^2*")")))+
        #scale_x_discrete(limit=c("amb","ele"),
        #                 label=c("amb"= "AMB",
        #                         "ele"= "ELE"))+
        xlab("")+
        #coord_cartesian(ylim=p1.y.range)+
        scale_fill_manual(name="",
                          values=c("amb"="blue3",
                                   "ele"="red2"),
                          labels=c("amb", "ele")); p4
    
    
    
    pdf(paste0(out.dir, "/QUJSM_microbial_pools.pdf"), 
        width=10, height=6)
    grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
    dev.off()
    
    
    
    ### compare QUJSM soil P with data for all depths
    subDF <- plotDF[plotDF$Variable%in%c("PMIC10", "PMIC30", "PMIC60"),]
    
    
    ### add data
    subDF2 <- data.frame("ModName"="obs",
                         "Trt"=rep(c("amb", "ele"), each=3),
                         "Variable"=rep(c("PMIC10", 
                                          "PMIC30",
                                          "PMIC60"), 2),
                         "meanvalue"=c(3.43, 1.99+3.43, 0.55+1.99+3.43,
                                       2.86, 2.38+2.86, 0.78+2.38+2.86),
                         sdvalue=c(0.88, sqrt((0.88^2+0.54^2)/2), sqrt((0.88^2+0.54^2+0.17^2)/3),
                                   0.24, sqrt((0.17^2+0.24^2)/2), sqrt((0.59^2+0.17^2+0.24^2)/3)))
    
    
    plotDF2 <- rbind(subDF, subDF2)
    
    
    
    # New facet label names for supp variable
    facet.labs <- c("QUJSM", "OBS")
    names(facet.labs) <- c("H_QUJSM", "obs")
    
    facet.labs <- as_labeller(c(`H_QUJSM` = "QUJSM", `obs` = "OBS"))
    
    
    p1 <- ggplot(data=plotDF2, 
                 aes(Variable, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=meanvalue-sdvalue,
                                             ymax=meanvalue+sdvalue), 
                      position=position_dodge(width=0.8), width=0.5,
                      col="black") +
        facet_wrap(~ModName,labeller = facet.labs)+
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
        ylab(expression(paste(P[microbe] * " pools (g C " * m^2*")")))+
        #scale_x_discrete(limit=c("amb","ele"),
        #                 label=c("amb"= "AMB",
        #                         "ele"= "ELE"))+
        xlab("")+
        #coord_cartesian(ylim=p1.y.range)+
        scale_fill_manual(name="",
                          values=c("amb"="blue3",
                                   "ele"="red2"),
                          labels=c("amb", "ele")); p1
    

    pdf(paste0(out.dir, "/QUJSM_microbial_pools_P.pdf"), 
        width=6, height=4)
    plot(p1)
    dev.off()
    
    
}