investigate_OCHDX_microbial_responses <- function (scenario) {
    
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
    ambDF <- subset(ambDF, ModName%in%c("E_OCHDP", 
                                        "G_OCHDX"))
    
    
    eleDF <- subset(eleDF, ModName%in%c("E_OCHDP",
                                        "G_OCHDX"))
    
    ### just to plot NMIC and PMIC pool and their changes in aCO2 and eCO2
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    
    myDF <- rbind(ambDF, eleDF)
    
    myDF <- myDF[,c("ModName", "YEAR", "Trt",
                    "NMICR", "PMICR", "deltaNMICR", "deltaPMICR")]
    
    sumDF <- summaryBy(NMICR+PMICR+deltaNMICR+deltaPMICR~ModName+Trt,
                       FUN=c(mean, sd), data=myDF, na.rm=T, keep.names=T)
    
    sumDF <- sumDF[sumDF$ModName=="G_OCHDX",]
    
    ### y.range
    if (scenario == "FIX") {
        p1.y.range <- c(12, 15)
        p2.y.range <- c(1.8, 2.2)
    } else {
        p1.y.range <- c(12, 18)
        p2.y.range <- c(2.0, 2.5)
    }
    
    
    
    ### plot
    p1 <- ggplot(data=sumDF, 
                 aes(Trt, NMICR.mean)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=NMICR.mean-NMICR.sd,
                                             ymax=NMICR.mean+NMICR.sd), 
                 position=position_dodge(width=0.2), width=0.2,col="black") +
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
        scale_x_discrete(limit=c("amb","ele"),
                         label=c("amb"= "AMB",
                                 "ele"= "ELE"))+
        xlab("")+
        coord_cartesian(ylim=p1.y.range)+
        scale_fill_manual(name="",
                          values=c("amb"="blue3",
                                   "ele"="red2"),
                          labels=c("amb", "ele"))
        
    
    p2 <- ggplot(data=sumDF, 
                 aes(Trt, PMICR.mean)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position=position_dodge(), col="black") +
        geom_errorbar(stat = "identity", aes(ymin=PMICR.mean-PMICR.sd,
                                             ymax=PMICR.mean+PMICR.sd), 
                      position=position_dodge(width=0.2), width=0.2,col="black") +
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
        scale_x_discrete(limit=c("amb","ele"),
                         label=c("amb"= "AMB",
                                 "ele"= "ELE"))+
        xlab("")+
        coord_cartesian(ylim=p2.y.range)+
        scale_fill_manual(name="",
                          values=c("amb"="blue3",
                                   "ele"="red2"),
                          labels=c("amb", "ele"))
    

    pdf(paste0(out.dir, "/ORCHIDEE_microbial_pools.pdf"), 
        width=10, height=6)
    grid.arrange(p1, p2, nrow=1, ncol=2)
    dev.off()
    
    
    
}