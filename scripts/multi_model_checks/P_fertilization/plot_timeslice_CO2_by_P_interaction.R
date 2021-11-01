plot_timeslice_CO2_by_P_interaction <- function (climate.scenario) {
    
    ### purpose:
    ### to plot the predicted trajectories
    ### also plot the CO2 response ratios,
    ### under var vs. fix
    ### under different P fertilization rates
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/PRD_output/", climate.scenario, "/")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read input - only the ambient CO2 treatment, 
    ### group into fixed and variable climate.
    inDF1 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_", 
                            climate.scenario, "_NOP_AMB_annual.rds"))
    inDF2 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_", 
                            climate.scenario, "_MDP_AMB_annual.rds"))
    inDF3 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_", 
                            climate.scenario, "_HIP_AMB_annual.rds"))
    
    inDF1$PTRT <- "NOP"
    inDF2$PTRT <- "MDP"
    inDF3$PTRT <- "HIP"
    
    myDF1 <- rbind(inDF1, rbind(inDF2, inDF3))
    
    
    ### group into fixed and variable climate.
    inDF1 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_", 
                            climate.scenario, "_NOP_ELE_annual.rds"))
    inDF2 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_", 
                            climate.scenario, "_MDP_ELE_annual.rds"))
    inDF3 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_", 
                            climate.scenario, "_HIP_ELE_annual.rds"))
    
    inDF1$PTRT <- "NOP"
    inDF2$PTRT <- "MDP"
    inDF3$PTRT <- "HIP"
    
    myDF2 <- rbind(inDF1, rbind(inDF2, inDF3))
    
    
    ### add CO2 treatment
    myDF1$Trt <- "AMB"
    myDF2$Trt <- "ELE"
    
    #### merge
    #myDF <- rbind(myDF1, myDF2)
    
    ### Calculate CO2 response ratio
    myDF3 <- myDF1
    
    l <- dim(myDF1)[2]
    myDF3[,3:(l-2)] <- myDF2[,3:(l-2)]/myDF1[,3:(l-2)]
    
    
    ### now we select the period of interest
    myDF3 <- myDF3[myDF3$YEAR%in%c(2013:2019, 2023:2029, 2063:2069),]
    myDF3$Period <- ifelse(myDF3$YEAR%in%c(2013:2019), "1_HIST",
                          ifelse(myDF3$YEAR%in%c(2023:2029), "2_SHORT", "3_LONG"))
    
    ### Calculate means
    plotDF <- summaryBy(.~ModName+PTRT+Period, FUN=c(mean,sd),
                        na.rm=T, keep.names=T, data=myDF3)
    
    
    ### remove the 1_HIST MDP and HIP scenarios
    plotDF1 <- subset(plotDF, Period != "1_HIST")
    plotDF2 <- subset(plotDF, Period == "1_HIST")
    plotDF2 <- subset(plotDF2, PTRT == "NOP")
    
    plotDF <- rbind(plotDF1, plotDF2)
    #plotDF$PTRT <- as.factor(plotDF$PTRT)
    
    #### Plot responses
    p1 <- ggplot(data=plotDF, 
                 aes(x=Period, y=GPP.mean, group=PTRT))+
        geom_bar(plotDF, stat = "identity", 
                 mapping=aes(Period, GPP.mean, fill=PTRT),
                 position=position_dodge(), col="black") +
        geom_errorbar(plotDF, stat = "identity", 
                 mapping=aes(Period, ymax=GPP.mean+GPP.sd, 
                             ymin=GPP.mean-GPP.sd),
                 position=position_dodge(width=0.9), 
                 width=0.5, col="black") +
        coord_cartesian(ylim=c(0.9, 1.4))+
        geom_hline(yintercept=1.0, col="black", lty=2)+
        scale_fill_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        scale_x_discrete(name="",
                         limits=c("1_HIST", "2_SHORT", "3_LONG"),
                         labels=c("2013-19", "2023-29", "2063-69"))+
        ylab(expression(CO[2] * " response of GPP"))+
        facet_wrap(ModName~., ncol=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5));p1
    
    
    
    #### Plot responses
    p2 <- ggplot(data=plotDF, 
                 aes(x=Period, y=NPP.mean, group=PTRT))+
        geom_bar(plotDF, stat = "identity", 
                 mapping=aes(Period, NPP.mean, fill=PTRT),
                 position=position_dodge(), col="black") +
        geom_errorbar(plotDF, stat = "identity", 
                      mapping=aes(Period, ymax=NPP.mean+NPP.sd, 
                                  ymin=NPP.mean-NPP.sd),
                      position=position_dodge(width=0.9), 
                      width=0.5, col="black") +
        coord_cartesian(ylim=c(0.9, 2.0))+
        geom_hline(yintercept=1.0, col="black", lty=2)+
        scale_fill_manual(name="Fertilization",
                          values=c("NOP"="pink",
                                   "MDP"="orange",
                                   "HIP"="red3"))+
        scale_x_discrete(name="",
                         limits=c("1_HIST", "2_SHORT", "3_LONG"),
                         labels=c("2013-19", "2023-29", "2063-69"))+
        ylab(expression(CO[2] * " response of NPP"))+
        facet_wrap(ModName~., ncol=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5));p2
    
    
    
    #### Plot responses
    p3 <- ggplot(data=plotDF, 
                 aes(x=Period, y=PL.mean, group=PTRT))+
        geom_bar(plotDF, stat = "identity", 
                 mapping=aes(Period, PL.mean, fill=PTRT),
                 position=position_dodge(), col="black") +
        geom_errorbar(plotDF, stat = "identity", 
                      mapping=aes(Period, ymax=PL.mean+PL.sd, 
                                  ymin=PL.mean-PL.sd),
                      position=position_dodge(width=0.9), 
                      width=0.5, col="black") +
        coord_cartesian(ylim=c(0.4, 1.4))+
        geom_hline(yintercept=1.0, col="black", lty=2)+
        scale_fill_manual(name="Fertilization",
                          values=c("NOP"="pink",
                                   "MDP"="orange",
                                   "HIP"="red3"))+
        scale_x_discrete(name="",
                         limits=c("1_HIST", "2_SHORT", "3_LONG"),
                         labels=c("2013-19", "2023-29", "2063-69"))+
        ylab(expression(CO[2] * " response of PL"))+
        facet_wrap(ModName~., ncol=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5));p3
    
    
    
    
    
    ### plot
    pdf(paste0(out.dir, 
               "CO2_response_of_P_treatments_", climate.scenario, 
               ".pdf"))
    for (i in 1:3) {
        print(get(paste("p",i,sep="")))
    }
    dev.off()
    
    
}