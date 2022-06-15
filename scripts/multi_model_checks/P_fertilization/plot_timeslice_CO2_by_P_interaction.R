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
    
    
    ### read in eCO2
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
    
    
    ### calculate BP (i.e. deltaCveg)
    myDF1$deltaCVEG <- rowSums(data.frame(myDF1$deltaCL, myDF1$deltaCW, 
                                 myDF1$deltaCFR, myDF1$deltaCCR,
                                 myDF1$CSTOR), na.rm=T)
    
    
    myDF2$deltaCVEG <- rowSums(data.frame(myDF2$deltaCL, myDF2$deltaCW, 
                                          myDF2$deltaCFR, myDF2$deltaCCR,
                                          myDF2$CSTOR), na.rm=T)
    
    
    ### add CO2 treatment
    myDF1$Trt <- "AMB"
    myDF2$Trt <- "ELE"
    
    l <- dim(myDF1)[2]
    
    tmpDF1<- cbind(myDF1[,-(l-2)], myDF1$PTRT)
    tmpDF2<- cbind(myDF2[,-(l-2)], myDF2$PTRT)
    
    colnames(tmpDF1)[l] <- "PTRT"
    colnames(tmpDF2)[l] <- "PTRT"
    
    myDF1 <- tmpDF1
    myDF2 <- tmpDF2
    
    #### merge
    #myDF <- rbind(myDF1, myDF2)
    
    ### Calculate CO2 response ratio
    myDF3 <- myDF1
    myDF3[,3:(l-2)] <- (myDF2[,3:(l-2)]-myDF1[,3:(l-2)])/myDF1[,3:(l-2)] * 100
    
    ### calculate CO2 response magnitude
    myDF4 <- myDF1
    myDF4[,3:(l-2)] <- myDF2[,3:(l-2)]-myDF1[,3:(l-2)]
    
    
    ### now we select the period of interest
    myDF3 <- myDF3[myDF3$YEAR%in%c(2013:2019, 2020:2022, 2023:2029, 2063:2069),]
    myDF3$Period <- ifelse(myDF3$YEAR%in%c(2013:2019), "1_HIST",
                           ifelse(myDF3$YEAR%in%c(2020:2022), "2_TMP",
                                  ifelse(myDF3$YEAR%in%c(2023:2029), "3_SHORT", "4_LONG")))
    
    myDF4 <- myDF4[myDF4$YEAR%in%c(2013:2019, 2020:2022, 2023:2029, 2063:2069),]
    myDF4$Period <- ifelse(myDF4$YEAR%in%c(2013:2019), "1_HIST",
                           ifelse(myDF4$YEAR%in%c(2020:2022), "2_TMP",
                                  ifelse(myDF4$YEAR%in%c(2023:2029), "3_SHORT", "4_LONG")))
    
    
    myDF1 <- myDF1[myDF1$YEAR%in%c(2013:2019, 2020:2022, 2023:2029, 2063:2069),]
    myDF1$Period <- ifelse(myDF1$YEAR%in%c(2013:2019), "1_HIST",
                           ifelse(myDF1$YEAR%in%c(2020:2022), "2_TMP",
                                  ifelse(myDF1$YEAR%in%c(2023:2029), "3_SHORT", "4_LONG")))
    
    
    ### Calculate means
    plotDFa <- summaryBy(.~ModName+PTRT+Period, FUN=c(mean,sd),
                        na.rm=T, keep.names=T, data=myDF3)
    
    plotDFb <- summaryBy(.~ModName+PTRT+Period, FUN=c(mean,sd),
                         na.rm=T, keep.names=T, data=myDF4)
    
    plotDFc <- summaryBy(.~ModName+PTRT+Period, FUN=c(mean,sd),
                         na.rm=T, keep.names=T, data=myDF1)
    
    ### remove the 1_HIST MDP and HIP scenarios
    plotDF1 <- subset(plotDFa, Period != "1_HIST")
    plotDF2 <- subset(plotDFa, Period == "1_HIST")
    plotDF2 <- subset(plotDF2, PTRT == "NOP")
    
    plotDFa <- rbind(plotDF1, plotDF2)
    
    
    plotDF1 <- subset(plotDFb, Period != "1_HIST")
    plotDF2 <- subset(plotDFb, Period == "1_HIST")
    plotDF2 <- subset(plotDF2, PTRT == "NOP")
    
    plotDFb <- rbind(plotDF1, plotDF2)
    
    plotDF1 <- subset(plotDFc, Period != "1_HIST")
    plotDF2 <- subset(plotDFc, Period == "1_HIST")
    plotDF2 <- subset(plotDF2, PTRT == "NOP")
    
    plotDFc <- rbind(plotDF1, plotDF2)
    
    ### remove two CN models
    plotDFa <- plotDFa[plotDFa$ModName%in%c("A_GDAYP", "B_ELMV1", "C_CABLP",
                                            "D_LPJGP", "E_OCHDP", "F_QUINC",
                                            "G_OCHDX", "H_QUJSM"),]
    
    plotDFb <- plotDFb[plotDFb$ModName%in%c("A_GDAYP", "B_ELMV1", "C_CABLP",
                                            "D_LPJGP", "E_OCHDP", "F_QUINC",
                                            "G_OCHDX", "H_QUJSM"),]
    
    plotDFc <- plotDFc[plotDFc$ModName%in%c("A_GDAYP", "B_ELMV1", "C_CABLP",
                                            "D_LPJGP", "E_OCHDP", "F_QUINC",
                                            "G_OCHDX", "H_QUJSM"),]
    
    
    model.names <- c("A_GDAYP", "B_ELMV1", "C_CABLP",
                     "D_LPJGP", "E_OCHDP", "F_QUINC",
                     "G_OCHDX", "H_QUJSM")
    
    model.labels <- c("A_GDAYP" = "GDAYP",
                      "B_ELMV1" = "ELMV1",
                      "C_CABLP" = "CABLP",
                      "D_LPJGP" = "LPJGP",
                      "E_OCHDP" = "OCDHP",
                      "F_QUINC" = "QUINC",
                      "G_OCHDX" = "OCHDX",
                      "H_QUJSM" = "QUJSM")
    
    
    ### we can't compare e and a over the 2060s, because a has risen as well.
    ### remove them.
    plotDFa <- plotDFa[plotDFa$Period!="4_LONG",]
    plotDFb <- plotDFb[plotDFb$Period!="4_LONG",]
    plotDFc <- plotDFc[plotDFc$Period!="4_LONG",]
    
    ### no need to include historic period
    plotDFa <- plotDFa[plotDFa$Period!="1_HIST",]
    plotDFb <- plotDFb[plotDFb$Period!="1_HIST",]
    plotDFc <- plotDFc[plotDFc$Period!="1_HIST",]
    
    ### there isn't much difference between 2020-22 and 2023-29.
    ### hence remove it in the plot
    plotDFa <- plotDFa[plotDFa$Period!="2_TMP",]
    plotDFb <- plotDFb[plotDFb$Period!="2_TMP",]
    plotDFc <- plotDFc[plotDFc$Period!="2_TMP",]
    
    
    ### calculate multi-model means
    ### calculate multi-model means and sds
    multDF1 <- summaryBy(.~PTRT+Period, data=plotDFa[,1:l],
                         FUN=c(mean,sd), keep.names=T, na.rm=T)
    
    multDF2 <- summaryBy(.~PTRT+Period, data=plotDFb[,1:l],
                         FUN=c(mean,sd), keep.names=T, na.rm=T)
    
    multDF3 <- summaryBy(.~PTRT+Period, data=plotDFc[,1:l],
                         FUN=c(mean,sd), keep.names=T, na.rm=T)
    
    
    colnames(multDF1) <- colnames(plotDFa)[2:dim(plotDFa)[2]]
    colnames(multDF2) <- colnames(plotDFb)[2:dim(plotDFa)[2]]
    colnames(multDF3) <- colnames(plotDFc)[2:dim(plotDFa)[2]]
    
    
    multDF1$ModName <- "Multi-model"
    multDF2$ModName <- "Multi-model"
    multDF3$ModName <- "Multi-model"
    
    multDF1 <- multDF1[,paste0(colnames(plotDFa))]
    multDF2 <- multDF2[,paste0(colnames(plotDFb))]
    multDF3 <- multDF3[,paste0(colnames(plotDFc))]
    
    
    plotDFa <- rbind(plotDFa, multDF1)
    plotDFb <- rbind(plotDFb, multDF2)
    plotDFc <- rbind(plotDFc, multDF3)
    
    
    
    
    
    #### Plot responses
    p1 <- ggplot(data=plotDFa, 
                 aes(x=ModName, y=GPP.mean, group=PTRT))+
        geom_bar(plotDFa, stat = "identity", 
                 mapping=aes(ModName, GPP.mean, fill=PTRT),
                 position=position_dodge(), col="black") +
        geom_errorbar(plotDFa, stat = "identity", 
                      mapping=aes(ModName, ymax=GPP.mean+GPP.sd, 
                                  ymin=GPP.mean-GPP.sd),
                      position=position_dodge(width=0.9), 
                      width=0.5, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        #coord_cartesian(ylim=c(90, 140))+
        #geom_hline(yintercept=1.0, col="black", lty=2)+
        scale_fill_manual(name="Fertilization",
                          values=c("NOP"="pink",
                                   "MDP"="orange",
                                   "HIP"="red3"))+
        scale_x_discrete(name="",
                         guide = guide_axis(n.dodge = 2),
                         limits=c(model.names, 
                                  "Multi-model"),
                         labels=c(model.labels,
                                  "Multi-model"=expression(bold("Multi-model"))))+
        ylab(expression(CO[2] * " effect (%)"))+
        #facet_wrap(Period~., ncol=4, 
        #           labeller=as_labeller(c("1_HIST"="2013-19", 
        #                                  "2_TMP"="2020-22",
        #                                  "3_SHORT"="2023-29", 
        #                                  "4_LONG"="2063-69")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              strip.text = element_text(size=20),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5));p1
    
    
    
    #### Plot responses
    p2 <- ggplot(data=plotDFb, 
                 aes(x=ModName, y=deltaCVEG.mean/1000, group=PTRT))+
        geom_bar(plotDFb, stat = "identity", 
                 mapping=aes(ModName, deltaCVEG.mean/1000, fill=PTRT),
                 position=position_dodge(), col="black") +
        geom_errorbar(plotDFb, stat = "identity", 
                      mapping=aes(ModName, ymax=(deltaCVEG.mean+deltaCVEG.sd)/1000, 
                                  ymin=(deltaCVEG.mean-deltaCVEG.sd)/1000),
                      position=position_dodge(width=0.9), 
                      width=0.5, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        #coord_cartesian(ylim=c(0.9, 2.0))+
        #geom_hline(yintercept=1.0, col="black", lty=2)+
        scale_fill_manual(name="Fertilization",
                          values=c("NOP"="pink",
                                   "MDP"="orange",
                                   "HIP"="red3"))+
        scale_x_discrete(name="",
                         guide = guide_axis(n.dodge = 2),
                         limits=c(model.names, 
                                  "Multi-model"),
                         labels=c(model.labels,
                                  "Multi-model"=expression(bold("Multi-model"))))+
        ylab(expression(CO[2] * " effect (kg C " * m^-2 * " " * yr^-1 * ")"))+
        #facet_wrap(Period~., ncol=4, 
        #           labeller=as_labeller(c("1_HIST"="2013-19", 
        #                                  "2_TMP"="2020-22",
        #                                  "3_SHORT"="2023-29", 
        #                                  "4_LONG"="2063-69")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              strip.text = element_text(size=20),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5));p2
    
    
    
    #### Plot responses
    p3 <- ggplot(data=plotDFa, 
                 aes(x=ModName, y=PLAB.mean, group=PTRT))+
        geom_bar(plotDFa, stat = "identity", 
                 mapping=aes(ModName, PLAB.mean, fill=PTRT),
                 position=position_dodge(), col="black") +
        geom_errorbar(plotDFa, stat = "identity", 
                      mapping=aes(ModName, ymax=PLAB.mean+PLAB.sd, 
                                  ymin=PLAB.mean-PLAB.sd),
                      position=position_dodge(width=0.9), 
                      width=0.5, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        #coord_cartesian(ylim=c(0.4, 1.4))+
        #geom_hline(yintercept=1.0, col="black", lty=2)+
        scale_fill_manual(name="Fertilization",
                          values=c("NOP"="pink",
                                   "MDP"="orange",
                                   "HIP"="red3"))+
        scale_x_discrete(name="",
                         guide = guide_axis(n.dodge = 2),
                         limits=c(model.names, 
                                  "Multi-model"),
                         labels=c(model.labels,
                                  "Multi-model"=expression(bold("Multi-model"))))+
        ylab(expression(CO[2] * " response of PLAB (%)"))+
        #facet_wrap(Period~., ncol=4, 
        #           labeller=as_labeller(c("1_HIST"="2013-19", 
        #                                  "2_TMP"="2020-22",
        #                                  "3_SHORT"="2023-29", 
        #                                  "4_LONG"="2063-69")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              strip.text = element_text(size=20),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5));p3
    
    
    #### Plot responses
    p4 <- ggplot(data=plotDFa, 
                 aes(x=ModName, y=PUP.mean, group=PTRT))+
        geom_bar(plotDFa, stat = "identity", 
                 mapping=aes(ModName, PUP.mean, fill=PTRT),
                 position=position_dodge(), col="black") +
        geom_errorbar(plotDFa, stat = "identity", 
                      mapping=aes(ModName, ymax=PUP.mean+PUP.sd, 
                                  ymin=PUP.mean-PUP.sd),
                      position=position_dodge(width=0.9), 
                      width=0.5, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        #coord_cartesian(ylim=c(0.4, 1.4))+
        #geom_hline(yintercept=1.0, col="black", lty=2)+
        scale_fill_manual(name="Fertilization",
                          values=c("NOP"="pink",
                                   "MDP"="orange",
                                   "HIP"="red3"))+
        scale_x_discrete(name="",
                         guide = guide_axis(n.dodge = 2),
                         limits=c(model.names, 
                                  "Multi-model"),
                         labels=c(model.labels,
                                  "Multi-model"=expression(bold("Multi-model"))))+
        ylab(expression(CO[2] * " effect (%)"))+
        #facet_wrap(Period~., ncol=4, 
        #           labeller=as_labeller(c("1_HIST"="2013-19", 
        #                                  "2_TMP"="2020-22",
        #                                  "3_SHORT"="2023-29", 
        #                                  "4_LONG"="2063-69")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              strip.text = element_text(size=20),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5));p4
    
    
    #### Plot responses
    p5 <- ggplot(data=plotDFb, 
                 aes(x=ModName, y=PMIN.mean, group=PTRT))+
        geom_bar(plotDFb, stat = "identity", 
                 mapping=aes(ModName, PMIN.mean, fill=PTRT),
                 position=position_dodge(), col="black") +
        geom_errorbar(plotDFb, stat = "identity", 
                      mapping=aes(ModName, ymax=PMIN.mean+PMIN.sd, 
                                  ymin=PMIN.mean-PMIN.sd),
                      position=position_dodge(width=0.9), 
                      width=0.5, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        #coord_cartesian(ylim=c(0.4, 1.4))+
        #geom_hline(yintercept=1.0, col="black", lty=2)+
        scale_fill_manual(name="Fertilization",
                          values=c("NOP"="pink",
                                   "MDP"="orange",
                                   "HIP"="red3"))+
        scale_x_discrete(name="",
                         guide = guide_axis(n.dodge = 2),
                         limits=c(model.names, 
                                  "Multi-model"),
                         labels=c(model.labels,
                                  "Multi-model"=expression(bold("Multi-model"))))+
        ylab(expression(CO[2] * " response of " * P[net] * " (%)"))+
        #facet_wrap(Period~., ncol=4, 
        #           labeller=as_labeller(c("1_HIST"="2013-19", 
        #                                  "2_TMP"="2020-22",
        #                                  "3_SHORT"="2023-29", 
        #                                  "4_LONG"="2063-69")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              strip.text = element_text(size=20),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5));p5
    
    
    
    ### P effect under aCO2
    p6 <- ggplot(data=plotDFc, 
                 aes(x=ModName, y=GPP.mean/1000, group=PTRT))+
        geom_bar(plotDFc, stat = "identity", 
                 mapping=aes(ModName, GPP.mean/1000, fill=PTRT),
                 position=position_dodge(), col="black") +
        geom_errorbar(plotDFc, stat = "identity", 
                      mapping=aes(ModName, ymax=(GPP.mean+GPP.sd)/1000, 
                                  ymin=(GPP.mean-GPP.sd)/1000),
                      position=position_dodge(width=0.9), 
                      width=0.5, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        #coord_cartesian(ylim=c(90, 140))+
        #geom_hline(yintercept=1.0, col="black", lty=2)+
        scale_fill_manual(name="Fertilization",
                          values=c("NOP"="pink",
                                   "MDP"="orange",
                                   "HIP"="red3"))+
        scale_x_discrete(name="",
                         guide = guide_axis(n.dodge = 2),
                         limits=c(model.names, 
                                  "Multi-model"),
                         labels=c(model.labels,
                                  "Multi-model"=expression(bold("Multi-model"))))+
        ylab(expression("GPP (kg C " * m^-2 * " " * yr^-1 * ")"))+
        #facet_wrap(Period~., ncol=4, 
        #           labeller=as_labeller(c("1_HIST"="2013-19", 
        #                                  "2_TMP"="2020-22",
        #                                  "3_SHORT"="2023-29", 
        #                                  "4_LONG"="2063-69")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              strip.text = element_text(size=20),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5));p6
    
    
    
    ### P effect under aCO2
    p7 <- ggplot(data=plotDFc, 
                 aes(x=ModName, y=deltaCVEG.mean/1000, group=PTRT))+
        geom_bar(plotDFc, stat = "identity", 
                 mapping=aes(ModName, deltaCVEG.mean/1000, fill=PTRT),
                 position=position_dodge(), col="black") +
        geom_errorbar(plotDFc, stat = "identity", 
                      mapping=aes(ModName, ymax=(deltaCVEG.mean+deltaCVEG.sd)/1000, 
                                  ymin=(deltaCVEG.mean-deltaCVEG.sd)/1000),
                      position=position_dodge(width=0.9), 
                      width=0.5, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        #coord_cartesian(ylim=c(90, 140))+
        #geom_hline(yintercept=1.0, col="black", lty=2)+
        scale_fill_manual(name="Fertilization",
                          values=c("NOP"="pink",
                                   "MDP"="orange",
                                   "HIP"="red3"))+
        scale_x_discrete(name="",
                         guide = guide_axis(n.dodge = 2),
                         limits=c(model.names, 
                                  "Multi-model"),
                         labels=c(model.labels,
                                  "Multi-model"=expression(bold("Multi-model"))))+
        ylab(expression(Delta * C[veg] * " (kg C " * m^-2 * " " * yr^-1 * ")"))+
        #facet_wrap(Period~., ncol=4, 
        #           labeller=as_labeller(c("1_HIST"="2013-19", 
        #                                  "2_TMP"="2020-22",
        #                                  "3_SHORT"="2023-29", 
        #                                  "4_LONG"="2063-69")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              strip.text = element_text(size=20),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5));p7
    
    
    
    ### P effect under aCO2
    p8 <- ggplot(data=plotDFc, 
                 aes(x=ModName, y=PUP.mean, group=PTRT))+
        geom_bar(plotDFc, stat = "identity", 
                 mapping=aes(ModName, PUP.mean, fill=PTRT),
                 position=position_dodge(), col="black") +
        geom_errorbar(plotDFc, stat = "identity", 
                      mapping=aes(ModName, ymax=PUP.mean+PUP.sd, 
                                  ymin=PUP.mean-PUP.sd),
                      position=position_dodge(width=0.9), 
                      width=0.5, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        #coord_cartesian(ylim=c(90, 140))+
        #geom_hline(yintercept=1.0, col="black", lty=2)+
        scale_fill_manual(name="Fertilization",
                          values=c("NOP"="pink",
                                   "MDP"="orange",
                                   "HIP"="red3"))+
        scale_x_discrete(name="",
                         guide = guide_axis(n.dodge = 2),
                         limits=c(model.names, 
                                  "Multi-model"),
                         labels=c(model.labels,
                                  "Multi-model"=expression(bold("Multi-model"))))+
        ylab(expression(P[upt] * " (g P " * m^-2 * " " * yr^-1 * ")"))+
        #facet_wrap(Period~., ncol=4, 
        #           labeller=as_labeller(c("1_HIST"="2013-19", 
        #                                  "2_TMP"="2020-22",
        #                                  "3_SHORT"="2023-29", 
        #                                  "4_LONG"="2063-69")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major.x=element_blank(),
              panel.grid.major.y=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              strip.text = element_text(size=20),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5));p8
    
    
    legends <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    
    plots <- plot_grid(p6, p1, # GPP
                       p7, p2, # delta Cveg
                       p8, p4, # Pupt
                       labels="auto", label_x=0.1, label_y=0.95,
                       label_size=24,
                       ncol=2)
    
    
    ### plot
    pdf(paste0(out.dir, 
               "CO2_response_of_P_treatments_", climate.scenario, 
               ".pdf"), width=16, height=14)
    plot_grid(plots,
              legends,
              ncol=1, rel_heights=c(1,0.1))
    
    dev.off()
    
    
    ### 
    
    
    
    
    
    
    
    
    
    
    
    
    ##### Plot responses
    #p1 <- ggplot(data=plotDF, 
    #             aes(x=ModName, y=CL.mean, group=PTRT))+
    #    geom_bar(plotDF, stat = "identity", 
    #             mapping=aes(ModName, CL.mean, fill=PTRT),
    #             position=position_dodge(), col="black") +
    #    geom_errorbar(plotDF, stat = "identity", 
    #                  mapping=aes(ModName, ymax=CL.mean+CL.sd, 
    #                              ymin=CL.mean-CL.sd),
    #                  position=position_dodge(width=0.9), 
    #                  width=0.5, col="black") +
    #    coord_cartesian(ylim=c(0.9, 1.4))+
    #    geom_hline(yintercept=1.0, col="black", lty=2)+
    #    scale_fill_manual(name="Fertilization",
    #                      values=c("NOP"="pink",
    #                               "MDP"="orange",
    #                               "HIP"="red3"))+
    #    scale_x_discrete(name="",
    #                     limits=model.names,
    #                     labels=model.labels)+
    #    ylab(expression(CO[2] * " response of CL"))+
    #    facet_wrap(Period~., ncol=4, 
    #               labeller=as_labeller(c("1_HIST"="2013-19", 
    #                                      "2_TMP"="2020-22",
    #                                      "3_SHORT"="2023-29", 
    #                                      "4_LONG"="2063-69")))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_line(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major.x=element_blank(),
    #          panel.grid.major.y=element_line(),
    #          legend.position="bottom",
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          strip.text = element_text(size=20),
    #          plot.title = element_text(size=14, face="bold.italic", 
    #                                    hjust = 0.5));p1
    #
    #
    #p2 <- ggplot(data=plotDF, 
    #             aes(x=ModName, y=CW.mean, group=PTRT))+
    #    geom_bar(plotDF, stat = "identity", 
    #             mapping=aes(ModName, CW.mean, fill=PTRT),
    #             position=position_dodge(), col="black") +
    #    geom_errorbar(plotDF, stat = "identity", 
    #                  mapping=aes(ModName, ymax=CW.mean+CW.sd, 
    #                              ymin=CW.mean-CW.sd),
    #                  position=position_dodge(width=0.9), 
    #                  width=0.5, col="black") +
    #    coord_cartesian(ylim=c(0.9, 1.3))+
    #    geom_hline(yintercept=1.0, col="black", lty=2)+
    #    scale_fill_manual(name="Fertilization",
    #                      values=c("NOP"="pink",
    #                               "MDP"="orange",
    #                               "HIP"="red3"))+
    #    scale_x_discrete(name="",
    #                     limits=model.names,
    #                     labels=model.labels)+
    #    ylab(expression(CO[2] * " response of CW"))+
    #    facet_wrap(Period~., ncol=4, 
    #               labeller=as_labeller(c("1_HIST"="2013-19", 
    #                                      "2_TMP"="2020-22",
    #                                      "3_SHORT"="2023-29", 
    #                                      "4_LONG"="2063-69")))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_line(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major.x=element_blank(),
    #          panel.grid.major.y=element_line(),
    #          legend.position="bottom",
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          strip.text = element_text(size=20),
    #          plot.title = element_text(size=14, face="bold.italic", 
    #                                    hjust = 0.5));p2
    #
    #
    #p3 <- ggplot(data=plotDF, 
    #             aes(x=ModName, y=CFR.mean, group=PTRT))+
    #    geom_bar(plotDF, stat = "identity", 
    #             mapping=aes(ModName, CFR.mean, fill=PTRT),
    #             position=position_dodge(), col="black") +
    #    geom_errorbar(plotDF, stat = "identity", 
    #                  mapping=aes(ModName, ymax=CFR.mean+CFR.sd, 
    #                              ymin=CFR.mean-CFR.sd),
    #                  position=position_dodge(width=0.9), 
    #                  width=0.5, col="black") +
    #    coord_cartesian(ylim=c(0.9, 2.0))+
    #    geom_hline(yintercept=1.0, col="black", lty=2)+
    #    scale_fill_manual(name="Fertilization",
    #                      values=c("NOP"="pink",
    #                               "MDP"="orange",
    #                               "HIP"="red3"))+
    #    scale_x_discrete(name="",
    #                     limits=model.names,
    #                     labels=model.labels)+
    #    ylab(expression(CO[2] * " response of CFR"))+
    #    facet_wrap(Period~., ncol=4, 
    #               labeller=as_labeller(c("1_HIST"="2013-19", 
    #                                      "2_TMP"="2020-22",
    #                                      "3_SHORT"="2023-29", 
    #                                      "4_LONG"="2063-69")))+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_line(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major.x=element_blank(),
    #          panel.grid.major.y=element_line(),
    #          legend.position="bottom",
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          strip.text = element_text(size=20),
    #          plot.title = element_text(size=14, face="bold.italic", 
    #                                    hjust = 0.5));p3
    #
    #### plot
    #pdf(paste0(out.dir, 
    #           "CO2_response_of_P_treatments_", climate.scenario, 
    #           "_vegetation_biomass.pdf"), width=16, height=6)
    #for (i in 1:3) {
    #    print(get(paste("p",i,sep="")))
    #}
    #dev.off()
}

