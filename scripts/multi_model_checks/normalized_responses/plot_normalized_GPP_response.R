plot_normalized_GPP_response <- function(scenario, eucDF) {
    ##################################################################
    #### Set up basics
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario, "/")
  
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_AMB_daily.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_ELE_daily.rds"))
    
    ### ignore NAs
    ambDF[ambDF<=-999] <- NA
    eleDF[eleDF<=-999] <- NA
    
    
    ambDF <- ambDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI")]
    
    eleDF <- eleDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI")]
    
    ambDF$Aleaf <- with(ambDF, GPP/LAI)
    eleDF$Aleaf <- with(eleDF, GPP/LAI)
    
    
    ### ignore N only models
    ambDF <- ambDF[ambDF$ModName!="I_GDAYN",]
    eleDF <- eleDF[eleDF$ModName!="I_GDAYN",]
    
    ambDF <- ambDF[ambDF$ModName!="J_LPJGN",]
    eleDF <- eleDF[eleDF$ModName!="J_LPJGN",]
    
    
    ### we have data for 2013-2016 period, subset obs for this period only
    ambDF <- ambDF[ambDF$YEAR%in%c(2013,2014,2015,2016),]
    eleDF <- eleDF[eleDF$YEAR%in%c(2013,2014,2015,2016),]
    
    
    ### calculate annual mean
    ambDF2 <- summaryBy(GPP+LAI+Aleaf~YEAR+ModName, FUN=c(mean,sd),
                        na.rm=T, keep.names=T, data=ambDF)
    
    eleDF2 <- summaryBy(GPP+LAI+Aleaf~YEAR+ModName, FUN=c(mean,sd),
                        na.rm=T, keep.names=T, data=eleDF)
    
    
    ambDF3 <- summaryBy(GPP+Aleaf~YEAR+ModName, FUN=sum,
                       na.rm=T, keep.names=T, data=ambDF)
    
    eleDF3 <- summaryBy(GPP+Aleaf~YEAR+ModName, FUN=sum,
                        na.rm=T, keep.names=T, data=eleDF)
    
    
    ambDF4 <- summaryBy(LAI~YEAR+ModName, FUN=max,
                        na.rm=T, keep.names=T, data=ambDF)
    
    eleDF4 <- summaryBy(LAI+Aleaf~YEAR+ModName, FUN=max,
                        na.rm=T, keep.names=T, data=eleDF)
    
    
    ##################################################################
    mod.list <- unique(ambDF2$ModName)
    
 
    
    
    ##################################################################
    ### plot time-averaged comparison
    ambDF2$Trt <- "amb"
    eleDF2$Trt <- "ele"
    mgDF1 <- rbind(ambDF2, eleDF2)
    
    sumDF1 <- summaryBy(Aleaf.mean+LAI.mean~ModName+Trt, FUN=c(mean, sd),
                       keep.names=T, na.rm=T, data=mgDF1)
    
    ambDF3$Trt <- "amb"
    eleDF3$Trt <- "ele"
    mgDF2 <- rbind(ambDF3, eleDF3)
    
    sumDF2 <- summaryBy(GPP~ModName+Trt, FUN=c(mean, sd),
                        keep.names=T, na.rm=T, data=mgDF2)
    
    
    mod.list <- unique(sumDF2$ModName)
    
    
    ### convert sd into zero
    sumDF1$Aleaf.mean.sd <- 0.0
    sumDF1$LAI.mean.sd <- 0.0
    
    sumDF2$GPP.sd <- 0.0

    
    ### calculate multi-model means and sds
    multDF1 <- summaryBy(Aleaf.mean.mean+LAI.mean.mean~Trt, data=sumDF1,
                         FUN=c(mean,sd), keep.names=T, na.rm=T)
    
    multDF2 <- summaryBy(GPP.mean~Trt, data=sumDF2,
                         FUN=c(mean,sd), keep.names=T, na.rm=T)
    
    colnames(multDF1) <- c("Trt", "Aleaf.mean.mean", "LAI.mean.mean", "Aleaf.mean.sd",
                           "LAI.mean.sd")
    colnames(multDF2) <- c("Trt", "GPP.mean", "GPP.sd")
    
    #sumDF3 <- merge(multDF1, multDF2, by="Trt")
    #colnames(sumDF3) <- c("Trt", "Aleaf.mean", "LAI.mean", "Aleaf.sd",
    #                      "LAI.sd", "GPP.mean", "GPP.sd")
    #sumDF3$ModName <- "Multi-model"
    
    multDF1$ModName <- "Multi-model"
    multDF2$ModName <- "Multi-model"
    
    multDF1 <- multDF1[,c("ModName", "Trt", "Aleaf.mean.mean", "LAI.mean.mean", "Aleaf.mean.sd",
                          "LAI.mean.sd")]
    
    multDF2 <- multDF2[,c("ModName", "Trt", "GPP.mean", "GPP.sd")]
    
    
    sumDF1 <- rbind(sumDF1, multDF1)
    sumDF2 <- rbind(sumDF2, multDF2)
    
    
    ### add "observations", annual, gpp and lai
    myobsDF <- import_MAESPA_GPP()
    #myobsDF <- eucDF[,c("Group", "Trt", "GPP", "LAI")]
    
    
    
    ### split Aleaf and LAI data
    plotDF1 <- sumDF1[,c("ModName", "Trt", "Aleaf.mean.mean", "Aleaf.mean.sd")]
    plotDF2 <- sumDF1[,c("ModName", "Trt", "LAI.mean.mean", "LAI.mean.sd")]
    plotDF3 <- sumDF2
    
    
    ### add obs
    plotDF1 <- rbind(plotDF1, myobsDF$aleafDF)
    plotDF2 <- rbind(plotDF2, myobsDF$laiDF)
    plotDF3 <- rbind(plotDF3, myobsDF$gppDF)
    
    
    
    ########################################################
    ### plotting
    p1 <- ggplot(data=plotDF1, 
                 aes(ModName, Aleaf.mean.mean, group=Trt)) +
      geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      geom_errorbar(aes(x=ModName, ymin=Aleaf.mean.mean-Aleaf.mean.sd, 
                        ymax=Aleaf.mean.mean+Aleaf.mean.sd), width=0.4,
                    position=position_dodge(width=1)) +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      scale_alpha_manual(name="Treatment",
                         values=c("amb" = 0.3, 
                                  "ele" = 1.0),
                         label=c("AMB", "ELE"))+
      scale_fill_manual(name="Model",
                        values=c(col.values, "black", "black"),
                        labels=c(model.labels, "Multi-model" = "M-M",
                                 "OBS" = "OBS"))+
      guides(alpha=guide_legend("Treatment"), fill = FALSE)+
      scale_x_discrete(limit=c(mod.list, "Multi-model", "OBS"),
                       label=c(model.labels, 
                               "Multi-model" = expression(bold("M-M")),
                               "OBS" = expression(bold("OBS"))))
    
    
    
    p2 <- ggplot(data=plotDF2, 
                 aes(ModName, LAI.mean.mean, group=Trt)) +
      geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5, 10.5, 11.5), lty=2)+
      geom_errorbar(aes(x=ModName, ymin=LAI.mean.mean-LAI.mean.sd, 
                        ymax=LAI.mean.mean+LAI.mean.sd), width=0.4,
                    position=position_dodge(width=1)) +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
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
      ylab(expression(paste("LAI")))+
      scale_alpha_manual(name="Treatment",
                         values=c("amb" = 0.3, 
                                  "ele" = 1.0),
                         label=c("AMB", "ELE"))+
      scale_fill_manual(name="Model",
                        values=c(col.values, "black", "black"),
                        labels=c(model.labels, "Multi-model" = "M-M",
                                 "OBS" = "Obs"))+
      guides(alpha=guide_legend("Treatment"), fill = FALSE)+
      scale_x_discrete(limit=c(mod.list, "Multi-model", "OBS"),
                       label=c(model.labels, 
                               "Multi-model" = expression(bold("M-M")),
                               "OBS" = expression(bold("OBS"))))
    
    
    p3 <- ggplot(data=plotDF3, 
                 aes(ModName, GPP.mean, group=Trt)) +
      geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5, 10.5, 11.5), lty=2)+
      geom_errorbar(aes(x=ModName, ymin=GPP.mean-GPP.sd, 
                        ymax=GPP.mean+GPP.sd), width=0.4,
                    position=position_dodge(width=0.9)) +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
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
      ylab(expression(paste("GPP (g C " * m^2 * " " * yr^-1, ")")))+
      scale_alpha_manual(name="Treatment",
                         values=c("amb" = 0.3, 
                                  "ele" = 1.0),
                         label=c("AMB", "ELE"))+
      scale_fill_manual(name="Model",
                        values=c(col.values, "black", "black"),
                        labels=c(model.labels, "Multi-model" = "M-M",
                                 "OBS"= "MAESPA"))+
      guides(alpha=guide_legend("Treatment"), fill = FALSE)+
      scale_x_discrete(limit=c(mod.list, "Multi-model", "OBS"),
                       label=c(model.labels, 
                               "Multi-model" = expression(bold("M-M")),
                               "OBS" = expression(bold("MAESPA"))))
    
    
    #legend_top_row <- get_legend(p1 + theme(legend.position="bottom",
    #                                        legend.box = 'horizontal',
    #                                        legend.box.just = 'left'))
    
    plots_left_column <- plot_grid(p3, p1, p2, 
                                   labels=c("a", "c", "e"),
                                   ncol=1, align="vh", axis = "l",
                                   label_x=0.1, label_y=0.95,
                                   label_size = 20)
    
    plots_top_row <- plot_grid(p3, labels=c("a"), ncol=1,
                               align="vh", axis = "l",
                               label_x=0.1, label_y=0.95,
                               label_size = 20)
    
    plots_ed_figure <- plot_grid(p1, p2, 
                                 labels=c("a", "b"),
                                 ncol=1, align="vh", axis = "l",
                                 label_x=0.1, label_y=0.95,
                                 label_size = 20)
    
    pdf(paste0(out.dir, "/MIP_photosynthesis_", 
               scenario, "_comparison_with_obs_ed_figure.pdf"), 
        width=12, height=10)
    plot_grid(plots_ed_figure)
    
    dev.off()
    
    
    
    
    
    
    
    
    
    ###############################################################################

    
    
    
    ### plot CO2 response ratios
    subDF <- plotDF2[plotDF2$ModName%in%c("A_GDAYP", "B_ELMV1",
                                          "C_CABLP", "D_LPJGP",
                                          "E_OCHDP", "F_QUINC",
                                          "G_OCHDX", "H_QUJSM"),]
    
    subDF1 <- subDF[subDF$Trt=="amb",]
    subDF2 <- subDF[subDF$Trt=="ele",]
    subDF1$LAI.mean.sd <- NULL
    subDF2$LAI.mean.sd <- NULL
    
    names(subDF1) <- names(subDF2) <- c("ModName", "Trt", "LAI")
    
    subDF <- merge(subDF1, subDF2, by="ModName")
    subDF$diff <- with(subDF, (LAI.y-LAI.x)/LAI.x)
    subDF$Trt.x <- NULL
    subDF$Trt.y <- NULL
    subDF$LAI.x <- NULL
    subDF$LAI.y <- NULL
    subDF$diff.sd <- 0.0
    
    tmpDF <- data.frame("ModName"="Multi-model",
                        "diff"=mean(subDF$diff),
                        "diff.sd"=sd(subDF$diff))
    
    laiDF.plot <- rbind(subDF, tmpDF)
    laiDF.plot$diff <- laiDF.plot$diff * 100
    laiDF.plot$diff.sd <- laiDF.plot$diff.sd * 100
    
    
    
    ### gpp
    subDF <- plotDF3[plotDF3$ModName%in%c("A_GDAYP", "B_ELMV1",
                                          "C_CABLP", "D_LPJGP",
                                          "E_OCHDP", "F_QUINC",
                                          "G_OCHDX", "H_QUJSM"),]
    
    subDF1 <- subDF[subDF$Trt=="amb",]
    subDF2 <- subDF[subDF$Trt=="ele",]
    subDF1$GPP.sd <- NULL
    subDF2$GPP.sd <- NULL
    
    names(subDF1) <- names(subDF2) <- c("ModName", "Trt", "GPP")
    
    subDF <- merge(subDF1, subDF2, by="ModName")
    subDF$diff <- with(subDF, (GPP.y-GPP.x)/GPP.x)
    subDF$Trt.x <- NULL
    subDF$Trt.y <- NULL
    subDF$GPP.x <- NULL
    subDF$GPP.y <- NULL
    subDF$diff.sd <- 0.0
    
    tmpDF <- data.frame("ModName"="Multi-model",
                        "diff"=mean(subDF$diff),
                        "diff.sd"=sd(subDF$diff))
    
    gppDF.plot <- rbind(subDF, tmpDF)
    

    gppDF <- myobsDF$gppDF
    
    
    ### read gpp
    tmpDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/MAESPA_output/maespa.year.ring.csv")
    
    tmpDF$Aleaf.550 <- with(tmpDF, GPP.sum.550/LAI.mean)
    tmpDF$Aleaf.400 <- with(tmpDF, GPP.sum.400/LAI.mean)
    tmpDF$Aleaf.diff <- with(tmpDF, (Aleaf.550-Aleaf.400)/Aleaf.400)
    
    ### calculate GPP field - considering LAI and SM influence
    aDF <- tmpDF[tmpDF$Ring%in%c("R2", "R3", "R6"),]
    eDF <- tmpDF[tmpDF$Ring%in%c("R1", "R4", "R5"),]
    
    asDF <- summaryBy(GPP.sum.400+Aleaf.400~year, data=aDF, FUN=mean, na.rm=T, keep.names=T)
    esDF <- summaryBy(GPP.sum.550+Aleaf.550~year, data=eDF, FUN=mean, na.rm=T, keep.names=T)
    
    mgDF <- merge(asDF, esDF, by="year")
    mgDF$diff <- with(mgDF, (GPP.sum.550-GPP.sum.400)/GPP.sum.400)
    mgDF$Aleaf_diff <- with(mgDF, (Aleaf.550-Aleaf.400)/Aleaf.400)
    
    ### ring-specific values
    v1 <- mean(mgDF$diff)
    v2 <- sd(mgDF$diff)
    
    ### CO2 response ratio of all 6 rings
    #v3 <- mean(tmpDF$c.response-1.0)
    #v4 <- sd(tmpDF$c.response-1.0)
    
    ### caclulate means for each ring
    tmpDF$diff <- with(tmpDF, (GPP.sum.550-GPP.sum.400)/GPP.sum.400)
    v3 <- mean(tmpDF$diff)
    v4 <- sd(tmpDF$diff)
    
    ### calculate Aleaf for treatment specific rings
    v5 <- mean(mgDF$Aleaf_diff)
    v6 <- sd(mgDF$Aleaf_diff)
    
    ### calculate Aleaf
    #v7 <- mean(tmpDF$Aleaf.diff)
    #v8 <- sd(tmpDF$Aleaf.diff)
    
    tmpDF1 <- data.frame("ModName"=c("Obs_field", "Obs_inst"#,
                                     #"Aleaf_field", "Aleaf_inst"
                                     ),
                         "diff"=c(v1, v3),#, v5, v7),
                         "diff.sd"=c(v2, v4))#v6, v8))
    
    gppDF.plot <- rbind(gppDF.plot, tmpDF1)
    
    
    
    #v1 <- 192/gppDF$GPP.mean[gppDF$Trt=="amb"]
    #v2 <- 0.13
    
    #tmpDF <- data.frame("ModName"="Obs",
    #                    "diff"=v1,
    #                    "diff.sd"=v2)
    
    #gppDF.plot <- rbind(gppDF.plot, tmpDF)
    
    
    gppDF.plot$diff <- gppDF.plot$diff * 100
    gppDF.plot$diff.sd <- gppDF.plot$diff.sd * 100
    
    gppDF.plot$ModName2 <- c(rep("Multi-model", 9),
                             "Obs_field",
                             "Obs_inst")
    
    subDF2 <- gppDF.plot[gppDF.plot$ModName%in%c("A_GDAYP","B_ELMV1",
                                                 "C_CABLP", "D_LPJGP",
                                                 "E_OCHDP", "F_QUINC",
                                                 "G_OCHDX", "H_QUJSM"),]
    
    subDF1 <- gppDF.plot[gppDF.plot$ModName%in%c("Obs_field", "Obs_inst"),]
    
    
    p3 <- ggplot(data=gppDF.plot, 
                 aes(ModName2, diff)) +
      geom_violin() +
      stat_summary(fun.y="mean", geom="crossbar", width=0.5, color="black")+
      geom_errorbar(data=subDF1, aes(x=ModName, ymin=diff-diff.sd, 
                                     ymax=diff+diff.sd), width=0.2,
                    position=position_dodge(width=1)) +
      geom_point(data=subDF2, aes(ModName2, diff, 
                                  col=ModName), 
                 position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9), 
                 size=6)+
      #ylim(-5, 15)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
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
      ylab(expression(paste(CO[2] * " response of GPP (%)")))+
      scale_x_discrete(limit=c("Multi-model", "Obs_field", "Obs_inst"),
                       label=c("Multi-model" = expression(bold("M-M")),
                               "Obs_field" = expression(bold("OBS"[field])),
                               "Obs_inst" = expression(bold("OBS"[inst]))))+
      scale_color_manual(name="Model",
                         values=c(col.values,
                                  "Obs_field"="black",
                                  "Obs_inst"="black"))
    
    plot(p3)
    
    
    
    ### plot CO2 response ratios
    subDF <- plotDF1[plotDF1$ModName%in%c("A_GDAYP", "B_ELMV1",
                                          "C_CABLP", "D_LPJGP",
                                          "E_OCHDP", "F_QUINC",
                                          "G_OCHDX", "H_QUJSM"),]
    
    subDF1 <- subDF[subDF$Trt=="amb",]
    subDF2 <- subDF[subDF$Trt=="ele",]
    subDF1$Aleaf.mean.sd <- NULL
    subDF2$Aleaf.mean.sd <- NULL
    
    names(subDF1) <- names(subDF2) <- c("ModName", "Trt", "Aleaf")
    
    subDF <- merge(subDF1, subDF2, by="ModName")
    subDF$diff <- with(subDF, (Aleaf.y-Aleaf.x)/Aleaf.x)
    subDF$Trt.x <- NULL
    subDF$Trt.y <- NULL
    subDF$Aleaf.x <- NULL
    subDF$Aleaf.y <- NULL
    subDF$diff.sd <- 0.0
    
    tmpDF <- data.frame("ModName"="Multi-model",
                        "diff"=mean(subDF$diff),
                        "diff.sd"=sd(subDF$diff))
    
    aleafDF.plot <- rbind(subDF, tmpDF)
    aleafDF.plot$diff <- aleafDF.plot$diff * 100
    aleafDF.plot$diff.sd <- aleafDF.plot$diff.sd * 100
    
    myDF <- read.csv("./validation_dataset/EucFACE_C_Budget_data/MAESPA_output/VJlimitedPhotoResponseCO2.csv")
    myDF <- myDF[myDF$A.inc >=0,]
    myDF <- myDF[myDF$A.inc <=2,]
    
    Ac.mean <- mean(myDF$Ac.inc-1)*100
    Ac.sd <- sd(myDF$Ac.inc-1)*100
    Aj.mean <- mean(myDF$Aj.inc-1)*100
    Aj.sd <- sd(myDF$Aj.inc-1)*100
    A.mean <- mean(myDF$A.inc-1)*100
    A.sd <- sd(myDF$A.inc-1)*100
    
    
    tmpDF1 <- data.frame("ModName"=c("A.mean", "Ac.mean", "Aj.mean", "A.obs"),
                         "diff"=c(A.mean, Ac.mean, Aj.mean, v5*100),
                         "diff.sd"=c(A.sd, Ac.sd, Aj.sd, v6*100))
    
    aleafDF.plot <- rbind(aleafDF.plot, tmpDF1)
    
    
    
    
    aleafDF.plot$ModName2 <- c(rep("Multi-model", 9),
                               "A.mean", "Ac.mean",
                               "Aj.mean", "A.obs")
    
    subDF2 <- aleafDF.plot[aleafDF.plot$ModName%in%c("A_GDAYP","B_ELMV1",
                                                     "C_CABLP", "D_LPJGP",
                                                     "E_OCHDP", "F_QUINC",
                                                     "G_OCHDX", "H_QUJSM"),]
    
    subDF1 <- aleafDF.plot[aleafDF.plot$ModName%in%c("A.mean", "Ac.mean",
                                                     "Aj.mean", "A.obs"),]
    
    
    p1 <- ggplot(data=aleafDF.plot, 
                 aes(ModName2, diff)) +
      geom_violin() +
      stat_summary(fun.y="mean", geom="crossbar", width=0.5, color="black")+
      geom_errorbar(data=subDF1, aes(x=ModName, ymin=diff-diff.sd, 
                                     ymax=diff+diff.sd), width=0.2,
                    position=position_dodge(width=1)) +
      geom_point(data=subDF2, aes(ModName2, diff, 
                                  col=ModName), 
                 position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9), 
                 size=6)+
      #ylim(-5, 15)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
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
      ylab(expression(paste(CO[2] * " response of Aleaf (%)")))+
      scale_x_discrete(limit=c("Multi-model", "A.mean", "Ac.mean", "Aj.mean", "A.obs"),
                       label=c("Multi-model" = expression(bold("M-M")),
                               "A.mean" = expression(bold("A"[long])),
                               "Ac.mean" = expression(bold("A"[c])),
                               "Aj.mean" = expression(bold("A"[j])),
                               "A.obs" = expression(bold("A"[obs]))))+
      scale_color_manual(name="Model",
                         values=c(col.values,
                                  "A.mean"="black",
                                  "Ac.mean"="black",
                                  "Aj.mean"="black",
                                  "A.obs"="black"))
    
    plot(p1)
    
    
    
    
    
    
    
    
    
    
    
    
    ### LAI
    laiDF <- read.csv("validation_dataset/EucFACE_LAI_2012_2016.csv")
    laiDF$YEAR <- year(laiDF$Date)
    laiDF <- subset(laiDF, YEAR>2012 & YEAR <= 2016)
    laisDF <- summaryBy(lai~Trt+YEAR, FUN=c(mean,sd), data=laiDF, na.rm=T, keep.names=T)
    
    laisDF$Trt[laisDF$Trt=="aCO2"] <- "amb"
    laisDF$Trt[laisDF$Trt=="eCO2"] <- "ele"
    
    aDF <- laisDF[laisDF$Trt=="amb",]
    eDF <- laisDF[laisDF$Trt=="ele",]

    laisDF <- merge(aDF, eDF, by="YEAR")
    laisDF$diff <- with(laisDF, (lai.mean.y-lai.mean.x)/lai.mean.x)
    
    v1 <- mean(laisDF$diff)*100
    v2 <- sd(laisDF$diff)*100
    
    tmpDF <- data.frame("ModName"="Obs",
                        "diff"=v1,
                        "diff.sd"=v2)
    
    laiDF.plot <- rbind(laiDF.plot, tmpDF)
    
    
    
    subDF1 <- laiDF.plot[laiDF.plot$ModName%in%c(#"Multi-model", 
                                                 "Obs"),]
    
    subDF1$diff <- subDF1$diff 
    subDF1$diff.sd <- subDF1$diff.sd 
    
    subDF2 <- laiDF.plot[laiDF.plot$ModName%in%c("A_GDAYP", "B_ELMV1",
                                                 "C_CABLP", "D_LPJGP",
                                                 "E_OCHDP", "F_QUINC",
                                                 "G_OCHDX", "H_QUJSM"),]
    subDF2$ModName2 <- "Multi-model"
    
    #subDF2$diff <- subDF2$diff * 100
    
    subDF1$ModName2 <- "Obs"
    
    subDF3 <- rbind(subDF2, subDF1)
    

    ### plotting
    
    p2 <- ggplot(data=subDF3, 
                 aes(ModName2, diff)) +
      geom_violin() +
      stat_summary(fun.y="mean", geom="crossbar", width=0.5, color="black")+
      geom_errorbar(data=subDF1, aes(x=ModName, ymin=diff-diff.sd, 
                                     ymax=diff+diff.sd), width=0.2,
                    position=position_dodge(width=1)) +
      geom_point(data=subDF2, aes(ModName2, diff, 
                                  col=ModName), 
                 position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9), 
                 size=6)+
      #ylim(-5, 15)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
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
      ylab(expression(paste(CO[2] * " response of LAI (%)")))+
      scale_x_discrete(limit=c("Multi-model", "Obs"),
                       label=c("Multi-model" = expression(bold("M-M")),
                               "Obs" = expression(bold("OBS"))))+
      scale_color_manual(name="Model",
                         values=c(col.values,
                                  "Obs"="black"))

    plot(p2)

    
    
    
    subDF1 <- plotDF2[plotDF2$ModName%in%c("Multi-model", "OBS"),]
    subDF2 <- plotDF2[plotDF2$ModName%in%c("A_GDAYP", "B_ELMV1",
                                           "C_CABLP", "D_LPJGP",
                                           "E_OCHDP", "F_QUINC",
                                           "G_OCHDX", "H_QUJSM"),]
    subDF2$ModName2 <- "Multi-model"
    
    p5 <- ggplot(data=subDF1, 
                 aes(ModName, LAI.mean.mean, group=Trt)) +
      geom_bar(data=subDF1,stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5, 10.5, 11.5), lty=2)+
      geom_errorbar(data=subDF1, aes(x=ModName, ymin=LAI.mean.mean-LAI.mean.sd, 
                        ymax=LAI.mean.mean+LAI.mean.sd), width=0.2,
                    position=position_dodge(width=1)) +
      geom_point(data=subDF2, aes(ModName2, LAI.mean.mean, group=Trt,
                                  col=ModName), 
                 position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9), 
                 size=6)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
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
      ylab(expression(paste("LAI")))+
      scale_alpha_manual(name="Treatment",
                         values=c("amb" = 0.3, 
                                  "ele" = 0.8),
                         label=c("AMB", "ELE"))+
      scale_fill_manual(name="Model",
                        values=c("black", "black"),
                        labels=c("Multi-model" = "Multi-model",
                                 "OBS" = "OBS"))+
      guides(alpha=guide_legend("Treatment"), fill = FALSE)+
      scale_x_discrete(limit=c("Multi-model", "OBS"),
                       label=c("Multi-model" = expression(bold("M-M")),
                               "OBS" = expression(bold("OBS"))))+
      scale_color_manual(name="Model",
                         values=c(col.values),
                         labels=c(model.labels))
    
    
    plot(p5)
    
    
    subDF1 <- plotDF3[plotDF3$ModName%in%c("Multi-model", "OBS"),]
    subDF2 <- plotDF3[plotDF3$ModName%in%c("A_GDAYP", "B_ELMV1",
                                           "C_CABLP", "D_LPJGP",
                                           "E_OCHDP", "F_QUINC",
                                           "G_OCHDX", "H_QUJSM"),]
    subDF2$ModName2 <- "Multi-model"
    
    p6 <- ggplot(data=subDF1, 
                 aes(ModName, GPP.mean, group=Trt)) +
      geom_bar(data=subDF1,stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5, 10.5, 11.5), lty=2)+
      geom_errorbar(data=subDF1, aes(x=ModName, ymin=GPP.mean-GPP.sd, 
                                     ymax=GPP.mean+GPP.sd), width=0.2,
                    position=position_dodge(width=1)) +
      geom_point(data=subDF2, aes(ModName2, GPP.mean, group=Trt,
                                  col=ModName), 
                 position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9), 
                 size=6)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
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
      ylab(expression(paste("GPP (g C " * m^2 * " " * yr^-1, ")")))+
      scale_alpha_manual(name="Treatment",
                         values=c("amb" = 0.3, 
                                  "ele" = 0.8),
                         label=c("AMB", "ELE"))+
      scale_fill_manual(name="Model",
                        values=c("black", "black"),
                        labels=c("Multi-model" = "Multi-model",
                                 "OBS" = "MAESPA"))+
      guides(alpha=guide_legend("Treatment"), fill = FALSE)+
      scale_x_discrete(limit=c("Multi-model", "OBS"),
                       label=c("Multi-model" = expression(bold("M-M")),
                               "OBS" = expression(bold("MAESPA"))))+
      scale_color_manual(name="Model",
                         values=c(col.values),
                         labels=c(model.labels))
    
    
    
    
    ### merge the two to get a combined DF
    gppDF.plot$ModName[gppDF.plot$ModName=="Obs_field"] <- "Obs"
    comDF <- merge(gppDF.plot, laiDF.plot, by=c("ModName"))
    colnames(comDF) <- c("ModName", "GPP_diff", "GPP_diff_sd", "ModName2",
                         "LAI_diff", "LAI_diff_sd")
    
    p7 <- ggplot(data=comDF, 
                 aes(GPP_diff, LAI_diff, group=ModName)) +
      geom_errorbar(data=comDF, aes(ymin=LAI_diff-LAI_diff_sd, 
                                    ymax=LAI_diff+LAI_diff_sd)) +
      geom_errorbarh(data=comDF, aes(xmin=GPP_diff-GPP_diff_sd, 
                                     xmax=GPP_diff+GPP_diff_sd)) +
      geom_point(data=comDF, aes(GPP_diff, LAI_diff, col=ModName),
                 size=6)+
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
      xlab(expression(paste(CO[2] * " response of GPP (%)")))+
      ylab(expression(paste(CO[2] * " response of LAI (%)")))+
      scale_color_manual(name="Model",
                         values=c(col.values,
                                  "Obs"="black"))
    
    plot(p7)
    
    
    plots_right_column <- plot_grid(p3, p7, p2, 
                                    labels=c("b", "d", "e"),
                                    ncol=1, align="vh", axis = "l",
                                    label_x=0.84, label_y=0.95,
                                    label_size = 20)
    
    
  
    pdf(paste0(out.dir, "/MIP_normalized_photosynthesis_response_OBS_", 
               scenario, "_comparison_with_obs.pdf"), 
        width=14, height=16)
    plot_grid(plots_left_column, plots_right_column,
              ncol=2, rel_widths=c(1,0.5))
    
    dev.off()
    
    
    
    plots_bottom_row <- plot_grid(p3, p5, p2, p7,
                                  labels=c("b", "c", "d", "e"),
                                  ncol=4, align="vh", axis = "l",
                                  label_x=0.84, label_y=0.95,
                                  rel_widths=c(1.2, 1,1,1),
                                  label_size = 20)
    
    pdf(paste0(out.dir, "/MIP_normalized_photosynthesis_response_OBS_", 
               scenario, "_comparison_with_obs2.pdf"), 
        width=14, height=8)
    plot_grid(plots_top_row, plots_bottom_row,
              ncol=1, rel_heights=c(1,1))
    
    dev.off()
    
    
    
    
    
    
    
    
    
    
    
    
    #########################################################################
    #### time sequence

    ### Aleaf
    p1 <- ggplot() +
      geom_point(data=ambDF2, aes(YEAR, Aleaf.mean, col=ModName))+
      geom_line(data=ambDF2, aes(YEAR, Aleaf.mean, col=ModName, lty=ModName))+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1,5))
    
    
    p2 <- ggplot() +
      geom_point(data=eleDF2, aes(YEAR, Aleaf.mean, col=ModName))+
      geom_line(data=eleDF2, aes(YEAR, Aleaf.mean, col=ModName, lty=ModName))+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1,5))
    
    ### LAI
    p3 <- ggplot() +
      geom_point(data=ambDF2, aes(YEAR, LAI.mean, col=ModName))+
      geom_line(data=ambDF2, aes(YEAR,LAI.mean, col=ModName, lty=ModName))+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("LAI")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1,4.5))
    
    
    p4 <- ggplot() +
      geom_point(data=eleDF2, aes(YEAR, LAI.mean, col=ModName))+
      geom_line(data=eleDF2, aes(YEAR, LAI.mean, col=ModName, lty=ModName))+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("LAI")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1,4.5))
    
    
    p5 <- ggplot() +
      geom_point(data=ambDF3, aes(YEAR, GPP, col=ModName))+
      geom_line(data=ambDF3, aes(YEAR, GPP, col=ModName, lty=ModName))+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(GPP* " (g C " * m^2 * " " * yr^-1, ")")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1000,3500))
    
    
    p6 <- ggplot() +
      geom_point(data=eleDF3, aes(YEAR, GPP, col=ModName))+
      geom_line(data=eleDF3, aes(YEAR, GPP, col=ModName, lty=ModName))+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(GPP * " (g C " * m^2 * " " * yr^-1, ")")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1000,3500))
    
    legend_top_row <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    plots_top_row <- plot_grid(p5, p6, 
                               p1, p2, 
                               p3, p4, 
                               labels=c("(a)", "(b)", "(c)", "(d)",
                                        "(e)", "(f)"),
                               ncol=2, align="vh", axis = "l",
                               label_x=0.86, label_y=0.95,
                               label_size = 18)
    
    
    #pdf(paste0(out.dir, "/MIP_normalized_photosynthesis_response_OBS_", 
    #           scenario, "_AMB_comparison.pdf"), 
    #    width=12, height=16)
    #plot_grid(plots_top_row,
    #          legend_top_row,
    #          ncol=1, rel_heights=c(1,0.2))
    #
    #dev.off()
    
}    

