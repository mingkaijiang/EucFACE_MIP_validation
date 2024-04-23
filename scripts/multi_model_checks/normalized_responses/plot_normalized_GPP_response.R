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
    p1_mm <- ggplot(data=plotDF1, 
                 aes(ModName, Aleaf.mean.mean, group=Trt)) +
      geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
            legend.spacing.y = unit(0, "pt"),
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
    
    
    
    p2_mm <- ggplot(data=plotDF2, 
                 aes(ModName, LAI.mean.mean, group=Trt)) +
      geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
            legend.position=c(0.95, 0.86),
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.spacing.y = unit(0, "pt"),
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
      guides(alpha=guide_legend(""), fill = FALSE)+
      scale_x_discrete(limit=c(mod.list, "Multi-model", "OBS"),
                       label=c(model.labels, 
                               "Multi-model" = expression(bold("M-M")),
                               "OBS" = expression(bold("OBS"))))
    
    
    p3_mm <- ggplot(data=plotDF3, 
                 aes(ModName, GPP.mean/1000, group=Trt)) +
      geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
      geom_errorbar(aes(x=ModName, ymin=(GPP.mean-GPP.sd)/1000, 
                        ymax=(GPP.mean+GPP.sd)/1000), width=0.4,
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
            legend.position=c(0.95, 0.86),
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("GPP (kg C " * m^2 * " " * yr^-1, ")")))+
      scale_alpha_manual(name="Treatment",
                         values=c("amb" = 0.3, 
                                  "ele" = 1.0),
                         label=c("AMB", "ELE"))+
      scale_fill_manual(name="Model",
                        values=c(col.values, "black", "black"),
                        labels=c(model.labels, "Multi-model" = "M-M",
                                 "OBS"= "SITE ESTIMATE"))+
      guides(alpha=guide_legend(""), fill = FALSE)+
      scale_x_discrete(limit=c(mod.list, "Multi-model", "OBS"),
                       label=c(model.labels, 
                               "Multi-model" = expression(bold("M-M")),
                               "OBS" = expression(bold("SITE ESTIMATE"))))
    
    #plot(p3_mm)
    
    
    #legend_top_row <- get_legend(p1 + theme(legend.position="bottom",
    #                                        legend.box = 'horizontal',
    #                                        legend.box.just = 'left'))
    
    plots_left_column <- plot_grid(p3_mm, p1_mm, p2_mm, 
                                   labels=c("A", "C", "E"),
                                   ncol=1, align="vh", axis = "l",
                                   label_x=0.1, label_y=0.95,
                                   label_size = 20)
    
    plots_top_row <- plot_grid(p3_mm, labels=c(""), ncol=1,
                                         align="vh", axis = "l",
                                         label_x=0.1, label_y=0.95,
                                         label_size = 20)
    
    plots_ed_figure <- plot_grid(p1_mm, p2_mm, 
                                 labels=c("A", "B"),
                                 ncol=1, align="vh", axis = "l",
                                 label_x=0.1, label_y=0.95,
                                 label_size = 20)
    
    pdf(paste0(out.dir, "/MIP_photosynthesis_", 
               scenario, "_comparison_with_obs_ed_figure.pdf"), 
        width=12, height=6)
    plot_grid(plots_ed_figure)
    
    dev.off()
    
    
    
    
    
    
    
    
    
    ###############################################################################

    ### plot CO2 response ratios
    subDF <- plotDF2[plotDF2$ModName%in%c("C_GDAYP", "A_ELMV1",
                                          "B_CABLP", "D_LPJGP",
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
    subDF <- plotDF3[plotDF3$ModName%in%c("C_GDAYP", "A_ELMV1",
                                          "B_CABLP", "D_LPJGP",
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
    
    
    myDF <- read.csv("./validation_dataset/EucFACE_C_Budget_data/MAESPA_output/VJlimitedPhotoResponseCO2.csv")
    myDF <- myDF[myDF$A.inc >=0,]
    myDF <- myDF[myDF$A.inc <=2,]
    
    Ac.mean <- mean(myDF$Ac.inc-1)*100
    Ac.sd <- sd(myDF$Ac.inc-1)*100
    Aj.mean <- mean(myDF$Aj.inc-1)*100
    Aj.sd <- sd(myDF$Aj.inc-1)*100
    A.mean <- mean(myDF$A.inc-1)*100
    A.sd <- sd(myDF$A.inc-1)*100
    
    
    tmpDF1 <- data.frame("ModName"=c("Ac.mean", "Aj.mean"),
                         "diff"=c(Ac.mean, Aj.mean),
                         "diff.sd"=c(Ac.sd, Aj.sd),
                         "ModName2"=c("Ac", "Aj"))
    
    gppDF.plot <- rbind(gppDF.plot, tmpDF1)
    
    
    subDF2 <- gppDF.plot[gppDF.plot$ModName%in%c("C_GDAYP","A_ELMV1",
                                                 "B_CABLP", "D_LPJGP",
                                                 "E_OCHDP", "F_QUINC",
                                                 "G_OCHDX", "H_QUJSM"),]
    
    subDF1 <- gppDF.plot[gppDF.plot$ModName%in%c("Obs_field", "Obs_inst", "Ac.mean", "Aj.mean"),]
    
    
    
    
    
    p3_co2 <- ggplot(data=gppDF.plot, 
                 aes(ModName2, diff)) +
      geom_violin(fill="white") +
      stat_summary(fun.y="mean", geom="crossbar", width=0.5, color="black")+
      geom_errorbar(data=subDF1, aes(x=ModName2, ymin=diff-diff.sd, 
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
      scale_x_discrete(limit=c("Multi-model", "Obs_field", "Obs_inst",
                               "Ac", "Aj"),
                       label=c("Multi-model" = expression("M-M"),
                               "Obs_field" = expression("OBS"[field]),
                               "Obs_inst" = expression("OBS"[inst]),
                               "Ac" = expression(A[c]),
                               "Aj"= expression(A[j])))+
      scale_color_manual(name="Model",
                         values=c(col.values,
                                  "Obs_field"="black",
                                  "Obs_inst"="black",
                                  "Ac"="black",
                                  "Aj"="black"))
    
    #plot(p3_co2)
    
    #### plot CO2 response ratios
    #subDF <- plotDF1[plotDF1$ModName%in%c("C_GDAYP", "A_ELMV1",
    #                                      "B_CABLP", "D_LPJGP",
    #                                      "E_OCHDP", "F_QUINC",
    #                                      "G_OCHDX", "H_QUJSM"),]
    #
    #subDF1 <- subDF[subDF$Trt=="amb",]
    #subDF2 <- subDF[subDF$Trt=="ele",]
    #subDF1$Aleaf.mean.sd <- NULL
    #subDF2$Aleaf.mean.sd <- NULL
    #
    #names(subDF1) <- names(subDF2) <- c("ModName", "Trt", "Aleaf")
    #
    #subDF <- merge(subDF1, subDF2, by="ModName")
    #subDF$diff <- with(subDF, (Aleaf.y-Aleaf.x)/Aleaf.x)
    #subDF$Trt.x <- NULL
    #subDF$Trt.y <- NULL
    #subDF$Aleaf.x <- NULL
    #subDF$Aleaf.y <- NULL
    #subDF$diff.sd <- 0.0
    #
    #tmpDF <- data.frame("ModName"="Multi-model",
    #                    "diff"=mean(subDF$diff),
    #                    "diff.sd"=sd(subDF$diff))
    #
    #aleafDF.plot <- rbind(subDF, tmpDF)
    #aleafDF.plot$diff <- aleafDF.plot$diff * 100
    #aleafDF.plot$diff.sd <- aleafDF.plot$diff.sd * 100
    #
    #myDF <- read.csv("./validation_dataset/EucFACE_C_Budget_data/MAESPA_output/VJlimitedPhotoResponseCO2.csv")
    #myDF <- myDF[myDF$A.inc >=0,]
    #myDF <- myDF[myDF$A.inc <=2,]
    #
    #Ac.mean <- mean(myDF$Ac.inc-1)*100
    #Ac.sd <- sd(myDF$Ac.inc-1)*100
    #Aj.mean <- mean(myDF$Aj.inc-1)*100
    #Aj.sd <- sd(myDF$Aj.inc-1)*100
    #A.mean <- mean(myDF$A.inc-1)*100
    #A.sd <- sd(myDF$A.inc-1)*100
    #
    #
    #tmpDF1 <- data.frame("ModName"=c("A.mean", "Ac.mean", "Aj.mean", "A.obs"),
    #                     "diff"=c(A.mean, Ac.mean, Aj.mean, v5*100),
    #                     "diff.sd"=c(A.sd, Ac.sd, Aj.sd, v6*100))
    #
    #aleafDF.plot <- rbind(aleafDF.plot, tmpDF1)
    #
    #
    #
    #
    #aleafDF.plot$ModName2 <- c(rep("Multi-model", 9),
    #                           "A.mean", "Ac.mean",
    #                           "Aj.mean", "A.obs")
    #
    #subDF2 <- aleafDF.plot[aleafDF.plot$ModName%in%c("C_GDAYP","A_ELMV1",
    #                                                 "B_CABLP", "D_LPJGP",
    #                                                 "E_OCHDP", "F_QUINC",
    #                                                 "G_OCHDX", "H_QUJSM"),]
    #
    #subDF1 <- aleafDF.plot[aleafDF.plot$ModName%in%c("A.mean", "Ac.mean",
    #                                                 "Aj.mean", "A.obs"),]
    #
    #
    #p1 <- ggplot(data=aleafDF.plot, 
    #             aes(ModName2, diff)) +
    #  geom_violin() +
    #  stat_summary(fun.y="mean", geom="crossbar", width=0.5, color="black")+
    #  geom_errorbar(data=subDF1, aes(x=ModName, ymin=diff-diff.sd, 
    #                                 ymax=diff+diff.sd), width=0.2,
    #                position=position_dodge(width=1)) +
    #  geom_point(data=subDF2, aes(ModName2, diff, 
    #                              col=ModName), 
    #             position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9), 
    #             size=6)+
    #  #ylim(-5, 15)+
    #  theme_linedraw() +
    #  theme(panel.grid.minor=element_blank(),
    #        axis.text.x=element_text(size=12),
    #        axis.title.x=element_blank(),
    #        axis.text.y=element_text(size=12),
    #        axis.title.y=element_text(size=14),
    #        legend.text=element_text(size=12),
    #        legend.title=element_text(size=14),
    #        panel.grid.major=element_blank(),
    #        legend.position="none",
    #        legend.box = 'horizontal',
    #        legend.box.just = 'left',
    #        plot.title = element_text(size=14, face="bold.italic", 
    #                                  hjust = 0.5))+
    #  ylab(expression(paste(CO[2] * " response of Aleaf (%)")))+
    #  scale_x_discrete(limit=c("Multi-model", "A.mean", "Ac.mean", "Aj.mean", "A.obs"),
    #                   label=c("Multi-model" = expression(bold("M-M")),
    #                           "A.mean" = expression(bold("A"[long])),
    #                           "Ac.mean" = expression(bold("A"[c])),
    #                           "Aj.mean" = expression(bold("A"[j])),
    #                           "A.obs" = expression(bold("A"[obs]))))+
    #  scale_color_manual(name="Model",
    #                     values=c(col.values,
    #                              "A.mean"="black",
    #                              "Ac.mean"="black",
    #                              "Aj.mean"="black",
    #                              "A.obs"="black"))
    #
    #plot(p1)
    
    
    
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
    
    subDF2 <- laiDF.plot[laiDF.plot$ModName%in%c("C_GDAYP", "A_ELMV1",
                                                 "B_CABLP", "D_LPJGP",
                                                 "E_OCHDP", "F_QUINC",
                                                 "G_OCHDX", "H_QUJSM"),]
    subDF2$ModName2 <- "Multi-model"
    
    #subDF2$diff <- subDF2$diff * 100
    
    subDF1$ModName2 <- "Obs"
    
    subDF3 <- rbind(subDF2, subDF1)
    

    ### plotting
    
    p2_co2 <- ggplot(data=subDF3, 
                 aes(ModName2, diff)) +
      geom_violin(fill="white") +
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
                       label=c("Multi-model" = expression("M-M"),
                               "Obs" = expression("OBS")))+
      scale_color_manual(name="Model",
                         values=c(col.values,
                                  "Obs"="black"))

    
    
    
    subDF1 <- plotDF2[plotDF2$ModName%in%c("Multi-model", "OBS"),]
    subDF2 <- plotDF2[plotDF2$ModName%in%c("C_GDAYP", "A_ELMV1",
                                           "B_CABLP", "D_LPJGP",
                                           "E_OCHDP", "F_QUINC",
                                           "G_OCHDX", "H_QUJSM"),]
    subDF2$ModName2 <- "Multi-model"
    
    p5_co2 <- ggplot(data=subDF1, 
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
    
    
    #plot(p5)
    
    
    subDF1 <- plotDF3[plotDF3$ModName%in%c("Multi-model", "OBS"),]
    subDF2 <- plotDF3[plotDF3$ModName%in%c("C_GDAYP", "A_ELMV1",
                                           "B_CABLP", "D_LPJGP",
                                           "E_OCHDP", "F_QUINC",
                                           "G_OCHDX", "H_QUJSM"),]
    subDF2$ModName2 <- "Multi-model"
    
    p6_co2 <- ggplot(data=subDF1, 
                 aes(ModName, GPP.mean, group=Trt)) +
      geom_bar(data=subDF1,stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
                                 "OBS" = "SITE ESTIMATE"))+
      guides(alpha=guide_legend("Treatment"), fill = FALSE)+
      scale_x_discrete(limit=c("Multi-model", "OBS"),
                       label=c("Multi-model" = expression(bold("M-M")),
                               "OBS" = expression(bold("SITE ESTIMATE"))))+
      scale_color_manual(name="Model",
                         values=c(col.values),
                         labels=c(model.labels))
    
    
    
    
    ### merge the two to get a combined DF
    gppDF.plot$ModName[gppDF.plot$ModName=="Obs_field"] <- "Obs"
    comDF <- merge(gppDF.plot, laiDF.plot, by=c("ModName"))
    colnames(comDF) <- c("ModName", "GPP_diff", "GPP_diff_sd", "ModName2",
                         "LAI_diff", "LAI_diff_sd")
    
    p7_co2 <- ggplot(data=comDF, 
                 aes(GPP_diff, LAI_diff, group=ModName)) +
      geom_errorbar(data=comDF, aes(ymin=LAI_diff-LAI_diff_sd, 
                                    ymax=LAI_diff+LAI_diff_sd)) +
      geom_errorbarh(data=comDF, aes(xmin=GPP_diff-GPP_diff_sd, 
                                     xmax=GPP_diff+GPP_diff_sd)) +
      geom_point(data=comDF, aes(GPP_diff, LAI_diff, col=ModName, pch=ModName),
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
                                  "Multi-model"="black",
                                  "Obs"="black"),
                         labels=c(model.labels, 
                                  "Multi-model"="M-M",
                                  "Obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "Multi-model"=19,"Obs"=15),
                         labels=c(model.labels, 
                                  "Multi-model"="M-M",
                                  "Obs"="OBS"))+
      annotate("text", x = 5, y = -1, label = "OBS")+
      annotate("text", x = 17, y = 5, label = "M-M")
    
    #plot(p7_co2)
    
    #common.legend <- get_legend(p1_co2 + theme(legend.position="right"))
    
    
    plots_lai_column <- plot_grid(p5_co2, p2_co2, 
                                  labels=c("", ""),
                                  ncol=1, align="vh", axis = "l",
                                  label_x=0.84, label_y=0.95,
                                  label_size = 20)
    
    plots_bottom_row <- plot_grid(p3_co2, plots_lai_column, p7_co2, 
                                  labels=c("", "", ""),
                                  ncol=3, align="vh", axis = "l",
                                  label_x=0.84, label_y=0.95,
                                  rel_widths=c(1.2, 1,1),
                                  label_size = 20)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ###############################################################################
    ### Leaf nutrient
    ### setting out path to store the files
    in.dir <- paste0(getwd(), "/output/MIP_output/processed_simulation")
    
    ### create output folder
    if(!dir.exists(in.dir)) {
      dir.create(in.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0(in.dir, "/MIP_OBS_", scenario, "_AMB_daily.rds"))
    eleDF <- readRDS(paste0(in.dir, "/MIP_OBS_", scenario, "_ELE_daily.rds"))
    
    ### ignore NAs
    ambDF[ambDF<=-999] <- NA
    eleDF[eleDF<=-999] <- NA
    
    ### remove two N only models
    ambDF <- ambDF[ambDF$ModName%in%c("C_GDAYP",
                                      "A_ELMV1",
                                      "B_CABLP",
                                      "D_LPJGP",
                                      "E_OCHDP",
                                      "F_QUINC",
                                      "G_OCHDX",
                                      "H_QUJSM"),]
    
    eleDF <- eleDF[eleDF$ModName%in%c("C_GDAYP",
                                      "A_ELMV1",
                                      "B_CABLP",
                                      "D_LPJGP",
                                      "E_OCHDP",
                                      "F_QUINC",
                                      "G_OCHDX",
                                      "H_QUJSM"),]
    
    
    ### Calculate ratios
    ambDF$LCN <- with(ambDF, CL/NL)
    eleDF$LCN <- with(eleDF, CL/NL)
    
    ambDF$LCP <- with(ambDF, CL/PL)
    eleDF$LCP <- with(eleDF, CL/PL)
    
    ambDF$LNP <- with(ambDF, NL/PL)
    eleDF$LNP <- with(eleDF, NL/PL)
    
    ambDF <- ambDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI", "NL", "PL",
                      "LCN", "LCP", "LNP", "NCON")]
    
    eleDF <- eleDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI", "NL", "PL",
                      "LCN", "LCP", "LNP", "NCON")]
    
    ambDF$Aleaf <- with(ambDF, GPP/LAI)
    eleDF$Aleaf <- with(eleDF, GPP/LAI)
    
    
    ambDF$NL_leaf <- with(ambDF, NL/LAI)
    ambDF$PL_leaf <- with(ambDF, PL/LAI)
    
    eleDF$NL_leaf <- with(eleDF, NL/LAI)
    eleDF$PL_leaf <- with(eleDF, PL/LAI)
    
    
    d <- dim(ambDF)[2]
    
    
    
    
    
    ##################################################################
    
    
    
    mod.list <- unique(ambDF$ModNmae)
    
    require(gridExtra)
    
    
    
    ### calculate multi-model means and then plot a vector with arrow to show directional change under eCO2
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    mgDF <- rbind(ambDF, eleDF)
    sumDF <- summaryBy(Aleaf+GPP+LAI+NL+PL+NL_leaf+PL_leaf+LCN+LCP+LNP~ModName+Trt, FUN=c(mean,sd), data=mgDF,
                       keep.names=T, na.rm=T)
    
    
    ### plot
    p1 <- ggplot() +
      #geom_segment(aes(x=sumDF$PL_leaf.mean[sumDF$Trt=="amb"], xend = sumDF$PL_leaf.mean[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$PL_leaf.mean[sumDF$Trt=="ele"], xend = sumDF$PL_leaf.mean[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$PL_leaf.mean[sumDF$Trt=="amb"]+sumDF$PL_leaf.sd[sumDF$Trt=="amb"], 
      #                 xend = sumDF$PL_leaf.mean[sumDF$Trt=="amb"]-sumDF$PL_leaf.sd[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$PL_leaf.mean[sumDF$Trt=="ele"]+sumDF$PL_leaf.sd[sumDF$Trt=="ele"], 
      #                 xend = sumDF$PL_leaf.mean[sumDF$Trt=="ele"]-sumDF$PL_leaf.sd[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$PL_leaf.mean[sumDF$Trt=="amb"], xend = sumDF$PL_leaf.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=1.0)+
      geom_point(data=sumDF, aes(PL_leaf.mean, Aleaf.mean, fill=ModName, #alpha=Trt, 
                                 pch=Trt), color="black", size=4)+
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
            #plot.margin = margin(t = 10,  # Top margin
            #                     r = 10,  # Right margin
            #                     b = 10,  # Bottom margin
            #                     l = 10),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      scale_fill_manual(name="Model",
                        values=c(col.values),
                        labels=c(model.labels))+
      scale_shape_manual(name=expression(CO[2] * " treatment"),
                         values=c("amb"=21, "ele"=24),
                         labels=c("amb", "ele"))+
      #scale_alpha_manual(name="Treatment",
      #                   values=c("amb" = 0.5, 
      #                            "ele" = 1.0),
      #                   label=c("AMB", "ELE"))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression("Leaf P content (g P " * m^-2 * " leaf)"))
    
    
    
    p2 <- ggplot() +
      #geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="amb"], xend = sumDF$NL_leaf.mean[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="ele"], xend = sumDF$NL_leaf.mean[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="amb"]+sumDF$NL_leaf.sd[sumDF$Trt=="amb"], 
      #                 xend = sumDF$NL_leaf.mean[sumDF$Trt=="amb"]-sumDF$NL_leaf.sd[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="ele"]+sumDF$NL_leaf.sd[sumDF$Trt=="ele"], 
      #                 xend = sumDF$NL_leaf.mean[sumDF$Trt=="ele"]-sumDF$NL_leaf.sd[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="amb"], xend = sumDF$NL_leaf.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=1.0)+
      geom_point(data=sumDF, aes(NL_leaf.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
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
      scale_fill_manual(name="Model",
                        values=c(col.values),
                        labels=c(model.labels))+
      scale_shape_manual(name=expression(CO[2] * " treatment"),
                         values=c("amb"=21, "ele"=24),
                         labels=c("amb", "ele"))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression("Leaf N content (g N " * m^-2 * " leaf)"))
    
    
    
    p3 <- ggplot() +
      #geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="amb"], xend = sumDF$LCP.mean[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="ele"], xend = sumDF$LCP.mean[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="amb"]+sumDF$LCP.sd[sumDF$Trt=="amb"], 
      #                 xend = sumDF$LCP.mean[sumDF$Trt=="amb"]-sumDF$LCP.sd[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="ele"]+sumDF$LCP.sd[sumDF$Trt=="ele"], 
      #                 xend = sumDF$LCP.mean[sumDF$Trt=="ele"]-sumDF$LCP.sd[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="amb"], xend = sumDF$LCP.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=1.0)+
      geom_point(data=sumDF, aes(LCP.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
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
      scale_fill_manual(name="Model",
                        values=c(col.values),
                        labels=c(model.labels))+
      scale_shape_manual(name=expression(CO[2] * " treatment"),
                         values=c("amb"=21, "ele"=24),
                         labels=c("amb", "ele"))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression("Leaf CP ratio"))
    
    
    
    p4 <- ggplot() +
      #geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="amb"], xend = sumDF$LCN.mean[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="ele"], xend = sumDF$LCN.mean[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="amb"]+sumDF$LCN.sd[sumDF$Trt=="amb"], 
      #                 xend = sumDF$LCN.mean[sumDF$Trt=="amb"]-sumDF$LCN.sd[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="ele"]+sumDF$LCN.sd[sumDF$Trt=="ele"], 
      #                 xend = sumDF$LCN.mean[sumDF$Trt=="ele"]-sumDF$LCN.sd[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="amb"], xend = sumDF$LCN.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=1.0)+
      geom_point(data=sumDF, aes(LCN.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
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
      scale_fill_manual(name="Model",
                        values=c(col.values),
                        labels=c(model.labels))+
      scale_shape_manual(name=expression(CO[2] * " treatment"),
                         values=c("amb"=21, "ele"=24),
                         labels=c("amb", "ele"))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression("Leaf CN ratio"))
    
    
    
    p5 <- ggplot() +
      #geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="amb"], xend = sumDF$LNP.mean[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="ele"], xend = sumDF$LNP.mean[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="amb"]+sumDF$LNP.sd[sumDF$Trt=="amb"], 
      #                 xend = sumDF$LNP.mean[sumDF$Trt=="amb"]-sumDF$LNP.sd[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="ele"]+sumDF$LNP.sd[sumDF$Trt=="ele"], 
      #                 xend = sumDF$LNP.mean[sumDF$Trt=="ele"]-sumDF$LNP.sd[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="amb"], xend = sumDF$LNP.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=1.0)+
      geom_point(data=sumDF, aes(LNP.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
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
      scale_fill_manual(name="Model",
                        values=c(col.values),
                        labels=c(model.labels))+
      scale_shape_manual(name=expression(CO[2] * " treatment"),
                         values=c("amb"=21, "ele"=24),
                         labels=c("amb", "ele"))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression("Leaf NP ratio"))
    
    
    
   
    
    ##################################################################
    ### calculate multiple model means for each year
    ## amb
    ambDF.mip <- summaryBy(.~YEAR+ModName, FUN=mean,
                           data=ambDF, keep.names=T, na.rm=T)
    
    eleDF.mip <- summaryBy(.~YEAR+ModName, FUN=mean,
                           data=eleDF, keep.names=T, na.rm=T)
    
    
    ### replace inf with na
    ambDF.mip[sapply(ambDF.mip, is.infinite)] <- NA
    ambDF.mip[sapply(ambDF.mip, is.nan)] <- NA
    
    eleDF.mip[sapply(eleDF.mip, is.infinite)] <- NA
    eleDF.mip[sapply(eleDF.mip, is.nan)] <- NA
    
    
    
    ### prepare CO2 df
    co2DF <- ambDF.mip
    d.rev <- dim(co2DF)[2]
    co2DF[,5:d.rev] <- eleDF.mip[,5:d.rev]-ambDF.mip[,5:d.rev]
    
    ### prepare CO2 pct difference df
    pctco2DF <- co2DF
    pctco2DF[,5:d.rev] <- co2DF[,5:d.rev]/ambDF.mip[,5:d.rev]*100.0
    
    
    
    
    ### calculate annual averages
    sumDF2 <- summaryBy(Aleaf+GPP+LAI+NL+PL+NL_leaf+PL_leaf+LCN+LCP+LNP~ModName, FUN=c(mean,sd), data=co2DF,
                        keep.names=T, na.rm=T)
    
    sumDF3 <- summaryBy(Aleaf+GPP+LAI+NL+PL+NL_leaf+PL_leaf+LCN+LCP+LNP~ModName, FUN=c(mean,sd), data=pctco2DF,
                        keep.names=T, na.rm=T)
    
    
    ### calculate multi-model means
    pctco2DF$ModName2 <- "I_MM"
    mmDF <- summaryBy(Aleaf+GPP+LAI+NL+PL+NL_leaf+PL_leaf+LCN+LCP+LNP~ModName2, FUN=c(mean,sd), data=pctco2DF,
                      keep.names=T, na.rm=T)
    colnames(mmDF) <- colnames(sumDF3)
    
    sumDF3 <- rbind(sumDF3, mmDF)
    
    ### add data
    obsDF <- mmDF
    obsDF$NL.mean <- NA
    obsDF$NL_leaf.mean <- NA
    obsDF$NL.sd <- NA
    obsDF$NL_leaf.sd <- NA
    obsDF$LNP.mean <- NA
    obsDF$LNP.sd <- NA
    obsDF$LCN.mean <- NA
    obsDF$LCN.sd <- NA
    
    
    obsDF$GPP.mean <- eucDF$GPP[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    obsDF$GPP.sd <- eucDF$GPP[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    obsDF$LAI.mean <- eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    obsDF$LAI.sd <- eucDF$LAI[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    obsDF$PL.mean <- eucDF$PL[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    obsDF$PL.sd <- eucDF$PL[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    obsDF$LCP.mean <- eucDF$CPL[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    obsDF$LCP.sd <- eucDF$CPL[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    
    
    
    ### read in gpp simulation to ignore LAI effect
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
    obsDF$Aleaf.mean <- 11
    obsDF$Aleaf.sd <- sd(mgDF$Aleaf_diff) * 100
    
    
    ### PL leaf
    v1 <- eucDF$PL[eucDF$Group=="mean"&eucDF$Trt=="aCO2"]/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="aCO2"]
    v2 <- eucDF$PL[eucDF$Group=="mean"&eucDF$Trt=="eCO2"]/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="eCO2"]
    
    v3 <- sqrt((eucDF$PL[eucDF$Group=="sd"&eucDF$Trt=="aCO2"]^2+eucDF$LAI[eucDF$Group=="sd"&eucDF$Trt=="aCO2"]^2)/2)
    v4 <- sqrt((eucDF$PL[eucDF$Group=="sd"&eucDF$Trt=="eCO2"]^2+eucDF$LAI[eucDF$Group=="sd"&eucDF$Trt=="eCO2"]^2)/2)
    
    
    ### calculate PL leaf    
    obsDF$PL_leaf.mean <- (v2-v1)/v1 * 100
    obsDF$PL_leaf.sd <- sqrt((v3^2+v4^2+v3^2)/3) * 100
    
    obsDF$ModName <- "OBS"
    
    sumDF3 <- rbind(sumDF3, obsDF)
    
    
    ### now prepare the plotting df
    plotDF1 <- sumDF3[sumDF3$ModName%in%c("C_GDAYP", "A_ELMV1", 
                                          "B_CABLP", "D_LPJGP",
                                          "E_OCHDP", "F_QUINC",
                                          "G_OCHDX", "H_QUJSM"),]
    
    plotDF2 <- sumDF3[sumDF3$ModName%in%c("I_MM", "OBS"),]
    
    
    
    ### plotting
    p6 <- ggplot() +
      geom_segment(aes(x=plotDF2$LCP.mean, xend = plotDF2$LCP.mean,
                       y=plotDF2$Aleaf.mean+plotDF2$Aleaf.sd, 
                       yend=plotDF2$Aleaf.mean-plotDF2$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=plotDF2$LCP.mean+plotDF2$LCP.sd, 
                       xend = plotDF2$LCP.mean-plotDF2$LCP.sd,
                       y=plotDF2$Aleaf.mean, 
                       yend=plotDF2$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(LCP.mean, Aleaf.mean, color=ModName, pch=ModName), 
                 size=4)+
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
      scale_color_manual(name="Model",
                         values=c(col.values, "I_MM"="grey30", "OBS"="grey"),
                         labels=c(model.labels, "I_MM"="M-M", "OBS"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "I_MM"=19, "OBS"=15))+
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      #guides(fill = guide_legend(override.aes = list(col = c(col.values))),
      #       color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf CP ratio (%)")))
    
    
    
    
    
    p7 <- ggplot() +
      geom_segment(aes(x=plotDF2$LCN.mean, xend = plotDF2$LCN.mean,
                       y=plotDF2$Aleaf.mean+plotDF2$Aleaf.sd, 
                       yend=plotDF2$Aleaf.mean-plotDF2$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=plotDF2$LCN.mean+plotDF2$LCN.sd, 
                       xend = plotDF2$LCN.mean-plotDF2$LCN.sd,
                       y=plotDF2$Aleaf.mean, 
                       yend=plotDF2$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(LCN.mean, Aleaf.mean, color=ModName, pch=ModName), 
                 size=4)+
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
      scale_color_manual(name="Model",
                         values=c(col.values, "I_MM"="grey30", "OBS"="grey"),
                         labels=c(model.labels, "I_MM"="M-M", "OBS"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "I_MM"=19,"OBS"=15))+
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf CN ratio (%)")))
    
    
    p8 <- ggplot() +
      geom_segment(aes(x=plotDF2$LNP.mean, xend = plotDF2$LNP.mean,
                       y=plotDF2$Aleaf.mean+plotDF2$Aleaf.sd, 
                       yend=plotDF2$Aleaf.mean-plotDF2$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=plotDF2$LNP.mean+plotDF2$LNP.sd, 
                       xend = plotDF2$LNP.mean-plotDF2$LNP.sd,
                       y=plotDF2$Aleaf.mean, 
                       yend=plotDF2$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(LNP.mean, Aleaf.mean, color=ModName, pch=ModName), 
                 size=4)+
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
      scale_color_manual(name="Model",
                         values=c(col.values, "I_MM"="grey30", "OBS"="grey"),
                         labels=c(model.labels, "I_MM"="M-M", "OBS"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "I_MM"=19,"OBS"=15))+
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf NP ratio (%)")))
    
    
    
    p9 <- ggplot() +
      geom_segment(aes(x=plotDF2$PL_leaf.mean, xend = plotDF2$PL_leaf.mean,
                       y=plotDF2$Aleaf.mean+plotDF2$Aleaf.sd, 
                       yend=plotDF2$Aleaf.mean-plotDF2$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=plotDF2$PL_leaf.mean+plotDF2$PL_leaf.sd, 
                       xend = plotDF2$PL_leaf.mean-plotDF2$PL_leaf.sd,
                       y=plotDF2$Aleaf.mean, 
                       yend=plotDF2$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(PL_leaf.mean, Aleaf.mean, color=ModName, pch=ModName), 
                 size=4)+
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
      scale_color_manual(name="Model",
                         values=c(col.values, "I_MM"="grey30", "OBS"="grey"),
                         labels=c(model.labels, "I_MM"="M-M", "OBS"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "I_MM"=19,"OBS"=15))+
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf P (%)")))
    
    
    
    
    
    p10 <- ggplot() +
      geom_segment(aes(x=plotDF2$NL_leaf.mean, xend = plotDF2$NL_leaf.mean,
                       y=plotDF2$Aleaf.mean+plotDF2$Aleaf.sd, 
                       yend=plotDF2$Aleaf.mean-plotDF2$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=plotDF2$NL_leaf.mean+plotDF2$NL_leaf.sd, 
                       xend = plotDF2$NL_leaf.mean-plotDF2$NL_leaf.sd,
                       y=plotDF2$Aleaf.mean, 
                       yend=plotDF2$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(NL_leaf.mean, Aleaf.mean, color=ModName, pch=ModName), 
                 size=4)+
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
      scale_color_manual(name="Model",
                         values=c(col.values, "I_MM"="grey30", "OBS"="grey"),
                         labels=c(model.labels, "I_MM"="M-M", "OBS"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "I_MM"=19,"OBS"=15))+
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf N (%)")))
    
    
    
    
    
    plot_nutrient_row <- plot_grid(p1, p2, p3, p4, p5,
                                   p9, p10,p6, p7, p8,
                                   labels=c("", "", ""),
                                   ncol=5, nrow=2, 
                                   align="vh", axis = "l",
                                   label_x=0.84, label_y=0.95,
                                   label_size = 20)
    
    plots_top_row <- plot_grid(p3_mm, p3_co2, 
                               labels=c("(a)", "(b)"),
                               ncol=2, rel_widths=c(1, 0.5),
                               align="vh", axis = "l",
                               label_x=c(0.06, 0.84), label_y=0.95,
                               label_size = 20)
   
    plots_mid_row <- plot_grid(p2_mm, p2_co2, 
                               labels=c("(c)", "(d)"),
                               ncol=2, rel_widths=c(1, 0.5),
                               align="vh", axis = "l",
                               label_x=c(0.06, 0.84), label_y=0.95,
                               label_size = 20)
    
    plots_bot_right_first_row <- plot_grid(p1, p2, p5,
                                           labels=c("(f)", "(g)", "(h)"),
                                           ncol=3, 
                                           align="vh", axis = "l",
                                           label_x=c(0.82, 0.8, 0.78), label_y=0.95,
                                           label_size = 20)
    
    plots_bot_right_second_row <- plot_grid(p9, p10, p8,
                                            labels=c("(i)", "(j)", "(k)"),
                                            ncol=3, 
                                            align="vh", axis = "l",
                                            label_x=c(0.82, 0.82, 0.78), label_y=0.95,
                                            label_size = 20)
    
    plots_bot_right_column <- plot_grid(plots_bot_right_first_row,
                                        plots_bot_right_second_row,
                                        labels=c("", "", ""),
                                        ncol=1, nrow=2,
                                        rel_heigths=c(1,1),
                                        align="vh", axis = "l",
                                        label_x=0.82, label_y=0.95,
                                        label_size = 20)
    
    
    plots_bot_row <- plot_grid(p7_co2, plots_bot_right_column,
                               labels=c("(e)"),
                               ncol=2, 
                               rel_widths=c(0.8, 1.4),
                               align="vh", axis = "l",
                               label_x=0.1, label_y=0.95,
                               label_size = 20)
    
    
    ###############################################################################

    
    #pdf(paste0(out.dir, "/MIP_normalized_photosynthesis_response_OBS_", 
    #           scenario, "_comparison_with_obs2.pdf"), 
    #    width=16, height=12)
    #
    #plot_grid(plots_top_row, plots_mid_row,
    #          plots_bot_row,
    #          ncol=1, rel_heights=c(1,1,2))
    #
    #dev.off()
    
    
    
    ###############################################################################
    
    plots_top_column <- plot_grid(p3_mm, 
                                  p2_mm, 
                                  #p1_mm,
                                  labels=c("A", "B"),
                                  ncol=1, #rel_widths=c(1, 1, 1),
                                  align="vh", axis = "l",
                                  label_x=0.06, label_y=0.95,
                                  label_size = 20)
    
    
    plots_bottom_row <- plot_grid(p3_co2, p2_co2, p7_co2, 
                                  labels=c("C", "D", "E"),
                                  ncol=3, rel_widths=c(1.2, 0.8, 1),
                                  align="vh", axis = "l",
                                  #label_x=c(0.86, 0.8, 0.83), 
                                  label_x=c(0.12, 0.18, 0.15),
                                  label_y=0.95,
                                  label_size = 20)
    
    
    legend_bottom_row <- get_legend(p7_co2 + theme(legend.position="bottom",
                                                   legend.box = 'horizontal',
                                                   legend.box.just = 'left'))
    
    
    pdf(paste0(out.dir, "/MIP_normalized_photosynthesis_response_OBS_", 
               scenario, "_comparison_with_obs2.pdf"), 
        width=12, height=12)
    
    plot_grid(plots_top_column, 
              plots_bottom_row,
              legend_bottom_row,
              ncol=1, rel_heights=c(1, 0.5, 0.1),
              align="vh", axis = "l",
              label_x=0.84, label_y=0.95,
              label_size = 20)
    
    dev.off()
    
}    

