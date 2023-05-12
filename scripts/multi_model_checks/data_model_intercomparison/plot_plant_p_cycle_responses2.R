plot_plant_p_cycle_responses2 <- function(eucDF,
                                         scenario) {
  
    
    ##################################################################
    ### Purpose:
    ### to compare model predictions against data,
    ### including ambient and elevated treatment means,
    ### stoichiometry, efficiency, residence time, etc.,
    ### and the CO2 response difference and ratio,
    ### try to include all relevant variables.
    
    
    ##################################################################
    #### Set up basics
    ### setting out path to store the files
    ### this is only valid for variable climate
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario, "/")
  
    ### create output folder
    if(!dir.exists(out.dir)) {
      dir.create(out.dir, showWarnings = FALSE)
    }
  
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_ELE_annual.rds"))
    
    d <- dim(ambDF)[2]
    
    ### remove N models
    ambDF <- ambDF[ambDF$ModName!="I_GDAYN",]
    ambDF <- ambDF[ambDF$ModName!="J_LPJGN",]
    eleDF <- eleDF[eleDF$ModName!="I_GDAYN",]
    eleDF <- eleDF[eleDF$ModName!="J_LPJGN",]
    
    #### calculate 4-yr means in the simulation datasets
    ambDF <- subset(ambDF, YEAR>2012 & YEAR<2017)
    eleDF <- subset(eleDF, YEAR>2012 & YEAR<2017)
    
    ### calculate % difference
    annDF.pct.diff <- ambDF
    annDF.pct.diff[,3:d] <- (eleDF[,3:d]-ambDF[,3:d])/ambDF[,3:d] * 100.0
    
    
    ambDF.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                           data=ambDF,
                           keep.names=T, na.rm=T)
    
    eleDF.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                           data=eleDF,
                           keep.names=T, na.rm=T)
    
    annDF.diff.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                                data=annDF.pct.diff,
                                keep.names=T, na.rm=T)
    
    
    ### get the list of models
    mod.list <- unique(ambDF.sum$ModName)
    nmod <- length(mod.list)
    
  
    ##########################################################################
    ### Plotting
    
    
    ################# Delta C pools  ####################
    vegDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                          ambDF=ambDF.sum,
                                                                          eleDF=eleDF.sum,
                                                                          difDF=annDF.diff.sum,
                                                                          var.list=c("deltaCL", "deltaCW", "deltaCFR", "deltaCCR", "deltaCSTOR"),
                                                                          calculate.total=T)
    
    ### split into ambDF, pctDF
    plotDF1 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("deltaCL", "deltaCW", "deltaCFR", "deltaCCR", "deltaCSTOR"),]
    plotDF2 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("Tot"),]
    plotDF3 <- vegDF[vegDF$Trt=="diff"&vegDF$Variable%in%c("Tot"),]
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    
    plotDF2$sdvalue[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                       "C_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### pass plotDF3 to make biomass production / GPP ratio figure
    bpDF <- plotDF3[,c("Group", "meanvalue", "sdvalue")]
    colnames(bpDF) <- c("Group", "BP_mean", "BP_sd")
    
    
    
    ################# Major nutrient pools  ####################
    ### Below I provide several variables to help constrain the nutrient cycles in the model, 
    ### namely labile inorganic P pool (PLAB), 
    ### soil net N and P mineralization rate (NMIN and PMIN), 
    ### plant N and P uptake (NUP and PUP), 
    ### and soil N and P leaching (NLEACH and PLEACH). 
    ### We did not include total soil P pool, 
    ### because its size could be misleading given that 
    ### the majority of the P in the soil is stored as occluded form unavailable for plants. 
    ### Note that in the table below, simulated results are for top 30 cm of the soil, 
    ### but observed data are for top 10 cm only. 
    vegDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                          ambDF=ambDF.sum,
                                                                          eleDF=eleDF.sum,
                                                                          difDF=annDF.diff.sum,
                                                                          var.list=c("PL", "PW", "PFR", "PCR", "PSTOR"),
                                                                          calculate.total=T)
    
    
    
    
    ### split into ambDF, pctDF
    plotDF1 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("PL","PW","PCR","PFR","PSTOR"),]
    plotDF2 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("Tot"),]
    
    plotDF3 <- vegDF[vegDF$Trt=="pct_diff"&vegDF$Variable%in%c("Tot"),]
    plotDF4 <- vegDF[vegDF$Trt=="pct_diff"&vegDF$Variable%in%c("PL","PW","PFR","PSTOR"),]
    
    plotDF5 <- vegDF[vegDF$Trt=="diff"&vegDF$Variable%in%c("Tot"),]
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    
    plotDF2$sdvalue[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                       "C_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### adding to bp DF
    tmpDF <- plotDF5[plotDF5$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF5 <- rbind(plotDF5, tmpDF2)
    
    plotDF5 <- plotDF5[,c("Group", "meanvalue", "sdvalue")]
    
    colnames(plotDF5) <- c("Group", "PVEG_mean", "PVEG_sd")
    bpDF <- merge(bpDF, plotDF5, by=c("Group"))
    
    
    
    
    
    
    
    ### Plotting
    ### additional to-do list:
    ### 1. fill color by manual selection
    p1 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        geom_errorbar(data=plotDF2, 
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        #geom_point(data=plotDF2, aes(x=Group, y=meanvalue), col="black",
        #         fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              #axis.text.x=element_text(size=14,angle = 45, 
              #                         vjust = 1, hjust = 1),
              axis.text.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              #axis.text.x=element_text(size=14,angle = 45, 
              #                         vjust = 1, hjust = 1),
              panel.grid.major=element_blank(),
              legend.position=c(0.85,0.2),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("P pools (g P " * m^2*")")))+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model" = expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name=expression(P[veg]),
                        values=c("PL"=cbbPalette[2],
                                 "PW"=cbbPalette[3],
                                 "PFR"=cbbPalette[4],
                                 "PCR"=cbbPalette[7],
                                 "PSTOR"=cbbPalette[8]),
                        labels=c("PL"=expression(P[leaf]), 
                                 "PW"=expression(P[wood]), 
                                 "PFR"=expression(P[froot]), 
                                 "PCR"=expression(P[croot]),
                                 "PSTOR"=expression(P[store])))+
        guides(fill=guide_legend(nrow=3)); p1
    
    
    p2 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group),
                 position=position_dodge2(), col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", stat="identity",
                      position=position_dodge2(), width=0.3)+
        #geom_point(data=plotDF3, aes(x=Group, y=meanvalue), col="black",
        #         fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              #axis.text.x=element_text(size=14,angle = 45, 
              #                         vjust = 1, hjust = 1),
              axis.text.x=element_text(size=14),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * " effect (%)"))+
        scale_x_discrete(limit=c(mod.list, "multi-model","obs"),
                         label=c(model.labels, "multi-model" = expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
      scale_fill_manual(name="Model",
                        values=c(col.values, 
                                 "multi-model"="grey30",
                                 "obs"="grey"),
                        labels=c(model.labels, "obs"= "OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "multi-model"="grey30", "obs"="grey"))),
             color = guide_legend(nrow=12, byrow=F)); p2
    
    
    
    ##################################################################################
    ### delta P pools
    vegDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                          ambDF=ambDF.sum,
                                                                          eleDF=eleDF.sum,
                                                                          difDF=annDF.diff.sum,
                                                                          var.list=c("deltaPL", "deltaPW", "deltaPFR", "deltaPCR", "deltaPSTOR"),
                                                                          calculate.total=T)
    
    
    
    
    ### split into ambDF, pctDF
    plotDF1 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("deltaPL","deltaPW","deltaPCR","deltaPFR","deltaPSTOR"),]
    plotDF2 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("Tot"),]
    
    plotDF3 <- vegDF[vegDF$Trt=="diff"&vegDF$Variable%in%c("Tot"),]
    plotDF4 <- vegDF[vegDF$Trt=="diff"&vegDF$Variable%in%c("deltaPL","deltaPW","deltaPFR","deltaPSTOR"),]
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    plotDF2$sdvalue[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                       "C_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF4[plotDF4$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF4 <- rbind(plotDF4, tmpDF2)
    
    
    
    ### adding to bp DF
    plotDF5 <- plotDF3[,c("Group", "meanvalue", "sdvalue")]
    colnames(plotDF5) <- c("Group", "deltaPVEG_mean", "deltaPVEG_sd")
    bpDF <- merge(bpDF, plotDF5, by=c("Group"))
    
    
    ### Plotting
    ### additional to-do list:
    ### 1. fill color by manual selection
    p3 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
      geom_bar(stat = "identity", aes(fill=Variable), 
               position="stack", col="black") +
      geom_errorbar(data=plotDF2, 
                    aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", 
                    position=position_dodge2(), width=0.3)+
      #geom_point(data=plotDF2, aes(x=Group, y=meanvalue), col="black",
      #           fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      xlab("")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            #axis.text.x=element_text(size=14,angle = 45, 
            #                         vjust = 1, hjust = 1),
            axis.text.x=element_text(size=14),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position=c(0.85,0.2),
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.background = element_rect(fill="grey",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(Delta * " P pools (g P " * m^2* " " * yr^-1 * ")"))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model" = expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))+
      scale_fill_manual(name=expression(P[veg]),
                        values=c("deltaPL"=cbbPalette[2],
                                 "deltaPW"=cbbPalette[3],
                                 "deltaPFR"=cbbPalette[4],
                                 "deltaPCR"=cbbPalette[7],
                                 "deltaPSTOR"=cbbPalette[8]),
                        labels=c("deltaPL"=expression(Delta*P[leaf]), 
                                 "deltaPW"=expression(Delta*P[wood]), 
                                 "deltaPFR"=expression(Delta*P[froot]), 
                                 "deltaPCR"=expression(Delta*P[croot]),
                                 "deltaPSTOR"=expression(Delta*P[store])))+
      guides(fill=guide_legend(nrow=3)); p3
    
    
    p4 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
      geom_bar(stat = "identity", aes(fill=Group),
               position=position_dodge2(), col="black") +
      geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", stat="identity",
                    position=position_dodge2(), width=0.3)+
      #geom_point(data=plotDF3, aes(x=Group, y=meanvalue), col="black",
      #           fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      xlab("")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            #axis.text.x=element_text(size=14,angle = 45, 
            #                         vjust = 1, hjust = 1),
            axis.text.x=element_text(size=14),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.background = element_rect(fill="grey",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(CO[2] * " effect (g P " * m^2* " " * yr^-1 * ")"))+
      scale_x_discrete(limit=c(mod.list, "multi-model","obs"),
                       label=c(model.labels, "multi-model" = expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))+
      scale_fill_manual(name="Model",
                        values=c(col.values, 
                                 "multi-model"="grey30",
                                 "obs"="grey"),
                        labels=c(model.labels, "obs"= "OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, 
                                                             "multi-model"="grey30", 
                                                             "obs"="grey"))),
             color = guide_legend(nrow=12, byrow=F));p4
    
    
    
    pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_P_pools.pdf"), 
        width=16, height=8)
    plot_grid(p1, p2,   
              p3, p4,
              labels=c("(a)", "(b)", "(c)", "(d)"), label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=2)
    dev.off()
    
    
    
    
    
    
    
    
    
    
    ################# Major growth P fluxes  ####################
    pfluxDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                            ambDF=ambDF.sum,
                                                                            eleDF=eleDF.sum,
                                                                            difDF=annDF.diff.sum,
                                                                            var.list=c("PGL", "PGW", "PGCR", "PGFR"),
                                                                            calculate.total=T)
    
    
    ### split into ambDF, pctDF
    plotDF1 <- pfluxDF[pfluxDF$Trt=="aCO2"&pfluxDF$Variable%in%c("PGL", "PGW", "PGCR", "PGFR"),]
    plotDF2 <- pfluxDF[pfluxDF$Trt=="aCO2"&pfluxDF$Variable%in%c("Tot"),]
    
    plotDF3 <- pfluxDF[pfluxDF$Trt=="pct_diff"&pfluxDF$Variable%in%c("Tot"),]
    plotDF4 <- pfluxDF[pfluxDF$Trt=="diff"&pfluxDF$Variable%in%c("Tot"),]
    
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    plotDF2$sdvalue[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                       "C_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF4[plotDF4$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF4 <- rbind(plotDF4, tmpDF2)
    
    
    plotDF5 <- plotDF4[,c("Group", "meanvalue", "sdvalue")]
    colnames(plotDF5) <- c("Group", "PG_mean", "PG_sd")
    bpDF <- merge(bpDF, plotDF5, by=c("Group"))
    
    
    
    
    ### plotting 
    p5 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        geom_errorbar(data=plotDF2,
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        #geom_point(data=plotDF2, aes(x=Group, y=meanvalue), col="black",
        #         fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              #axis.text.x=element_text(size=14,angle = 45, 
              #                         vjust = 1, hjust = 1),
              axis.text.x=element_text(size=14),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position=c(.8,.8),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(P[demand] * " (g P " * m^2 * " " * yr^-1 * ")")))+
        scale_fill_manual(name=expression(P[demand]),
                        values=c("PGL"=cbbPalette[2],
                                 "PGW"=cbbPalette[3],
                                 "PGFR"=cbbPalette[4],
                                 "PGCR"=cbbPalette[7]),
                        labels=c("PGL"=expression(P[leaf]), 
                                 "PGW"=expression(P[wood]), 
                                 "PGFR"=expression(P[froot]), 
                                 "PGCR"=expression(P[croot])))+
      guides(fill=guide_legend(nrow=2))+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS")))); p5
    
    
    p6 <- ggplot(data=plotDF4, 
                 aes(Group, meanvalue)) +
      geom_bar(stat = "identity", aes(fill=Group), 
               position="stack", col="black") +
      geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", 
                    position=position_dodge2(), width=0.3)+
      #geom_point(data=plotDF3, aes(x=Group, y=meanvalue), col="black",
      #         fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      xlab("")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            #axis.text.x=element_text(size=14,angle = 45, 
            #                         vjust = 1, hjust = 1),
            axis.text.x=element_text(size=14),
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
      #ylab(expression(CO[2] * " effect (%)"))+
      ylab(expression(paste(CO[2] * "effect (g P " * m^2 * " " * yr^-1 * ")")))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))+
      scale_fill_manual(name="Model",
                        values=c(col.values, 
                                 "multi-model"="grey30",
                                 "obs"="grey"),
                        labels=c(model.labels, "obs"= "OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "multi-model"="grey30", "obs"="grey"))),
             color = guide_legend(nrow=12, byrow=F)); p6
    
    
    
    
    ################# P uptake, P resorption and P demand ####################
    pfluxDF1 <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                             ambDF=ambDF.sum,
                                                                             eleDF=eleDF.sum,
                                                                             difDF=annDF.diff.sum,
                                                                             var.list=c("PUP"),
                                                                             calculate.total=F)
    
    
    pfluxDF2 <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                            ambDF=ambDF.sum,
                                                                            eleDF=eleDF.sum,
                                                                            difDF=annDF.diff.sum,
                                                                            var.list=c("PGL", "PGW", "PGCR", "PGFR"),
                                                                            calculate.total=T)
    
    
    pfluxDF2 <- pfluxDF2[pfluxDF2$Variable=="Tot",]
    pfluxDF2$Variable <- "Pdemand"
    
    pfluxDF3 <- pfluxDF1
    
    for(i in unique(pfluxDF3$Group)) {
      for(j in unique(pfluxDF3$Trt)) {
        pfluxDF3$meanvalue[pfluxDF3$Group==i&pfluxDF3$Trt==j] <- 
          pfluxDF2$meanvalue[pfluxDF2$Group==i&pfluxDF2$Trt==j] - pfluxDF1$meanvalue[pfluxDF1$Group==i&pfluxDF1$Trt==j] 
      }
    }
    
    pfluxDF3$Variable <- "Presorb"
    
    
    pfluxDF <- rbind(pfluxDF1, pfluxDF3)
    
    ### split into ambDF, pctDF
    plotDF1 <- pfluxDF[pfluxDF$Trt=="aCO2"&pfluxDF$Variable%in%c("Presorb", "PUP"),]

    plotDF2 <- pfluxDF2[pfluxDF2$Trt=="aCO2"&pfluxDF2$Variable%in%c("Pdemand"),]
    
    #plotDF3 <- pfluxDF2[pfluxDF2$Trt=="diff"&pfluxDF2$Variable%in%c("Pdemand"),]
    plotDF3 <- pfluxDF2[pfluxDF2$Trt=="pct_diff"&pfluxDF2$Variable%in%c("Pdemand"),]
    
    
    plotDF4 <- pfluxDF[pfluxDF$Trt=="diff"&pfluxDF$Variable%in%c("Presorb", "PUP"),]
    
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF4[plotDF4$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF4 <- rbind(plotDF4, tmpDF2)
    
    
    plotDF2$sdvalue[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                       "C_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    tmpDF1 <- plotDF4[plotDF4$Variable=="PUP",c("Group", "meanvalue", "sdvalue")]
    tmpDF2 <- plotDF4[plotDF4$Variable=="Presorb",c("Group", "meanvalue", "sdvalue")]
    
    colnames(tmpDF1) <- c("Group", "PUP_mean", "PUP_sd")
    colnames(tmpDF2) <- c("Group", "Presorb_mean", "Presorb_sd")
    
    bpDF <- merge(bpDF, tmpDF1, by=c("Group"))
    bpDF <- merge(bpDF, tmpDF2, by=c("Group"))
    
    
    ### plotting 
    p7 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
      geom_bar(stat = "identity", aes(fill=Variable), 
               position="stack", col="black") +
      geom_errorbar(data=plotDF2,
                    aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", 
                    position=position_dodge2(), width=0.3)+
      #geom_point(data=plotDF2, aes(x=Group, y=meanvalue), col="black",
      #         fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      xlab("")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            #axis.text.x=element_text(size=14,angle = 45, 
            #                         vjust = 1, hjust = 1),
            axis.text.x=element_text(size=14),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position=c(.7,.8),
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.background = element_rect(fill="grey",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(P[demand] * " (g C " * m^2 * " " * yr^-1 * ")")))+
      scale_fill_manual(name=expression(P[demand]),
                        values=c("PUP"=Diverge_hsv_Palette[2],
                                 "Presorb"=Diverge_hsv_Palette[8]),
                        labels=c("PUP"=expression(P[upt]), 
                                 "Presorb"=expression(P[res])))+
      guides(fill=guide_legend(nrow=2))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS")))); p7
    
    
    
    
    #p6 <- ggplot(data=plotDF3, 
    #             aes(Group, meanvalue)) +
    #  geom_bar(stat = "identity", aes(fill=Group), 
    #           position="stack", col="black") +
    #  geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
    #                    ymax=meanvalue+sdvalue), 
    #                col="black", 
    #                position=position_dodge2(), width=0.3)+
    #  #geom_point(data=plotDF3, aes(x=Group, y=meanvalue), col="black",
    #  #         fill="white", size=2, pch=21)+
    #  geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
    #  xlab("")+
    #  theme_linedraw() +
    #  theme(panel.grid.minor=element_blank(),
    #        axis.text.x=element_text(size=12),
    #        axis.title.x=element_text(size=14),
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
    #  ylab(expression(CO[2] * " effect (%)"))+
    #  scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
    #                   label=c(model.labels, "multi-model"=expression(bold("M-M")),
    #                           "obs" = expression(bold("OBS"))))+
    #  scale_fill_manual(name="Model",
    #                    values=c(col.values, 
    #                             "multi-model"="grey30",
    #                             "obs"="grey"),
    #                    labels=c(model.labels, "obs"= "OBS"))+
    #  guides(fill = guide_legend(override.aes = list(col = c(col.values, "multi-model"="grey30", "obs"="grey"))),
    #         color = guide_legend(nrow=12, byrow=F)); p6
    
    
    p8 <- ggplot(data=plotDF4, 
                 aes(Group, meanvalue, group=Variable)) +
      geom_bar(stat = "identity", aes(fill=Variable), 
               position=position_dodge2(), col="black") +
      geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue, group=Variable), 
                    col="black", 
                    position=position_dodge2(), width=0.9)+
      #geom_point(data=plotDF4, aes(x=Group, y=meanvalue), 
      #           position=position_dodge2(width=0.9), col="black",
      #           fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      xlab("")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            #axis.text.x=element_text(size=14,angle = 45, 
            #                         vjust = 1, hjust = 1),
            axis.text.x=element_text(size=14),
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
      ylab(expression(CO[2] * " effect (g P " * m^-2 * " " * yr^-1 * ")"))+
      scale_fill_manual(name=expression(P[demand]),
                        values=c("PUP"=Diverge_hsv_Palette[2],
                                 "Presorb"=Diverge_hsv_Palette[8]),
                        labels=c("PUP"=expression(P[upt]), 
                                 "Presorb"=expression(P[res])))+
      guides(fill=guide_legend(nrow=2))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))));p8
    
    
    
    
    pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_P_demand_and_uptake.pdf"), 
        width=16, height=16)
    plot_grid(p1, p2, 
              p3, p4,
              p5, p6,   
              p7, p8,
              labels=c("(a)", "(b)", "(c)", "(d)",
                       "(e)", "(f)", "(g)", "(h)"), label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=2)
    dev.off()
    
    
    
    
    
    
    ################# P uptake and mineralization ####################
    pfluxDF1 <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                             ambDF=ambDF.sum,
                                                                             eleDF=eleDF.sum,
                                                                             difDF=annDF.diff.sum,
                                                                             var.list=c("PUP"),
                                                                             calculate.total=F)
    
    
    pfluxDF2 <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                             ambDF=ambDF.sum,
                                                                             eleDF=eleDF.sum,
                                                                             difDF=annDF.diff.sum,
                                                                             var.list=c("PMIN", "PBIOCHMIN"),
                                                                             calculate.total=T)
    
    
    pfluxDF2 <- pfluxDF2[pfluxDF2$Variable=="Tot",]
    pfluxDF2$Variable <- "PMIN"
    
    #pfluxDF <- rbind(pfluxDF1, pfluxDF2)
    pfluxDF <- pfluxDF2
    
    ### split into ambDF, pctDF
    plotDF1 <- pfluxDF[pfluxDF$Trt=="aCO2",]
    plotDF2 <- pfluxDF[pfluxDF$Trt=="aCO2",]
    
    plotDF3 <- pfluxDF[pfluxDF$Trt=="pct_diff",]
    plotDF4 <- pfluxDF[pfluxDF$Trt=="diff",]
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    plotDF2$sdvalue[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                       "C_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    

    ### add multi-model mean
    tmpDF <- plotDF4[plotDF4$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF4 <- rbind(plotDF4, tmpDF2)
    
    
    
    plotDF5 <- plotDF4[,c("Group", "meanvalue", "sdvalue")]
    colnames(plotDF5) <- c("Group", "PMIN_mean", "PMIN_sd")
    bpDF <- merge(bpDF, plotDF5, by=c("Group"))
    
    
    
    ### plotting 
    p9 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue, group=Variable)) +
      geom_bar(stat = "identity", aes(fill=Group), 
               position=position_dodge2(), col="black") +
      geom_errorbar(data=plotDF2,
                    aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", 
                    position=position_dodge2(), width=0.9)+
      #geom_point(data=plotDF2, aes(x=Group, y=meanvalue), 
      #           position=position_dodge2(width=0.9), col="black",
      #         fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      xlab("")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            #axis.text.x=element_text(size=14,angle = 45, 
            #                         vjust = 1, hjust = 1),
            axis.text.x=element_text(size=14),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.background = element_rect(fill="grey",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(P[net] * " (g P " * m^2 * " " * yr^-1 * ")"))+
      scale_fill_manual(name="Model",
                        values=c(col.values, 
                                 "multi-model"="grey30",
                                 "obs"="grey"),
                        labels=c(model.labels, "obs"= "OBS"))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS")))); p9
    
    
    p10 <- ggplot(data=plotDF3, 
                  aes(Group, meanvalue, group=Variable)) +
      geom_bar(stat = "identity", aes(fill=Group), 
               position=position_dodge2(), col="black") +
      geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue, group=Variable), 
                    col="black", 
                    position=position_dodge2(), width=0.9)+
      #geom_point(data=plotDF3, aes(x=Group, y=meanvalue), 
      #         position=position_dodge2(width=0.9), col="black",
      #         fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      xlab("")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            #axis.text.x=element_text(size=14,angle = 45, 
            #                         vjust = 1, hjust = 1),
            axis.text.x=element_text(size=14),
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
      ylab(expression(CO[2] * " effect (%)"))+
      scale_fill_manual(name="Model",
                        values=c(col.values, 
                                 "multi-model"="grey30",
                                 "obs"="grey"),
                        labels=c(model.labels, "obs"= "OBS"))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))));p10
    

    
    
    #p9 <- ggplot(data=plotDF1, 
    #             aes(Group, meanvalue, group=Variable)) +
    #  geom_bar(stat = "identity", aes(fill=Variable), 
    #           position=position_dodge2(), col="black") +
    #  geom_errorbar(data=plotDF2,
    #                aes(x=Group, ymin=meanvalue-sdvalue,
    #                    ymax=meanvalue+sdvalue), 
    #                col="black", 
    #                position=position_dodge2(), width=0.9)+
    #  #geom_point(data=plotDF2, aes(x=Group, y=meanvalue), 
    #  #           position=position_dodge2(width=0.9), col="black",
    #  #         fill="white", size=2, pch=21)+
    #  geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
    #  xlab("")+
    #  theme_linedraw() +
    #  theme(panel.grid.minor=element_blank(),
    #        #axis.text.x=element_text(size=14,angle = 45, 
    #        #                         vjust = 1, hjust = 1),
    #        axis.text.x=element_text(size=14),
    #        axis.title.x=element_text(size=14),
    #        axis.text.y=element_text(size=12),
    #        axis.title.y=element_text(size=14),
    #        legend.text=element_text(size=12),
    #        legend.title=element_text(size=14),
    #        panel.grid.major=element_blank(),
    #        legend.position=c(.1,.7),
    #        legend.box = 'horizontal',
    #        legend.box.just = 'left',
    #        legend.background = element_rect(fill="grey",
    #                                         size=0.5, linetype="solid", 
    #                                         colour ="black"),
    #        plot.title = element_text(size=14, face="bold.italic", 
    #                                  hjust = 0.5))+
    #  ylab(expression(paste("P fluxes (g C " * m^2 * " " * yr^-1 * ")")))+
    #  scale_fill_manual(name=expression(P[fluxes]),
    #                    values=c("PMIN"=Diverge_hsv_Palette[4],
    #                             "PUP"=Diverge_hsv_Palette[2]),
    #                    labels=c("PMIN"=expression(P[net]), 
    #                             "PUP"=expression(P[upt])))+
    #  scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
    #                   label=c(model.labels, "multi-model"=expression(bold("M-M")),
    #                           "obs" = expression(bold("OBS")))); p9
    #
    #
    #p10 <- ggplot(data=plotDF3, 
    #              aes(Group, meanvalue, group=Variable)) +
    #  geom_bar(stat = "identity", aes(fill=Variable), 
    #           position=position_dodge2(), col="black") +
    #  geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
    #                    ymax=meanvalue+sdvalue, grou=Variable), 
    #                col="black", 
    #                position=position_dodge2(), width=0.9)+
    #  #geom_point(data=plotDF3, aes(x=Group, y=meanvalue), 
    #  #         position=position_dodge2(width=0.9), col="black",
    #  #         fill="white", size=2, pch=21)+
    #  geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
    #  xlab("")+
    #  theme_linedraw() +
    #  theme(panel.grid.minor=element_blank(),
    #        #axis.text.x=element_text(size=14,angle = 45, 
    #        #                         vjust = 1, hjust = 1),
    #        axis.text.x=element_text(size=14),
    #        axis.title.x=element_text(size=14),
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
    #  ylab(expression(CO[2] * " effect (%)"))+
    #  #scale_y_break(c(-400, -30, 50, 410))+
    #  scale_fill_manual(name=expression(P[fluxes]),
    #                    values=c("PMIN"=Diverge_hsv_Palette[4],
    #                             "PUP"=Diverge_hsv_Palette[2]),
    #                    labels=c("PMIN"=expression(P[net]), 
    #                             "PUP"=expression(P[upt])))+
    #  #ylim(c(-30, 50))+
    #  scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
    #                   label=c(model.labels, "multi-model"=expression(bold("M-M")),
    #                           "obs" = expression(bold("OBS"))));p10
    
    
    
    ################## PUE ####################
    budgetDF <- prepare_P_budget_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                                 ambDF=ambDF.sum,
                                                                                 eleDF=eleDF.sum,
                                                                                 difDF=annDF.diff.sum)
    
    
    ### add GPP use efficiency - model output and observations
    tmpDF1 <- data.frame("Variable"=rep("GPP_PUE", 36),
                         "Group"=rep(c("obs","A_GDAYP",
                                       "B_ELMV1", "C_CABLP",
                                       "D_LPJGP", "E_OCHDP",
                                       "F_QUINC", "G_OCHDX",
                                       "H_QUJSM"), each=4),
                         "Trt"=rep(c("aCO2", "eCO2", "diff", "pct_diff"), 9),
                         "meanvalue"=NA,
                         "sdvalue"=NA)
    
    tmpDF1$meanvalue[tmpDF1$Group=="obs"&tmpDF1$Trt=="aCO2"] <- eucDF$GPP_use[eucDF$Group=="mean"&eucDF$Trt=="aCO2"]
    tmpDF1$meanvalue[tmpDF1$Group=="obs"&tmpDF1$Trt=="eCO2"] <- eucDF$GPP_use[eucDF$Group=="mean"&eucDF$Trt=="eCO2"]
    
    tmpDF1$sdvalue[tmpDF1$Group=="obs"&tmpDF1$Trt=="aCO2"] <- eucDF$GPP_use[eucDF$Group=="sd"&eucDF$Trt=="aCO2"]
    tmpDF1$sdvalue[tmpDF1$Group=="obs"&tmpDF1$Trt=="eCO2"] <- eucDF$GPP_use[eucDF$Group=="sd"&eucDF$Trt=="eCO2"]
    
    tmpDF1$meanvalue[tmpDF1$Group=="obs"&tmpDF1$Trt=="diff"] <- eucDF$GPP_use[eucDF$Group=="mean"&eucDF$Trt=="diff"]
    tmpDF1$meanvalue[tmpDF1$Group=="obs"&tmpDF1$Trt=="pct_diff"] <- eucDF$GPP_use[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    
    tmpDF1$sdvalue[tmpDF1$Group=="obs"&tmpDF1$Trt=="diff"] <- eucDF$GPP_use[eucDF$Group=="sd"&eucDF$Trt=="diff"]
    tmpDF1$sdvalue[tmpDF1$Group=="obs"&tmpDF1$Trt=="pct_diff"] <- eucDF$GPP_use[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    
    ### calculate model GPP_use
    ambDF.sum$GPP_use.mean <- with(ambDF.sum, GPP.mean/PGL.mean)
    eleDF.sum$GPP_use.mean <- with(eleDF.sum, GPP.mean/PGL.mean)
    annDF.diff.sum$GPP_use.mean <- with(annDF.diff.sum, GPP.mean/PGL.mean)
    
    
    ### assign model values
    for (i in mod.list) {
      tmpDF1$meanvalue[tmpDF1$Group==i&tmpDF1$Trt=="aCO2"] <- ambDF.sum$GPP_use.mean[ambDF.sum$ModName==i]
      tmpDF1$meanvalue[tmpDF1$Group==i&tmpDF1$Trt=="eCO2"] <- eleDF.sum$GPP_use.mean[eleDF.sum$ModName==i]
      tmpDF1$meanvalue[tmpDF1$Group==i&tmpDF1$Trt=="diff"] <- annDF.diff.sum$GPP_use.mean[annDF.diff.sum$ModName==i]

    }
    
    budgetDF <- rbind(budgetDF, tmpDF1)
    
    
    ### select variables, PUE and GPP_PUE
    subDF2 <- subset(budgetDF, Variable%in%c("PUE", "GPP_PUE"))
    
    
    plotDF1 <- subDF2[subDF2$Trt=="aCO2",]
    plotDF2 <- subDF2[subDF2$Trt=="diff",]
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    
    ### add into BP DF
    tmpDF1 <- plotDF2[plotDF2$Variable=="PUE",c("Group", "meanvalue", "sdvalue")]
    tmpDF2 <- plotDF2[plotDF2$Variable=="GPP_PUE",c("Group", "meanvalue", "sdvalue")]
    
    colnames(tmpDF1) <- c("Group", "PUE_mean", "PUE_sd")
    colnames(tmpDF2) <- c("Group", "GPP_PUE_mean", "GPP_PUE_sd")
    
    bpDF <- merge(bpDF, tmpDF1, by=c("Group"))
    bpDF <- merge(bpDF, tmpDF2, by=c("Group"))
    
    
    
    
    ## PUE BP and GPP
    p11 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue, group=Variable)) +
      geom_bar(stat = "identity", aes(fill=Variable), 
               position="dodge", col="black") +
      geom_errorbar(data=plotDF1,
                    aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", width=0.2,
                    position=position_dodge(width=1))+
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            #axis.text.x=element_text(size=14,angle = 45, 
            #                         vjust = 1, hjust = 1),
            axis.text.x=element_text(size=14),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position=c(.70,.8),
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.background = element_rect(fill="grey",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression("PUE (g " * C^-1 * " g " * P^-1 * ")"))+
      xlab("")+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))+
      scale_fill_manual(name="Variable",
                        values=c("PUE"=cbbPalette[2], 
                                 "GPP_PUE"=cbbPalette[6]),
                        label=c("PUE"=expression(PUE[BP]),
                                "GPP_PUE"=expression(PUE[GPP])))+
      guides(fill=guide_legend(nrow=2)); p11
    
    
    
    
    p12 <- ggplot(data=plotDF2, 
                  aes(Group, meanvalue)) +
      geom_bar(stat = "identity", aes(fill=Variable), 
               position=position_dodge2(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      xlab("")+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            #axis.text.x=element_text(size=14,angle = 45, 
            #                         vjust = 1, hjust = 1),
            axis.text.x=element_text(size=14),
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
      ylab(expression(CO[2] * " effect (ele - amb)"))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))+
      scale_fill_manual(name="Variable",
                        values=c("PUE"=cbbPalette[2], 
                                 "GPP_PUE"=cbbPalette[6]),
                        label=c("PUE"=expression(PUE[BP]),
                                "GPP_PUE"=expression(PUE[GPP])))+
      guides(fill=guide_legend(nrow=2)); p12
    
    
    
    
    
    ################# CP ratios ####################
    subDF2 <- subset(budgetDF, Variable%in%c("CPL", "CPW", "CPFR"#, "CPFLIT", 
                                             #"CPSOIL"
                                             ))
    
    
    plotDF1 <- subDF2[subDF2$Trt=="aCO2",]
    plotDF2 <- subDF2[subDF2$Trt=="pct_diff",]
    plotDF3 <- subDF2[subDF2$Trt=="diff",]
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    plotDF2$sdvalue[plotDF2$Group%in%c("A_GDAYP", "B_ELMV1",
                                       "C_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("A_GDAYP", "B_ELMV1",
                                        "C_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    

    ### add into BP DF
    tmpDF1 <- plotDF3[plotDF3$Variable=="CPL",c("Group", "meanvalue", "sdvalue")]
    tmpDF2 <- plotDF3[plotDF3$Variable=="CPW",c("Group", "meanvalue", "sdvalue")]
    tmpDF3 <- plotDF3[plotDF3$Variable=="CPFR",c("Group", "meanvalue", "sdvalue")]
    
    colnames(tmpDF1) <- c("Group", "CPL_mean", "CPL_sd")
    colnames(tmpDF2) <- c("Group", "CPW_mean", "CPW_sd")
    colnames(tmpDF3) <- c("Group", "CPFR_mean", "CPFR_sd")
    
    bpDF <- merge(bpDF, tmpDF1, by=c("Group"))
    bpDF <- merge(bpDF, tmpDF2, by=c("Group"))
    bpDF <- merge(bpDF, tmpDF3, by=c("Group"))
    
    
    
    
    
    
    
    
    
    
    ## stoichiometry
    p13 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue, group=Variable)) +
      geom_bar(stat = "identity", aes(fill=Variable), 
               position="dodge", col="black") +
      geom_errorbar(data=plotDF1,
                    aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", width=0.2,
                    position=position_dodge(width=1))+
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            #axis.text.x=element_text(size=14,angle = 45, 
            #                         vjust = 1, hjust = 1),
            axis.text.x=element_text(size=14),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position=c(.75,.8),
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.background = element_rect(fill="grey",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab("CP stoichiometry")+
      xlab("")+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))+
      scale_fill_manual(name="Variable",
                        values=c("CPL"=cbbPalette[4], 
                                 "CPW"=cbbPalette[3],
                                 "CPFR"=cbbPalette[8],
                                 "CPFLIT"=cbbPalette[2],
                                 "CPSOIL"=cbbPalette[6]),
                        label=c("CPL"=expression(CP[leaf]),
                                "CPW"=expression(CP[wood]),
                                "CPFR"=expression(CP[froot]),
                                "CPFLIT"=expression(CP[flit]),
                                "CPSOIL"=expression(CP[soil])))+
      guides(fill=guide_legend(nrow=2)); p13
    
    
    
    
    p14 <- ggplot(data=plotDF2, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position=position_dodge2(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              #axis.text.x=element_text(size=14,angle = 45, 
              #                         vjust = 1, hjust = 1),
              axis.text.x=element_text(size=14),
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
        ylab(expression(CO[2] * " effect (%)"))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Variable",
                          values=c("CPL"=cbbPalette[4], 
                                   "CPW"=cbbPalette[3],
                                   "CPFR"=cbbPalette[8],
                                   "CPFLIT"=cbbPalette[2],
                                   "CPSOIL"=cbbPalette[6]),
                          label=c("CPL"=expression(CP[leaf]),
                                  "CPW"=expression(CP[wood]),
                                  "CPFR"=expression(CP[froot]),
                                  "CPFLIT"=expression(CP[flit]),
                                  "CPSOIL"=expression(CP[soil])))+
        guides(fill=guide_legend(nrow=2)); p14
    
    
    
    
    
    
    ###########################################################################
    
    pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_P_use_variables.pdf"), 
        width=16, height=20)
    plot_grid(p11, p12, # PUE
              p13, p14, # cp ratios
              p7, p8,   # p uptake and resorption
              p9, p10,  # Puptake and P min 
              labels=c("(a)", "(b)", "(c)", "(d)",
                       "(e)", "(f)", "(g)", "(h)"), label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=2)
    dev.off()
    
    
    
    ###########################################################################
    
    ### use CO2 effect of CP flexibility / Pdem / Pupt / P net / delta Pveg to explain 
    ### the CO2 effect on BP and NEP
    
    p1 <- ggplot() +
      geom_segment(aes(x=bpDF$BP_mean, xend = bpDF$BP_mean,
                       y=bpDF$deltaPVEG_mean+bpDF$deltaPVEG_sd, 
                       yend=bpDF$deltaPVEG_mean-bpDF$deltaPVEG_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$BP_mean+bpDF$BP_sd, 
                       xend = bpDF$BP_mean-bpDF$BP_sd,
                       y=bpDF$deltaPVEG_mean, 
                       yend=bpDF$deltaPVEG_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(BP_mean, deltaPVEG_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="grey", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("A_GDAYP"=19,"B_ELMV1"=19,
                                  "C_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15))+
      xlab(expression(paste(CO[2] * " effect on BP (g C " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * Delta * P[veg] * " ( g P " * m^-2 * " " * yr^-1 * ")"))); p1
    
    
    
    p2 <- ggplot() +
      geom_segment(aes(x=bpDF$BP_mean, xend = bpDF$BP_mean,
                       y=bpDF$PG_mean+bpDF$PG_sd, 
                       yend=bpDF$PG_mean-bpDF$PG_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$BP_mean+bpDF$BP_sd, 
                       xend = bpDF$BP_mean-bpDF$BP_sd,
                       y=bpDF$PG_mean, 
                       yend=bpDF$PG_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(BP_mean, PG_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="grey", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("A_GDAYP"=19,"B_ELMV1"=19,
                                  "C_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15))+
      xlab(expression(paste(CO[2] * " effect on BP (g C " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * P[dem] * " ( g P " * m^-2 * " " * yr^-1 * ")"))); p2
    
    
    
    p3 <- ggplot() +
      geom_segment(aes(x=bpDF$BP_mean, xend = bpDF$BP_mean,
                       y=bpDF$PUP_mean+bpDF$PUP_sd, 
                       yend=bpDF$PUP_mean-bpDF$PUP_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$BP_mean+bpDF$BP_sd, 
                       xend = bpDF$BP_mean-bpDF$BP_sd,
                       y=bpDF$PUP_mean, 
                       yend=bpDF$PUP_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(BP_mean, PUP_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="grey", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("A_GDAYP"=19,"B_ELMV1"=19,
                                  "C_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15))+
      xlab(expression(paste(CO[2] * " effect on BP (g C " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * P[up] * " ( g P " * m^-2 * " " * yr^-1 * ")"))); p3
    
    
    
    
    p4 <- ggplot() +
      geom_segment(aes(x=bpDF$Presorb_mean, xend = bpDF$Presorb_mean,
                       y=bpDF$PUP_mean+bpDF$PUP_sd, 
                       yend=bpDF$PUP_mean-bpDF$PUP_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$Presorb_mean+bpDF$Presorb_sd, 
                       xend = bpDF$Presorb_mean-bpDF$Presorb_sd,
                       y=bpDF$PUP_mean, 
                       yend=bpDF$PUP_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(Presorb_mean, PUP_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="grey", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("A_GDAYP"=19,"B_ELMV1"=19,
                                  "C_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15))+
      xlab(expression(paste(CO[2] * " effect on " * P[up] * " ( g P " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * P[res] * " ( g P " * m^-2 * " " * yr^-1 * ")"))); p4
    
    
    
    
    p5 <- ggplot() +
      geom_segment(aes(x=bpDF$PUP_mean, xend = bpDF$PUP_mean,
                       y=bpDF$PMIN_mean+bpDF$PMIN_sd, 
                       yend=bpDF$PMIN_mean-bpDF$PMIN_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$PUP_mean+bpDF$PUP_sd, 
                       xend = bpDF$PUP_mean-bpDF$PUP_sd,
                       y=bpDF$PMIN_mean, 
                       yend=bpDF$PMIN_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(PUP_mean, PMIN_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="grey", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("A_GDAYP"=19,"B_ELMV1"=19,
                                  "C_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15))+
      xlab(expression(paste(CO[2] * " effect on " * P[up] * " ( g P " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * P[net] * " ( g P " * m^-2 * " " * yr^-1 * ")"))); p5
    
    
    
    ###########################################################################
    
    pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_P_use_variables2.pdf"), 
        width=16, height=8)
    plot_grid(p1, p2, p3, p4, p5,NA,
              labels=c("(a)", "(b)", "(c)", "(d)",
                       "(e)", "(f)"), label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=3)
    dev.off()
   
    
}


