plot_plant_p_cycle_responses <- function(eucDF,
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
    tmpDF <- plotDF1[plotDF1$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    
    plotDF2$sdvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                       "B_CABLP", "D_LPJGP",
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
    tmpDF <- plotDF1[plotDF1$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    
    plotDF2$sdvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                       "B_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### adding to bp DF
    tmpDF <- plotDF5[plotDF5$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
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
        geom_vline(xintercept=c(6.5, 8.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position=c(0.64,0.2),
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
                        values=c("PL"="#FF6F91",
                                 "PW"="#FFC75F",
                                 "PFR"="#D65DB1",
                                 "PCR"="#845EC2",
                                 "PSTOR"="#FF9671"),
                        labels=c("PL"=expression(P[leaf]), 
                                 "PW"=expression(P[wood]), 
                                 "PFR"=expression(P[froot]), 
                                 "PCR"=expression(P[croot]),
                                 "PSTOR"=expression(P[store])))+
        guides(fill=guide_legend(nrow=3))
    
    
    p2 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group),
                 position=position_dodge2(), col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", stat="identity",
                      position=position_dodge2(), width=0.3)+
        geom_vline(xintercept=c(6.5, 8.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
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
             color = guide_legend(nrow=12, byrow=F))+
      ylim(-10, 10)
    
    
    
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
    tmpDF <- plotDF1[plotDF1$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    plotDF2$sdvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                       "B_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF4[plotDF4$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
            legend.position=c(0.18,0.2),
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
                        values=c("deltaPL"="#FF6F91",
                                 "deltaPW"="#FFC75F",
                                 "deltaPFR"="#D65DB1",
                                 "deltaPCR"="#845EC2",
                                 "deltaPSTOR"="#FF9671"),
                        labels=c("deltaPL"=expression(Delta*P[leaf]), 
                                 "deltaPW"=expression(Delta*P[wood]), 
                                 "deltaPFR"=expression(Delta*P[froot]), 
                                 "deltaPCR"=expression(Delta*P[croot]),
                                 "deltaPSTOR"=expression(Delta*P[store])))+
      guides(fill=guide_legend(nrow=3))
    
    
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
             color = guide_legend(nrow=12, byrow=F))
    
    
    
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
    
    plotDF42 <- pfluxDF[pfluxDF$Trt=="diff"&pfluxDF$Variable%in%c("PGL", "PGW", "PGCR", "PGFR"),]
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    plotDF2$sdvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                       "B_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF4[plotDF4$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF4 <- rbind(plotDF4, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF42[plotDF42$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF42 <- rbind(plotDF42, tmpDF2)
    
    
    plotDF5 <- plotDF4[,c("Group", "meanvalue", "sdvalue")]
    colnames(plotDF5) <- c("Group", "PG_mean", "PG_sd")
    bpDF <- merge(bpDF, plotDF5, by=c("Group"))
    
    
    plotDF5 <- plotDF3[,c("Group", "meanvalue", "sdvalue")]
    colnames(plotDF5) <- c("Group", "PG_pct_mean", "PG_pct_sd")
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
        geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
              legend.position=c(.64,.8),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(P[dem] * " (g P " * m^2 * " " * yr^-1 * ")")))+
        scale_fill_manual(name=expression(P[demand]),
                          values=c("PGL"="#FF6F91",
                                   "PGW"="#FFC75F",
                                   "PGFR"="#D65DB1",
                                   "PGCR"="#845EC2"),
                        labels=c("PGL"=expression(P[leaf]), 
                                 "PGW"=expression(P[wood]), 
                                 "PGFR"=expression(P[froot]), 
                                 "PGCR"=expression(P[croot])))+
      guides(fill=guide_legend(nrow=2))+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))
    
    
    p6 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
      geom_bar(stat = "identity", aes(fill=Group), 
               position="stack", col="black") +
      geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", 
                    position=position_dodge2(), width=0.3)+
      #geom_point(data=plotDF3, aes(x=Group, y=meanvalue), col="black",
      #         fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
      ylab(expression(paste(CO[2] * "effect (%)")))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))+
      scale_fill_manual(name="Model",
                        values=c(col.values, 
                                 "multi-model"="grey30",
                                 "obs"="grey"),
                        labels=c(model.labels, "obs"= "OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "multi-model"="grey30", "obs"="grey"))),
             color = guide_legend(nrow=12, byrow=F))
    
    
    
    
    p62 <- ggplot(data=plotDF42, 
                  aes(Group, meanvalue, group=Variable)) +
      geom_bar(stat = "identity", aes(fill=Variable), 
               position="stack", col="black") +
      geom_errorbar(data=plotDF4,
                    aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", 
                    position=position_dodge2(), width=0.3)+
      geom_point(data=plotDF4, aes(x=Group, y=meanvalue), 
                 position=position_dodge2(width=0.9), col="black",
                 fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
            legend.background = element_rect(fill="grey",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(CO[2] * " effect (g P " * m^-2 * " " * yr^-1 * ")"))+
      scale_fill_manual(name=expression(P[demand]),
                        values=c("PGL"="#FF6F91",
                                 "PGW"="#FFC75F",
                                 "PGFR"="#D65DB1",
                                 "PGCR"="#845EC2"),
                        labels=c("PGL"=expression(P[leaf]), 
                                 "PGW"=expression(P[wood]), 
                                 "PGFR"=expression(P[froot]), 
                                 "PGCR"=expression(P[croot])))+
      guides(fill=guide_legend(nrow=2))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))
    
    
    
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
    
    #pfluxDF3 <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
    #                                                                         ambDF=ambDF.sum,
    #                                                                         eleDF=eleDF.sum,
    #                                                                         difDF=annDF.diff.sum,
    #                                                                         var.list=c("deltaPSTOR"),
    #                                                                         calculate.total=F)
    
    #pfluxDF3 <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
    #                                                                         ambDF=ambDF.sum,
    #                                                                         eleDF=eleDF.sum,
    #                                                                         difDF=annDF.diff.sum,
    #                                                                         var.list=c("PLRETR", "PWRETR", "PFRRETR", "PCRRETR"),
    #                                                                         calculate.total=T)
    
    
    
    pfluxDF2 <- pfluxDF2[pfluxDF2$Variable=="Tot",]
    pfluxDF2$Variable <- "Pdemand"
    
    ### calculate PRETR
    tmpDF1 <- pfluxDF1[pfluxDF1$Trt%in%c("aCO2", "eCO2"),]
    tmpDF2 <- pfluxDF2[pfluxDF2$Trt%in%c("aCO2", "eCO2"),]
    
    
    pfluxDF3 <- pfluxDF1
    pfluxDF3$Variable <- "PRETR"
    
    for(i in unique(tmpDF1$Group)) {
      for(j in unique(tmpDF1$Trt)) {
        pfluxDF3$meanvalue[pfluxDF3$Group==i&pfluxDF3$Trt==j] <- tmpDF2$meanvalue[tmpDF2$Group==i&tmpDF2$Trt==j] - tmpDF1$meanvalue[tmpDF1$Group==i&tmpDF1$Trt==j] 
        
        pfluxDF3$sdvalue[pfluxDF3$Group==i&pfluxDF3$Trt==j] <- 
          sqrt(sum(c(tmpDF2$sdvalue[tmpDF2$Group==i&tmpDF2$Trt==j]^2, tmpDF1$sdvalue[tmpDF1$Group==i&tmpDF1$Trt==j]^2), na.rm=T)/2)
      }
    }
    
    for(i in unique(pfluxDF3$Group)) {
      pfluxDF3$meanvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="diff"] <- pfluxDF3$meanvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="eCO2"]-pfluxDF3$meanvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="aCO2"]
      
      pfluxDF3$sdvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="diff"] <- sqrt(sum(c(pfluxDF3$sdvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="eCO2"]^2,
                                                                             pfluxDF3$sdvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="aCO2"]^2),
                                                                           na.rm=T)/2)
      
      pfluxDF3$meanvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="pct_diff"] <- pfluxDF3$meanvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="diff"]/pfluxDF3$meanvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="aCO2"]*100
      
      pfluxDF3$sdvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="pct_diff"] <- sqrt(sum(c(pfluxDF3$sdvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="eCO2"]^2,
                                                                                 pfluxDF3$sdvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="aCO2"]^2,
                                                                                 pfluxDF3$sdvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="aCO2"]^2),
                                                                               na.rm=T)/3)/pfluxDF3$meanvalue[pfluxDF3$Group==i&pfluxDF3$Trt=="aCO2"]*100
      
    }
    

    
    pfluxDF <- rbind(pfluxDF1, pfluxDF3)
    
    ### split into ambDF, pctDF
    plotDF1 <- pfluxDF[pfluxDF$Trt=="aCO2"&pfluxDF$Variable%in%c("PRETR", "PUP"),]

    plotDF2 <- pfluxDF2[pfluxDF2$Trt=="aCO2"&pfluxDF2$Variable%in%c("Pdemand"),]
    
    #plotDF3 <- pfluxDF2[pfluxDF2$Trt=="diff"&pfluxDF2$Variable%in%c("Pdemand"),]
    plotDF3 <- pfluxDF2[pfluxDF2$Trt=="pct_diff"&pfluxDF2$Variable%in%c("Pdemand"),]
    
    
    plotDF4 <- pfluxDF[pfluxDF$Trt=="diff"&pfluxDF$Variable%in%c("PRETR", "PUP"),]
    
    plotDF42 <- pfluxDF[pfluxDF$Trt=="pct_diff"&pfluxDF$Variable%in%c("PRETR", "PUP"),]
    
    
    ### revise sd in the data, based on data
    plotDF1$sdvalue[plotDF1$Variable=="PRETR"&plotDF1$Group=="obs"&plotDF1$Trt=="aCO2"] <- 0.03
    plotDF1$sdvalue[plotDF1$Variable=="PRETR"&plotDF1$Group=="obs"&plotDF1$Trt=="eCO2"] <- 0.097
    
    plotDF4$sdvalue[plotDF4$Variable=="PRETR"&plotDF4$Group=="obs"&plotDF4$Trt=="diff"] <- sqrt((0.03^2+0.097^2)/2)
    #plotDF42$sdvalue[plotDF42$Variable=="PRETR"&plotDF42$Group=="obs"&plotDF42$Trt=="pct_diff"] <- sqrt((0.03^2+0.097^2+0.03^2)/3)/plotDF1$sdvalue[plotDF1$Variable=="PRETR"&plotDF1$Group=="obs"&plotDF1$Trt=="aCO2"]*100
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    plotDF2$sdvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                       "B_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF4[plotDF4$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF4 <- rbind(plotDF4, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF42[plotDF42$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF42 <- rbind(plotDF42, tmpDF2)
    
    
    
    ### add to bp DF
    tmpDF1 <- plotDF4[plotDF4$Variable=="PUP",c("Group", "meanvalue", "sdvalue")]
    tmpDF2 <- plotDF4[plotDF4$Variable=="PRETR",c("Group", "meanvalue", "sdvalue")]
    
    colnames(tmpDF1) <- c("Group", "PUP_mean", "PUP_sd")
    colnames(tmpDF2) <- c("Group", "PRETR_mean", "PRETR_sd")
    
    bpDF <- merge(bpDF, tmpDF1, by=c("Group"))
    bpDF <- merge(bpDF, tmpDF2, by=c("Group"))
    
    
    
    tmpDF1 <- plotDF42[plotDF42$Variable=="PUP",c("Group", "meanvalue", "sdvalue")]
    tmpDF2 <- plotDF42[plotDF42$Variable=="PRETR",c("Group", "meanvalue", "sdvalue")]
    
    colnames(tmpDF1) <- c("Group", "PUP_pct_mean", "PUP_pct_sd")
    colnames(tmpDF2) <- c("Group", "PRETR_pct_mean", "PRETR_pct_sd")
    
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
      ylab(expression(paste(P[dem] * " (g C " * m^2 * " " * yr^-1 * ")")))+
      scale_fill_manual(name=expression(P[demand]),
                        values=c("PUP"=Diverge_hsv_Palette[2],
                                 "PRETR"=Diverge_hsv_Palette[8]),
                        labels=c("PUP"=expression(P[upt]), 
                                 "PRETR"=expression(P[res])))+
      guides(fill=guide_legend(nrow=2))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))
    
    
    
    plotDF43 <- plotDF42[plotDF42$Group%in%c("multi-model", "obs"),]
    
    p8 <- ggplot(data=plotDF42, 
                 aes(Group, meanvalue, group=Variable)) +
      geom_bar(stat = "identity", aes(fill=Variable), 
               position=position_dodge2(), col="black") +
      geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue, group=Variable), 
                    col="black", 
                    position=position_dodge2(), width=0.9)+
      geom_point(data=plotDF43, aes(x=Group, y=meanvalue), 
                 position=position_dodge2(width=0.9), col="black",
                 fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
            legend.background = element_rect(fill="grey",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      #ylab(expression(CO[2] * " effect (g P " * m^-2 * " " * yr^-1 * ")"))+
      ylab(expression(CO[2] * "(%)"))+
      scale_y_continuous(limits=c(-22, 40),
                         breaks=c(-20, -10, 0, 10, 20, 30))+
      scale_fill_manual(name=expression(P[demand]),
                        values=c("PUP"=Diverge_hsv_Palette[2],
                                 "PRETR"=Diverge_hsv_Palette[8]),
                        labels=c("PUP"=expression(P[upt]), 
                                 "PRETR"=expression(P[res])))+
      guides(fill=guide_legend(nrow=2))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))
    
 
    

    
    
    
    
    
    ################# P uptake and mineralization ####################
    pfluxDF1 <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                             ambDF=ambDF.sum,
                                                                             eleDF=eleDF.sum,
                                                                             difDF=annDF.diff.sum,
                                                                             var.list=c("PUP"),
                                                                             calculate.total=F)
    
    
    
    pfluxDF2 <- pfluxDF1[pfluxDF1$Variable=="PUP",]
    pfluxDF2$Variable <- "PUP"
    
    #pfluxDF <- rbind(pfluxDF1, pfluxDF2)
    pfluxDF <- pfluxDF2
    
    ### split into ambDF, pctDF
    plotDF1 <- pfluxDF[pfluxDF$Trt=="aCO2",]
    plotDF2 <- pfluxDF[pfluxDF$Trt=="aCO2",]
    
    plotDF3 <- pfluxDF[pfluxDF$Trt=="pct_diff",]
    plotDF4 <- pfluxDF[pfluxDF$Trt=="diff",]
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    plotDF2$sdvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                       "B_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF4[plotDF4$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
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
    
    
    
    plotDF5 <- plotDF3[,c("Group", "meanvalue", "sdvalue")]
    colnames(plotDF5) <- c("Group", "PMIN_pct_mean", "PMIN_pct_sd")
    bpDF <- merge(bpDF, plotDF5, by=c("Group"))
    
    
    
    ### plotting 
    p7 <- ggplot(data=plotDF1, 
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
      ylab(expression(P[upt] * " (g P " * m^2 * " " * yr^-1 * ")"))+
      scale_fill_manual(name="Model",
                        values=c(col.values, 
                                 "multi-model"="grey30",
                                 "obs"="grey"),
                        labels=c(model.labels, "obs"= "OBS"))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))
    
    
    p8 <- ggplot(data=plotDF3, 
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
                               "obs" = expression(bold("OBS"))))
    
    
    
    
    
    
    ###########################################################
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
    tmpDF <- plotDF1[plotDF1$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    plotDF2$sdvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                       "B_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    

    ### add multi-model mean
    tmpDF <- plotDF4[plotDF4$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
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
    
    
    
    plotDF5 <- plotDF3[,c("Group", "meanvalue", "sdvalue")]
    colnames(plotDF5) <- c("Group", "PMIN_pct_mean", "PMIN_pct_sd")
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
                               "obs" = expression(bold("OBS"))))
    
    
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
                               "obs" = expression(bold("OBS"))))
  
    
    ################## PUE ####################
    budgetDF <- prepare_P_budget_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                                 ambDF=ambDF.sum,
                                                                                 eleDF=eleDF.sum,
                                                                                 difDF=annDF.diff.sum)
    
    
    ### add GPP use efficiency - model output and observations
    tmpDF1 <- data.frame("Variable"=rep("GPP_PUE", 36),
                         "Group"=rep(c("obs","C_GDAYP",
                                       "A_ELMV1", "B_CABLP",
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
      tmpDF1$meanvalue[tmpDF1$Group==i&tmpDF1$Trt=="pct_diff"] <- tmpDF1$meanvalue[tmpDF1$Group==i&tmpDF1$Trt=="diff"]/tmpDF1$meanvalue[tmpDF1$Group==i&tmpDF1$Trt=="aCO2"]*100
      

    }
    
    budgetDF <- rbind(budgetDF, tmpDF1)
    
    
    ### select variables, PUE and GPP_PUE
    subDF2 <- subset(budgetDF, Variable%in%c("PUE", "GPP_PUE"))
    
    
    plotDF1 <- subDF2[subDF2$Trt=="aCO2",]
    plotDF2 <- subDF2[subDF2$Trt=="diff",]
    plotDF3 <- subDF2[subDF2$Trt=="pct_diff",]
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF3 <- rbind(plotDF3, tmpDF2)
    
    
    
    ### add into BP DF
    tmpDF1 <- plotDF2[plotDF2$Variable=="PUE",c("Group", "meanvalue", "sdvalue")]
    tmpDF2 <- plotDF2[plotDF2$Variable=="GPP_PUE",c("Group", "meanvalue", "sdvalue")]
    
    colnames(tmpDF1) <- c("Group", "PUE_mean", "PUE_sd")
    colnames(tmpDF2) <- c("Group", "GPP_PUE_mean", "GPP_PUE_sd")
    
    bpDF <- merge(bpDF, tmpDF1, by=c("Group"))
    bpDF <- merge(bpDF, tmpDF2, by=c("Group"))
    
    
    
    tmpDF1 <- plotDF3[plotDF3$Variable=="PUE",c("Group", "meanvalue", "sdvalue")]
    tmpDF2 <- plotDF3[plotDF3$Variable=="GPP_PUE",c("Group", "meanvalue", "sdvalue")]
    
    colnames(tmpDF1) <- c("Group", "PUE_pct_mean", "PUE_pct_sd")
    colnames(tmpDF2) <- c("Group", "GPP_PUE_pct_mean", "GPP_PUE_pct_sd")
    
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
      guides(fill=guide_legend(nrow=2))
    
    
    
    
    p12 <- ggplot(data=plotDF2, 
                  aes(Group, meanvalue)) +
      geom_bar(stat = "identity", aes(fill=Variable), 
               position=position_dodge2(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
      guides(fill=guide_legend(nrow=2))
    
    
    
    
    ################# Plab####################
    plabDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                           ambDF=ambDF.sum,
                                                                           eleDF=eleDF.sum,
                                                                           difDF=annDF.diff.sum,
                                                                           var.list=c("PLAB"),
                                                                           calculate.total=F)
    
    
    
    ### split into ambDF, pctDF
    plotDF1 <- plabDF[plabDF$Trt=="aCO2"&plabDF$Variable%in%c("PLAB"),]
    
    plotDF2 <- plabDF[plabDF$Trt=="pct_diff",]
    
    ### ELMV1 assumes top 1 m soil, not top 10 cm
    plotDF1$meanvalue[plotDF1$Group=="A_ELMV1"] <- plotDF1$meanvalue[plotDF1$Group=="A_ELMV1"]/ 10
    plotDF1$sdvalue[plotDF1$Group=="A_ELMV1"] <- plotDF1$sdvalue[plotDF1$Group=="A_ELMV1"]/ 10
    
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    plotDF2$sdvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                       "B_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    
    ### pass plotDF3 to make biomass production / GPP ratio figure
    co2DF <- plotDF2[,c("Group", "meanvalue", "sdvalue")]
    colnames(co2DF) <- c("Group", "PLAB_mean", "PLAB_sd")
    
    
    
    
    
    #### Plotting
    p17 <- ggplot(data=plotDF1, 
                  aes(Group, meanvalue, group=Group)) +
      geom_bar(stat = "identity", aes(fill=Group), 
               position="dodge", col="black") +
      geom_errorbar(data=plotDF1, 
                    aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", 
                    position=position_dodge2(), width=0.3)+
      #geom_point(data=plotDF1, aes(x=Group, y=meanvalue), 
      #           position=position_dodge2(width=0.9), col="black",
      #           fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
      theme_linedraw() +
      #scale_y_break(c(2,10))+
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
            legend.background = element_rect(fill="grey",
                                             size=0.5, linetype="solid", 
                                             colour ="black"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(P[lab] * " (g P " * m^-2 * ")"))+
      xlab("")+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))+
      scale_fill_manual(name="Model",
                        values=c(col.values, 
                                 "multi-model"="grey30",
                                 "obs"="grey"),
                        labels=c(model.labels, "obs"= "OBS"))+
      guides(fill=guide_legend(nrow=6))
    
    
    
    
    p18 <- ggplot(data=plotDF2, 
                  aes(Group, meanvalue)) +
      geom_bar(stat = "identity", aes(fill=Group), 
               position=position_dodge2(), col="black") +
      geom_errorbar(data=plotDF2, 
                    aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", 
                    position=position_dodge2(), width=0.3)+
      #geom_point(data=plotDF2, aes(x=Group, y=meanvalue), 
      #           position=position_dodge2(width=0.9), col="black",
      #           fill="white", size=2, pch=21)+
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
      xlab("")+
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
      ylab(expression(CO[2] * " effect (%)"))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))+
      scale_fill_manual(name="Model",
                        values=c(col.values, 
                                 "multi-model"="grey30",
                                 "obs"="grey"),
                        labels=c(model.labels, "obs"= "OBS"))+
      guides(fill=guide_legend(nrow=6))
    
    
    
    
    pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_P_demand_and_uptake.pdf"), 
        width=18, height=16)
    plot_grid(#p5, p62,   
              p7, p8,
              p3, p4,
              p1, p2, 
              p17, p18,
              labels=c("A", "B", "C", "D",
                       "E", "F", "G", "H"), label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=2)
    dev.off()
    
    
    
    
    ################# CP ratios ####################
    subDF2 <- subset(budgetDF, Variable%in%c("CPL", "CPW", "CPFR"#, "CPFLIT", 
                                             #"CPSOIL"
                                             ))
    
    
    plotDF1 <- subDF2[subDF2$Trt=="aCO2",]
    plotDF2 <- subDF2[subDF2$Trt=="pct_diff",]
    plotDF3 <- subDF2[subDF2$Trt=="diff",]
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    plotDF2$sdvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                       "B_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### add multi-model mean
    tmpDF <- plotDF3[plotDF3$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
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
    
    
    ### add into BP DF
    tmpDF1 <- plotDF2[plotDF2$Variable=="CPL",c("Group", "meanvalue", "sdvalue")]
    tmpDF2 <- plotDF2[plotDF2$Variable=="CPW",c("Group", "meanvalue", "sdvalue")]
    tmpDF3 <- plotDF2[plotDF2$Variable=="CPFR",c("Group", "meanvalue", "sdvalue")]
    
    colnames(tmpDF1) <- c("Group", "CPL_pct_mean", "CPL_pct_sd")
    colnames(tmpDF2) <- c("Group", "CPW_pct_mean", "CPW_pct_sd")
    colnames(tmpDF3) <- c("Group", "CPFR_pct_mean", "CPFR_pct_sd")
    
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
                        #values=c("CPL"=cbbPalette[4], 
                        #         "CPW"=cbbPalette[3],
                        #         "CPFR"=cbbPalette[8],
                        #         "CPFLIT"=cbbPalette[2],
                        #         "CPSOIL"=cbbPalette[6]),
                        values=c("CPL"="#FF6F91",
                                 "CPW"="#FFC75F",
                                 "CPFR"="#D65DB1"),
                        label=c("CPL"=expression(CP[leaf]),
                                "CPW"=expression(CP[wood]),
                                "CPFR"=expression(CP[froot]),
                                "CPFLIT"=expression(CP[flit]),
                                "CPSOIL"=expression(CP[soil])))+
      guides(fill=guide_legend(nrow=2))
    
    
    
    
    p14 <- ggplot(data=plotDF2, 
                  aes(Group, meanvalue, group=Variable)) +
      #geom_bar(stat = "identity", aes(fill=Variable), 
      #         position=position_dodge2(), col="black") +
      geom_bar(stat = "identity", aes(fill=Variable), 
               position="dodge", col="black") +
      geom_errorbar(data=plotDF2,
                    aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", width=0.2,
                    position=position_dodge(width=1))+
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
                        values=c("CPL"="#FF6F91",
                                 "CPW"="#FFC75F",
                                 "CPFR"="#D65DB1"),
                        label=c("CPL"=expression(CP[leaf]),
                                "CPW"=expression(CP[wood]),
                                "CPFR"=expression(CP[froot]),
                                "CPFLIT"=expression(CP[flit]),
                                "CPSOIL"=expression(CP[soil])))+
      guides(fill=guide_legend(nrow=2))
    
    
    ###########################################################################
    #pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_P_use_variables.pdf"), 
    #    width=16, height=20)
    #plot_grid(p11, p12, # PUE
    #          p13, p14, # cp ratios
    #          p7, p8,   # p uptake and resorption
    #          p9, p10,  # Puptake and P min 
    #          labels=c("(a)", "(b)", "(c)", "(d)",
    #                   "(e)", "(f)", "(g)", "(h)"), label_x=0.1, label_y=0.95,
    #          label_size=24,
    #          ncol=2)
    #dev.off()
    
    
    
    ###########################################################################
    
    ### use CO2 effect of CP flexibility / Pdem / Pupt / P net / delta Pveg to explain 
    ### the CO2 effect on BP and NEP
    
    p1_co2 <- ggplot() +
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15))+
      xlab(expression(paste(CO[2] * " effect on BP (g C " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * Delta * P[veg] * " ( g P " * m^-2 * " " * yr^-1 * ")")))
    
    
    p2_co2 <- ggplot() +
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on BP (g C " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * P[dem] * " ( g P " * m^-2 * " " * yr^-1 * ")")))
    
    
    
    p3_co2 <- ggplot() +
      geom_segment(aes(x=bpDF$BP_mean, xend = bpDF$BP_mean,
                       y=bpDF$PG_pct_mean+bpDF$PG_pct_sd, 
                       yend=bpDF$PG_pct_mean-bpDF$PG_pct_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$BP_mean+bpDF$BP_sd, 
                       xend = bpDF$BP_mean-bpDF$BP_sd,
                       y=bpDF$PG_pct_mean, 
                       yend=bpDF$PG_pct_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(BP_mean, PG_pct_mean, color=Group, pch=Group), 
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
      guides(color=guide_legend(nrow=1))+
      scale_color_manual(name="Model",
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on BP (g C " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * P[dem] * " (%)")))
    
    
    
    p4_co2 <- ggplot() +
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on BP (g C " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * P[upt] * " ( g P " * m^-2 * " " * yr^-1 * ")")))
    
    
    
    p5_co2 <- ggplot() +
      geom_segment(aes(x=bpDF$BP_mean, xend = bpDF$BP_mean,
                       y=bpDF$PUP_pct_mean+bpDF$PUP_pct_sd, 
                       yend=bpDF$PUP_pct_mean-bpDF$PUP_pct_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$BP_mean+bpDF$BP_sd, 
                       xend = bpDF$BP_mean-bpDF$BP_sd,
                       y=bpDF$PUP_pct_mean, 
                       yend=bpDF$PUP_pct_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(BP_mean, PUP_pct_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on BP (g C " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * P[upt] * " (%)")))
    
    
    p6_co2 <- ggplot() +
      geom_segment(aes(x=bpDF$PRETR_mean, xend = bpDF$PRETR_mean,
                       y=bpDF$PUP_mean+bpDF$PUP_sd, 
                       yend=bpDF$PUP_mean-bpDF$PUP_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$PRETR_mean+bpDF$PRETR_sd, 
                       xend = bpDF$PRETR_mean-bpDF$PRETR_sd,
                       y=bpDF$PUP_mean, 
                       yend=bpDF$PUP_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(PRETR_mean, PUP_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on " * P[res] * " ( g P " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * P[upt] * " ( g P " * m^-2 * " " * yr^-1 * ")")))
    
    
    
    
    p7_co2 <- ggplot() +
      geom_segment(aes(x=bpDF$PRETR_pct_mean, xend = bpDF$PRETR_pct_mean,
                       y=bpDF$PUP_pct_mean+bpDF$PUP_pct_sd, 
                       yend=bpDF$PUP_pct_mean-bpDF$PUP_pct_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$PRETR_pct_mean+bpDF$PRETR_pct_sd, 
                       xend = bpDF$PRETR_pct_mean-bpDF$PRETR_pct_sd,
                       y=bpDF$PUP_pct_mean, 
                       yend=bpDF$PUP_pct_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(PRETR_pct_mean, PUP_pct_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on " * P[res] * " (%)")))+
      ylab(expression(paste(CO[2] * " effect on " * P[upt] * " (%)")))
    
    
    
    
    
    
    p8_co2 <- ggplot() +
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on " * P[upt] * " ( g P " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * P[net] * " ( g P " * m^-2 * " " * yr^-1 * ")")))
    
    
    p9_co2 <- ggplot() +
      geom_segment(aes(x=bpDF$PUP_pct_mean, xend = bpDF$PUP_pct_mean,
                       y=bpDF$PMIN_pct_mean+bpDF$PMIN_pct_sd, 
                       yend=bpDF$PMIN_pct_mean-bpDF$PMIN_pct_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$PUP_pct_mean+bpDF$PUP_pct_sd, 
                       xend = bpDF$PUP_pct_mean-bpDF$PUP_pct_sd,
                       y=bpDF$PMIN_pct_mean, 
                       yend=bpDF$PMIN_pct_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(PUP_pct_mean, PMIN_pct_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on " * P[upt] * " (%)")))+
      ylab(expression(paste(CO[2] * " effect on " * P[net] * " (%)")))
    
    
    
    
    p10_co2 <- ggplot() +
      geom_segment(aes(x=bpDF$GPP_PUE_mean, xend = bpDF$GPP_PUE_mean,
                       y=bpDF$PUE_mean+bpDF$PUE_sd, 
                       yend=bpDF$PUE_mean-bpDF$PUE_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$GPP_PUE_mean+bpDF$GPP_PUE_sd, 
                       xend = bpDF$GPP_PUE_mean-bpDF$GPP_PUE_sd,
                       y=bpDF$PUE_mean, 
                       yend=bpDF$PUE_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(GPP_PUE_mean, PUE_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on " * PUE[GPP] * " ( g C " * g^-1 * " P)")))+
      ylab(expression(paste(CO[2] * " effect on " * PUE[NPP] * " ( g C " * g^-1 * " P)")))
    
    
    
    
    p11_co2 <- ggplot() +
      geom_segment(aes(x=bpDF$GPP_PUE_pct_mean, xend = bpDF$GPP_PUE_pct_mean,
                       y=bpDF$PUE_pct_mean+bpDF$PUE_pct_sd, 
                       yend=bpDF$PUE_pct_mean-bpDF$PUE_pct_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$GPP_PUE_pct_mean+bpDF$GPP_PUE_pct_sd, 
                       xend = bpDF$GPP_PUE_pct_mean-bpDF$GPP_PUE_pct_sd,
                       y=bpDF$PUE_pct_mean, 
                       yend=bpDF$PUE_pct_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(GPP_PUE_pct_mean, PUE_pct_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on " * PUE[GPP] * " (%)")))+
      ylab(expression(paste(CO[2] * " effect on " * PUE[NPP] * " (%)")))
    
    
    
    
    p12_co2 <- ggplot() +
      geom_segment(aes(x=bpDF$CPL_mean, xend = bpDF$CPL_mean,
                       y=bpDF$CPFR_mean+bpDF$CPFR_sd, 
                       yend=bpDF$CPFR_mean-bpDF$CPFR_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$CPL_mean+bpDF$CPL_sd, 
                       xend = bpDF$CPL_mean-bpDF$CPL_sd,
                       y=bpDF$CPFR_mean, 
                       yend=bpDF$CPFR_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(CPL_mean, CPFR_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on leaf CP (ele - amb)")))+
      ylab(expression(paste(CO[2] * " effect on fineroot CP (ele - amb)")))
    
    
    
    
    p13_co2 <- ggplot() +
      geom_segment(aes(x=bpDF$CPL_pct_mean, xend = bpDF$CPL_pct_mean,
                       y=bpDF$CPFR_pct_mean+bpDF$CPFR_pct_sd, 
                       yend=bpDF$CPFR_pct_mean-bpDF$CPFR_pct_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$CPL_pct_mean+bpDF$CPL_pct_sd, 
                       xend = bpDF$CPL_pct_mean-bpDF$CPL_pct_sd,
                       y=bpDF$CPFR_pct_mean, 
                       yend=bpDF$CPFR_pct_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(CPL_pct_mean, CPFR_pct_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on leaf CP (%)")))+
      ylab(expression(paste(CO[2] * " effect on fineroot CP (%)")))
    
    
    
    
    
    
    p14_co2 <- ggplot() +
      geom_segment(aes(x=bpDF$BP_mean, xend = bpDF$BP_mean,
                       y=bpDF$CPW_pct_mean+bpDF$CPW_pct_sd, 
                       yend=bpDF$CPW_pct_mean-bpDF$CPW_pct_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$BP_mean+bpDF$BP_sd, 
                       xend = bpDF$BP_mean-bpDF$BP_sd,
                       y=bpDF$CPW_pct_mean, 
                       yend=bpDF$CPW_pct_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(BP_mean, CPW_pct_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on BP (g C " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on wood CP ratio (%)")))
    
    
    
    
    p15_co2 <- ggplot() +
      geom_segment(aes(x=bpDF$deltaPVEG_mean, xend = bpDF$deltaPVEG_mean,
                       y=bpDF$PG_mean+bpDF$PG_sd, 
                       yend=bpDF$PG_mean-bpDF$PG_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=bpDF$deltaPVEG_mean+bpDF$deltaPVEG_sd, 
                       xend = bpDF$deltaPVEG_mean-bpDF$deltaPVEG_sd,
                       y=bpDF$PG_mean, 
                       yend=bpDF$PG_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(deltaPVEG_mean, PG_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      xlab(expression(paste(CO[2] * " effect on " * Delta * P[veg] * " (g P " * m^-2 * " " * yr^-1 * ")")))+
      ylab(expression(paste(CO[2] * " effect on " * P[dem] * " (g P " * m^-2 * " " * yr^-1 * ")")))
    
    
    p16_co2 <- ggplot() +
      geom_segment(aes(y=bpDF$deltaPVEG_mean, yend = bpDF$deltaPVEG_mean,
                       x=bpDF$PG_pct_mean+bpDF$PG_pct_sd, 
                       xend=bpDF$PG_pct_mean-bpDF$PG_pct_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(y=bpDF$deltaPVEG_mean+bpDF$deltaPVEG_sd, 
                       yend = bpDF$deltaPVEG_mean-bpDF$deltaPVEG_sd,
                       x=bpDF$PG_pct_mean, 
                       xend=bpDF$PG_pct_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(PG_pct_mean, deltaPVEG_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      ylab(expression(paste(CO[2] * " effect on " * Delta * P[veg] * " (g P " * m^-2 * " " * yr^-1 * ")")))+
      xlab(expression(paste(CO[2] * " effect on " * P[dem] * " (%)")))
    
    
    
    
    p17_co2 <- ggplot() +
      geom_segment(aes(y=bpDF$PG_pct_mean, yend = bpDF$PG_pct_mean,
                       x=bpDF$PUE_pct_mean+bpDF$PUE_pct_sd, 
                       xend=bpDF$PUE_pct_mean-bpDF$PUE_pct_sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(y=bpDF$PG_pct_mean+bpDF$PG_pct_sd, 
                       yend = bpDF$PG_pct_mean-bpDF$PG_pct_sd,
                       x=bpDF$PUE_pct_mean, 
                       xend=bpDF$PUE_pct_mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=bpDF, aes(PUE_pct_mean, PG_pct_mean, color=Group, pch=Group), 
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
                         values=c(col.values, "multi-model"="black", "obs"="black"),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "multi-model"=19, "obs"=15),
                         labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
      ylab(expression(paste(CO[2] * " effect on " * P[dem] * " (%)")))+
      xlab(expression(paste(CO[2] * " effect on " * PUE[NPP] * " (%)")))
    
    
    #plot(p17_co2)
    
    
    
    
    ################# Plab####################
    plabDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                           ambDF=ambDF.sum,
                                                                           eleDF=eleDF.sum,
                                                                           difDF=annDF.diff.sum,
                                                                           var.list=c("PLAB"),
                                                                           calculate.total=F)
    
    
    
    ### split into ambDF, pctDF
    plotDF1 <- plabDF[plabDF$Trt=="aCO2"&plabDF$Variable%in%c("PLAB"),]
    
    plotDF2 <- plabDF[plabDF$Trt=="pct_diff",]
    
    ### ELMV1 assumes top 1 m soil, not top 10 cm
    plotDF1$meanvalue[plotDF1$Group=="A_ELMV1"] <- plotDF1$meanvalue[plotDF1$Group=="A_ELMV1"]/ 10
    plotDF1$sdvalue[plotDF1$Group=="A_ELMV1"] <- plotDF1$sdvalue[plotDF1$Group=="A_ELMV1"]/ 10
    
    
    
    ### add multi-model mean
    tmpDF <- plotDF1[plotDF1$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    ### add multi-model mean
    tmpDF <- plotDF2[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                        "B_CABLP", "D_LPJGP",
                                        "E_OCHDP", "F_QUINC",
                                        "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(meanvalue~Variable+Trt, FUN=c(mean,sd),
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$Group <- "multi-model"
    tmpDF2 <- tmpDF2[,c("Variable", "Group", "Trt", "meanvalue.mean", "meanvalue.sd")]
    colnames(tmpDF2) <- c("Variable", "Group", "Trt", "meanvalue", "sdvalue")
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    plotDF2$sdvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                       "B_CABLP", "D_LPJGP",
                                       "E_OCHDP", "F_QUINC",
                                       "G_OCHDX", "H_QUJSM")] <- NA
    
    
    
    ### pass plotDF3 to make biomass production / GPP ratio figure
    co2DF <- plotDF2[,c("Group", "meanvalue", "sdvalue")]
    colnames(co2DF) <- c("Group", "PLAB_mean", "PLAB_sd")
    
    
 
    
    ###########################################################################
    
    #pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_P_use_variables2.pdf"), 
    #    width=20, height=12)
    #plot_grid(p1_co2, p2_co2, p3_co2, p4_co2, p5_co2,
    #          p6_co2, p7_co2, p8_co2, p9_co2, p10_co2,
    #          p11_co2, p12_co2, p13_co2, p14_co2, NA,
    #          #labels=c("(a)", "(b)", "(c)", "(d)", "(e)", 
    #          #         "(f)", "(g)", "(h)", "(i)", "(j)", 
    #          #         "(k)", "(l)", "(m)", "(n)"), 
    #          label_x=0.1, label_y=0.95,
    #          label_size=24,
    #          ncol=5, nrow=3)
    #dev.off()
    
    
    
    
    
    
    ###########################################################################
    
    ### plot deltaPveg and Pdem on same row and merge with CO2 plot
    #plots_first_row <- plot_grid(p3, p4, p1_co2, 
    #                             label_x=0.1, label_y=0.95,
    #                             label_size=24,
    #                             rel_widths=c(2, 2, 1),
    #                             ncol=3, nrow=1)
    #
    #
    #plots_second_row <- plot_grid(p5, p8, p15_co2, 
    #                              label_x=0.1, label_y=0.95,
    #                              label_size=24,
    #                              rel_widths=c(2, 2, 1),
    #                              ncol=3, nrow=1)
    #
    #
    #plots_third_row <- plot_grid(p13, p14, p13_co2, 
    #                              label_x=0.1, label_y=0.95,
    #                              label_size=24,
    #                              rel_widths=c(2, 2, 1),
    #                              ncol=3, nrow=1)
    #
    #plots_fourth_row <- plot_grid(p11, p12, p11_co2, 
    #                              label_x=0.1, label_y=0.95,
    #                              label_size=24,
    #                              rel_widths=c(2, 2, 1),
    #                              ncol=3, nrow=1)
    
    
    
    
    #pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_P_use_variables3.pdf"), 
    #    width=20, height=16)
    #plot_grid(plots_first_row,
    #          plots_second_row,
    #          plots_third_row,
    #          plots_fourth_row,
    #          ncol=1, nrow=4)
    #dev.off()  
    
    
    
    ###########################################################################
    ### prepare fraction of the CO2 response of Pupt and Pretr as a fraction of 
    ### the CO2 response of Pdemand
    
    
    
    
    
    
    
    
    ###########################################################################
    
    plots_first_row <- plot_grid(p3_co2, p1_co2, p17_co2,
                                 label_x=0.1, label_y=0.95,
                                 label_size=24,
                                 rel_widths=c(1,1),
                                 ncol=3, nrow=1)
    
    
    plots_second_row <- plot_grid(p13, p14,
                                 label_x=0.1, label_y=0.95,
                                 label_size=24,
                                 rel_widths=c(1,1),
                                 ncol=2, nrow=1)
    
    plots_legend_row1 <-  get_legend(p1_co2 + theme(legend.position="bottom",
                                                      legend.box = 'horizontal',
                                                      legend.box.just = 'left')
                                     + guides(fill=guide_legend(nrow=1,byrow=TRUE)))
    
    
    
    plots_legend_row2 <-  get_legend(p13 + theme(legend.position="bottom",
                                                    legend.box = 'horizontal',
                                                    legend.box.just = 'left')
                                     + guides(fill=guide_legend(nrow=1,byrow=TRUE)))
    
    
    #pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_P_use_variables4.pdf"), 
    #    width=16, height=12)
    #plot_grid(plots_first_row,
    #          plots_legend_row1,
    #          plots_second_row, 
    #          plots_legend_row2,
    #          rel_heights=c(1, 0.2, 1, 0.2),
    #          nrow=4, ncol=1)
    #dev.off()
    
    
    
    
    ###########################################################################
    
    plots_first_row <- plot_grid(p5, p6, 
                                 label_x=c(0.1, 0.09), label_y=0.95,
                                 label_size=24,
                                 labels=c("A", "B"), 
                                 rel_widths=c(1,1),
                                 ncol=2, nrow=1)
    

    
    plots_third_row <- plot_grid(p13, p14, 
                                  label_x=c(0.1, 0.08), label_y=0.95,
                                  label_size=24,
                                  labels=c("C", "D"), 
                                  rel_widths=c(1,1),
                                  ncol=2, nrow=1)
    
    
    plots_fourth_row <- plot_grid(p3_co2, p5_co2, p13_co2, #p14_co2,
                                      label_x=0.12, label_y=0.95,
                                      label_size=24,
                                      labels=c("E", "F", "G"), 
                                      rel_widths=c(1,1,1,1),
                                      ncol=3, nrow=1)
    
    
    plots_legend_fourth_row_right <-  get_legend(p3_co2 + theme(legend.position="bottom",
                                                                legend.box = 'horizontal',
                                                                legend.box.just = 'left'))
    
    
    pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_P_use_variables5.pdf"), 
        width=18, height=12)
    plot_grid(plots_first_row,
              #plots_second_row, 
              plots_third_row, 
              plots_fourth_row,
              plots_legend_fourth_row_right,
              rel_heights=c(1, 1, 1.2, 0.2),
              nrow=4, ncol=1)
    dev.off()
    
    
    
    
    
    
}


