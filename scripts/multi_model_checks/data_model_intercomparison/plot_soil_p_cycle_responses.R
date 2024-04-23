plot_soil_p_cycle_responses <- function(eucDF,
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
      
      
      ### forcing sd to zero
      
      
      
      ### check GDAY PUP
      #ambDF.sum$PUP.mean[ambDF.sum$ModName=="C_GDAYP"]
      #eleDF.sum$PUP.mean[eleDF.sum$ModName=="C_GDAYP"]
      
      
      ##########################################################################
      ### Plotting
      
      
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
      p1 <- ggplot(data=plotDF1, 
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
      
      
      
      
      p2 <- ggplot(data=plotDF2, 
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
      
      
      
      
      
      ################# delta P lab ####################
      plabDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                             ambDF=ambDF.sum,
                                                                             eleDF=eleDF.sum,
                                                                             difDF=annDF.diff.sum,
                                                                             var.list=c("deltaPLAB"),
                                                                             calculate.total=F)
      
      
      
      ### split into ambDF, pctDF
      plotDF1 <- plabDF[plabDF$Trt=="aCO2"&plabDF$Variable%in%c("deltaPLAB"),]
      
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
      
  
      
      
      ### plotting
      p1_1 <- ggplot(data=plotDF1, 
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
      
      
      
      
      p2_1 <- ggplot(data=plotDF2, 
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
      
      
      
      
      
      ###############################################################################
      #### Pmin
      pminDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                             ambDF=ambDF.sum,
                                                                             eleDF=eleDF.sum,
                                                                             difDF=annDF.diff.sum,
                                                                             var.list=c("PMIN", "PBIOCHMIN"),
                                                                             calculate.total=T)
      
      pminDF <- pminDF[pminDF$Variable=="Tot",]
      pminDF$Variable <- "PMIN"
      
      #### PUP
      pupDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                            ambDF=ambDF.sum,
                                                                            eleDF=eleDF.sum,
                                                                            difDF=annDF.diff.sum,
                                                                            var.list=c("PUP"),
                                                                            calculate.total=F)
      
      totDF <- rbind(pminDF, pupDF)
      
      
      ### split into ambDF, pctDF
      plotDF1 <- pminDF[pminDF$Trt=="aCO2"&pminDF$Variable%in%c("PMIN"),]
      plotDF2 <- pminDF[pminDF$Trt=="pct_diff",]
      
      plotDF3 <- totDF[totDF$Trt=="aCO2"&totDF$Variable%in%c("PMIN", "PUP"),]
      plotDF4 <- totDF[totDF$Trt=="pct_diff",]
      
      
      ### OCHDP and OCHDX: PMIN is the net P mineralization flux, ignoring biochemical mineralization;
      ###                  PBIOCHMIN is the gross flux.
      ###                  Easy solution is the subtract PBIOCHMIN from the total,
      ###                  and because PBIOCHMIN = PMIN, we can just divide the total by 2
      plotDF1$meanvalue[plotDF1$Group=="E_OCHDP"] <- plotDF1$meanvalue[plotDF1$Group=="E_OCHDP"]/ 2
      plotDF1$meanvalue[plotDF1$Group=="G_OCHDX"] <- plotDF1$meanvalue[plotDF1$Group=="G_OCHDX"]/ 2
      
      plotDF3$meanvalue[plotDF3$Group=="E_OCHDP"] <- plotDF3$meanvalue[plotDF3$Group=="E_OCHDP"]/ 2
      plotDF3$meanvalue[plotDF3$Group=="G_OCHDX"] <- plotDF3$meanvalue[plotDF3$Group=="G_OCHDX"]/ 2
      
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
      
      
      ### pass plotDF3 to make biomass production / GPP ratio figure
      tDF1 <- plotDF4[plotDF4$Variable=="PUP",c("Group", "meanvalue", "sdvalue")]
      tDF2 <- plotDF4[plotDF4$Variable=="PMIN",c("Group", "meanvalue", "sdvalue")]
      
      colnames(tDF1) <- c("Group", "PUP_mean", "PUP_sd")
      colnames(tDF2) <- c("Group", "PMIN_mean", "PMIN_sd")
      
      co2DF <- merge(co2DF, tDF1, by=c("Group"))
      co2DF <- merge(co2DF, tDF2, by=c("Group"))
      
      
      
      
      ### plotting  
      p3 <- ggplot(data=plotDF1, 
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
        ylab(expression(P[net] * " (g P " * m^-2 * " " * yr^-1 * ")"))+
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
      
      
      
      
      p4 <- ggplot(data=plotDF2, 
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
      
      
      #pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_soil_P_variables.pdf"), 
      #    width=16, height=16)
      #plot_grid(p1, p2,  
      #          p3, p4,
      #          labels=c("(a)", "(b)", "(c)", "(d)",
      #                   "(e)", "(f)"), label_x=0.1, label_y=0.95,
      #          label_size=24,
      #          ncol=2)
      #dev.off()
      
      
      
  
      p3_1 <- ggplot(data=plotDF3, 
                   aes(Group, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        geom_errorbar(data=plotDF3,
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", width=0.2,
                      position=position_dodge(width=1))+
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
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(P[net] * " (g P " * m^-2 * " " * yr^-1 * ")"))+
        xlab("")+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Variable",
                          values=c("PUP"=cbbPalette[4], 
                                   "PMIN"=cbbPalette[3]),
                          label=c("PUP"=expression(P[upt]),
                                  "PMIN"=expression(P[net])))+
        guides(fill=guide_legend(nrow=6))
      
      
      
      
      p4_1 <- ggplot(data=plotDF4, 
                     aes(Group, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        geom_errorbar(data=plotDF4,
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", width=0.2,
                      position=position_dodge(width=1))+
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
        scale_fill_manual(name="Variable",
                          values=c("PUP"=cbbPalette[4], 
                                   "PMIN"=cbbPalette[3]),
                          label=c("PUP"=expression(P[upt]),
                                  "PMIN"=expression(P[net])))+
        guides(fill=guide_legend(nrow=6))
      
      
      
      
      ################# organic P and inorganic P ####################
      soilDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                            ambDF=ambDF.sum,
                                                                            eleDF=eleDF.sum,
                                                                            difDF=annDF.diff.sum,
                                                                            var.list=c("PPMIN", "PPORG"),
                                                                            calculate.total=F)
      
      
      ### split into ambDF, pctDF
      plotDF1 <- soilDF[soilDF$Trt=="aCO2",]
      plotDF2 <- soilDF[soilDF$Trt=="pct_diff",]
      plotDF3 <- soilDF[soilDF$Trt=="diff",]
    
      
      ### elmv1 is for top 1 m, scale it down
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
      
      
      ## stoichiometry
      p5 <- ggplot(data=plotDF1, 
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
              axis.text.x=element_text(size=12),
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
        ylab(expression("Soil P pools (g P " * m^-2 * ")"))+
        xlab("")+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Variable",
                          values=c("PPMIN"=cbbPalette[4], 
                                   "PPORG"=cbbPalette[3]),
                          label=c("PPMIN"=expression(P[inorganic]),
                                  "PPORG"=expression(P[organic])))+
        guides(fill=guide_legend(nrow=2)); p5
      
      
      
      
      p6 <- ggplot(data=plotDF2, 
                    aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position=position_dodge2(), col="black") +
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
        scale_fill_manual(name="Variable",
                          values=c("PPMIN"=cbbPalette[4], 
                                   "PPORG"=cbbPalette[3]),
                          label=c("PPMIN"=expression(P[inorganic]),
                                  "PPORG"=expression(P[organic])))+
        guides(fill=guide_legend(nrow=2)); p6
      
      
      
      
      
      ################# delta organic P and inorganic P ####################
      soilDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                             ambDF=ambDF.sum,
                                                                             eleDF=eleDF.sum,
                                                                             difDF=annDF.diff.sum,
                                                                             var.list=c("deltaPPMIN", "deltaPPORG"),
                                                                             calculate.total=F)
      
      
      ### split into ambDF, pctDF
      plotDF1 <- soilDF[soilDF$Trt=="aCO2",]
      plotDF2 <- soilDF[soilDF$Trt=="pct_diff",]
      plotDF3 <- soilDF[soilDF$Trt=="diff",]
      
      
      ### elmv1 is for top 1 m, scale it down
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
      
      
      ## plot
      p7 <- ggplot(data=plotDF1, 
                   aes(Group, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        geom_errorbar(data=plotDF1,
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", width=0.2,
                      position=position_dodge(width=1))+
        geom_vline(xintercept=c(6.5, 8.5), lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
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
        ylab(expression(Delta * "Soil P pools (g P " * m^-2 * ")"))+
        xlab("")+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Variable",
                          values=c("deltaPPMIN"=cbbPalette[4], 
                                   "deltaPPORG"=cbbPalette[3]),
                          label=c("deltaPPMIN"=expression(Delta*P[inorganic]),
                                  "deltaPPORG"=expression(Delta*P[organic])))+
        guides(fill=guide_legend(nrow=2)); p7
      
      
      
      
      p8 <- ggplot(data=plotDF2, 
                   aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position=position_dodge2(), col="black") +
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
        scale_fill_manual(name="Variable",
                          values=c("deltaPPMIN"=cbbPalette[4], 
                                   "deltaPPORG"=cbbPalette[3]),
                          label=c("deltaPPMIN"=expression(Delta*P[inorganic]),
                                  "deltaPPORG"=expression(Delta*P[organic])))+
        guides(fill=guide_legend(nrow=2)); p8
      
      
      ################# RHET ####################
      plabDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                             ambDF=ambDF.sum,
                                                                             eleDF=eleDF.sum,
                                                                             difDF=annDF.diff.sum,
                                                                             var.list=c("RHET"),
                                                                             calculate.total=F)
      
      
      
      ### split into ambDF, pctDF
      plotDF1 <- plabDF[plabDF$Trt=="aCO2"&plabDF$Variable%in%c("RHET"),]
      
      plotDF2 <- plabDF[plabDF$Trt=="pct_diff",]
      
      ### ELMV1 assumes top 1 m soil, not top 10 cm
      #plotDF1$meanvalue[plotDF1$Group=="A_ELMV1"] <- plotDF1$meanvalue[plotDF1$Group=="A_ELMV1"]
      #plotDF1$sdvalue[plotDF1$Group=="A_ELMV1"] <- plotDF1$sdvalue[plotDF1$Group=="A_ELMV1"]/ 10
      
      
      
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
      tDF1 <- plotDF2[,c("Group", "meanvalue", "sdvalue")]
  
      colnames(tDF1) <- c("Group", "RHET_mean", "RHET_sd")
  
      co2DF <- merge(co2DF, tDF1, by=c("Group"))
  
      
      
      
      ### plotting
      p9 <- ggplot(data=plotDF1, 
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
        ylab(expression(R[het] * " (g C " * m^-2 * " " * yr^-1 * ")"))+
        xlab("")+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Model",
                          values=c(col.values, 
                                   "multi-model"="grey30",
                                   "obs"="grey"),
                          labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p9
      
      
      
      
      p10 <- ggplot(data=plotDF2, 
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
        guides(fill=guide_legend(nrow=6)); p10
      
      
      
      
      ###########################################################################
      p1_co2 <- ggplot() +
        geom_segment(aes(x=co2DF$PMIN_mean, xend = co2DF$PMIN_mean,
                         y=co2DF$PUP_mean+co2DF$PUP_sd, 
                         yend=co2DF$PUP_mean-co2DF$PUP_sd),
                     lwd=0.5, color="grey")+
        geom_segment(aes(x=co2DF$PMIN_mean+co2DF$PMIN_sd, 
                         xend = co2DF$PMIN_mean-co2DF$PMIN_sd,
                         y=co2DF$PUP_mean, 
                         yend=co2DF$PUP_mean), 
                     lwd=0.5, color="grey")+
        geom_point(data=co2DF, aes(PMIN_mean, PUP_mean, color=Group, pch=Group), 
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
                           values=c(col.values, "multi-model"="grey30", "obs"="grey"),
                           labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
        scale_shape_manual(name="Model",
                           values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                    "B_CABLP"=19,"D_LPJGP"=19,
                                    "E_OCHDP"=19,"F_QUINC"=19,
                                    "G_OCHDX"=19,"H_QUJSM"=19,
                                    "multi-model"=19, "obs"=15))+
        xlab(expression(paste(CO[2] * " effect on " * P[net] * " (%)")))+
        ylab(expression(paste(CO[2] * " effect on " * P[upt] * " (%)")))
      
      
      
      p2_co2 <- ggplot() +
        geom_segment(aes(x=co2DF$PMIN_mean, xend = co2DF$PMIN_mean,
                         y=co2DF$PLAB_mean+co2DF$PLAB_sd, 
                         yend=co2DF$PLAB_mean-co2DF$PLAB_sd),
                     lwd=0.5, color="grey")+
        geom_segment(aes(x=co2DF$PMIN_mean+co2DF$PMIN_sd, 
                         xend = co2DF$PMIN_mean-co2DF$PMIN_sd,
                         y=co2DF$PLAB_mean, 
                         yend=co2DF$PLAB_mean), 
                     lwd=0.5, color="grey")+
        geom_point(data=co2DF, aes(PMIN_mean, PLAB_mean, color=Group, pch=Group), 
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
                           values=c(col.values, "multi-model"="grey30", "obs"="grey"),
                           labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
        scale_shape_manual(name="Model",
                           values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                    "B_CABLP"=19,"D_LPJGP"=19,
                                    "E_OCHDP"=19,"F_QUINC"=19,
                                    "G_OCHDX"=19,"H_QUJSM"=19,
                                    "multi-model"=19, "obs"=15))+
        xlab(expression(paste(CO[2] * " effect on " * P[net] * " (%)")))+
        ylab(expression(paste(CO[2] * " effect on " * P[lab] * " (%)")))
      
      
      p3_co2 <- ggplot() +
        geom_segment(aes(x=co2DF$PUP_mean, xend = co2DF$PUP_mean,
                         y=co2DF$PLAB_mean+co2DF$PLAB_sd, 
                         yend=co2DF$PLAB_mean-co2DF$PLAB_sd),
                     lwd=0.5, color="grey")+
        geom_segment(aes(x=co2DF$PUP_mean+co2DF$PUP_sd, 
                         xend = co2DF$PUP_mean-co2DF$PUP_sd,
                         y=co2DF$PLAB_mean, 
                         yend=co2DF$PLAB_mean), 
                     lwd=0.5, color="grey")+
        geom_point(data=co2DF, aes(PUP_mean, PLAB_mean, color=Group, pch=Group), 
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
                           values=c(col.values, "multi-model"="grey30", "obs"="grey"),
                           labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
        scale_shape_manual(name="Model",
                           values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                    "B_CABLP"=19,"D_LPJGP"=19,
                                    "E_OCHDP"=19,"F_QUINC"=19,
                                    "G_OCHDX"=19,"H_QUJSM"=19,
                                    "multi-model"=19, "obs"=15))+
        xlab(expression(paste(CO[2] * " effect on " * P[upt] * " (%)")))+
        ylab(expression(paste(CO[2] * " effect on " * P[lab] * " (%)")))
      
      
      p4_co2 <- ggplot() +
        geom_segment(aes(x=co2DF$PMIN_mean, xend = co2DF$PMIN_mean,
                         y=co2DF$RHET_mean+co2DF$RHET_sd, 
                         yend=co2DF$RHET_mean-co2DF$RHET_sd),
                     lwd=0.5, color="grey")+
        geom_segment(aes(x=co2DF$PMIN_mean+co2DF$PMIN_sd, 
                         xend = co2DF$PMIN_mean-co2DF$PMIN_sd,
                         y=co2DF$RHET_mean, 
                         yend=co2DF$RHET_mean), 
                     lwd=0.5, color="grey")+
        geom_point(data=co2DF, aes(PMIN_mean, RHET_mean, color=Group, pch=Group), 
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
                           values=c(col.values, "multi-model"="grey30", "obs"="grey"),
                           labels=c(model.labels, "multi-model"="M-M", "obs"="OBS"))+
        scale_shape_manual(name="Model",
                           values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                    "B_CABLP"=19,"D_LPJGP"=19,
                                    "E_OCHDP"=19,"F_QUINC"=19,
                                    "G_OCHDX"=19,"H_QUJSM"=19,
                                    "multi-model"=19, "obs"=15))+
        xlab(expression(paste(CO[2] * " effect on " * P[net] * " (%)")))+
        ylab(expression(paste(CO[2] * " effect on " * R[het] * " (%)")))
      
  
      
      ###########################################################################
      
      pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_soil_P_variables.pdf"), 
          width=16, height=8)
      plot_grid(p3, p4,
                #p1, p2,
                #p1_1, p2_1,
                #p3_1, p4_1,
                p9, p10,
                labels=c("A", "B", "C", "D"), label_x=0.1, label_y=0.95,
                label_size=24,
                ncol=2)
      dev.off()
      
      
      
      #### second
      pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_soil_P_variables2.pdf"), 
          width=16, height=4)
      plot_grid(p5, p6,
                labels=c("A", "B", "C", "D",
                         "E", "F"), label_x=0.1, label_y=0.95,
                label_size=24,
                ncol=2)
      dev.off()
      
     
      #### third
      pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_soil_P_variables3.pdf"), 
          width=8, height=8)
      plot_grid(p1_co2, p2_co2,
                p3_co2, p4_co2,
                labels=c("A", "B", "C", "D"), label_x=0.2, label_y=0.95,
                label_size=24,
                ncol=2)
      dev.off()
      
      
}


