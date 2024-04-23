plot_growth_and_nep_response <- function(scenario, eucDF) {
    
    
    
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
    bpDF <- plotDF3
    
    ### Plotting
    ### additional to-do list:
    ### 1. fill color by manual selection
    p5 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        geom_errorbar(data=plotDF2, 
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        geom_point(data=plotDF2, aes(x=Group, y=meanvalue), col="black",
                   fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position=c(.2,.2),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        ylab(expression(paste(Delta * C[veg] * " (g C " * m^2 * " " * yr^-1 * ")")))+
        #ylab(expression(paste("Biomass production (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
        #scale_fill_manual(name=expression(Delta*C[veg]),
        #                  values=c("deltaCL"=cbbPalette[2],
        #                           "deltaCW"=cbbPalette[3],
        #                           "deltaCFR"=cbbPalette[4],
        #                           "deltaCCR"=cbbPalette[7],
        #                           "deltaCSTOR"=cbbPalette[8]),
        #                  labels=c("deltaCL"=expression(Delta*C[leaf]), 
        #                           "deltaCW"=expression(Delta*C[wood]), 
        #                           "deltaCFR"=expression(Delta*C[froot]), 
        #                           "deltaCCR"=expression(Delta*C[croot]),
        #                           "deltaCSTOR"=expression(Delta*C[store])))+
    scale_fill_manual(name=expression(Delta*C[veg]),
                      values=c("deltaCL"=cbbPalette[2],
                               "deltaCW"=cbbPalette[3],
                               "deltaCFR"=cbbPalette[4],
                               "deltaCCR"=cbbPalette[7],
                               "deltaCSTOR"=cbbPalette[8]),
                      labels=c("deltaCL"=expression(Delta*C[leaf]), 
                               "deltaCW"=expression(Delta*C[wood]), 
                               "deltaCFR"=expression(Delta*C[froot]), 
                               "deltaCCR"=expression(Delta*C[croot]),
                               "deltaCSTOR"=expression(Delta*C[store])))+
        guides(fill=guide_legend(nrow=3, byrow=T),
               pch="none")
    
    
    p6 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group),
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        #geom_point(data=plotDF3, aes(x=Group, y=meanvalue), col="black",
        #           fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
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
        ylab(expression(CO[2] * " effect (g C " * m^2 * " " * yr^-1 * ")"))+
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
    
    
    
    
    ################# NEP ####################
    cfluxDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                            ambDF=ambDF.sum,
                                                                            eleDF=eleDF.sum,
                                                                            difDF=annDF.diff.sum,
                                                                            var.list=c("NEP"),
                                                                            calculate.total=F)
    
    ### split into ambDF, pctDF
    plotDF1 <- cfluxDF[cfluxDF$Trt=="aCO2",]
    plotDF2 <- cfluxDF[cfluxDF$Trt=="diff",]

    
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
    nepDF <- plotDF2
    
    
    ### plotting GPP, NPP, and RAU
    p7 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        #geom_point(data=plotDF1, aes(x=Group, y=meanvalue), col="black",
        #           fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
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
        ylab(expression(paste("NEP (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Model",
                          values=c(col.values, 
                                   "multi-model"="grey30",
                                   "obs"="grey"),
                          labels=c(model.labels, "obs"= "OBS"))+
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "multi-model"="grey30", "obs"="grey"))),
               color = guide_legend(nrow=12, byrow=F)); p7
    
    
    p8 <- ggplot(data=plotDF2, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        #geom_point(data=plotDF2, aes(x=Group, y=meanvalue), col="black",
        #           fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
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
        ylab(expression(CO[2] * " effect (g C " * m^2 * " " * yr^-1 * ")"))+
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
    
    
    pdf(paste0(out.dir, "/MIP_", scenario, "_growth_and_nep_response.pdf"), 
        width=18, height=10)
    plot_grid(
        #p3, p4, # GPP
        p5, p6, # delta Cveg
        #p9, p10, # Allocation
        p7, p8, # NEP
        labels="auto", label_x=0.1, label_y=0.95,
        label_size=24,
        ncol=2)
    dev.off()

     
    
    ################# Major carbon fluxes  ####################
    cfluxDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                            ambDF=ambDF.sum,
                                                                            eleDF=eleDF.sum,
                                                                            difDF=annDF.diff.sum,
                                                                            var.list=c("NPP", "RAU"),
                                                                            calculate.total=T)
    
    ### split into ambDF, pctDF
    plotDF1 <- cfluxDF[cfluxDF$Trt=="aCO2"&cfluxDF$Variable%in%c("NPP","RAU"),]
    plotDF2 <- cfluxDF[cfluxDF$Trt=="aCO2"&cfluxDF$Variable%in%c("Tot"),]
    plotDF3 <- cfluxDF[cfluxDF$Trt=="pct_diff"&cfluxDF$Variable%in%c("Tot"),]
    plotDF4 <- cfluxDF[cfluxDF$Trt=="diff"&cfluxDF$Variable%in%c("Tot"),]
    
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
    
    
    ### remove unwanted points
    plotDF2$meanvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                         "B_CABLP", "D_LPJGP",
                                         "E_OCHDP", "F_QUINC",
                                         "G_OCHDX", "H_QUJSM")] <- NA
    
    
    ### pass plotDF3 to make biomass production / GPP ratio figure
    gppDF <- plotDF4
    
    
    ### plotting GPP, NPP, and RAU
    p3 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        geom_errorbar(data=plotDF2,
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        geom_point(data=plotDF2,aes(Group, meanvalue), 
                   size=2,fill="white", pch=21, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
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
              legend.position=c(.1,.2),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Gross primary production (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Variable",
                          values=c("NPP"="green", "RAU"="yellow"),
                          labels=c("NPP"="NPP",
                                   "RAU"=expression(R[auto]))); p3
    
    
    p4 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        #geom_point(aes(Group, meanvalue), 
        #           size=2,fill="white", pch=21, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
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
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "multi-model"="grey30", "obs"="grey"))),
               color = guide_legend(nrow=12, byrow=F)); p4
    
    
    
    ### plot the CO2 response ratio of biomass production over the
    ### CO2 response ratio of GPP to see how they compare
    ratioDF <- merge(gppDF, bpDF, by="Group")
    ratioDF$Variable.x <- NULL
    ratioDF$Variable.y <- NULL
    ratioDF$Trt.x <- NULL
    ratioDF$Trt.y <- NULL
    
    colnames(ratioDF) <- c("Group", "GPP.mean", "GPP.sd",
                           "BP.mean", "BP.sd")
    
    ratioDF <- merge(ratioDF, nepDF, by="Group")
    colnames(ratioDF) <- c("Group", "GPP.mean", "GPP.sd",
                           "BP.mean", "BP.sd",
                           "Variable", "Trt", 
                           "NEP.mean", "NEP.sd")
    
    
    #################### allocation coefficient  ####################
    ### Allocation coefficients are calculated different comparing the data and the model. 
    ### In the EucFACE data, allocation to leaf includes allocation to overstorey and understorey leaves, 
    ### and allocation to root includes allocation to overstorey and understorey roots. 
    ### In the data, there is also an additional allocation coefficient to Mycorrhizae, 
    ### which can be grouped with allocation to root as total belowground allocation. 
    ### This total belowground allocation is comparable to allocation coefficient to root in the model. 
    allocDF <- prepare_allocation_coef_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                                       ambDF=ambDF.sum,
                                                                                       eleDF=eleDF.sum,
                                                                                       difDF=annDF.diff.sum)
    
    
    ### split into ambDF, pctDF
    plotDF1 <- allocDF[allocDF$Trt=="aCO2",]
    plotDF2 <- allocDF[allocDF$Trt=="diff",]
    plotDF3 <- allocDF[allocDF$Trt=="pct_diff",]
    
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
    
    
    
    ### Plotting
    p9 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
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
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Allocation coefficients")+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Variable",
                          values=c("Canopy"=cbbPalette[4], 
                                   "Wood"=cbbPalette[3],
                                   "Root"=cbbPalette[8],
                                   "Other"=cbbPalette[2]))+
        guides(fill=guide_legend(nrow=2)); p9
    
    
    p10 <- ggplot(data=plotDF2, 
                  aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position=position_dodge2(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
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
              legend.position=c(.3,.8),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * " effect (ele - amb)"))+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Variable",
                          values=c("Canopy"=cbbPalette[4], 
                                   "Wood"=cbbPalette[3],
                                   "Root"=cbbPalette[8],
                                   "Other"=cbbPalette[2]))+
        guides(fill=guide_legend(nrow=2)); p10
    
    
    
    
    
    
    pdf(paste0(out.dir, "/MIP_", scenario, "_gpp_response_multi-model.pdf"), 
        width=18, height=10)
    plot_grid(
        p3, p4, # GPP
        p9, p10, # Allocation
        labels="AUTO", label_x=0.1, label_y=0.95,
        label_size=24,
        ncol=2)
    dev.off()
    
    
    
    
    ################# Major carbon pools  ####################
    ### Firstly we will check the major carbon pools, 
    ### as these data are provided in Table 1 in the parameter file. 
    ### Note that:
    ### * CFR combines fineroot (< 2 mm in diameter) and intermediate root (2-3 mm) in the observation;
    ### * CL includes overstorey leaf only in the observation;
    ### * CW includes branch and stem in the model simulation.
    vegDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                          ambDF=ambDF.sum,
                                                                          eleDF=eleDF.sum,
                                                                          difDF=annDF.diff.sum,
                                                                          var.list=c("CL", "CW", "CFR", "CCR", "CSTOR"),
                                                                          calculate.total=T)
    
    ### split into ambDF, pctDF
    plotDF1 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("CL","CW","CCR","CFR","CSTOR"),]
    plotDF2 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("Tot"),]
    
    
    plotDF3 <- vegDF[vegDF$Trt=="pct_diff"&vegDF$Variable%in%c("Tot"),]
    plotDF4 <- plotDF3

    
    
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
    
    
    ### delete unwanted points
    plotDF2$meanvalue[plotDF2$Group%in%c("C_GDAYP", "A_ELMV1",
                                         "B_CABLP", "D_LPJGP",
                                         "E_OCHDP", "F_QUINC",
                                         "G_OCHDX", "H_QUJSM")] <- NA
    
    plotDF4$meanvalue[plotDF4$Group%in%c("C_GDAYP", "A_ELMV1",
                                         "B_CABLP", "D_LPJGP",
                                         "E_OCHDP", "F_QUINC",
                                         "G_OCHDX", "H_QUJSM")] <- NA
    
    ### Plotting
    p1 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        geom_errorbar(data=plotDF2, 
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        #geom_point(data=plotDF2,aes(Group, meanvalue), 
        #           size=2,fill="white", pch=21, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position=c(.85,.2),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Carbon pools (g C " * m^2*")")))+
        scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                         label=c(model.labels, "multi-model"=expression(bold("M-M")),
                                 "obs" = expression(bold("OBS"))))+
        guides(fill=guide_legend(nrow=3))+
        scale_fill_manual(name=expression(C[veg]),
                          values=c("CL"=cbbPalette[2],
                                   "CW"=cbbPalette[3],
                                   "CFR"=cbbPalette[4],
                                   "CCR"=cbbPalette[7],
                                   "CSTOR"=cbbPalette[8]),
                          labels=c("CL"=expression(C[leaf]), 
                                   "CW"=expression(C[wood]), 
                                   "CFR"=expression(C[froot]), 
                                   "CCR"=expression(C[croot]),
                                   "CSTOR"=expression(C[store])));p1
    
    
    p2 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group),
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        #geom_point(aes(Group, meanvalue), 
        #           size=2,fill="white", pch=21, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
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
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "multi-model"="grey30", "obs"="grey"))),
               color = guide_legend(nrow=12, byrow=F)); p2
    
    
    
    
    pdf(paste0(out.dir, "/MIP_", scenario, "_Cveg_response_multi-model.pdf"), 
        width=18, height=6)
    plot_grid(p1,p2,
        labels="auto", label_x=0.1, label_y=0.95,
        label_size=24,
        ncol=2)
    dev.off()
    
    
    
}
