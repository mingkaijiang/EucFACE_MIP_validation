make_time_averaged_data_model_comparison_over_obs_period <- function(eucDF,
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
    
    
    ### check GDAY PUP
    #ambDF.sum$PUP.mean[ambDF.sum$ModName=="A_GDAYP"]
    #eleDF.sum$PUP.mean[eleDF.sum$ModName=="A_GDAYP"]
    
    
    ##########################################################################
    ### Plotting
    
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
              legend.position=c(.85,.2),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Carbon pools (g C " * m^2*")")))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        guides(fill=guide_legend(nrow=3))
    
    
    p2 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group),
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
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
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Model",
                           values=c(col.values, obs="black"),
                           labels=c(model.labels, "obs"= "OBS"))+
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "obs"="black"))),
               color = guide_legend(nrow=12, byrow=F))
    
    
    
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
        ylab(expression(paste("Carbon fluxes (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Variable",
                          values=c("NPP"="green", "RAU"="yellow"))
    
    
    p4 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
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
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, obs="black"),
                          labels=c(model.labels, "obs"= "OBS"))+
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "obs"="black"))),
               color = guide_legend(nrow=12, byrow=F))
    
    
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
    
    ### Plotting
    ### additional to-do list:
    ### 1. fill color by manual selection
    p5 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        geom_point(data=plotDF2, aes(x=Group, y=meanvalue), col="black",
                   fill="red", pch=21)+
        geom_errorbar(data=plotDF2, 
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
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
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(Delta * " Carbon pools (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))
    
    
    p6 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group),
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
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
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, obs="black"),
                          labels=c(model.labels, "obs"= "OBS"))+
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "obs"="black"))),
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
    
    
    ### plotting GPP, NPP, and RAU
    p7 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
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
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, obs="black"),
                          labels=c(model.labels, "obs"= "OBS"))+
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "obs"="black"))),
               color = guide_legend(nrow=12, byrow=F))
    
    
    p8 <- ggplot(data=plotDF2, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
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
        ylab(expression(CO[2] * " effect (g C " * m^2 * " " * yr-1 * ")"))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, obs="black"),
                          labels=c(model.labels, "obs"= "OBS"))+
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "obs"="black"))),
               color = guide_legend(nrow=12, byrow=F))
    
    
    
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
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Variable",
                          values=c("Canopy"=cbbPalette[4], 
                                   "Wood"=cbbPalette[3],
                                   "Root"=cbbPalette[8],
                                   "Other"=cbbPalette[2]))+
        guides(fill=guide_legend(nrow=2))
    
    
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
        ylab(expression(CO[2] * " effect (unitless)"))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Variable",
                          values=c("Canopy"=cbbPalette[4], 
                                   "Wood"=cbbPalette[3],
                                   "Root"=cbbPalette[8],
                                   "Other"=cbbPalette[2]))+
        guides(fill=guide_legend(nrow=2))
    
    
    pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_C_variables.pdf"), 
        width=16, height=16)
    plot_grid(p1, p2, p3, p4, p9, p10, p7, p8, 
              labels="auto", label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=2)
    dev.off()
    
    
    
    
    
    
    
    
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
              legend.position=c(0.85,0.8),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Phosphorus pools (g C " * m^2*")")))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Variable",
                          values=c("PL"=cbbPalette[4], 
                                   "PW"=cbbPalette[3],
                                   "PFR"=cbbPalette[8],
                                   "PSTOR"=cbbPalette[2],
                                   "PCR"=cbbPalette[6]))+
        guides(fill=guide_legend(nrow=3))
    
    
    p2 <- ggplot(data=plotDF4, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable),
                 position=position_dodge2(), col="black") +
        #geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
        #                  ymax=meanvalue+sdvalue), 
        #              col="black", 
        #              position=position_dodge2(), width=0.3)+
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
              legend.position=c(.85,.2),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * " effect (%)"))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Variable",
                          values=c("PL"=cbbPalette[4], 
                                   "PW"=cbbPalette[3],
                                   "PFR"=cbbPalette[8],
                                   "PSTOR"=cbbPalette[2]))+
        guides(fill=guide_legend(nrow=2))
    
    
    
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
    
    
    ### plotting 
    p3 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        geom_errorbar(data=plotDF2,
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
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
              legend.position=c(.1,.8),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Growth P fluxes (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))
    
    
    p4 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
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
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Model",
                          values=c(col.values, obs="black"),
                          labels=c(model.labels, "obs"= "OBS"))+
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "obs"="black"))),
               color = guide_legend(nrow=12, byrow=F))
    
    
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
    
    pfluxDF <- rbind(pfluxDF1, pfluxDF2)
    
    ### split into ambDF, pctDF
    plotDF1 <- pfluxDF[pfluxDF$Trt=="aCO2",]
    plotDF2 <- pfluxDF[pfluxDF$Trt=="aCO2",]
    
    plotDF3 <- pfluxDF[pfluxDF$Trt=="pct_diff",]
    
    
    ### plotting 
    p5 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position=position_dodge2(), col="black") +
        geom_errorbar(data=plotDF2,
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2())+
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
        ylab(expression(paste("Phosphorus fluxes (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))
    
    
    p6 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position=position_dodge2(), col="black") +
        #geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
        #                  ymax=meanvalue+sdvalue, grou=Variable), 
        #              col="black", 
        #              position=position_dodge2(), width=0.3)+
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
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))
    

    
    
    ################# P uptake and mineralization ####################
    budgetDF <- prepare_P_budget_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                                 ambDF=ambDF.sum,
                                                                                 eleDF=eleDF.sum,
                                                                                 difDF=annDF.diff.sum)
    
    subDF1 <- subset(budgetDF, Variable%in%c("PUPREQ"))
    
    
    plotDF1 <- subDF1[subDF1$Trt=="aCO2",]
    plotDF2 <- subDF1[subDF1$Trt=="pct_diff",]
    
    
    ### plotting 
    p7 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
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
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(P[up] * " / " * P[req]))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))
    
    
    p8 <- ggplot(data=plotDF2, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
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
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * " effect (%)"))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))
    
    
    
    ################# P uptake and mineralization ####################
    subDF2 <- subset(budgetDF, Variable%in%c("CPL", "CPW", "CPFR", "CPFLIT", "CPSOIL"))
    
    
    plotDF1 <- subDF2[subDF2$Trt=="aCO2",]
    plotDF2 <- subDF2[subDF2$Trt=="pct_diff",]
  
    
    ## stoichiometry
    p9 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
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
              legend.position=c(.75,.8),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("CP stoichiometry")+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Variable",
                          values=c("CPL"=cbbPalette[4], 
                                   "CPW"=cbbPalette[3],
                                   "CPFR"=cbbPalette[8],
                                   "CPFLIT"=cbbPalette[2],
                                   "CPSOIL"=cbbPalette[6]),
                          label=c(expression(CP[L]),
                                  expression(CP[W]),
                                  expression(CP[FR]),
                                  expression(CP[FLIT]),
                                  expression(CP[SOIL])))+
        guides(fill=guide_legend(nrow=2))
    
    
    
    
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
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * " effect (%)"))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = "OBS"))+
        scale_fill_manual(name="Variable",
                          values=c("CPL"=cbbPalette[4], 
                                   "CPW"=cbbPalette[3],
                                   "CPFR"=cbbPalette[8],
                                   "CPFLIT"=cbbPalette[2],
                                   "CPSOIL"=cbbPalette[6]),
                          label=c(expression(CP[L]),
                                  expression(CP[W]),
                                  expression(CP[FR]),
                                  expression(CP[FLIT]),
                                  expression(CP[SOIL])))+
        guides(fill=guide_legend(nrow=2))
    
    
    
    pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_P_variables.pdf"), 
        width=16, height=16)
    plot_grid(p1, p2, p5, p6, p7, p8, p9, p10,
              labels="auto", label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=2)
    dev.off()
    
    
    
    
    ###########################################################################
    ##### Step 4. Time-varying validation
    #
    ##### Leaf area index
    #### A time series LAI data over the period of 2012 - 2016 was provided for validation purpose. 
    #### Models should aim to match the magnitude of LAI as well as its temporal patterns. 
    #### Note that in the observed dataset, the LAI data is really indicative of the vegetation structure as well as canopy leaf area. 
    #### validation LAI
    #laiDF <- read.csv("validation_dataset/EucFACE_LAI_2012_2016.csv")
    #laiDF <- laiDF[laiDF$Trt=="aCO2",]
    #laiDF$Date <- as.Date(as.character(laiDF$Date))
    #laiDF$ModName <- "OBS"
    #names(laiDF)[names(laiDF)=="lai"] <- "LAI"
    #laiDF <- laiDF[,c("Date", "LAI", "ModName")]
    #
    #
    #### read in multi-model lai data
    #modDF <- readRDS(paste0(out.dir, "/MIP_obs_var_amb_daily.rds"))
    #
    #### simulated LAI, subset
    #subDF <- subset(modDF, YEAR <= 2016)
    #subDF <- subDF[,c("YEAR", "DOY", "Date", "LAI", "ModName")]
    #subDF$Date <- as.Date(as.character(subDF$Date))
    #subDF <- subDF[,c("Date", "LAI", "ModName")]
    #    
    #### there is something wrong with LPJGP-VD, we may need to do something about it later
    ##tDF <- subDF[subDF$ModName=="L_LPJGP-VD",]
    ##summary(tDF$LAI)
    #
    #### merge the two dataset
    #testDF1 <- rbind(subDF, laiDF)
    #
    #### plot all data
    #p1 <- ggplot(testDF1, aes(x=Date)) +
    #    geom_line(aes(y=LAI, color=ModName, lty=ModName), lwd = 1) +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=12),
    #          panel.grid.major=element_blank(),
    #          plot.title = element_text(size = 10, face = "bold"),
    #          legend.position="right")+
    #    scale_color_manual(name="Model",
    #                       values=c(col.values, "OBS"="black"),
    #                       labels=c(model.labels, "OBS"= "OBS"))+
    #    scale_linetype_manual(name="Model", 
    #                          values=c(linetype.values, "OBS"=1),
    #                          labels=c(model.labels, "OBS"="OBS"))+
    #    guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
    #                                                   lty = c(linetype.values, "OBS"=1))),
    #           color = guide_legend(nrow=12, byrow=F))+
    #    ylab("LAI"); p1
    #
    #
    ##### Soil respiration
    #### The measured soil respiration rate represents both root 
    #### and soil heterotrophic respiration flux. 
    #### It was up-scaled from the LICOR chambers by averaging 
    #### all measurements within the same treatment. 
    #### It was a model product, 
    #### in that we used DAMM model to establish relationship with soil temperature, 
    #### and then obtained the daily rate throughout the year. 
    #### Nevertheless, we expect modelers to provide a good match simulation to this dataset. 
    #
    #### Note that we didn't ask the modelers to output soil respiration flux in the output protocol. 
    #### Please add heterotrophic respiration and root respiration to obtain soil respiration flux. 
    #### Also, please note that, the unit for all carbon fluxes is given in the output protocol, as gC m-2 d-1. 
    #### validation Rsoil
    #rsoilDF <- read.csv("validation_dataset/EucFACE_daily_soil_respiration_flux_2013_2015.csv")
    #rsoilDF <- rsoilDF[rsoilDF$Trt=="aCO2",]
    #rsoilDF$Date <- as.Date(as.character(rsoilDF$Date))
    #
    #### convert unit, from mg m-2 d-1 to g m-2 d-1
    #rsoilDF$Rsoil <- rsoilDF$Rsoil_mg_m2_d / 1000.0
    #rsoilDF$ModName <- "OBS"
    #rsoilDF <- rsoilDF[,c("Date", "Rsoil", "ModName")]
    #
    #### simulated Rsoil, subset
    #subDF <- subset(modDF, YEAR <= 2015 & YEAR > 2012)
    #subDF <- subDF[,c("YEAR", "DOY", "Date", "RHET", "RCR", "RFR", "ModName")]
    #subDF$Date <- as.Date(as.character(subDF$Date))
    #
    #subDF[subDF<=-999.] <- NA
    #
    #subDF$Rsoil<- rowSums(data.frame(subDF$RHET, subDF$RCR, subDF$RFR), na.rm=T)
    #subDF <- subDF[,c("Date", "Rsoil", "ModName")]
    #
    #
    #### merge the two dataset
    #testDF1 <- rbind(subDF, rsoilDF)
    #
    #### plot all data
    #p2 <- ggplot(testDF1, aes(x=Date)) +
    #    geom_line(aes(y=Rsoil, color=ModName, lty=ModName), lwd = 1) +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=12),
    #          panel.grid.major=element_blank(),
    #          plot.title = element_text(size = 10, face = "bold"),
    #          legend.position="right")+
    #    scale_color_manual(name="Model",
    #                       values=c(col.values, "OBS"="black"),
    #                       labels=c(model.labels, "OBS"= "OBS"))+
    #    scale_linetype_manual(name="Model", 
    #                          values=c(linetype.values, "OBS"=1),
    #                          labels=c(model.labels, "OBS"="OBS"))+
    #    guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
    #                                                   lty = c(linetype.values, "OBS"=1))),
    #           color = guide_legend(nrow=12, byrow=F))+
    #ylab("Soil respiration flux"); p2
    #
    #
    #
    #
    #### print plots to file, change numbering if needed
    #pdf(paste0(out.dir, '/MIP_Time_varying_variables.pdf',sep=''),width=12,height=8)
    #for (i in 1:2) {
    #    print(get(paste("p",i,sep="")))
    #}
    #dev.off()
    #
    ###########################################################################
    
    
}