plot_MIP_CO2_response_comparison <- function (eucDF) {
    
    ##################################################################
    #### Set up basics
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/obs_var_output")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE, recursive = T)
    }
    
    
    ### read in anual datasets
    annDF.amb <- readRDS(paste0(out.dir, "/MIP_obs_var_amb_annual.rds"))
    annDF.ele <- readRDS(paste0(out.dir, "/MIP_obs_var_ele_annual.rds"))
    
    
    #### calculate 4-yr means in the simulation datasets
    annDF.amb <- subset(annDF.amb, YEAR>2012 & YEAR<2017)
    annDF.ele <- subset(annDF.ele, YEAR>2012 & YEAR<2017)
    
    annDF.pct.diff <- annDF.amb
    annDF.pct.diff[,3:149] <- (annDF.ele[,3:149]-annDF.amb[,3:149])/annDF.amb[,3:149] * 100.0
    
    annDF.amb.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                               data=annDF.amb,
                               keep.names=T, na.rm=T)
    
    annDF.ele.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                               data=annDF.ele,
                               keep.names=T, na.rm=T)
    
    annDF.diff.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                                data=annDF.pct.diff,
                                keep.names=T, na.rm=T)
    
    
    

    ### prepare simulation results
    simDF <- prepare_simulation_results_for_comparison_against_data(source.dir, mod.abb)
    
    ### merge simulation and observation datasets
    myDF <- merge_simulation_and_observation_datasets(eucDF, simDF)
    
    ### convert into long
    lDF <- melt(setDT(myDF), id.vars = c("Source","Group", "Trt"), variable.name = "Variable")
        
    ### subset for plotting
    subDF1 <- lDF[lDF$Trt%in%c("aCO2", "eCO2") & lDF$Group=="mean",]
    subDF2 <- lDF[lDF$Trt%in%c("aCO2", "eCO2") & lDF$Group=="sd",]
    subDF3 <- lDF[lDF$Trt=="pct_diff" & lDF$Group=="mean",]
    subDF4 <- lDF[lDF$Trt=="pct_diff" & lDF$Group=="sd",]
    
    ### add sd to dataset
    plotDF1 <- merge(subDF1, subDF2, by=c("Source","Trt", "Variable"), all=T)
    plotDF2 <- merge(subDF3, subDF4, by=c("Source", "Trt", "Variable"), all=T)
    
    plotDF1$Group.x <- NULL
    plotDF1$Group.y <- NULL
    plotDF2$Group.x <- NULL
    plotDF2$Group.y <- NULL
    
    colnames(plotDF1) <- colnames(plotDF2) <- c("Source", "Trt", "Variable", "meanv", "sd")
    
    
    ## reivse the model abbreviation
    if (check.mod == "QUJSM") {
        print("QUJSM model")
        mod.abb <- "QUJSM"
    } else {
        print(paste0(mod.abb, " model"))
    }
    
    
    
    ### prepare plotting
    ### plot CO2 response ratio
    pdf(paste0(out.dir, "/MIP_OBS_NOP_D_CO2_ratio_against_data.pdf"))
    
    ### all carbon pools
    subDF1 <- plotDF2[plotDF2$Variable%in%c("CL", "CW", "CFR", "CFLIT", "CSOIL"),]
        
    p1 <- ggplot(subDF1) +
        geom_bar(aes(x=Source, y=meanv, fill=Variable), 
                 stat="identity", position=position_dodge())+
        geom_errorbar(aes(x=Source, ymin = meanv-sd, ymax=meanv+sd,
                          col=Variable), width=0.2,
                      stat="identity", position=position_dodge(width=0.9))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * " response (%)"))+
        scale_x_discrete(labels = c("Obs" = "OBS", "Sim" = mod.abb))
    
    plot(p1)
    
    ### major carbon fluxes
    subDF2 <- plotDF2[plotDF2$Variable%in%c("GPP", "NPP", "RAU", "RHET", "RECO"),]
    
    p2 <- ggplot(subDF2) +
        geom_bar(aes(x=Source, y=meanv, fill=Variable), 
                 stat="identity", position=position_dodge())+
        geom_errorbar(aes(x=Source, ymin = meanv-sd, ymax=meanv+sd,
                          col=Variable), width=0.2,
                      stat="identity", position=position_dodge(width=0.9))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * " response (%)"))+
        scale_x_discrete(labels = c("Obs" = "OBS", "Sim" = mod.abb))
    
    
    plot(p2)
    
    
    ### major growth fluxes
    subDF3 <- plotDF2[plotDF2$Variable%in%c("CGL", "CGW", "CGFR", "CEX"),]
    
    p3 <- ggplot(subDF3) +
        geom_bar(aes(x=Source, y=meanv, fill=Variable), 
                 stat="identity", position=position_dodge())+
        geom_errorbar(aes(x=Source, ymin = meanv-sd, ymax=meanv+sd,
                          col=Variable), width=0.2,
                      stat="identity", position=position_dodge(width=0.9))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * " response (%)"))+
        scale_x_discrete(labels = c("Obs" = "OBS", "Sim" = mod.abb))
    
    
    plot(p3)
    
    ### delta pool and mores
    
    
    
    
    
    
    
    dev.off()
    
    
}