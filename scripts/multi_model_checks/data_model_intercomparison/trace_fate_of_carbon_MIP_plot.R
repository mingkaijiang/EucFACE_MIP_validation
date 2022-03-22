trace_fate_of_carbon_MIP_plot <- function(scenario) {
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario, "/")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_ELE_annual.rds"))
    
    
    ### calculate 4-yr means in the simulation datasets
    ambDF <- subset(ambDF, YEAR>2012 & YEAR<2017)
    eleDF <- subset(eleDF, YEAR>2012 & YEAR<2017)
    
    d<-dim(ambDF)[2]
    
    ### calculate CO2 effect (real magnitude)
    co2DF <- ambDF
    co2DF[,3:d] <- eleDF[,3:d]-ambDF[,3:d]
    
    ### prepare the variables to extract
    var.list <- c("ModName", "YEAR", 
                  "GPP", "NPP",
                  "RAU", "RHET", "CVOC",
                  "CGL", "CGW", "CGFR", "CGCR", "CEX", "CREPR", 
                  "CLITIN", "CWLIN", "CFRLIN", "CCRLIN", 
                  "CL", "CW", "CFR", "CCR", "CSOIL", "CFLIT", "CSTOR",
                  "deltaCL", "deltaCW", "deltaCFR",
                  "deltaCCR", "deltaCSOIL", "deltaCFLIT", "deltaCSTOR")
    
    ### extract only a subset of variables
    tmpDF1 <- ambDF[,var.list]
    tmpDF2 <- eleDF[,var.list]
    tmpDF3 <- co2DF[,var.list]
    
    
    ### get some descriptive numbers
    d <- dim(tmpDF3)[2]
    mod.list <- unique(tmpDF3$ModName)
    nmod <- length(mod.list)
    
    ### convert into long format
    longDF1 <- reshape2::melt(tmpDF1, id.vars = c("ModName", "YEAR"))
    longDF2 <- reshape2::melt(tmpDF2, id.vars = c("ModName", "YEAR"))
    longDF3 <- reshape2::melt(tmpDF3, id.vars = c("ModName", "YEAR"))
    
    sumDF1 <- summaryBy(value~ModName+variable, FUN=c(mean, sd),
                       data=longDF1, na.rm=T, keep.names=T)
    
    sumDF2 <- summaryBy(value~ModName+variable, FUN=c(mean, sd),
                       data=longDF2, na.rm=T, keep.names=T)
    
    sumDF3 <- summaryBy(value~ModName+variable, FUN=c(mean, sd),
                      data=longDF3, na.rm=T, keep.names=T)
    
    
    ### prepare GPP = NPP + Ra plot
    ### and GPP = R + delta C pools
    gppDF <- prepare_basic_GPP_plot(ambDF=sumDF1, eleDF=sumDF2)
    
    ### plotting script, GPP
    gppciDF <- gppDF$ciDF
    gppstackDF <- gppDF$stackDF
    
    ### work on co2 effect - i.e. fate of C
    fateDF <- prepare_fate_of_C_plot(inDF=sumDF3)
    
    ### plotting script, GPP
    fateciDF <- fateDF$ciDF
    fatestackDF <- fateDF$stackDF
    
    
    ### allocation coefficients
    allocDF <- prepare_alloc_for_individual_model_plot(ambDF=sumDF1,
                                                       eleDF=sumDF2)
    
    
    
    
    ### loop through each model to plot
    for (i in mod.list) {
        
        ### get the data
        ## gpp
        plotDF1 <- gppciDF[gppciDF$ModName==i,]
        plotDF2 <- gppstackDF[gppstackDF$ModName==i,]
        
        ## fate of C
        plotDF3 <- fateciDF[fateciDF$ModName==i,]
        plotDF4 <- fatestackDF[fatestackDF$ModName==i,]
        
        ### alloc
        plotDF5 <- allocDF[allocDF$ModName==i,]
        
        ### aCO2 vs. eCO2
        p1 <- ggplot() +  
            geom_bar(plotDF2, stat = "identity", 
                     mapping=aes(Method, value.mean, fill=variable),
                     position="stack", col="black") +
            geom_errorbar(data=plotDF1, 
                          mapping=aes(x=Method, ymin=value.mean-value.sd, 
                                      ymax=value.mean+value.sd), 
                          width=0.1, size=0.6, color="black") + 
            geom_point(data=plotDF1, 
                       mapping=aes(x=Method, y=value.mean), 
                       size=2, shape=21, fill="white", col="black")+
            xlab("") + 
            facet_wrap( ~ Trt)+
            ylab(expression(paste("GPP (g C ", m^-2, " ", yr^-1, ")"))) +
            scale_fill_manual(name="Variable",
                              values=c("GPP"=GreensPalette[1],
                                       "NPP"=GreensPalette[3],
                                       "RAU"=YlOrRdPalette[2],
                                       "RHET"=YlOrRdPalette[6],
                                       "deltaCVEG"=GreensPalette[7],
                                       "deltaCSTOR"=GreensPalette[5],
                                       "deltaCSOIL"=GreensPalette[8]),
                              labels=c("GPP"="GPP", 
                                       "NPP"="NPP",
                                       "RAU"="RAU",
                                       "RHET"="RHET",
                                       "deltaCVEG"=expression(Delta * C[VEG]),
                                       "deltaCSTOR"=expression(Delta * C[STOR]),
                                       "deltaCSOIL"=expression(Delta * C[SOIL])))+
            scale_x_discrete(breaks=c("GPP", "NPP+RAU", "R+deltaC"),
                             labels=c("GPP"="GPP",
                                      "NPP+RAU"="NPP+RAU",
                                      "R+deltaC"=expression("R+"*Delta*"C"))) +
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.title.x = element_text(size=14), 
                  axis.text.x = element_text(size=14),
                  axis.text.y=element_text(size=14),
                  axis.title.y=element_text(size=14),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=14),
                  panel.grid.major=element_blank(),
                  legend.position="none",
                  legend.text.align=0,
                  strip.text.x = element_text(size = 20))
        
        ### CO2 effect
        p2 <- ggplot() +  
            geom_hline(yintercept=0)+
            geom_bar(plotDF4, stat = "identity", 
                     mapping=aes(Method, value.mean, fill=variable),
                     position="stack", col="black") +
            geom_errorbar(data=plotDF3, 
                          mapping=aes(x=Method, ymin=value.mean-value.sd, 
                                      ymax=value.mean+value.sd), 
                          width=0.1, size=0.6, color="black") + 
            geom_point(data=plotDF3, 
                       mapping=aes(x=Method, y=value.mean), 
                       size=2, shape=21, fill="white", col="black")+
            xlab("") + 
            ylab(expression(paste(CO[2] * " effect (g C ", m^-2, " ", yr^-1, ")"))) +
            scale_fill_manual(name="Variable",
                              values=c("GPP"=GreensPalette[1],
                                       "NPP"=GreensPalette[3],
                                       "RAU"=YlOrRdPalette[2],
                                       "RHET"=YlOrRdPalette[6],
                                       "deltaCVEG"=GreensPalette[7],
                                       "deltaCSTOR"=GreensPalette[5],
                                       "deltaCSOIL"=GreensPalette[8]),
                              labels=c("GPP"="GPP", 
                                       "NPP"="NPP",
                                       "RAU"="RAU",
                                       "RHET"="RHET",
                                       "deltaCVEG"=expression(Delta * C[VEG]),
                                       "deltaCSTOR"=expression(Delta * C[STOR]),
                                       "deltaCSOIL"=expression(Delta * C[SOIL])))+
            scale_x_discrete(breaks=c("GPP", "NPP+RAU", "R+deltaC"),
                             labels=c("GPP"="GPP",
                                      "NPP+RAU"="NPP+RAU",
                                      "R+deltaC"=expression("R+"*Delta*"C"))) +
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.title.x = element_text(size=14), 
                  axis.text.x = element_text(size=14),
                  axis.text.y=element_text(size=14),
                  axis.title.y=element_text(size=14),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=14),
                  panel.grid.major=element_blank(),
                  legend.position="right",
                  legend.text.align=0)#+
        #scale_y_continuous(limits=c(-205, 600), 
        #                   breaks=c(-200, -100, 0, 100, 200, 400, 600),
        #                   labels=c(-200, -100, 0, 100, 200, 400, 600))+
        #guides(fill=guide_legend(ncol=2),legend.justification = c(0, 1))+
        #annotate(geom="text", x=1, y=-100, label="CABLP", size=7); p2
        
        
        
        
        p3 <- ggplot(data=plotDF5, 
                      aes(ymax=ymax, ymin=ymin, 
                          xmax=4, xmin=3, fill=Variable)) +
          geom_rect() +
          geom_label( x=3.5, aes(y=labelPosition, label=label)) +
          coord_polar(theta="y") + 
          xlim(c(2, 4)) +
          facet_wrap( ~ Trt)+
          #theme_void() +
          theme(axis.ticks = element_blank(),
                panel.grid.minor=element_blank(),
                axis.text.x=element_blank(),
                axis.title.x=element_text(size=14),
                axis.text.y=element_blank(),
                axis.title.y=element_text(size=14),
                legend.text=element_text(size=12),
                legend.title=element_text(size=14),
                panel.grid.major=element_blank(),
                legend.position="none",
                legend.box = 'horizontal',
                legend.box.just = 'left',
                plot.title = element_text(size=14, face="bold.italic", 
                                          hjust = 0.5),
                strip.text.x = element_text(size = 20))+
          ylab("Allocation coefficient")+
          scale_fill_manual(name="Variable",
                            values=c("Canopy"=cbbPalette[4], 
                                     "Wood"=cbbPalette[3],
                                     "Root"=cbbPalette[8],
                                     "Other"=cbbPalette[5]))+
          guides(fill=guide_legend(nrow=2))
        
        
        
        lay <- rbind(c(1,1),
                     c(2,3))
        
        ### Plotting
        pdf(paste0(out.dir, "/fate_of_C_", i, ".pdf"), width=12, height=10)
        
        #plot_grid(p1, p2,
        #          ncol=1)
        
        grid.arrange(p1, p2, p3, layout_matrix=lay)
        dev.off()
        
    }
    
  
    
    #### Normalize to the CO2 effect on GPP
    subDF1 <- subset(fatestackDF, variable=="GPP")
    subDF2 <- subset(fatestackDF, variable!="GPP")
    
    for (i in mod.list) {
      subDF2$norm.value[subDF2$ModName==i]<-subDF2$value.mean[subDF2$ModName==i]/subDF1$value.mean[subDF1$ModName==i]
    }
    
    plotDF1 <- subset(subDF2, Method=="NPP+RAU")
    plotDF2 <- subset(subDF2, Method=="R+deltaC")
    
    
    ### plotting
    p1 <- ggplot(data=plotDF1, 
                 aes(ModName, norm.value, group=variable)) +
      geom_hline(yintercept=c(0,1), lty=1)+
      geom_bar(stat = "identity", aes(fill=variable), 
               position=position_stack(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bottom",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("Normalized " * CO[2] * " responses")))+
      scale_fill_manual(name="Component",
                        values=c("NPP"=GreensPalette[3],
                                 "RAU"=YlOrRdPalette[2]),
                        labels=c("NPP"="NPP",
                                 "RAU"="RAU"))+
      scale_x_discrete(limit=c(mod.list),
                       label=c(model.labels))
    
    
    
    p2 <- ggplot(data=plotDF2, 
                 aes(ModName, norm.value, group=variable)) +
      geom_hline(yintercept=c(0,1), lty=1)+
      geom_bar(stat = "identity", aes(fill=variable), 
               position=position_stack(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="bottom",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("Normalized " * CO[2] * " responses")))+
      scale_fill_manual(name="Component",
                        values=c("RAU"=YlOrRdPalette[2],
                                 "RHET"=YlOrRdPalette[6],
                                 "deltaCVEG"=GreensPalette[5],
                                 "deltaCSOIL"=GreensPalette[8]),
                        labels=c("RAU"="RAU",
                                 "RHET"="RHET",
                                 "deltaCVEG"=expression(Delta * C[VEG]),
                                 "deltaCSOIL"=expression(Delta * C[SOIL])))+
      scale_x_discrete(limit=c(mod.list),
                       label=c(model.labels))
    
    
    plot(p2)
    
    
    plots_top_row <- plot_grid(p1, p2, 
                               labels=c("(a)", "(b)"),
                               ncol=1, align="vh", axis = "l",
                               label_x=0.9, label_y=0.96,
                               label_size = 18)
    
    
    pdf(paste0(out.dir, "/MIP_normalized_fate_of_C_", 
               scenario, "_comparison.pdf"), 
        width=8, height=8)
    plot_grid(plots_top_row,
              ncol=1, rel_heights=c(1,0.1))
    
    dev.off()
    
    #  end
}

