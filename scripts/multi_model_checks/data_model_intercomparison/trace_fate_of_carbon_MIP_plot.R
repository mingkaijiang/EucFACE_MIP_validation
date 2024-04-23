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
    
    
    
    
    #### loop through each model to plot
    #for (i in mod.list) {
    #    
    #    ### get the data
    #    ## gpp
    #    plotDF1 <- gppciDF[gppciDF$ModName==i,]
    #    plotDF2 <- gppstackDF[gppstackDF$ModName==i,]
    #    
    #    ## fate of C
    #    plotDF3 <- fateciDF[fateciDF$ModName==i,]
    #    plotDF4 <- fatestackDF[fatestackDF$ModName==i,]
    #    
    #    ### alloc
    #    plotDF5 <- allocDF[allocDF$ModName==i,]
    #    
    #    ### aCO2 vs. eCO2
    #    p1 <- ggplot() +  
    #        geom_bar(plotDF2, stat = "identity", 
    #                 mapping=aes(Method, value.mean, fill=variable),
    #                 position="stack", col="black") +
    #        geom_errorbar(data=plotDF1, 
    #                      mapping=aes(x=Method, ymin=value.mean-value.sd, 
    #                                  ymax=value.mean+value.sd), 
    #                      width=0.1, size=0.6, color="black") + 
    #        geom_point(data=plotDF1, 
    #                   mapping=aes(x=Method, y=value.mean), 
    #                   size=2, shape=21, fill="white", col="black")+
    #        xlab("") + 
    #        facet_wrap( ~ Trt)+
    #        ylab(expression(paste("GPP (g C ", m^-2, " ", yr^-1, ")"))) +
    #        scale_fill_manual(name="Variable",
    #                          values=c("GPP"=GreensPalette[1],
    #                                   "NPP"=GreensPalette[3],
    #                                   "RAU"=YlOrRdPalette[2],
    #                                   "RHET"=YlOrRdPalette[6],
    #                                   "deltaCVEG"=GreensPalette[6],
    #                                   "deltaCSTOR"=GreensPalette[9],
    #                                   "deltaCSOIL"=GreensPalette[4]),
    #                          labels=c("GPP"="GPP", 
    #                                   "NPP"="NPP",
    #                                   "RAU"="RAU",
    #                                   "RHET"="RHET",
    #                                   "deltaCVEG"=expression(Delta * C[VEG]),
    #                                   "deltaCSTOR"=expression(Delta * C[STOR]),
    #                                   "deltaCSOIL"=expression(Delta * C[SOIL])))+
    #        scale_x_discrete(breaks=c("GPP", "NPP+RAU", "R+deltaC"),
    #                         labels=c("GPP"="GPP",
    #                                  "NPP+RAU"="NPP+RAU",
    #                                  "R+deltaC"=expression("R+"*Delta*"C"))) +
    #        theme_linedraw() +
    #        theme(panel.grid.minor=element_blank(),
    #              axis.title.x = element_text(size=14), 
    #              axis.text.x = element_text(size=14),
    #              axis.text.y=element_text(size=14),
    #              axis.title.y=element_text(size=14),
    #              legend.text=element_text(size=14),
    #              legend.title=element_text(size=14),
    #              panel.grid.major=element_blank(),
    #              legend.position="none",
    #              legend.text.align=0,
    #              strip.text.x = element_text(size = 20))
    #    
    #    ### CO2 effect
    #    p2 <- ggplot() +  
    #        geom_hline(yintercept=0)+
    #        geom_bar(plotDF4, stat = "identity", 
    #                 mapping=aes(Method, value.mean, fill=variable),
    #                 position="stack", col="black") +
    #        geom_errorbar(data=plotDF3, 
    #                      mapping=aes(x=Method, ymin=value.mean-value.sd, 
    #                                  ymax=value.mean+value.sd), 
    #                      width=0.1, size=0.6, color="black") + 
    #        geom_point(data=plotDF3, 
    #                   mapping=aes(x=Method, y=value.mean), 
    #                   size=2, shape=21, fill="white", col="black")+
    #        xlab("") + 
    #        ylab(expression(paste(CO[2] * " effect (g C ", m^-2, " ", yr^-1, ")"))) +
    #        scale_fill_manual(name="Variable",
    #                          values=c("GPP"=GreensPalette[1],
    #                                   "NPP"=GreensPalette[3],
    #                                   "RAU"=YlOrRdPalette[2],
    #                                   "RHET"=YlOrRdPalette[6],
    #                                   "deltaCVEG"=GreensPalette[6],
    #                                   "deltaCSTOR"=GreensPalette[9],
    #                                   "deltaCSOIL"=GreensPalette[4]),
    #                          labels=c("GPP"="GPP", 
    #                                   "NPP"="NPP",
    #                                   "RAU"="RAU",
    #                                   "RHET"="RHET",
    #                                   "deltaCVEG"=expression(Delta * C[VEG]),
    #                                   "deltaCSTOR"=expression(Delta * C[STOR]),
    #                                   "deltaCSOIL"=expression(Delta * C[SOIL])))+
    #        scale_x_discrete(breaks=c("GPP", "NPP+RAU", "R+deltaC"),
    #                         labels=c("GPP"="GPP",
    #                                  "NPP+RAU"="NPP+RAU",
    #                                  "R+deltaC"=expression("R+"*Delta*"C"))) +
    #        theme_linedraw() +
    #        theme(panel.grid.minor=element_blank(),
    #              axis.title.x = element_text(size=14), 
    #              axis.text.x = element_text(size=14),
    #              axis.text.y=element_text(size=14),
    #              axis.title.y=element_text(size=14),
    #              legend.text=element_text(size=14),
    #              legend.title=element_text(size=14),
    #              panel.grid.major=element_blank(),
    #              legend.position="right",
    #              legend.text.align=0)#+
    #    #scale_y_continuous(limits=c(-205, 600), 
    #    #                   breaks=c(-200, -100, 0, 100, 200, 400, 600),
    #    #                   labels=c(-200, -100, 0, 100, 200, 400, 600))+
    #    #guides(fill=guide_legend(ncol=2),legend.justification = c(0, 1))+
    #    #annotate(geom="text", x=1, y=-100, label="CABLP", size=7); p2
    #    
    #    
    #    
    #    
    #    p3 <- ggplot(data=plotDF5, 
    #                  aes(ymax=ymax, ymin=ymin, 
    #                      xmax=4, xmin=3, fill=Variable)) +
    #      geom_rect() +
    #      geom_label( x=3.5, aes(y=labelPosition, label=label)) +
    #      coord_polar(theta="y") + 
    #      xlim(c(2, 4)) +
    #      facet_wrap( ~ Trt)+
    #      #theme_void() +
    #      theme(axis.ticks = element_blank(),
    #            panel.grid.minor=element_blank(),
    #            axis.text.x=element_blank(),
    #            axis.title.x=element_text(size=14),
    #            axis.text.y=element_blank(),
    #            axis.title.y=element_text(size=14),
    #            legend.text=element_text(size=12),
    #            legend.title=element_text(size=14),
    #            panel.grid.major=element_blank(),
    #            legend.position="none",
    #            legend.box = 'horizontal',
    #            legend.box.just = 'left',
    #            plot.title = element_text(size=14, face="bold.italic", 
    #                                      hjust = 0.5),
    #            strip.text.x = element_text(size = 20))+
    #      ylab("Allocation coefficient")+
    #      scale_fill_manual(name="Variable",
    #                        values=c("Canopy"=cbbPalette[4], 
    #                                 "Wood"=cbbPalette[3],
    #                                 "Root"=cbbPalette[8],
    #                                 "Other"=cbbPalette[5]))+
    #      guides(fill=guide_legend(nrow=2))
    #    
    #    
    #    
    #    lay <- rbind(c(1,1),
    #                 c(2,3))
    #    
    #    ### Plotting
    #    pdf(paste0(out.dir, "/fate_of_C_", i, ".pdf"), width=12, height=10)
    #    
    #    #plot_grid(p1, p2,
    #    #          ncol=1)
    #    
    #    grid.arrange(p1, p2, p3, layout_matrix=lay)
    #    dev.off()
    #    
    #}
    
    
    ################################################
    i <- "C_GDAYP"
    
    ### get the data
    ## gpp
    plotDF1 <- gppciDF[gppciDF$ModName==i,]
    plotDF2 <- gppstackDF[gppstackDF$ModName==i,]
    
    ## fate of C
    plotDF3 <- fateciDF[fateciDF$ModName==i,]
    plotDF4 <- fatestackDF[fatestackDF$ModName==i,]
    
    ### alloc
    plotDF5 <- allocDF[allocDF$ModName==i,]
    plotDF51 <- plotDF5[plotDF5$Trt=="amb",]
    plotDF52 <- plotDF5[plotDF5$Trt=="ele",]
    
    
    ### CO2 effect
    p_gdayp1 <- ggplot() +  
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
      #scale_fill_manual(name="Component",
      #                  values=c("GPP"=cbbPalette[6],
      #                           "NPP"=cbbPalette[3],
      #                           "RAU"=cbbPalette[2],
      #                           "RHET"=cbbPalette[8],
      #                           "deltaCVEG"=cbbPalette[4],
      #                           "deltaCSTOR"=cbbPalette[5],
      #                           "deltaCSOIL"=cbbPalette[7]),
      #                  labels=c("GPP"="GPP", 
      #                           "NPP"="NPP",
      #                           "RAU"=expression(R[auto]),
      #                           "RHET"=expression(R[het]),
      #                           "deltaCVEG"=expression(Delta * C[veg]),
      #                           "deltaCSTOR"=expression(Delta * C[lab]),
      #                           "deltaCSOIL"=expression(Delta * C[soil])))+
      scale_fill_manual(name="Component",
                        values=c(#"GPP"="#42A7F2",
                                 "NPP"="#47E3E9",
                                 "RAU"="#C9FA86",
                                 "RHET"="#F9F871",
                                 "deltaCVEG"="#8180D7",
                                 "deltaCSTOR"="#BEFCFE",
                                 "deltaCSOIL"="#FFE8D3"),
                        labels=c(#"GPP"="GPP", 
                                 "NPP"="NPP",
                                 "RAU"=expression(R[auto]),
                                 "RHET"=expression(R[het]),
                                 "deltaCVEG"=expression(Delta * C[veg]),
                                 "deltaCSTOR"=expression(Delta * C[lab]),
                                 "deltaCSOIL"=expression(Delta * C[soil])))+
      scale_x_discrete(breaks=c("GPP", "NPP+RAU", "R+deltaC"),
                       labels=c("GPP"="GPP",
                                "NPP+RAU"=expression("NPP+" * R[auto]),
                                "R+deltaC"=expression("R+"*Delta*"C"))) +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=14),
            axis.text.y=element_text(size=14),
            axis.title.y=element_blank(),
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.text.align=0)+
    scale_y_continuous(limits=c(-50, 800), 
                       breaks=c(0, 100, 200, 400, 600, 800),
                       labels=c(0, 100, 200, 400, 600, 800))+
    annotate(geom="text", x=1, y=750, label="GDAYP", size=7); p_gdayp1
    
    
    p_gdayp2 <- ggplot(data=plotDF5, 
                       aes(ymax=ymax, ymin=ymin, 
                           xmax=4, xmin=3, fill=Variable)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label=label)) +
      coord_polar(theta="y") + 
      xlim(c(2, 4)) +
      facet_wrap( ~ Trt, labeller = as_labeller(c("amb"="AMB", "ele"="ELE")))+
      theme_linedraw() +
      theme(axis.ticks = element_blank(),
            panel.grid.minor=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_text(size=14),
            axis.text.y=element_blank(),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5),
            strip.text.x = element_text(size = 20))+
      ylab("GDAYP")+
      scale_fill_manual(name="Variable",
                        values=c("Canopy"=Diverge_hsv_Palette[2], 
                                 "Wood"=Diverge_hsv_Palette[4],
                                 "Root"=Diverge_hsv_Palette[6],
                                 "Other"=Diverge_hsv_Palette[8]))+
      guides(fill=guide_legend(nrow=2))
  
    
    
  test <- subset(plotDF4, variable!="GPP")
    
  legend_plot_only <- ggplot() +  
      geom_hline(yintercept=0)+
      geom_bar(test, stat = "identity", 
               mapping=aes(Method, value.mean, fill=variable),
               position="stack", col="black") +
      #geom_errorbar(data=plotDF3, 
      #              mapping=aes(x=Method, ymin=value.mean-value.sd, 
      #                          ymax=value.mean+value.sd), 
      #              width=0.1, size=0.6, color="black") + 
      #geom_point(data=plotDF3, 
      #           mapping=aes(x=Method, y=value.mean), 
      #           size=2, shape=21, fill="white", col="black")+
      xlab("") + 
      ylab(expression(paste(CO[2] * " effect (g C ", m^-2, " ", yr^-1, ")"))) +
    scale_fill_manual(name="Component",
                      values=c(#"GPP"="#42A7F2",
                        "NPP"="#47E3E9",
                        "RAU"="#C9FA86",
                        "RHET"="#F9F871",
                        "deltaCVEG"="#8180D7",
                        "deltaCSTOR"="#BEFCFE",
                        "deltaCSOIL"="#FFE8D3"),
                      labels=c(#"GPP"="GPP", 
                        "NPP"="NPP",
                        "RAU"=expression(R[auto]),
                        "RHET"=expression(R[het]),
                        "deltaCVEG"=expression(Delta * C[veg]),
                        "deltaCSTOR"=expression(Delta * C[lab]),
                        "deltaCSOIL"=expression(Delta * C[soil])))+
      scale_x_discrete(breaks=c("GPP", "NPP+RAU", "R+deltaC"),
                       labels=c("GPP"="GPP",
                                "NPP+RAU"=expression("NPP+" * R[auto]),
                                "R+deltaC"=expression("R+"*Delta*"C"))) +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=14),
            axis.text.y=element_text(size=14),
            axis.title.y=element_blank(),
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.text.align=0)+
      scale_y_continuous(limits=c(-50, 800), 
                         breaks=c(0, 100, 200, 400, 600, 800),
                         labels=c(0, 100, 200, 400, 600, 800))+
      annotate(geom="text", x=1, y=750, label="GDAYP", size=7) 
  
    
    
    ################################################
    i <- "A_ELMV1"
    
    ### get the data
    ## gpp
    plotDF1 <- gppciDF[gppciDF$ModName==i,]
    plotDF2 <- gppstackDF[gppstackDF$ModName==i,]
    
    ## fate of C
    plotDF3 <- fateciDF[fateciDF$ModName==i,]
    plotDF4 <- fatestackDF[fatestackDF$ModName==i,]
    
    ### alloc
    plotDF5 <- allocDF[allocDF$ModName==i,]
    plotDF51 <- plotDF5[plotDF5$Trt=="amb",]
    plotDF52 <- plotDF5[plotDF5$Trt=="ele",]
    
    
    ### CO2 effect
    p_elmv11 <- ggplot() +  
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
                        values=c("GPP"="#42A7F2",
                                 "NPP"="#47E3E9",
                                 "RAU"="#C9FA86",
                                 "RHET"="#F9F871",
                                 "deltaCVEG"="#8180D7",
                                 "deltaCSTOR"="#BEFCFE",
                                 "deltaCSOIL"="#FFE8D3"),
                        labels=c("GPP"="GPP", 
                                 "NPP"="NPP",
                                 "RAU"="RAU",
                                 "RHET"="RHET",
                                 "deltaCVEG"=expression(Delta * C[veg]),
                                 "deltaCSTOR"=expression(Delta * C[lab]),
                                 "deltaCSOIL"=expression(Delta * C[soil])))+
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
            legend.text.align=0)+
      scale_y_continuous(limits=c(-50, 800), 
                         breaks=c(0, 100, 200, 400, 600, 800),
                         labels=c(0, 100, 200, 400, 600, 800))+
      annotate(geom="text", x=1, y=750, label="ELMV1", size=7)
    
    
    p_elmv12 <- ggplot(data=plotDF5, 
                       aes(ymax=ymax, ymin=ymin, 
                           xmax=4, xmin=3, fill=Variable)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label=label)) +
      coord_polar(theta="y") + 
      xlim(c(2, 4)) +
      facet_wrap( ~ Trt, labeller = as_labeller(c("amb"="AMB", "ele"="ELE")))+
      theme_linedraw() +
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
      ylab("ELMV1")+
      scale_fill_manual(name="Variable",
                        values=c("Canopy"=Diverge_hsv_Palette[2], 
                                 "Wood"=Diverge_hsv_Palette[4],
                                 "Root"=Diverge_hsv_Palette[6],
                                 "Other"=Diverge_hsv_Palette[8]))+
      guides(fill=guide_legend(nrow=2))
    
    
    
    ################################################
    i <- "B_CABLP"
    
    ### get the data
    ## gpp
    plotDF1 <- gppciDF[gppciDF$ModName==i,]
    plotDF2 <- gppstackDF[gppstackDF$ModName==i,]
    
    ## fate of C
    plotDF3 <- fateciDF[fateciDF$ModName==i,]
    plotDF4 <- fatestackDF[fatestackDF$ModName==i,]
    
    ### alloc
    plotDF5 <- allocDF[allocDF$ModName==i,]
    plotDF51 <- plotDF5[plotDF5$Trt=="amb",]
    plotDF52 <- plotDF5[plotDF5$Trt=="ele",]
    
    
    ### CO2 effect
    p_cablp1 <- ggplot() +  
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
                        values=c("GPP"="#42A7F2",
                                 "NPP"="#47E3E9",
                                 "RAU"="#C9FA86",
                                 "RHET"="#F9F871",
                                 "deltaCVEG"="#8180D7",
                                 "deltaCSTOR"="#BEFCFE",
                                 "deltaCSOIL"="#FFE8D3"),
                        labels=c("GPP"="GPP", 
                                 "NPP"="NPP",
                                 "RAU"="RAU",
                                 "RHET"="RHET",
                                 "deltaCVEG"=expression(Delta * C[veg]),
                                 "deltaCSTOR"=expression(Delta * C[lab]),
                                 "deltaCSOIL"=expression(Delta * C[soil])))+
      scale_x_discrete(breaks=c("GPP", "NPP+RAU", "R+deltaC"),
                       labels=c("GPP"="GPP",
                                "NPP+RAU"="NPP+RAU",
                                "R+deltaC"=expression("R+"*Delta*"C"))) +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=14),
            axis.text.y=element_text(size=14),
            axis.title.y=element_blank(),
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.text.align=0)+
      scale_y_continuous(limits=c(-50, 800), 
                         breaks=c(0, 100, 200, 400, 600, 800),
                         labels=c(0, 100, 200, 400, 600, 800))+
      annotate(geom="text", x=1, y=750, label="CABLP", size=7)
    
    
    p_cablp2 <- ggplot(data=plotDF5, 
                       aes(ymax=ymax, ymin=ymin, 
                           xmax=4, xmin=3, fill=Variable)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label=label)) +
      coord_polar(theta="y") + 
      xlim(c(2, 4)) +
      facet_wrap( ~ Trt, labeller = as_labeller(c("amb"="AMB", "ele"="ELE")))+
      #theme_void() +
      theme_linedraw() +
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
      ylab("CABLP")+
      scale_fill_manual(name="Variable",
                        values=c("Canopy"=Diverge_hsv_Palette[2], 
                                 "Wood"=Diverge_hsv_Palette[4],
                                 "Root"=Diverge_hsv_Palette[6],
                                 "Other"=Diverge_hsv_Palette[8]))+
      guides(fill=guide_legend(nrow=2))
    

    
    
    ################################################
    i <- "D_LPJGP"
    
    ### get the data
    ## gpp
    plotDF1 <- gppciDF[gppciDF$ModName==i,]
    plotDF2 <- gppstackDF[gppstackDF$ModName==i,]
    
    ## fate of C
    plotDF3 <- fateciDF[fateciDF$ModName==i,]
    plotDF4 <- fatestackDF[fatestackDF$ModName==i,]
    
    ### alloc
    plotDF5 <- allocDF[allocDF$ModName==i,]
    plotDF51 <- plotDF5[plotDF5$Trt=="amb",]
    plotDF52 <- plotDF5[plotDF5$Trt=="ele",]
    
    
    ### CO2 effect
    p_lpjgp1 <- ggplot() +  
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
                        values=c("GPP"="#42A7F2",
                                 "NPP"="#47E3E9",
                                 "RAU"="#C9FA86",
                                 "RHET"="#F9F871",
                                 "deltaCVEG"="#8180D7",
                                 "deltaCSTOR"="#BEFCFE",
                                 "deltaCSOIL"="#FFE8D3"),
                        labels=c("GPP"="GPP", 
                                 "NPP"="NPP",
                                 "RAU"="RAU",
                                 "RHET"="RHET",
                                 "deltaCVEG"=expression(Delta * C[veg]),
                                 "deltaCSTOR"=expression(Delta * C[lab]),
                                 "deltaCSOIL"=expression(Delta * C[soil])))+
      scale_x_discrete(breaks=c("GPP", "NPP+RAU", "R+deltaC"),
                       labels=c("GPP"="GPP",
                                "NPP+RAU"="NPP+RAU",
                                "R+deltaC"=expression("R+"*Delta*"C"))) +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=14),
            axis.text.y=element_text(size=14),
            axis.title.y=element_blank(),
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.text.align=0)+
      scale_y_continuous(limits=c(-50, 800), 
                         breaks=c(0, 100, 200, 400, 600, 800),
                         labels=c(0, 100, 200, 400, 600, 800))+
      annotate(geom="text", x=1, y=750, label="LPJGP", size=7)
    
    

    p_lpjgp2 <- ggplot(data=plotDF5, 
                       aes(ymax=ymax, ymin=ymin, 
                           xmax=4, xmin=3, fill=Variable)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label=label)) +
      coord_polar(theta="y") + 
      xlim(c(2, 4)) +
      facet_wrap( ~ Trt, labeller = as_labeller(c("amb"="AMB", "ele"="ELE")))+
      #theme_void() +
      theme_linedraw() +
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
      ylab("LPJGP")+
      scale_fill_manual(name="Variable",
                        values=c("Canopy"=Diverge_hsv_Palette[2], 
                                 "Wood"=Diverge_hsv_Palette[4],
                                 "Root"=Diverge_hsv_Palette[6],
                                 "Other"=Diverge_hsv_Palette[8]))+
      guides(fill=guide_legend(nrow=2))
    
    plot(p_lpjgp2)
    
    
    
    
    ################################################
    i <- "E_OCHDP"
    
    ### get the data
    ## gpp
    plotDF1 <- gppciDF[gppciDF$ModName==i,]
    plotDF2 <- gppstackDF[gppstackDF$ModName==i,]
    
    ## fate of C
    plotDF3 <- fateciDF[fateciDF$ModName==i,]
    plotDF4 <- fatestackDF[fatestackDF$ModName==i,]
    
    ### alloc
    plotDF5 <- allocDF[allocDF$ModName==i,]
    plotDF51 <- plotDF5[plotDF5$Trt=="amb",]
    plotDF52 <- plotDF5[plotDF5$Trt=="ele",]
    
    
    ### CO2 effect
    p_ochdp1 <- ggplot() +  
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
                        values=c("GPP"="#42A7F2",
                                 "NPP"="#47E3E9",
                                 "RAU"="#C9FA86",
                                 "RHET"="#F9F871",
                                 "deltaCVEG"="#8180D7",
                                 "deltaCSTOR"="#BEFCFE",
                                 "deltaCSOIL"="#FFE8D3"),
                        labels=c("GPP"="GPP", 
                                 "NPP"="NPP",
                                 "RAU"="RAU",
                                 "RHET"="RHET",
                                 "deltaCVEG"=expression(Delta * C[veg]),
                                 "deltaCSTOR"=expression(Delta * C[lab]),
                                 "deltaCSOIL"=expression(Delta * C[soil])))+
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
            legend.text.align=0)+
      scale_y_continuous(limits=c(-50, 800), 
                         breaks=c(0, 100, 200, 400, 600, 800),
                         labels=c(0, 100, 200, 400, 600, 800))+
      annotate(geom="text", x=1, y=750, label="OCHDP", size=7)
    
    
    p_ochdp2 <- ggplot(data=plotDF5, 
                       aes(ymax=ymax, ymin=ymin, 
                           xmax=4, xmin=3, fill=Variable)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label=label)) +
      coord_polar(theta="y") + 
      xlim(c(2, 4)) +
      facet_wrap( ~ Trt, labeller = as_labeller(c("amb"="AMB", "ele"="ELE")))+
      theme_linedraw() +
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
      ylab("OCHDP")+
      scale_fill_manual(name="Variable",
                        values=c("Canopy"=Diverge_hsv_Palette[2], 
                                 "Wood"=Diverge_hsv_Palette[4],
                                 "Root"=Diverge_hsv_Palette[6],
                                 "Other"=Diverge_hsv_Palette[8]))+
      guides(fill=guide_legend(nrow=2))
    
    
    
    ################################################
    i <- "F_QUINC"
    
    ### get the data
    ## gpp
    plotDF1 <- gppciDF[gppciDF$ModName==i,]
    plotDF2 <- gppstackDF[gppstackDF$ModName==i,]
    
    ## fate of C
    plotDF3 <- fateciDF[fateciDF$ModName==i,]
    plotDF4 <- fatestackDF[fatestackDF$ModName==i,]
    
    ### alloc
    plotDF5 <- allocDF[allocDF$ModName==i,]
    plotDF51 <- plotDF5[plotDF5$Trt=="amb",]
    plotDF52 <- plotDF5[plotDF5$Trt=="ele",]
    
    
    ### CO2 effect
    p_quinc1 <- ggplot() +  
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
                        values=c("GPP"="#42A7F2",
                                 "NPP"="#47E3E9",
                                 "RAU"="#C9FA86",
                                 "RHET"="#F9F871",
                                 "deltaCVEG"="#8180D7",
                                 "deltaCSTOR"="#BEFCFE",
                                 "deltaCSOIL"="#FFE8D3"),
                        labels=c("GPP"="GPP", 
                                 "NPP"="NPP",
                                 "RAU"="RAU",
                                 "RHET"="RHET",
                                 "deltaCVEG"=expression(Delta * C[veg]),
                                 "deltaCSTOR"=expression(Delta * C[lab]),
                                 "deltaCSOIL"=expression(Delta * C[soil])))+
      scale_x_discrete(breaks=c("GPP", "NPP+RAU", "R+deltaC"),
                       labels=c("GPP"="GPP",
                                "NPP+RAU"="NPP+RAU",
                                "R+deltaC"=expression("R+"*Delta*"C"))) +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=14),
            axis.text.y=element_text(size=14),
            axis.title.y=element_blank(),
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.text.align=0)+
      scale_y_continuous(limits=c(-50, 800), 
                         breaks=c(0, 100, 200, 400, 600, 800),
                         labels=c(0, 100, 200, 400, 600, 800))+
      annotate(geom="text", x=1, y=750, label="QUINC", size=7)
    
    

    p_quinc2 <- ggplot(data=plotDF5, 
                       aes(ymax=ymax, ymin=ymin, 
                           xmax=4, xmin=3, fill=Variable)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label=label)) +
      coord_polar(theta="y") + 
      xlim(c(2, 4)) +
      facet_wrap( ~ Trt, labeller = as_labeller(c("amb"="AMB", "ele"="ELE")))+
      theme_linedraw() +
      theme(axis.ticks = element_blank(),
            panel.grid.minor=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=14),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5),
            strip.text.x = element_text(size = 20))+
      ylab("QUINC")+
      scale_fill_manual(name="Variable",
                        values=c("Canopy"=Diverge_hsv_Palette[2], 
                                 "Wood"=Diverge_hsv_Palette[4],
                                 "Root"=Diverge_hsv_Palette[6],
                                 "Other"=Diverge_hsv_Palette[8]))+
      guides(fill=guide_legend(nrow=2))
    
    
    ################################################
    i <- "G_OCHDX"
    
    ### get the data
    ## gpp
    plotDF1 <- gppciDF[gppciDF$ModName==i,]
    plotDF2 <- gppstackDF[gppstackDF$ModName==i,]
    
    ## fate of C
    plotDF3 <- fateciDF[fateciDF$ModName==i,]
    plotDF4 <- fatestackDF[fatestackDF$ModName==i,]
    
    ### alloc
    plotDF5 <- allocDF[allocDF$ModName==i,]
    plotDF51 <- plotDF5[plotDF5$Trt=="amb",]
    plotDF52 <- plotDF5[plotDF5$Trt=="ele",]
    
    
    ### CO2 effect
    p_ochdx1 <- ggplot() +  
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
                        values=c("GPP"="#42A7F2",
                                 "NPP"="#47E3E9",
                                 "RAU"="#C9FA86",
                                 "RHET"="#F9F871",
                                 "deltaCVEG"="#8180D7",
                                 "deltaCSTOR"="#BEFCFE",
                                 "deltaCSOIL"="#FFE8D3"),
                        labels=c("GPP"="GPP", 
                                 "NPP"="NPP",
                                 "RAU"="RAU",
                                 "RHET"="RHET",
                                 "deltaCVEG"=expression(Delta * C[veg]),
                                 "deltaCSTOR"=expression(Delta * C[lab]),
                                 "deltaCSOIL"=expression(Delta * C[soil])))+
      scale_x_discrete(breaks=c("GPP", "NPP+RAU", "R+deltaC"),
                       labels=c("GPP"="GPP",
                                "NPP+RAU"="NPP+RAU",
                                "R+deltaC"=expression("R+"*Delta*"C"))) +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=14),
            axis.text.y=element_text(size=14),
            axis.title.y=element_blank(),
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.text.align=0)+
      scale_y_continuous(limits=c(-50, 800), 
                         breaks=c(0, 100, 200, 400, 600, 800),
                         labels=c(0, 100, 200, 400, 600, 800))+
      annotate(geom="text", x=1, y=750, label="OCHDX", size=7)
    
    
    p_ochdx2 <- ggplot(data=plotDF5, 
                       aes(ymax=ymax, ymin=ymin, 
                           xmax=4, xmin=3, fill=Variable)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label=label)) +
      coord_polar(theta="y") + 
      xlim(c(2, 4)) +
      facet_wrap( ~ Trt, labeller = as_labeller(c("amb"="AMB", "ele"="ELE")))+
      theme_linedraw() +
      theme(axis.ticks = element_blank(),
            panel.grid.minor=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=14),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5),
            strip.text.x = element_text(size = 20))+
      ylab("OCHDX")+
      scale_fill_manual(name="Variable",
                        values=c("Canopy"=Diverge_hsv_Palette[2], 
                                 "Wood"=Diverge_hsv_Palette[4],
                                 "Root"=Diverge_hsv_Palette[6],
                                 "Other"=Diverge_hsv_Palette[8]))+
      guides(fill=guide_legend(nrow=2))
    
    
    ################################################
    i <- "H_QUJSM"
    
    ### get the data
    ## gpp
    plotDF1 <- gppciDF[gppciDF$ModName==i,]
    plotDF2 <- gppstackDF[gppstackDF$ModName==i,]
    
    ## fate of C
    plotDF3 <- fateciDF[fateciDF$ModName==i,]
    plotDF4 <- fatestackDF[fatestackDF$ModName==i,]
    
    ### alloc
    plotDF5 <- allocDF[allocDF$ModName==i,]
    plotDF51 <- plotDF5[plotDF5$Trt=="amb",]
    plotDF52 <- plotDF5[plotDF5$Trt=="ele",]
    
    
    ### CO2 effect
    p_qujsm1 <- ggplot() +  
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
                        values=c("GPP"="#42A7F2",
                                 "NPP"="#47E3E9",
                                 "RAU"="#C9FA86",
                                 "RHET"="#F9F871",
                                 "deltaCVEG"="#8180D7",
                                 "deltaCSTOR"="#BEFCFE",
                                 "deltaCSOIL"="#FFE8D3"),
                        labels=c("GPP"="GPP", 
                                 "NPP"="NPP",
                                 "RAU"="RAU",
                                 "RHET"="RHET",
                                 "deltaCVEG"=expression(Delta * C[veg]),
                                 "deltaCSTOR"=expression(Delta * C[lab]),
                                 "deltaCSOIL"=expression(Delta * C[soil])))+
      scale_x_discrete(breaks=c("GPP", "NPP+RAU", "R+deltaC"),
                       labels=c("GPP"="GPP",
                                "NPP+RAU"="NPP+RAU",
                                "R+deltaC"=expression("R+"*Delta*"C"))) +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=14),
            axis.text.y=element_text(size=14),
            axis.title.y=element_blank(),
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.text.align=0)+
      scale_y_continuous(limits=c(-50, 800), 
                         breaks=c(0, 100, 200, 400, 600, 800),
                         labels=c(0, 100, 200, 400, 600, 800))+
      annotate(geom="text", x=1, y=750, label="QUJSM", size=7)
    
  
    p_qujsm2 <- ggplot(data=plotDF5, 
                       aes(ymax=ymax, ymin=ymin, 
                           xmax=4, xmin=3, fill=Variable)) +
      geom_rect() +
      geom_label(x=3.5, aes(y=labelPosition, label=label)) +
      coord_polar(theta="y") + 
      xlim(c(2, 4)) +
      facet_wrap( ~ Trt, labeller = as_labeller(c("amb"="AMB", "ele"="ELE")))+
      theme_linedraw() +
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
      ylab("QUJSM")+
      scale_fill_manual(name="Variable",
                        values=c("Canopy"=Diverge_hsv_Palette[2], 
                                 "Wood"=Diverge_hsv_Palette[4],
                                 "Root"=Diverge_hsv_Palette[6],
                                 "Other"=Diverge_hsv_Palette[8]))+
      guides(fill=guide_legend(nrow=2))
    
    
    
    
    
    ##############
    plots_legend_row <-  get_legend(p_gdayp1 + theme(legend.position="bottom",
                                                     legend.box = 'horizontal',
                                                     legend.box.just = 'left')
                                    + guides(fill=guide_legend(nrow=1,byrow=TRUE)))
    
    plots_2_row <- plot_grid(p_elmv11, p_cablp1, p_gdayp1, p_lpjgp1, 
                             labels=c("A", "B", "C", "D"),
                             ncol=4, align="vh", axis = "l",
                             label_x=0.85, label_y=0.98,
                             label_size = 18)
    
    
    plots_4_row <- plot_grid(p_ochdp1, p_quinc1, p_ochdx1, p_qujsm1, 
                             labels=c("E", "F", "G", "H"),
                             ncol=4, align="vh", axis = "l",
                             label_x=0.85, label_y=0.98,
                             label_size = 18)
    
    
    plots_3_row <- plot_grid(p_elmv12, p_cablp2, p_gdayp2, p_lpjgp2,
                             labels=c(""),
                             ncol=4, align="vh", axis = "l",
                             label_x=0.86, label_y=0.98,
                             label_size = 18)
    
    plots_5_row <- plot_grid(p_ochdp2, p_quinc2, p_ochdx2, p_qujsm2,
                             labels=c(""),
                             ncol=4, align="vh", axis = "l",
                             label_x=0.86, label_y=0.98,
                             label_size = 18)
    
    #plot_mm_row <- plot_grid()
    
    
    pdf(paste0(out.dir, "/MIP_normalized_fate_of_C_", 
               scenario, "_comparison3.pdf"), 
        width=16, height=8)
    plot_grid(plots_2_row,
              plots_4_row,
              plots_legend_row,
              ncol=1, rel_heights=c(1, 1,0.2))
    
    dev.off()
    
    pdf(paste0(out.dir, "/MIP_normalized_fate_of_C_", 
               scenario, "_comparison4.pdf"), 
        width=16, height=8)
    plot_grid(plots_3_row,
              plots_5_row,
              ncol=1, rel_heights=c(1, 1))
    
    dev.off()
    
    
    
    
    
  
    ####################################################################################################################################
    
    #### Normalize to the CO2 effect on GPP
    subDF1 <- subset(fatestackDF, variable=="GPP")
    subDF2 <- subset(fatestackDF, variable!="GPP")
    
    for (i in mod.list) {
      subDF2$norm.value[subDF2$ModName==i]<-subDF2$value.mean[subDF2$ModName==i]/subDF1$value.mean[subDF1$ModName==i]
    }
    
    plotDF1 <- subset(subDF2, Method=="NPP+RAU")
    plotDF2 <- subset(subDF2, Method=="R+deltaC")
    
    
    ### only include CNP models
    plotDF1 <- plotDF1[plotDF1$ModName!="I_GDAYN",]
    plotDF2 <- plotDF2[plotDF2$ModName!="I_GDAYN",]
    
    plotDF1 <- plotDF1[plotDF1$ModName!="J_LPJGN",]
    plotDF2 <- plotDF2[plotDF2$ModName!="J_LPJGN",]
    
    
    ### add obs
    myobsDF <- add_obs_normalized_allocation_response()
    
    
    
    ### add observation
    mod.list.rev <- mod.list[1:8]
    
    ### merge
    plotDF1 <- plotDF1[,c("ModName", "variable", "norm.value")]
    plotDF2 <- plotDF2[,c("ModName", "variable", "norm.value")]
    
    plotDF1 <- rbind(plotDF1, myobsDF$outDF1)
    plotDF2 <- rbind(plotDF2, myobsDF$outDF2)
    
    
    ### add multi-model means
    tmpDF <- plotDF1[plotDF1$ModName%in%c("C_GDAYP", "A_ELMV1",
                                          "B_CABLP", "D_LPJGP",
                                          "E_OCHDP", "F_QUINC",
                                          "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(norm.value~variable, FUN=mean,
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$ModName <- "I_MM"
    tmpDF2 <- tmpDF2[,c("ModName", "variable", "norm.value")]

    plotDF1 <- rbind(plotDF1, tmpDF2)
    
    
    ### add multi-model means
    tmpDF <- plotDF2[plotDF2$ModName%in%c("C_GDAYP", "A_ELMV1",
                                          "B_CABLP", "D_LPJGP",
                                          "E_OCHDP", "F_QUINC",
                                          "G_OCHDX", "H_QUJSM"),]
    tmpDF2 <- summaryBy(norm.value~variable, FUN=mean,
                        na.rm=T, data=tmpDF, keep.names=T)
    tmpDF2$ModName <- "I_MM"
    tmpDF2 <- tmpDF2[,c("ModName", "variable", "norm.value")]
    
    plotDF2 <- rbind(plotDF2, tmpDF2)
    
    
    ### arrange positions
    plotDF2$v2[plotDF2$variable=="RAU"] <- "1_RAU"
    plotDF2$v2[plotDF2$variable=="RHET"] <- "2_RHET"
    plotDF2$v2[plotDF2$variable=="deltaCSOIL"] <- "3_deltaCSOIL"
    plotDF2$v2[plotDF2$variable=="deltaCSTOR"] <- "4_deltaCSTOR"
    plotDF2$v2[plotDF2$variable=="deltaCVEG"] <- "5_deltaCVEG"

    
    
    
    ### plotting
    p1 <- ggplot(data=plotDF1, 
                 aes(ModName, norm.value, group=variable)) +
      #geom_hline(yintercept=c(0,1), lty=1)+
      geom_bar(stat = "identity", aes(fill=variable), 
               position=position_stack(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
      ylab(expression(paste("Normalized " * CO[2] * " response")))+
      #scale_fill_manual(name="Component",
      #                  values=c("NPP"=GreensPalette[5],
      #                           "RAU"=YlOrRdPalette[6]),
      #                  labels=c("NPP"="NPP",
      #                           "RAU"=expression(R[auto])))+
      scale_fill_manual(name="Component",
                        values=c(#"GPP"="#42A7F2",
                                 "NPP"="#47E3E9",
                                 "RAU"="#C9FA86",
                                 "RHET"="#F9F871",
                                 "deltaCVEG"="#8180D7",
                                 "deltaCSTOR"="#BEFCFE",
                                 "deltaCSOIL"="#FFE8D3"),
                        labels=c("NPP"="NPP",
                                 "RAU"=expression(R[auto])))+
      scale_x_discrete(limit=c(mod.list.rev, "I_MM", "OBS"),
                       label=c(model.labels, 
                               "I_MM"=expression(bold("M-M")),
                               "OBS"=expression(bold("OBS"))))
    
    
    p2 <- ggplot(data=plotDF2, 
                 aes(ModName, norm.value, group=variable)) +
      #geom_hline(yintercept=c(0,1), lty=1)+
      geom_bar(stat = "identity", aes(fill=variable), 
               position=position_stack(), col="black") +
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("Normalized " * CO[2] * " response")))+
      scale_fill_manual(name="Component",
                        values=c(#"GPP"="#42A7F2",
                                 "NPP"="#47E3E9",
                                 "RAU"="#C9FA86",
                                 "RHET"="#F9F871",
                                 "deltaCVEG"="#8180D7",
                                 "deltaCSTOR"="#BEFCFE",
                                 "deltaCSOIL"="#FFE8D3"),
                        labels=c("RAU"=expression(R[auto]),
                                 "RHET"=expression(R[het]),
                                 "deltaCVEG"=expression(Delta * C[veg]),
                                 "deltaCSTOR"=expression(Delta * C[lab]),
                                 "deltaCSOIL"=expression(Delta * C[soil])))+
    scale_x_discrete(limit=c(mod.list.rev, "I_MM", "OBS"),
                     label=c(model.labels, 
                             "I_MM"=expression(bold("M-M")),
                             "OBS"=expression(bold("OBS"))))

    
    ####################################################################################################################################
    plots_top_row <- plot_grid(p1, p2, 
                               labels=c("A", "B"),
                               ncol=2, align="vh", axis = "l",
                               label_x=0.94, label_y=0.97,
                               label_size = 18)
    
    plots_legend_row <-  get_legend(legend_plot_only + theme(legend.position="bottom",
                                                     legend.box = 'horizontal',
                                                     legend.box.just = 'left')
                                    + guides(fill=guide_legend(nrow=1,byrow=TRUE)))
    
    
    pdf(paste0(out.dir, "/MIP_normalized_fate_of_C_", 
               scenario, "_comparison.pdf"), 
        width=14, height=4)
    plot_grid(plots_top_row,
              plots_legend_row,
              ncol=1, rel_heights=c(1, 0.4))
    
    dev.off()
    
    
    
    
    
    
    
    
    
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
    plotDF4 <- vegDF[vegDF$Trt=="diff"&vegDF$Variable%in%c("deltaCL", "deltaCW", "deltaCFR", "deltaCCR", "deltaCSTOR"),]
    
    
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
      xlab("")+
      scale_y_continuous(limits=c(-100,100))+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position=c("none"),
            legend.box = 'horizontal',
            legend.box.just = 'left',
            #legend.background = element_rect(fill="lightgrey",
            #                                 size=0.5, linetype="solid", 
            #                                 colour ="black"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
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
    scale_fill_manual(name="Component",
                      values=c("deltaCL"="#FF6F91",
                               "deltaCW"="#FFC75F",
                               "deltaCFR"="#D65DB1",
                               "deltaCCR"="#845EC2",
                               "deltaCSTOR"="#FF9671"),
                      labels=c("deltaCL"=expression(Delta*C[leaf]), 
                               "deltaCW"=expression(Delta*C[wood]), 
                               "deltaCFR"=expression(Delta*C[froot]), 
                               "deltaCCR"=expression(Delta*C[croot]),
                               "deltaCSTOR"=expression(Delta*C[store])))+
      guides(fill=guide_legend(nrow=2, byrow=T),
             pch="none")
    
    
    p6 <- ggplot(data=plotDF4, 
                 aes(Group, meanvalue)) +
      geom_bar(stat = "identity", aes(fill=Variable), 
               position="stack", col="black") +
      geom_errorbar(data=plotDF3, 
                    aes(x=Group, ymin=meanvalue-sdvalue,
                        ymax=meanvalue+sdvalue), 
                    col="black", 
                    position=position_dodge2(), width=0.3)+
      geom_point(data=plotDF3, aes(x=Group, y=meanvalue), col="black",
                 fill="white", size=2, pch=21)+
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
      ylab(expression(CO[2] * " effect (g C " * m^2 * " " * yr^-1 * ")"))+
      scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
                       label=c(model.labels, "multi-model"=expression(bold("M-M")),
                               "obs" = expression(bold("OBS"))))+
      scale_fill_manual(name=expression(Delta*C[veg]),
                        values=c("deltaCL"="#FF6F91",
                                 "deltaCW"="#FFC75F",
                                 "deltaCFR"="#D65DB1",
                                 "deltaCCR"="#845EC2",
                                 "deltaCSTOR"="#FF9671"),
                        labels=c("deltaCL"=expression(Delta*C[leaf]), 
                                 "deltaCW"=expression(Delta*C[wood]), 
                                 "deltaCFR"=expression(Delta*C[froot]), 
                                 "deltaCCR"=expression(Delta*C[croot]),
                                 "deltaCSTOR"=expression(Delta*C[store])))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "multi-model"="grey30", "obs"="grey"))),
             color = guide_legend(nrow=12, byrow=F))
    
    
    #p62 <- ggplot(data=plotDF3, 
    #             aes(Group, meanvalue)) +
    #  geom_bar(stat = "identity", aes(fill=Group),
    #           position="stack", col="black") +
    #  geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
    #                    ymax=meanvalue+sdvalue), 
    #                col="black", 
    #                position=position_dodge2(), width=0.3)+
    #  #geom_point(data=plotDF3, aes(x=Group, y=meanvalue), col="black",
    #  #           fill="white", size=2, pch=21)+
    #  geom_vline(xintercept=c(6.5, 8.5), lty=2)+
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
    #  ylab(expression(CO[2] * " effect (g C " * m^2 * " " * yr^-1 * ")"))+
    #  scale_x_discrete(limit=c(mod.list, "multi-model", "obs"),
    #                   label=c(model.labels, "multi-model"=expression(bold("M-M")),
    #                           "obs" = expression(bold("OBS"))))+
    #  scale_fill_manual(name="Model",
    #                    values=c(col.values, 
    #                             "multi-model"="grey30",
    #                             "obs"="grey"),
    #                    labels=c(model.labels, "obs"= "OBS"))+
    #  guides(fill = guide_legend(override.aes = list(col = c(col.values, "multi-model"="grey30", "obs"="grey"))),
    #         color = guide_legend(nrow=12, byrow=F))
    
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
      xlab("")+
      scale_y_continuous(limits=c(-200, 300))+
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
                        labels=c(model.labels, "multi-model"="M-M", "obs"= "OBS"))+
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
      geom_vline(xintercept=c(6.5, 8.5), lty=2)+
      xlab("")+
      scale_y_continuous(limits=c(-180, 300))+
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
    
    
    
    
    plots_mid_row <- plot_grid(p5, p6, # delta Cveg
                               labels=c("C", "D"), label_x=0.94, label_y=0.97,
                               label_size=20,
                               ncol=2)
    
    plots_legend_row2 <-  get_legend(p5 + theme(legend.position="bottom",
                                                     legend.box = 'horizontal',
                                                     legend.box.just = 'left')
                                    + guides(fill=guide_legend(nrow=1,byrow=TRUE)))
    
    
    plots_bot_row <- plot_grid(p7, p8, # NEP
                               labels=c("E", "F"), label_x=0.94, label_y=0.97,
                               label_size=20,
                               ncol=2)
    
    plots_legend_row3 <-  get_legend(p7 + theme(legend.position="bottom",
                                                legend.box = 'horizontal',
                                                legend.box.just = 'left')
                                     + guides(fill=guide_legend(nrow=1,byrow=TRUE)))
    
    
    pdf(paste0(out.dir, "/MIP_normalized_fate_of_C_", 
               scenario, "_comparison.pdf"), 
        width=16, height=12)
    plot_grid(plots_top_row,
              plots_legend_row,
              plots_mid_row,
              plots_legend_row2,
              plots_bot_row,
              plots_legend_row3,
              ncol=1,
              rel_heights=c(1,0.2,
                            1,0.2, 
                            1,0.2))
    dev.off()
    
    
    
    
     #  end
}

