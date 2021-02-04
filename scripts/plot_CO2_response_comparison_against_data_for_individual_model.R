plot_CO2_response_comparison_against_data_for_individual_model <- function (source.dir, 
                                                                            mod.abb, 
                                                                            out.dir,
                                                                            eucDF) {
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    
    ### prepare simulation results
    simDF <- prepare_simulation_results_for_comparison_against_data(source.dir, mod.abb)
    
    ### merge simulation and observation datasets
    myDF <- merge_simulation_and_observation_datasets(eucDF, simDF)
    
    ### convert into long format
    subDF1 <- myDF[myDF$Trt%in%c("aCO2", "eCO2") & myDF$Group=="mean",]
    subDF2 <- myDF[myDF$Trt%in%c("aCO2", "eCO2") & myDF$Group=="sd",]
    subDF3 <- myDF[myDF$Trt=="pct_diff" & myDF$Group=="mean",]
    subDF4 <- myDF[myDF$Trt=="pct_diff" & myDF$Group=="sd",]
    

    require(tidyr)
    keycol <- "variable"
    valuecol <- "measurement"
    gathercols <- c("control", "cond1", "cond2")
    
    lDF1 <- gather_(subDF1, keycol, valuecol, gathercols)

        
    ### split into different plotting DFs
    plotDF1 <- myDF[myDF$Trt%in%c("aCO2", "eCO2"),]
    plotDF2 <- subset(myDF, Trt=="pct_diff")
    

    ### prepare plotting
    ### plot CO2 response ratio
    pdf(paste0(out.dir, "/", mod.abb, "_OBS_NOP_D_CO2_ratio_against_data.pdf"))
    
    ### GPP
    p1 <- ggplot(plotDF2) +
        geom_bar(aes(x = Source, y = plotDF[,i], fill = Trt, pch = Trt), size=4)+
        geom_line(aes(x = Source, y = plotDF[,i], col=Trt))+
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
        ylab(paste0(colnames(plotDF)[i]))+
        scale_color_manual(name="",
                           limits=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                    paste0(sim.period, "_FIX_", nutrient.trt)),
                           labels=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                    paste0(sim.period, "_FIX_", nutrient.trt)),
                           values=c("orange", "cyan"),
                           guide=guide_legend(nrow=4))+
        scale_fill_manual(name="",
                          limits=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                   paste0(sim.period, "_FIX_", nutrient.trt)),
                          labels=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                   paste0(sim.period, "_FIX_", nutrient.trt)),
                          values=c("orange", "cyan"),
                          guide=guide_legend(nrow=4))+
        scale_linetype_manual(name="",
                              limits=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                       paste0(sim.period, "_FIX_", nutrient.trt)),
                              labels=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                       paste0(sim.period, "_FIX_", nutrient.trt)),
                              values=c("solid", "dotted"),
                              guide=guide_legend(nrow=4))+
        scale_shape_manual(name="",
                           limits=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                    paste0(sim.period, "_FIX_", nutrient.trt)),
                           labels=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                    paste0(sim.period, "_FIX_", nutrient.trt)),
                           values=c(24,21),
                           guide=guide_legend(nrow=4))+
        ggtitle(colnames(plotDF)[i])+
        xlab("Year")
    
    
    plot(p1)
    
    
    dev.off()
    
    
}