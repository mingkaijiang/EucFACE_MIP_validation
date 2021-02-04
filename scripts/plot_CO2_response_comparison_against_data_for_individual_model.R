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
    
    
    
    plotDF <- rbind(CO2DF1, CO2DF2)
    
    ### get column names
    col.names <- names(plotDF)
    
    ### get dimension
    d <- dim(plotDF)
    n <- d[2]
    
    ### plot CO2 response ratio
    pdf(paste0(out.dir, "/", mod.abb, "_", sim.period, "_", nutrient.trt, "_D_CO2_ratio.pdf"))
    
    for (i in 2:(n-1)) {
        p1 <- ggplot(plotDF) +
            geom_hline(yintercept=1)+
            geom_point(aes(x = YEAR, y = plotDF[,i], fill = Trt, pch = Trt), size=4)+
            geom_line(aes(x = YEAR, y = plotDF[,i], col=Trt))+
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
    }
    
    dev.off()
    
    
}