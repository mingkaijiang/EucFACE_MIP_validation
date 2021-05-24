plot_CO2_absolute_response_for_individual_model <- function(source.dir, 
                                                         mod.abb, 
                                                         out.dir,
                                                         sim.period,
                                                         nutrient.trt,
                                                         clim.trt) {
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### ambient CO2, over observed period (2012-2019)
    ambDF <- read.csv(paste0(source.dir, "/EUC_", mod.abb, "_", sim.period, "_", clim.trt,"_AMB_", nutrient.trt, "_D.csv"))  # dry

    ### elevated CO2, over observed period (2012-2019)
    eleDF <- read.csv(paste0(source.dir, "/EUC_", mod.abb, "_", sim.period, "_", clim.trt,"_ELE_", nutrient.trt, "_D.csv"))  # dry

    ### obtain means, sums for stocks and fluxes
    if (mod.abb == "CABLP") {
        ambDF <- convert_into_annual_CABLP(ambDF)
        eleDF <- convert_into_annual_CABLP(eleDF)
    } else {
        ambDF <- convert_into_annual(ambDF)
        eleDF <- convert_into_annual(eleDF)
    }

    
    ### get dimension
    d <- dim(ambDF)
    n <- d[2]
    
    ### merge amb and ele
    ambDF$Trt <- paste0(sim.period, "_", clim.trt, "_", "AMB_", nutrient.trt)
    eleDF$Trt <- paste0(sim.period, "_", clim.trt, "_", "ELE_", nutrient.trt)

    plotDF <- rbind(ambDF, eleDF)

    ### get column names
    col.names <- names(plotDF)
    
    ### get dimension
    d <- dim(plotDF)
    n <- d[2]
    
    ### plot absolute CO2 response 
    pdf(paste0(out.dir, "/", mod.abb, "_", sim.period, "_", clim.trt, "_", nutrient.trt, "_D_CO2_abs.pdf"))
    
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
                               limits=c(paste0(sim.period, "_", clim.trt, "_", "AMB_", nutrient.trt), 
                                        paste0(sim.period, "_", clim.trt, "_", "ELE_", nutrient.trt)),
                               labels=c(paste0(sim.period, "_", clim.trt, "_", "AMB_", nutrient.trt), 
                                        paste0(sim.period, "_", clim.trt, "_", "ELE_", nutrient.trt)),
                               values=c("orange", "cyan"),
                               guide=guide_legend(nrow=4))+
            scale_fill_manual(name="",
                              limits=c(paste0(sim.period, "_", clim.trt, "_", "AMB_", nutrient.trt), 
                                       paste0(sim.period, "_", clim.trt, "_", "ELE_", nutrient.trt)),
                              labels=c(paste0(sim.period, "_", clim.trt, "_", "AMB_", nutrient.trt), 
                                       paste0(sim.period, "_", clim.trt, "_", "ELE_", nutrient.trt)),
                              values=c("orange", "cyan"),
                              guide=guide_legend(nrow=4))+
            scale_linetype_manual(name="",
                                  limits=c(paste0(sim.period, "_", clim.trt, "_", "AMB_", nutrient.trt), 
                                           paste0(sim.period, "_", clim.trt, "_", "ELE_", nutrient.trt)),
                                  labels=c(paste0(sim.period, "_", clim.trt, "_", "AMB_", nutrient.trt), 
                                           paste0(sim.period, "_", clim.trt, "_", "ELE_", nutrient.trt)),
                                  values=c("solid", "dotted"),
                                  guide=guide_legend(nrow=4))+
            scale_shape_manual(name="",
                               limits=c(paste0(sim.period, "_", clim.trt, "_", "AMB_", nutrient.trt), 
                                        paste0(sim.period, "_", clim.trt, "_", "ELE_", nutrient.trt)),
                               labels=c(paste0(sim.period, "_", clim.trt, "_", "AMB_", nutrient.trt), 
                                        paste0(sim.period, "_", clim.trt, "_", "ELE_", nutrient.trt)),
                               values=c(24,21),
                               guide=guide_legend(nrow=4))+
            ggtitle(colnames(plotDF)[i])+
            xlab("Year")
        
        
        plot(p1)
    }
    
    dev.off()
    
    
}
