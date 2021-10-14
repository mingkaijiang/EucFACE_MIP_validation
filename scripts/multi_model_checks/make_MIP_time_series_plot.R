make_MIP_time_series_plot <- function(scenario) {
    ##################################################################
    #### Set up basics
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario, "/")
  
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", 
                            scenario, "_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", 
                            scenario, "_ELE_annual.rds"))
    
    d <- dim(ambDF)[2]
    
    ### ignore NAs
    ambDF[ambDF<=-999] <- NA
    eleDF[eleDF<=-999] <- NA
    
    ### prepare CO2 df
    co2DF <- ambDF
    co2DF[,3:d] <- eleDF[,3:d]-ambDF[,3:d]
    
    ### prepare CO2 pct difference df
    pctco2DF <- co2DF
    pctco2DF[,3:d] <- co2DF[,3:d]/ambDF[,3:d]*100.0
    
    
    ### calculate multiple model means for each year
    ### amb
    ambDF.mip <- summaryBy(.~YEAR, FUN=c(mean, sd),
                           data=ambDF, keep.names=T, na.rm=T)
    
    d2 <- dim(ambDF.mip)[2]
    
    #ambDF.mip.sd <- cbind(ambDF.mip$YEAR, ambDF.mip[,d:d2])
    
    #ambDF.mip <- ambDF.mip[,1:(d-1)]
    
    ### co2
    co2DF.mip <- summaryBy(.~YEAR, FUN=c(mean, sd),
                           data=co2DF, keep.names=T, na.rm=T)
    
    #co2DF.mip.sd <- cbind(co2DF.mip$YEAR, co2DF.mip[,d:d2])
    
    #co2DF.mip <- co2DF.mip[,1:(d-1)]
    
    ### co2 pct
    pctco2DF.mip <- summaryBy(.~YEAR, FUN=c(mean, sd),
                              data=pctco2DF, keep.names=T, na.rm=T)
    
    #pctco2DF.mip.sd <- cbind(pctco2DF.mip$YEAR, pctco2DF.mip[,d:d2])
    
    #pctco2DF.mip <- pctco2DF.mip[,1:(d-1)]
    
    
    ### replace inf with na
    ambDF.mip[sapply(ambDF.mip, is.infinite)] <- NA
    ambDF.mip[sapply(ambDF.mip, is.nan)] <- NA
    
    co2DF.mip[sapply(co2DF.mip, is.infinite)] <- NA
    co2DF.mip[sapply(co2DF.mip, is.nan)] <- NA
    
    pctco2DF.mip[sapply(pctco2DF.mip, is.infinite)] <- NA
    pctco2DF.mip[sapply(pctco2DF.mip, is.nan)] <- NA
    

    ##################################################################
    
    
    
    
    
    ##################################################################
    require(gridExtra)
    
    pdf(paste0(out.dir, "/MIP_time_series_obs_", scenario, "_comparison.pdf"), 
        width=8, height=16)
    
    ### plot MIP
    for (i in 3:d) {
        p1 <- ggplot() +
            geom_ribbon(data=ambDF.mip, 
                        aes(YEAR, ymin=ambDF.mip[,(i-1)]-ambDF.mip[,(i+151)],
                            ymax=ambDF.mip[,(i-1)]+ambDF.mip[,(i+151)]),
                        fill=alpha("grey", 0.3))+
            geom_line(data=ambDF, 
                      aes(YEAR, ambDF[,i], col=ModName, lty=ModName),lwd=1.5) +
            geom_line(data=ambDF.mip, aes(YEAR, ambDF.mip[,(i-1)]), col="black", lwd=2)+
            ggtitle(paste0(names(ambDF)[i]))+
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_text(size=12),
                  axis.title.x=element_blank(),
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
            scale_color_manual(name="Model",
                               values=col.values,
                               labels=model.labels)+
            scale_linetype_manual(name="Model", 
                                  values=linetype.values,
                                  labels=model.labels)+
            guides(fill = guide_legend(override.aes = list(col = col.values,
                                                           lty = linetype.values)),
                   color = guide_legend(nrow=6, byrow=F),
                   linetype = guide_legend(override.aes = list(size = 0.5)))+
            ylab(expression(paste("Ambient " * CO[2])))
        
        
        p2 <- ggplot() +
            geom_ribbon(data=co2DF.mip, 
                        aes(YEAR, ymin=co2DF.mip[,i-1]-co2DF.mip[,i+151],
                            ymax=co2DF.mip[,i-1]+co2DF.mip[,i+151]),
                        fill=alpha("grey", 0.3))+
            geom_line(data=co2DF, 
                      aes(YEAR, co2DF[,i], col=ModName, lty=ModName)) +
            geom_line(data=co2DF.mip, aes(YEAR, co2DF.mip[,i-1]), col="black", lwd=2)+
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_text(size=12),
                  axis.title.x=element_blank(),
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
          scale_color_manual(name="Model",
                             values=col.values,
                             labels=model.labels)+
          scale_linetype_manual(name="Model", 
                                values=linetype.values,
                                labels=model.labels)+
          guides(fill = guide_legend(override.aes = list(col = col.values,
                                                         lty = linetype.values)),
                 color = guide_legend(nrow=6, byrow=F),
                 linetype = guide_legend(override.aes = list(size = 0.5)))+
            ylab(expression(paste(CO[2] * " effect (difference)")))
        
        
        p3 <- ggplot() +
            geom_ribbon(data=pctco2DF.mip, 
                        aes(YEAR, ymin=pctco2DF.mip[,i-1]-pctco2DF.mip[,i+151],
                            ymax=pctco2DF.mip[,i-1]+pctco2DF.mip[,i+151]),
                        fill=alpha("grey", 0.3))+
            geom_line(data=pctco2DF, 
                      aes(YEAR, pctco2DF[,i], col=ModName, lty=ModName)) +
            geom_line(data=pctco2DF.mip, aes(YEAR, pctco2DF.mip[,i-1]), col="black", lwd=2)+
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
          scale_color_manual(name="Model",
                             values=col.values,
                             labels=model.labels)+
          scale_linetype_manual(name="Model", 
                                values=linetype.values,
                                labels=model.labels)+
          guides(fill = guide_legend(override.aes = list(col = col.values,
                                                         lty = linetype.values)),
                 color = guide_legend(nrow=6, byrow=F),
                 linetype = guide_legend(override.aes = list(size = 0.5)))+
            ylab(expression(paste(CO[2] * " effect (ratio %)")))
        
        
        #all.legend <- get_legend(p3 + theme(legend.position="bottom",
        #                                    legend.box = 'horizontal',
        #                                    legend.box.just = 'left'))
        #
        #combined_plots <- plot_grid(p1, p2, p3,
        #                             labels=c("(a)", "(b)", "(c)"), 
        #                             ncol=1, align="vh", axis = "l",
        #                             label_x=0.15, label_y=0.9)
        #
        #plot_grid(combined_plots, all.legend,
        #          ncol=1, rel_heights=c(1,0.3))
        
        grid.arrange(p1, p2, p3, nrow=3)

    }
    
    dev.off()
    
    
  
    
    
}    

