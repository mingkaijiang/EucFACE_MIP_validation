plot_photosynthesis_relationships <- function(scenario) {
    ##################################################################
    #### Set up basics
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/obs_", scenario, "_output")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0(out.dir, "/MIP_obs_", scenario, "_amb_daily.rds"))
    eleDF <- readRDS(paste0(out.dir, "/MIP_obs_", scenario, "_ele_daily.rds"))
    
    ### ignore NAs
    ambDF[ambDF<=-999] <- NA
    eleDF[eleDF<=-999] <- NA
    
    ambDF$LCN <- with(ambDF, CL/NL)
    eleDF$LCN <- with(eleDF, CL/NL)
    
    ambDF$LCP <- with(ambDF, CL/PL)
    eleDF$LCP <- with(eleDF, CL/PL)
    
    ambDF$LNP <- with(ambDF, NL/PL)
    eleDF$LNP <- with(eleDF, NL/PL)
    
    ambDF <- ambDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI", 
                      "LCN", "LCP", "LNP", "NCON")]
    
    eleDF <- eleDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI", 
                      "LCN", "LCP", "LNP", "NCON")]
    
    d <- dim(ambDF)[2]
    
    
    with(ambDF, plot(GPP~LCP))
    with(eleDF, points(GPP~LCP, col="red"))
    
    ### prepare CO2 df
    co2DF <- ambDF
    co2DF[,5:d] <- eleDF[,5:d]-ambDF[,5:d]
    
    ### prepare CO2 pct difference df
    pctco2DF <- co2DF
    pctco2DF[,5:d] <- co2DF[,5:d]/ambDF[,5:d]*100.0
    
    
    ### calculate multiple model means for each year
    ### amb
    #ambDF.mip <- summaryBy(.~YEAR, FUN=c(mean, sd),
    #                       data=ambDF, keep.names=T, na.rm=T)
    #
    #d2 <- dim(ambDF.mip)[2]
    #
    #### co2
    #co2DF.mip <- summaryBy(.~YEAR, FUN=c(mean, sd),
    #                       data=co2DF, keep.names=T, na.rm=T)
    #
    #### co2 pct
    #pctco2DF.mip <- summaryBy(.~YEAR, FUN=c(mean, sd),
    #                          data=pctco2DF, keep.names=T, na.rm=T)
    #
    #### replace inf with na
    #ambDF.mip[sapply(ambDF.mip, is.infinite)] <- NA
    #ambDF.mip[sapply(ambDF.mip, is.nan)] <- NA
    #
    #co2DF.mip[sapply(co2DF.mip, is.infinite)] <- NA
    #co2DF.mip[sapply(co2DF.mip, is.nan)] <- NA
    #
    #pctco2DF.mip[sapply(pctco2DF.mip, is.infinite)] <- NA
    #pctco2DF.mip[sapply(pctco2DF.mip, is.nan)] <- NA
    
    ##################################################################
    
    ### leaf NC ratio
    p1 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, GPP)) +
      geom_hex(bins=50) +
      geom_smooth(method = lm, se = FALSE)+
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
      ylab(expression(paste("GPP (g C " * m^2 * " " * yr^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio"))); p1
    
    ### leaf PC ratio
    p2 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, GPP)) +
      geom_hex(bins=50) +
      geom_smooth(method = lm, se = FALSE)+
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
      ylab(expression(paste("GPP (g C " * m^2 * " " * yr^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio"))); p2
    
    require(scatterplot3d)
    scatterplot3d(x = ambDF$GPP[ambDF$ModName==i], 
           y = ambDF$LCN[ambDF$ModName==i], 
           z = ambDF$LCP[ambDF$ModName==i],
           xlab="GPP", ylab="leaf CN", zlab="leaf CP")
    
    i <- "GDAYP"
    test <- ambDF[ambDF$ModName==i,]
      
    scatterplot3d(x = test$GPP, 
                         y = test$LCN, 
                         z = test$LCP,
                         xlab="GPP", ylab="leaf CN", zlab="leaf CP",
                         pch = 16, highlight.3d=T)
    
    
    # Add regression plane
    my.lm <- lm(test$GPP ~ test$LCN + test$LCP)

    summary(my.lm)
  
    
    test <- test[order(test$LCN, test$LCP),]
    
    require(akima)
    require(rgl)
    test2 <- interp(test$LCN, test$LCP, test$GPP)
    dim(test2$z)
    surface3d(test2$x,test2$y,test2$z)
    points3d(test2)
    
    persp(x = test$LCN, 
          y = test$LCP,
          z = test$GPP)
    
    ##################################################################
    require(gridExtra)
    
    pdf(paste0(out.dir, "/MIP_photosynthesis_relationships_obs_", scenario, "_comparison.pdf"), width=8, height=16)
    
    
    ### plot MIP
    for (i in 5:d) {
        p1 <- ggplot() +
            geom_ribbon(data=ambDF.mip, 
                        aes(YEAR, ymin=ambDF.mip[,i-1]-ambDF.mip[,i+146],
                            ymax=ambDF.mip[,i-1]+ambDF.mip[,i+146]),
                        fill=alpha("grey", 0.3))+
            geom_line(data=ambDF, 
                      aes(YEAR, ambDF[,i], col=ModName)) +
            geom_line(data=ambDF.mip, aes(YEAR, ambDF.mip[,i-1]), col="black", lwd=2)+
            ggtitle(paste0(names(ambDF)[i]))+
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
            ylab(expression(paste("Ambient " * CO[2])))
        
        
        p2 <- ggplot() +
            geom_ribbon(data=co2DF.mip, 
                        aes(YEAR, ymin=co2DF.mip[,i-1]-co2DF.mip[,i+146],
                            ymax=co2DF.mip[,i-1]+co2DF.mip[,i+146]),
                        fill=alpha("grey", 0.3))+
            geom_line(data=co2DF, 
                      aes(YEAR, co2DF[,i], col=ModName)) +
            geom_line(data=co2DF.mip, aes(YEAR, co2DF.mip[,i-1]), col="black", lwd=2)+
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
            ylab(expression(paste(CO[2] * " effect (difference)")))
        
        
        p3 <- ggplot() +
            geom_ribbon(data=pctco2DF.mip, 
                        aes(YEAR, ymin=pctco2DF.mip[,i-1]-pctco2DF.mip[,i+146],
                            ymax=pctco2DF.mip[,i-1]+pctco2DF.mip[,i+146]),
                        fill=alpha("grey", 0.3))+
            geom_line(data=pctco2DF, 
                      aes(YEAR, pctco2DF[,i], col=ModName)) +
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
            ylab(expression(paste(CO[2] * " effect (ratio %)")));p3
        
        grid.arrange(p1, p2, p3, nrow=3)
        
    }
    
    dev.off()
    
    
  
    
    
}    

