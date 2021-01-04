plot_historic_validation <- function() {
    ### purpose: 
    ### plot 1750 - to 2069 to just smoothness of the re-start breaks,
    ### and the magnitude of some key response variables
    
    ### historic df
    histDF <- read.csv("outputs/EUC_amb_equilib.csv",skip=1)
    
    ### obs
    obsDF <- read.csv("outputs/EUC_simulated_DRY_AMB_2012_2019.csv", skip=1)
    
    ### future
    futDF <- read.csv("outputs/EUC_simulated_DRY_AMB_NOP_2020_2069.csv", skip=1)
    
    ### trt
    histDF$Trt <- "Hist"
    obsDF$Trt <- "Obs"
    futDF$Trt <- "Fut"
    
    ### merge
    myDF <- rbind(histDF, obsDF, futDF)
    
    ### calculate annual mean and/or sum
    sumDF1 <- summaryBy(shoot+lai+branch+stem+root+croot+shootn+branchn+stemn+rootn+crootn+
                            shootp+branchp+stemp+rootp+crootp+soilc+soiln+soilp+inorgn+inorgp+
                            inorgavlp+inorglabp+inorgsorbp+inorgssorbp+inorgoccp+inorgparp+
                            litterc+littercag+littercbg+litternag+litternbg+litterpag+litterpbg+
                            activesoil+slowsoil+passivesoil+activesoiln+slowsoiln+passivesoiln+
                            activesoilp+slowsoilp+passivesoilp~year+Trt, FUN=mean,
                        data=myDF, na.rm=T, keep.names=T)
    
    
    sumDF2 <- summaryBy(et+transpiration+soil_evap+canopy_evap+runoff+nep+gpp+npp+hetero_resp+
                            auto_resp+cpleaf+cpbranch+cpstem+cproot+
                            nuptake+ngross+nmineralisation+nloss+puptake+pgross+
                            pmineralisation+ploss+leafretransn+leafretransp~year+Trt,
                        FUN=sum, data=myDF, na.rm=T, keep.names=T)
    
    ### merge
    plotDF <- merge(sumDF1, sumDF2, by=c("year", "Trt"))
    
    ## number of columns
    n <- dim(plotDF)[2]
    
    
    pdf("outputs/analysis/historic_validation.pdf")
    for (i in 3:n) {
        p <- ggplot(plotDF) +
            geom_point(aes(x = year, y = plotDF[,i]*100, fill = Trt, pch = Trt), size=4)+
            geom_line(aes(x = year, y = plotDF[,i]*100, col=Trt))+
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
            ylab(paste0(colnames(plotDF)[i], " [g m-2 (yr-1 for flux)]"))+
            scale_color_manual(name="",
                               limits=c("Hist", "Obs", "Fut"),
                               labels=c("Hist", "Obs", "Fut"),
                               values=c("grey", "green", "purple"),
                               guide=guide_legend(nrow=1))+
            scale_fill_manual(name="",
                              limits=c("Hist", "Obs", "Fut"),
                              labels=c("Hist", "Obs", "Fut"),
                              values=c("grey", "green", "purple"),
                              guide=guide_legend(nrow=1))+
            scale_linetype_manual(name="",
                                  limits=c("Hist", "Obs", "Fut"),
                                  labels=c("Hist", "Obs", "Fut"),
                                  values=c("dotted", "solid", "solid"),
                                  guide=guide_legend(nrow=1))+
            scale_shape_manual(name="",
                               limits=c("Hist", "Obs", "Fut"),
                               labels=c("Hist", "Obs", "Fut"),
                               values=c(24,21,21),
                               guide=guide_legend(nrow=1))+
            ggtitle(colnames(plotDF)[i])+
            xlab("Year")+
            scale_x_continuous(limits=c(1750, 2070),
                               breaks=c(1750, 1800, 1850, 1900, 1950, 2000, 2050))
        
        
        plot(p)
    }
    
    dev.off()
    
    
    
}