check_forcing_data_consistency <- function(scenario) {
    ##################################################################
    #### Set up basics
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/obs_", scenario, "_output")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0(out.dir, "/MIP_obs_", scenario, "_amb_annual.rds"))
    eleDF <- readRDS(paste0(out.dir, "/MIP_obs_", scenario, "_ele_annual.rds"))
    
    ambDF <- ambDF[,c("ModName", "YEAR", "CO2", "PAR",
                      "TAIR", "TSOIL", "VPD", "PREC",
                      "NDEP")]
    
    names(ambDF)[names(ambDF)=="CO2"] <- "aCO2"
    
    eleDF <- eleDF[,c("ModName", "YEAR", "CO2")]
    names(eleDF)[names(eleDF)=="CO2"] <- "eCO2"
    
    ambDF <- merge(ambDF, eleDF, by=c("ModName", "YEAR"))

    d <- dim(ambDF)[2]
    
    ambDF[ambDF<=-999] <- NA

    ### add forcing data
    if (scenario == "var") {
      
      obsDF <- read.csv(paste0("/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/Met_data/output/observed/csv/daily/EUC_met_observed_", 
                               scenario, "_daily_2012_2019.csv"), skip=3, header=F)
    } else if (scenario == "fix") {
      obsDF <- read.csv(paste0("/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/Met_data/output/observed/csv/daily/EUC_met_observed_", 
                               scenario, "_daily_2012_2019.csv"), skip=3, header=F)
    }
    
    
    var.list <- c("YEAR", "DOY", "SWdown", "PAR", "LWdown",
                   "Tair", "Rain", "VPD", "RH", "Wind", "PSurf",
                   "CO2ambient", "CO2elevated", "SoilTemp", "Ndep")
    colnames(obsDF) <- var.list
    
    sumDF <- summaryBy(Rain~YEAR, data=obsDF, FUN=sum,
                       na.rm=T, keep.names=T)
    
    mnDF <- summaryBy(PAR+Tair+VPD+CO2ambient+CO2elevated+SoilTemp+Ndep~YEAR,
                      data=obsDF, FUN=mean, na.rm=T, keep.names=T)
    
    obsDF <- merge(sumDF, mnDF, by=c("YEAR"))
    obsDF$ModName <- "OBS"
    obsDF <- obsDF[,c("ModName", "YEAR", "CO2ambient", "PAR",
                      "Tair", "SoilTemp", "VPD", "Rain", "Ndep",
                      "CO2elevated")]
    colnames(obsDF) <- colnames(ambDF)
    
    obsDF$TAIR <- obsDF$TAIR - 273.15
    obsDF$TSOIL <- obsDF$TSOIL - 273.15
    obsDF$VPD <- obsDF$VPD / 1000
    
    plotDF <- rbind(obsDF, ambDF)
    
    plotDF$PAR[plotDF$ModName%in%c("I_GDAYN", "A_GDAYP", "C_CABLP")] <- plotDF$PAR[plotDF$ModName%in%c("I_GDAYN", "A_GDAYP", "C_CABLP")] * 2
    
    
    ##################################################################
    
    ### plot MIP

    ### CO2 ambient
    p1 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, aCO2, col=ModName),lwd=1.5) +
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
      ylab(expression(paste("Ambient " * CO[2])))
    
    ### CO2 elevated
    p2 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, eCO2, col=ModName),lwd=1.5) +
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
      ylab(expression(paste("Elevated " * CO[2])))
    
    ### Tair
    p3 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, TAIR, col=ModName),lwd=1.5) +
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
      ylab(expression(T[air] * " (degree C)"))
      
    
    ### Tsoil
    p4 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, TSOIL, col=ModName),lwd=1.5) +
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
      ylab(expression(T[soil] * " (degree C)"))
    
    
    ### prec
    p5 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, PREC, col=ModName),lwd=1.5) +
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
      ylab(expression(MAP * " (mm)"))
    
    
    ### NDEP
    p6 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, NDEP, col=ModName),lwd=1.5) +
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
      ylab(expression(N[dep] * " (g N m-2 yr-1)"))
    
    
    
    ### VPD
    p7 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, VPD, col=ModName),lwd=1.5) +
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
      ylab(expression(VPD * " (kPa)"))
    
    ### PAR
    p8 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, PAR, col=ModName),lwd=1.5) +
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
      ylab(expression(PAR * " (umol m-2 s-1)"))
    
    
    legend_row <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    plots_combined <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8,
                               labels="AUTO",
                               ncol=2, 
                               align="vh", axis = "l",
                               #label_x=c(0.86,0.16), label_y=0.95,
                               label_size = 18)
    
    
      pdf(paste0(out.dir, "/MIP_time_series_met_consistency_", scenario, "_check.pdf"), 
          width=8, height=16)
      plot_grid(plots_combined,
                legend_row,
                ncol=1, rel_heights=c(1,0.2))
      dev.off()
    
    
    
}    

