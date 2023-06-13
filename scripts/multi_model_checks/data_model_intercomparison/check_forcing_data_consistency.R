check_forcing_data_consistency <- function(scenario) {
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
    if (scenario == "VAR") {
      
      obsDF <- read.csv(paste0("/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/Met_data/output/observed/csv/daily/EUC_met_observed_var_daily_2012_2019.csv"), skip=3, header=F)
    } else if (scenario == "FIX") {
      obsDF <- read.csv(paste0("/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/Met_data/output/observed/csv/daily/EUC_met_observed_fix_daily_2012_2019.csv"), skip=3, header=F)
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
    
    plotDF$PAR[plotDF$ModName%in%c("I_GDAYN", "C_GDAYP", "B_CABLP")] <- plotDF$PAR[plotDF$ModName%in%c("I_GDAYN", "C_GDAYP", "B_CABLP")] * 2
    
    
    ##################################################################
    
    
    col.values <- c(
                    "A_ELMV1" = SpectralPalette[1],
                    "B_CABLP" = SpectralPalette[2],
                    "C_GDAYP" = SpectralPalette[3],
                    "D_LPJGP" = SpectralPalette[4],
                    "E_OCHDP" = SpectralPalette[5],
                    "F_QUINC" = SpectralPalette[6],
                    "G_OCHDX" = SpectralPalette[7],
                    "H_QUJSM" = SpectralPalette[8],
                    "I_GDAYN" = SpectralPalette[3],
                    "J_LPJGN" = SpectralPalette[4])
    
    
    model.labels <- c("C_GDAYP" = "GDAYP",
                      "A_ELMV1" = "ELMV1",
                      "B_CABLP" = "CABLP",
                      "D_LPJGP" = "LPJGP",
                      "E_OCHDP" = "OCDHP",
                      "F_QUINC" = "QUINC",
                      "G_OCHDX" = "OCHDX",
                      "H_QUJSM" = "QUJSM",
                      "I_GDAYN" = "GDAYN",
                      "J_LPJGN" = "LPJGN")
    
    
    linetype.values <- c("C_GDAYP" = 1,
                         "A_ELMV1" = 1,
                         "B_CABLP" = 1,
                         "D_LPJGP" = 1,
                         "E_OCHDP" = 1,
                         "F_QUINC" = 1,
                         "G_OCHDX" = 1,
                         "H_QUJSM" = 1,
                         "I_GDAYN" = 2,
                         "J_LPJGN" = 2)
    
    
    ### plot MIP

    ### CO2 ambient
    p1 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, aCO2, col=ModName, lty=ModName),lwd=1.5) +
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
      scale_color_manual(name="Model",
                         values=c(col.values, "OBS"="black"),
                         labels=c(model.labels, "OBS"="OBS"))+
      scale_linetype_manual(name="Model", 
                            values=c(linetype.values, "OBS"=1),
                            labels=c(model.labels, "OBS"="OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
                                                     lty = c(linetype.values, "OBS"=1))),
             color = guide_legend(nrow=4, byrow=F),
             linetype = guide_legend(override.aes = list(size = 0.5)))+
      ylab(expression(paste("Ambient " * CO[2])))
    
    ### CO2 elevated
    p2 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, eCO2, col=ModName, lty=ModName),lwd=1.5) +
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
      scale_color_manual(name="Model",
                         values=c(col.values, "OBS"="black"),
                         labels=c(model.labels, "OBS"="OBS"))+
      scale_linetype_manual(name="Model", 
                            values=c(linetype.values, "OBS"=1),
                            labels=c(model.labels, "OBS"="OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
                                                     lty = c(linetype.values, "OBS"=1))),
             color = guide_legend(nrow=6, byrow=F),
             linetype = guide_legend(override.aes = list(size = 0.5)))+
      ylab(expression(paste("Elevated " * CO[2])))
    
    ### Tair
    p3 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, TAIR, col=ModName, lty=ModName),lwd=1.5) +
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
      scale_color_manual(name="Model",
                         values=c(col.values, "OBS"="black"),
                         labels=c(model.labels, "OBS"="OBS"))+
      scale_linetype_manual(name="Model", 
                            values=c(linetype.values, "OBS"=1),
                            labels=c(model.labels, "OBS"="OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
                                                     lty = c(linetype.values, "OBS"=1))),
             color = guide_legend(nrow=6, byrow=F),
             linetype = guide_legend(override.aes = list(size = 0.5)))+
      ylab(expression(T[air] * " (degree C)"))
      
    
    ### Tsoil
    p4 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, TSOIL, col=ModName, lty=ModName),lwd=1.5) +
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
      scale_color_manual(name="Model",
                         values=c(col.values, "OBS"="black"),
                         labels=c(model.labels, "OBS"="OBS"))+
      scale_linetype_manual(name="Model", 
                            values=c(linetype.values, "OBS"=1),
                            labels=c(model.labels, "OBS"="OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
                                                     lty = c(linetype.values, "OBS"=1))),
             color = guide_legend(nrow=6, byrow=F),
             linetype = guide_legend(override.aes = list(size = 0.5)))+
      ylab(expression(T[soil] * " (degree C)"))
    
    
    ### prec
    p5 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, PREC, col=ModName, lty=ModName),lwd=1.5) +
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
      scale_color_manual(name="Model",
                         values=c(col.values, "OBS"="black"),
                         labels=c(model.labels, "OBS"="OBS"))+
      scale_linetype_manual(name="Model", 
                            values=c(linetype.values, "OBS"=1),
                            labels=c(model.labels, "OBS"="OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
                                                     lty = c(linetype.values, "OBS"=1))),
             color = guide_legend(nrow=6, byrow=F),
             linetype = guide_legend(override.aes = list(size = 0.5)))+
      ylab(expression(MAP * " (mm)"))
    
    
    ### NDEP
    p6 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, NDEP, col=ModName, lty=ModName),lwd=1.5) +
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
      scale_color_manual(name="Model",
                         values=c(col.values, "OBS"="black"),
                         labels=c(model.labels, "OBS"="OBS"))+
      scale_linetype_manual(name="Model", 
                            values=c(linetype.values, "OBS"=1),
                            labels=c(model.labels, "OBS"="OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
                                                     lty = c(linetype.values, "OBS"=1))),
             color = guide_legend(nrow=6, byrow=F),
             linetype = guide_legend(override.aes = list(size = 0.5)))+
      ylab(expression(N[dep] * " (g N m-2 yr-1)"))
    
    
    
    ### VPD
    p7 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, VPD, col=ModName, lty=ModName),lwd=1.5) +
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
      scale_color_manual(name="Model",
                         values=c(col.values, "OBS"="black"),
                         labels=c(model.labels, "OBS"="OBS"))+
      scale_linetype_manual(name="Model", 
                            values=c(linetype.values, "OBS"=1),
                            labels=c(model.labels, "OBS"="OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
                                                     lty = c(linetype.values, "OBS"=1))),
             color = guide_legend(nrow=6, byrow=F),
             linetype = guide_legend(override.aes = list(size = 0.5)))+
      ylab(expression(VPD * " (kPa)"))
    
    ### PAR
    p8 <- ggplot() +
      geom_line(data=plotDF, 
                aes(YEAR, PAR, col=ModName, lty=ModName),lwd=1.5) +
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
      scale_color_manual(name="Model",
                         values=c(col.values, "OBS"="black"),
                         labels=c(model.labels, "OBS"="OBS"))+
      scale_linetype_manual(name="Model", 
                            values=c(linetype.values, "OBS"=1),
                            labels=c(model.labels, "OBS"="OBS"))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
                                                     lty = c(linetype.values, "OBS"=1))),
             color = guide_legend(nrow=6, byrow=F),
             linetype = guide_legend(override.aes = list(size = 0.5)))+
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

