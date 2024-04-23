plot_photosynthesis_relationships <- function(scenario, eucDF) {
    ##################################################################
    #### Set up basics
    
    ### setting out path to store the files
    in.dir <- paste0(getwd(), "/output/MIP_output/processed_simulation")
    
    ### create output folder
    if(!dir.exists(in.dir)) {
        dir.create(in.dir, showWarnings = FALSE)
    }
    
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario)
    
    ### read in anual datasets
    ambDF <- readRDS(paste0(in.dir, "/MIP_OBS_", scenario, "_AMB_daily.rds"))
    eleDF <- readRDS(paste0(in.dir, "/MIP_OBS_", scenario, "_ELE_daily.rds"))
    
    ### ignore NAs
    ambDF[ambDF<=-999] <- NA
    eleDF[eleDF<=-999] <- NA
    
    ### remove two N only models
    ambDF <- ambDF[ambDF$ModName%in%c("A_ELMV1",
                                      "B_CABLP",
                                      "C_GDAYP",
                                      "D_LPJGP",
                                      "E_OCHDP",
                                      "F_QUINC",
                                      "G_OCHDX",
                                      "H_QUJSM"),]
    
    eleDF <- eleDF[eleDF$ModName%in%c("A_ELMV1",
                                      "B_CABLP",
                                      "C_GDAYP",
                                      "D_LPJGP",
                                      "E_OCHDP",
                                      "F_QUINC",
                                      "G_OCHDX",
                                      "H_QUJSM"),]
    
    
    ### Calculate ratios
    ambDF$LCN <- with(ambDF, CL/NL)
    eleDF$LCN <- with(eleDF, CL/NL)
    
    ambDF$LCP <- with(ambDF, CL/PL)
    eleDF$LCP <- with(eleDF, CL/PL)
    
    ambDF$LNP <- with(ambDF, NL/PL)
    eleDF$LNP <- with(eleDF, NL/PL)
    
    ambDF <- ambDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI", "NL", "PL",
                      "LCN", "LCP", "LNP", "NCON")]
    
    eleDF <- eleDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI", "NL", "PL",
                      "LCN", "LCP", "LNP", "NCON")]
    
    ambDF$Aleaf <- with(ambDF, GPP/LAI)
    eleDF$Aleaf <- with(eleDF, GPP/LAI)
    
    
    ambDF$NL_leaf <- with(ambDF, NL/LAI)
    ambDF$PL_leaf <- with(ambDF, PL/LAI)
    
    eleDF$NL_leaf <- with(eleDF, NL/LAI)
    eleDF$PL_leaf <- with(eleDF, PL/LAI)
    
    
    d <- dim(ambDF)[2]

    
    
    
    
    ##################################################################
    
    
    
    mod.list <- unique(ambDF$ModNmae)
    
    require(gridExtra)
    
    col.values <- c("A_ELMV1" = SpectralPalette[1],
      "B_CABLP" = SpectralPalette[2],
      "C_GDAYP" = SpectralPalette[3],
      "D_LPJGP" = SpectralPalette[4],
      "E_OCHDP" = SpectralPalette[5],
      "F_QUINC" = SpectralPalette[6],
      "G_OCHDX" = SpectralPalette[7],
      "H_QUJSM" = SpectralPalette[8])
    
    
    ### calculate multi-model means and then plot a vector with arrow to show directional change under eCO2
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    mgDF <- rbind(ambDF, eleDF)
    sumDF <- summaryBy(Aleaf+GPP+LAI+NL+PL+NL_leaf+PL_leaf+LCN+LCP+LNP~ModName+Trt, FUN=c(mean,sd), data=mgDF,
                        keep.names=T, na.rm=T)
    
    
    ### plot
    p1 <- ggplot() +
      geom_segment(aes(x=sumDF$PL_leaf.mean[sumDF$Trt=="amb"], xend = sumDF$PL_leaf.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=1.0)+
      geom_point(data=sumDF, 
                 aes(PL_leaf.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_text(size=14),
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
      scale_fill_manual(name="Model",
                        values=c(col.values),
                        labels=c(model.labels))+
      scale_shape_manual(name=expression(CO[2] * " treatment"),
                         values=c("amb"=21, "ele"=24),
                         labels=c("amb"=expression(aCO[2]), "ele"=expression(eCO[2])))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values)),
                                 nrow=1, byrow=TRUE))+
      xlab(expression("Leaf P content (g P " * m^-2 * " leaf)"))
    

    p2 <- ggplot() +
      #geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="amb"], xend = sumDF$NL_leaf.mean[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="ele"], xend = sumDF$NL_leaf.mean[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="amb"]+sumDF$NL_leaf.sd[sumDF$Trt=="amb"], 
      #                 xend = sumDF$NL_leaf.mean[sumDF$Trt=="amb"]-sumDF$NL_leaf.sd[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="ele"]+sumDF$NL_leaf.sd[sumDF$Trt=="ele"], 
      #                 xend = sumDF$NL_leaf.mean[sumDF$Trt=="ele"]-sumDF$NL_leaf.sd[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="amb"], xend = sumDF$NL_leaf.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=1.0)+
      geom_point(data=sumDF, aes(NL_leaf.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
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
      scale_fill_manual(name="Model",
                        values=c(col.values),
                        labels=c(model.labels))+
      scale_shape_manual(name=expression(CO[2] * " treatment"),
                         values=c("amb"=21, "ele"=24),
                         labels=c("amb", "ele"))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression("Leaf N content (g N " * m^-2 * " leaf)"))

    
    
    p3 <- ggplot() +
      #geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="amb"], xend = sumDF$LCP.mean[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="ele"], xend = sumDF$LCP.mean[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="amb"]+sumDF$LCP.sd[sumDF$Trt=="amb"], 
      #                 xend = sumDF$LCP.mean[sumDF$Trt=="amb"]-sumDF$LCP.sd[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="ele"]+sumDF$LCP.sd[sumDF$Trt=="ele"], 
      #                 xend = sumDF$LCP.mean[sumDF$Trt=="ele"]-sumDF$LCP.sd[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="amb"], xend = sumDF$LCP.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=1.0)+
      geom_point(data=sumDF, aes(LCP.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
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
      scale_fill_manual(name="Model",
                        values=c(col.values),
                        labels=c(model.labels))+
      scale_shape_manual(name=expression(CO[2] * " treatment"),
                         values=c("amb"=21, "ele"=24),
                         labels=c("amb", "ele"))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression("Leaf CP ratio"))
    
    
    
    p4 <- ggplot() +
      #geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="amb"], xend = sumDF$LCN.mean[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="ele"], xend = sumDF$LCN.mean[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="amb"]+sumDF$LCN.sd[sumDF$Trt=="amb"], 
      #                 xend = sumDF$LCN.mean[sumDF$Trt=="amb"]-sumDF$LCN.sd[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="ele"]+sumDF$LCN.sd[sumDF$Trt=="ele"], 
      #                 xend = sumDF$LCN.mean[sumDF$Trt=="ele"]-sumDF$LCN.sd[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="amb"], xend = sumDF$LCN.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=1.0)+
      geom_point(data=sumDF, aes(LCN.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
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
      scale_fill_manual(name="Model",
                        values=c(col.values),
                        labels=c(model.labels))+
      scale_shape_manual(name=expression(CO[2] * " treatment"),
                         values=c("amb"=21, "ele"=24),
                         labels=c("amb", "ele"))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression("Leaf CN ratio"))
    
    
    
    p5 <- ggplot() +
      #geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="amb"], xend = sumDF$LNP.mean[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="ele"], xend = sumDF$LNP.mean[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="amb"]+sumDF$LNP.sd[sumDF$Trt=="amb"], 
      #                 xend = sumDF$LNP.mean[sumDF$Trt=="amb"]-sumDF$LNP.sd[sumDF$Trt=="amb"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
      #             lwd=0.5, color="grey")+
      #geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="ele"]+sumDF$LNP.sd[sumDF$Trt=="ele"], 
      #                 xend = sumDF$LNP.mean[sumDF$Trt=="ele"]-sumDF$LNP.sd[sumDF$Trt=="ele"],
      #                 y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
      #                 yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
      #             lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="amb"], xend = sumDF$LNP.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=1.0)+
      geom_point(data=sumDF, aes(LNP.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
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
      scale_fill_manual(name="Model",
                        values=c(col.values),
                        labels=c(model.labels))+
      scale_shape_manual(name=expression(CO[2] * " treatment"),
                         values=c("amb"=21, "ele"=24),
                         labels=c("amb", "ele"))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression("Leaf NP ratio"))
    
    
    
    common.legend <- get_legend(p1 + theme(legend.position="right"))
    
    
    ### pdf
    #pdf(paste0(out.dir, "/MIP_photosynthesis_relationships_obs_", scenario, "_comparison.pdf"), 
    #    width=10, height=16)
    #grid.arrange(p1, p2, p3, p4, p5, common.legend,
    #          ncol = 2)
    #
    #dev.off()
    
    
    
    
    
    
    
    ##################################################################
    ### calculate multiple model means for each year
    ## amb
    ambDF.mip <- summaryBy(.~YEAR+ModName, FUN=mean,
                           data=ambDF, keep.names=T, na.rm=T)
    
    eleDF.mip <- summaryBy(.~YEAR+ModName, FUN=mean,
                           data=eleDF, keep.names=T, na.rm=T)
    
    
    ### replace inf with na
    ambDF.mip[sapply(ambDF.mip, is.infinite)] <- NA
    ambDF.mip[sapply(ambDF.mip, is.nan)] <- NA
    
    eleDF.mip[sapply(eleDF.mip, is.infinite)] <- NA
    eleDF.mip[sapply(eleDF.mip, is.nan)] <- NA
    
    
    
    ### prepare CO2 df
    co2DF <- ambDF.mip
    d.rev <- dim(co2DF)[2]
    co2DF[,5:d.rev] <- eleDF.mip[,5:d.rev]-ambDF.mip[,5:d.rev]
    
    ### prepare CO2 pct difference df
    pctco2DF <- co2DF
    pctco2DF[,5:d.rev] <- co2DF[,5:d.rev]/ambDF.mip[,5:d.rev]*100.0
    
    
    
    
    ### calculate annual averages
    sumDF2 <- summaryBy(Aleaf+GPP+LAI+NL+PL+NL_leaf+PL_leaf+LCN+LCP+LNP~ModName, FUN=c(mean,sd), data=co2DF,
                       keep.names=T, na.rm=T)
    
    sumDF3 <- summaryBy(Aleaf+GPP+LAI+NL+PL+NL_leaf+PL_leaf+LCN+LCP+LNP~ModName, FUN=c(mean,sd), data=pctco2DF,
                        keep.names=T, na.rm=T)
    
    
    ### calculate multi-model means
    pctco2DF$ModName2 <- "I_MM"
    mmDF <- summaryBy(Aleaf+GPP+LAI+NL+PL+NL_leaf+PL_leaf+LCN+LCP+LNP~ModName2, FUN=c(mean,sd), data=pctco2DF,
                      keep.names=T, na.rm=T)
    colnames(mmDF) <- colnames(sumDF3)
    
    sumDF3 <- rbind(sumDF3, mmDF)
    
    ### add data
    obsDF <- mmDF
    obsDF$NL.mean <- NA
    obsDF$NL_leaf.mean <- NA
    obsDF$NL.sd <- NA
    obsDF$NL_leaf.sd <- NA
    obsDF$LNP.mean <- NA
    obsDF$LNP.sd <- NA
    obsDF$LCN.mean <- NA
    obsDF$LCN.sd <- NA
    
    
    obsDF$GPP.mean <- eucDF$GPP[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    obsDF$GPP.sd <- eucDF$GPP[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    obsDF$LAI.mean <- eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    obsDF$LAI.sd <- eucDF$LAI[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    obsDF$PL.mean <- eucDF$PL[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    obsDF$PL.sd <- eucDF$PL[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    obsDF$LCP.mean <- eucDF$CPL[eucDF$Group=="mean"&eucDF$Trt=="pct_diff"]
    obsDF$LCP.sd <- eucDF$CPL[eucDF$Group=="sd"&eucDF$Trt=="pct_diff"]
    
    
    
    
    ### read in gpp simulation to ignore LAI effect
    tmpDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/MAESPA_output/maespa.year.ring.csv")
    
    tmpDF$Aleaf.550 <- with(tmpDF, GPP.sum.550/LAI.mean)
    tmpDF$Aleaf.400 <- with(tmpDF, GPP.sum.400/LAI.mean)
    tmpDF$Aleaf.diff <- with(tmpDF, (Aleaf.550-Aleaf.400)/Aleaf.400)
    
    ### calculate GPP field - considering LAI and SM influence
    aDF <- tmpDF[tmpDF$Ring%in%c("R2", "R3", "R6"),]
    eDF <- tmpDF[tmpDF$Ring%in%c("R1", "R4", "R5"),]
    
    asDF <- summaryBy(GPP.sum.400+Aleaf.400~year, data=aDF, FUN=mean, na.rm=T, keep.names=T)
    esDF <- summaryBy(GPP.sum.550+Aleaf.550~year, data=eDF, FUN=mean, na.rm=T, keep.names=T)
    
    mgDF <- merge(asDF, esDF, by="year")
    mgDF$diff <- with(mgDF, (GPP.sum.550-GPP.sum.400)/GPP.sum.400)
    mgDF$Aleaf_diff <- with(mgDF, (Aleaf.550-Aleaf.400)/Aleaf.400)
    
    ### ring-specific values
    obsDF$Aleaf.mean <- 11
    obsDF$Aleaf.sd <- sd(mgDF$Aleaf_diff) * 100
    
    
    ### PL leaf
    laiDF <- read.csv("validation_dataset/EucFACE_LAI_2012_2016.csv")
    
    pl <- c(0.21/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="eCO2"], 
            0.24/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="aCO2"], 
            0.20/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="aCO2"], 
            0.22/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="eCO2"], 
            0.26/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="eCO2"], 
            0.23/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="aCO2"])
    
    
    v1 <- mean(c(pl[2],pl[3],pl[6]))
    v2 <- mean(c(pl[1],pl[4],pl[5]))
    
    v3 <- sd(c(pl[2],pl[3],pl[6]))
    v4 <- sd(c(pl[1],pl[4],pl[5]))
    

    ### calculate PL leaf    
    obsDF$PL_leaf.mean <- (v2-v1)/v1 * 100
    obsDF$PL_leaf.sd <- sqrt((v3^2+v4^2)/2) / v1 * 100
    
    
    ### NL leaf
    nl <- c(4.61/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="eCO2"], 
            4.93/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="aCO2"], 
            4.34/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="aCO2"], 
            4.75/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="eCO2"], 
            5.48/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="eCO2"], 
            4.57/eucDF$LAI[eucDF$Group=="mean"&eucDF$Trt=="aCO2"])
    
    v1 <- mean(c(nl[2],nl[3],nl[6]))
    v2 <- mean(c(nl[1],nl[4],nl[5]))
    
    v3 <- sd(c(nl[2],nl[3],nl[6]))
    v4 <- sd(c(nl[1],nl[4],nl[5]))
    
    
    ### calculate NL leaf    
    obsDF$NL_leaf.mean <- (v2-v1)/v1 * 100
    obsDF$NL_leaf.sd <- sqrt((v3^2+v4^2)/2) / v1 * 100
    
    
    ### CN leaf
    v1 <- mean(c(33.88,33.95,38.3))
    v2 <- mean(c(33.27, 34.01, 33.08))
    
    v3 <- sd(c(33.88,33.95,38.3))
    v4 <- sd(c(33.27, 34.01, 33.08))
    
    
    ### calculate CN leaf    
    obsDF$LCN.mean <- (v2-v1)/v1 * 100
    obsDF$LCN.sd <- sqrt((v3^2+v4^2)/2) / v1 * 100
    
    
    ### NP leaf
    v1 <- mean(c(23.0, 22.87, 22.85))
    v2 <- mean(c(22.52, 23.55, 21.88))
    
    v3 <- sd(c(23.0, 22.87, 22.85))
    v4 <- sd(c(22.52, 23.55, 21.88))
    
    
    ### calculate CN leaf    
    obsDF$LNP.mean <- (v2-v1)/v1 * 100
    obsDF$LNP.sd <- sqrt((v3^2+v4^2)/2) / v1 * 100
    
    
    
    ### assign obs to model dataframe
    obsDF$ModName <- "OBS"
    
    sumDF3 <- rbind(sumDF3, obsDF)
    
    
    ### now prepare the plotting df
    plotDF1 <- sumDF3[sumDF3$ModName%in%c("A_ELMV1", "B_CABLP", 
                                          "C_GDAYP", "D_LPJGP",
                                          "E_OCHDP", "F_QUINC",
                                          "G_OCHDX", "H_QUJSM"),]
    
    plotDF2 <- sumDF3[sumDF3$ModName%in%c("I_MM", "OBS"),]
    
    
    
    ### plotting
    p6 <- ggplot() +
      geom_segment(aes(x=plotDF2$LCP.mean, xend = plotDF2$LCP.mean,
                       y=plotDF2$Aleaf.mean+plotDF2$Aleaf.sd, 
                       yend=plotDF2$Aleaf.mean-plotDF2$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=plotDF2$LCP.mean+plotDF2$LCP.sd, 
                       xend = plotDF2$LCP.mean-plotDF2$LCP.sd,
                       y=plotDF2$Aleaf.mean, 
                       yend=plotDF2$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(LCP.mean, Aleaf.mean, color=ModName, pch=ModName), 
                 size=4)+
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
      scale_color_manual(name="Model",
                        values=c(col.values, "I_MM"="grey30", "OBS"="grey"),
                        labels=c(model.labels, "I_MM"="M-M", "OBS"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "I_MM"=19, "OBS"=15))+
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      #guides(fill = guide_legend(override.aes = list(col = c(col.values))),
      #       color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf CP ratio (%)")))
    
    
    
    
    
    p7 <- ggplot() +
      geom_segment(aes(x=plotDF2$LCN.mean, xend = plotDF2$LCN.mean,
                       y=plotDF2$Aleaf.mean+plotDF2$Aleaf.sd, 
                       yend=plotDF2$Aleaf.mean-plotDF2$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=plotDF2$LCN.mean+plotDF2$LCN.sd, 
                       xend = plotDF2$LCN.mean-plotDF2$LCN.sd,
                       y=plotDF2$Aleaf.mean, 
                       yend=plotDF2$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(LCN.mean, Aleaf.mean, color=ModName, pch=ModName), 
                 size=4)+
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
      scale_color_manual(name="Model",
                        values=c(col.values, "I_MM"="grey30", "OBS"="grey"),
                        labels=c(model.labels, "I_MM"="M-M", "OBS"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "I_MM"=19,"OBS"=15))+
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      #guides(fill = guide_legend(override.aes = list(col = c(col.values))),
      #       color = guide_legend(nrow=1, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf CN ratio (%)")))
    
    
    p8 <- ggplot() +
      geom_segment(aes(x=plotDF2$LNP.mean, xend = plotDF2$LNP.mean,
                       y=plotDF2$Aleaf.mean+plotDF2$Aleaf.sd, 
                       yend=plotDF2$Aleaf.mean-plotDF2$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=plotDF2$LNP.mean+plotDF2$LNP.sd, 
                       xend = plotDF2$LNP.mean-plotDF2$LNP.sd,
                       y=plotDF2$Aleaf.mean, 
                       yend=plotDF2$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(LNP.mean, Aleaf.mean, color=ModName, pch=ModName), 
                 size=4)+
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
      scale_color_manual(name="Model",
                        values=c(col.values, "I_MM"="grey30", "OBS"="grey"),
                        labels=c(model.labels, "I_MM"="M-M", "OBS"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "I_MM"=19,"OBS"=15))+
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      #guides(fill = guide_legend(override.aes = list(col = c(col.values))),
      #       color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf NP ratio (%)")))
    
    

    p9 <- ggplot() +
      geom_segment(aes(x=plotDF2$PL_leaf.mean, xend = plotDF2$PL_leaf.mean,
                       y=plotDF2$Aleaf.mean+plotDF2$Aleaf.sd, 
                       yend=plotDF2$Aleaf.mean-plotDF2$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=plotDF2$PL_leaf.mean+plotDF2$PL_leaf.sd, 
                       xend = plotDF2$PL_leaf.mean-plotDF2$PL_leaf.sd,
                       y=plotDF2$Aleaf.mean, 
                       yend=plotDF2$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(PL_leaf.mean, Aleaf.mean, color=ModName, pch=ModName), 
                 size=4)+
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
      scale_color_manual(name="Model",
                        values=c(col.values, "I_MM"="grey30", "OBS"="grey"),
                        labels=c(model.labels, "I_MM"="M-M", "OBS"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "I_MM"=19,"OBS"=15),
                         labels=c(model.labels, "I_MM"="M-M", "OBS"="OBS"))+
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      #guides(fill = guide_legend(override.aes = list(col = c(col.values))),
      #       color = guide_legend(nrow=1, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf P content (%)")))
    
    
    
    p10 <- ggplot() +
      geom_segment(aes(x=plotDF2$NL_leaf.mean, xend = plotDF2$NL_leaf.mean,
                       y=plotDF2$Aleaf.mean+plotDF2$Aleaf.sd, 
                       yend=plotDF2$Aleaf.mean-plotDF2$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=plotDF2$NL_leaf.mean+plotDF2$NL_leaf.sd, 
                       xend = plotDF2$NL_leaf.mean-plotDF2$NL_leaf.sd,
                       y=plotDF2$Aleaf.mean, 
                       yend=plotDF2$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(NL_leaf.mean, Aleaf.mean, color=ModName, pch=ModName), 
                 size=4)+
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
      scale_color_manual(name="Model",
                        values=c(col.values, "I_MM"="grey30", "OBS"="grey"),
                        labels=c(model.labels, "I_MM"="M-M", "OBS"="OBS"))+
      scale_shape_manual(name="Model",
                         values=c("C_GDAYP"=19,"A_ELMV1"=19,
                                  "B_CABLP"=19,"D_LPJGP"=19,
                                  "E_OCHDP"=19,"F_QUINC"=19,
                                  "G_OCHDX"=19,"H_QUJSM"=19,
                                  "I_MM"=19,"OBS"=15))+
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      #guides(fill = guide_legend(override.aes = list(col = c(col.values))),
      #       color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf N content (%)")))
    
    
    
    
    
    plot_nutrient_row1 <- plot_grid(p1, p2, p3, p4, p5,
                                    labels=c("A", "B", "C", "D", "E"),
                                    ncol=5, align="vh", axis = "l",
                                    label_x=0.86, label_y=0.95,
                                    label_size = 20)
    
    
    legend_row1 <- get_legend(p1 + theme(legend.position="bottom",
                                         legend.box = 'horizontal',
                                         legend.box.just = 'left'))
    
    
    
    plot_nutrient_row2 <- plot_grid(p9, p10, p6, p7, p8,
                                    labels=c("F", "G", "H", "I", "J"),
                                    ncol=5, align="vh", axis = "l",
                                    label_x=0.86, label_y=0.95,
                                    label_size = 20)
    
    
    legend_row2 <- get_legend(p9 + theme(legend.position="bottom",
                                         legend.box = 'horizontal',
                                         legend.box.just = 'left')
                              + guides(fill = guide_legend(override.aes = list(col = c(col.values))),
                                       color = guide_legend(nrow=1, byrow=F)))
    
    
    
    pdf(paste0(out.dir, "/MIP_normalized_photosynthesis_response_OBS_", 
               scenario, "_comparison_with_obs3.pdf"), 
        width=18, height=8)
    plot_grid(plot_nutrient_row1,
              legend_row1,
              plot_nutrient_row2,
              legend_row2,
              rel_heights=c(1,0.2,1,0.2),
              labels=c(""),
              ncol=1, align="vh", axis = "l",
              label_x=0.84, label_y=0.95,
              label_size = 20)
    
    dev.off()
    
    
  
}    

