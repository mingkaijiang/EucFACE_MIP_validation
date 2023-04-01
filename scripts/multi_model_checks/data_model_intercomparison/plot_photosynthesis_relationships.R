plot_photosynthesis_relationships <- function(scenario) {
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
    ambDF <- ambDF[ambDF$ModName%in%c("A_GDAYP",
                                      "B_ELMV1",
                                      "C_CABLP",
                                      "D_LPJGP",
                                      "E_OCHDP",
                                      "F_QUINC",
                                      "G_OCHDX",
                                      "H_QUJSM"),]
    
    eleDF <- eleDF[eleDF$ModName%in%c("A_GDAYP",
                                      "B_ELMV1",
                                      "C_CABLP",
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
    
    
    d <- dim(ambDF)[2]
    
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
    
    
    
    mod.list <- unique(ambDF$ModNmae)
    
    require(gridExtra)
    
    
    #p1 <- ggplot(data=ambDF, aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i)); p1
    #
    #
    #require(plotly)
    #plot_ly(x=ambDF$NL, y=ambDF$PL, z=ambDF$Aleaf, type="scatter3d", mode="markers", color=ambDF$ModName)
    #
    #plot_ly(x=ambDF$LCN, y=ambDF$LCP, z=ambDF$Aleaf, type="scatter3d", mode="markers", color=ambDF$ModName)
    #
    #plot_ly(x=ambDF$LCN[ambDF$ModName=="C_CABLP"], y=ambDF$LCP[ambDF$ModName=="C_CABLP"], 
    #        z=ambDF$Aleaf[ambDF$ModName=="C_CABLP"], type="scatter3d", mode="markers", color=ambDF$Aleaf[ambDF$ModName=="C_CABLP"])
    
    
    ### calculate multi-model means and then plot a vector with arrow to show directional change under eCO2
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    mgDF <- rbind(ambDF, eleDF)
    sumDF <- summaryBy(Aleaf+GPP+LAI+NL+PL+LCN+LCP+LNP~ModName+Trt, FUN=c(mean,sd), data=mgDF,
                        keep.names=T, na.rm=T)
    
    
    ### plot
    p1 <- ggplot() +
      geom_segment(aes(x=sumDF$PL.mean[sumDF$Trt=="amb"], xend = sumDF$PL.mean[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$PL.mean[sumDF$Trt=="ele"], xend = sumDF$PL.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$PL.mean[sumDF$Trt=="amb"]+sumDF$PL.sd[sumDF$Trt=="amb"], 
                       xend = sumDF$PL.mean[sumDF$Trt=="amb"]-sumDF$PL.sd[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$PL.mean[sumDF$Trt=="ele"]+sumDF$PL.sd[sumDF$Trt=="ele"], 
                       xend = sumDF$PL.mean[sumDF$Trt=="ele"]-sumDF$PL.sd[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$PL.mean[sumDF$Trt=="amb"], xend = sumDF$PL.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=2.0)+
      geom_point(data=sumDF, aes(PL.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
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
      xlab(expression("Leaf P content (g P " * m^-2 * ")"))
    
    
    
    p2 <- ggplot() +
      geom_segment(aes(x=sumDF$NL.mean[sumDF$Trt=="amb"], xend = sumDF$NL.mean[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$NL.mean[sumDF$Trt=="ele"], xend = sumDF$NL.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$NL.mean[sumDF$Trt=="amb"]+sumDF$NL.sd[sumDF$Trt=="amb"], 
                       xend = sumDF$NL.mean[sumDF$Trt=="amb"]-sumDF$NL.sd[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$NL.mean[sumDF$Trt=="ele"]+sumDF$NL.sd[sumDF$Trt=="ele"], 
                       xend = sumDF$NL.mean[sumDF$Trt=="ele"]-sumDF$NL.sd[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$NL.mean[sumDF$Trt=="amb"], xend = sumDF$NL.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=2.0)+
      geom_point(data=sumDF, aes(NL.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
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
      xlab(expression("Leaf N content (g N " * m^-2 * ")"))

    
    
    p3 <- ggplot() +
      geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="amb"], xend = sumDF$LCP.mean[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="ele"], xend = sumDF$LCP.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="amb"]+sumDF$LCP.sd[sumDF$Trt=="amb"], 
                       xend = sumDF$LCP.mean[sumDF$Trt=="amb"]-sumDF$LCP.sd[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="ele"]+sumDF$LCP.sd[sumDF$Trt=="ele"], 
                       xend = sumDF$LCP.mean[sumDF$Trt=="ele"]-sumDF$LCP.sd[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCP.mean[sumDF$Trt=="amb"], xend = sumDF$LCP.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=2.0)+
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
      geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="amb"], xend = sumDF$LCN.mean[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="ele"], xend = sumDF$LCN.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="amb"]+sumDF$LCN.sd[sumDF$Trt=="amb"], 
                       xend = sumDF$LCN.mean[sumDF$Trt=="amb"]-sumDF$LCN.sd[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="ele"]+sumDF$LCN.sd[sumDF$Trt=="ele"], 
                       xend = sumDF$LCN.mean[sumDF$Trt=="ele"]-sumDF$LCN.sd[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LCN.mean[sumDF$Trt=="amb"], xend = sumDF$LCN.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=2.0)+
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
      geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="amb"], xend = sumDF$LNP.mean[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="ele"], xend = sumDF$LNP.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="amb"]+sumDF$LNP.sd[sumDF$Trt=="amb"], 
                       xend = sumDF$LNP.mean[sumDF$Trt=="amb"]-sumDF$LNP.sd[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="ele"]+sumDF$LNP.sd[sumDF$Trt=="ele"], 
                       xend = sumDF$LNP.mean[sumDF$Trt=="ele"]-sumDF$LNP.sd[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$LNP.mean[sumDF$Trt=="amb"], xend = sumDF$LNP.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=2.0)+
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
    pdf(paste0(out.dir, "/MIP_photosynthesis_relationships_obs_", scenario, "_comparison.pdf"), 
        width=10, height=16)
    grid.arrange(p1, p2, p3, p4, p5, common.legend,
              ncol = 2)
    
    dev.off()
    
    
    
    
    
    
    
    ##################################################################
    ##### Model
    #i <- "CABLP"
    #
    #### leaf NC ratio
    #p1 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p2 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
 #
    #  
    ##### Model
    #i <- "OCHDP"
    #
    #### leaf NC ratio
    #p3 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p4 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "GDAYP"
    #
    #### leaf NC ratio
    #p5 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p6 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "LPJGP"
    #
    #### leaf NC ratio
    #p7 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p8 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "OCHDX"
    #
    #### leaf NC ratio
    #p9 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p10 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "QUINC"
    #
    #### leaf NC ratio
    #p11 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p12 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "QUJSM"
    #
    #### leaf NC ratio
    #p13 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p14 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
 #
    #
    #
    #pdf(paste0(out.dir, "/MIP_photosynthesis_relationships_obs_", scenario, "_amb_comparison.pdf"), 
    #    width=6, height=20)
    #grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
    #          p11, p12, p13, p14,
    #          ncol = 2)
    #
    #dev.off()
    #
    #
    #
    #
    ###################################################################
    #
    #
    #
    ##### Model
    #i <- "CABLP"
    #
    #### leaf NC ratio
    #p1 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p2 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    #
    ##### Model
    #i <- "OCHDP"
    #
    #### leaf NC ratio
    #p3 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p4 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "GDAYP"
    #
    #### leaf NC ratio
    #p5 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p6 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "LPJGP"
    #
    #### leaf NC ratio
    #p7 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p8 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "OCHDX"
    #
    #### leaf NC ratio
    #p9 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p10 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "QUINC"
    #
    #### leaf NC ratio
    #p11 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p12 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "QUJSM"
    #
    #### leaf NC ratio
    #p13 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p14 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    #
    #pdf(paste0(out.dir, "/MIP_photosynthesis_relationships_obs_", scenario, "_ele_comparison.pdf"), 
    #    width=6, height=20)
    #grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
    #             p11, p12, p13, p14,
    #             ncol = 2)
    #
    #dev.off()
    #
    #
    ###################################################################
    #### change in nutrient concentration
    #
    ##sumDF <- summaryBy(Aleaf+LCN+LCP~ModName, FUN=c(mean, sd),
    ##                   data=co2DF, na.rm=T, keep.names=T)
    #
    ##### Model
    #i <- "CABLP"
    #
    #### leaf NC ratio
    #p1 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p2 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    #
    ##### Model
    #i <- "OCHDP"
    #
    #### leaf NC ratio
    #p3 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p4 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "GDAYP"
    #
    #### leaf NC ratio
    #p5 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p6 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "LPJGP"
    #
    #### leaf NC ratio
    #p7 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p8 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "OCHDX"
    #
    #### leaf NC ratio
    #p9 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p10 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "QUINC"
    #
    #### leaf NC ratio
    #p11 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p12 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "QUJSM"
    #
    #### leaf NC ratio
    #p13 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p14 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
    #  geom_hex(bins=50) +
    #  geom_smooth(method = lm, se = FALSE)+
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
    #  ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste(Delta * "Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    #
    #pdf(paste0(out.dir, "/MIP_photosynthesis_relationships_obs_", scenario, "_co2_abs_comparison.pdf"), 
    #    width=6, height=20)
    #grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
    #             p11, p12, p13, p14,
    #             ncol = 2)
    #
    #dev.off()
    #
    #
    ###################################################################
    #### arrow plot
    #mgDF <- merge(ambDF, eleDF, by=c("YEAR", "DOY", "Date", "ModName"))
    #
    #mgDF$LCNgroup <- round(mgDF$LCN.x, 1)
    #mgDF$LCPgroup <- round(mgDF$LCP.x, 0)
    #
    ##mgDF$LCPgroup[mgDF$ModName%in%c("GDAYP","LPJGP", "QUINC", "QUJSM")]
    #
    #
    #
    #mgDF.lcn <- summaryBy(LCN.x+LCN.y+Aleaf.x+Aleaf.y~ModName+LCNgroup, 
    #                      FUN=mean, data=mgDF,
    #                      na.rm=T, keep.names=T)
    #
    #mgDF.lcp <- summaryBy(LCP.x+LCP.y+Aleaf.x+Aleaf.y~ModName+LCPgroup, 
    #                      FUN=mean, data=mgDF,
    #                      na.rm=T, keep.names=T)
    #
    #
    ##### Model
    #i <- "CABLP"
    #
    #### leaf NC ratio
    #p1 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    #p2 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    #
    #
    ##### Model
    #i <- "OCHDP"
    #
    #### leaf NC ratio
    #p3 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    #### leaf PC ratio
    #p4 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "GDAYP"
    #
    #### leaf NC ratio
    #p5 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p6 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "LPJGP"
    #
    #### leaf NC ratio
    #p7 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p8 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "OCHDX"
    #
    #### leaf NC ratio
    #p9 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p10 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "QUINC"
    #
    #### leaf NC ratio
    #p11 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p12 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    ##### Model
    #i <- "QUJSM"
    #
    #### leaf NC ratio
    #p13 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
    #               aes(x = LCN.x, y = Aleaf.x, 
    #                   xend = LCN.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CN ratio")))+
    #  ggtitle(paste0(i))
    #
    #### leaf PC ratio
    #p14 <- ggplot() +
    #  geom_segment(data=mgDF[mgDF$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = alpha("yellow", 0.5),
    #               arrow = arrow(length = unit(0.03, "npc")))+
    #  geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
    #               aes(x = LCP.x, y = Aleaf.x, 
    #                   xend = LCP.y, yend = Aleaf.y),
    #               colour = "black",
    #               arrow = arrow(length = unit(0.03, "npc")))+
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
    #  ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
    #  xlab(expression(paste("Leaf CP ratio")))+
    #  ggtitle(paste0(i))
    #
    #
    #
    #pdf(paste0(out.dir, "/MIP_photosynthesis_relationships_obs_", scenario, "_co2_arrow_comparison.pdf"), 
    #    width=6, height=20)
    #grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
    #             p11, p12, p13, p14,
    #             ncol = 2)
    #
    #dev.off()
    
    #require(scatterplot3d)
    #scatterplot3d(x = ambDF$GPP[ambDF$ModName==i], 
    #       y = ambDF$LCN[ambDF$ModName==i], 
    #       z = ambDF$LCP[ambDF$ModName==i],
    #       xlab="GPP", ylab="leaf CN", zlab="leaf CP")
    #
    #i <- "GDAYP"
    #test <- ambDF[ambDF$ModName==i,]
    #  
    #scatterplot3d(x = test$GPP, 
    #                     y = test$LCN, 
    #                     z = test$LCP,
    #                     xlab="GPP", ylab="leaf CN", zlab="leaf CP",
    #                     pch = 16, highlight.3d=T)
    #
    
    
    
  
    
    
}    

