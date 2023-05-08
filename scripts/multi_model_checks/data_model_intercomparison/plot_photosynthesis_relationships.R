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
    
    
    ambDF$NL_leaf <- with(ambDF, NL/LAI)
    ambDF$PL_leaf <- with(ambDF, PL/LAI)
    
    eleDF$NL_leaf <- with(eleDF, NL/LAI)
    eleDF$PL_leaf <- with(eleDF, PL/LAI)
    
    
    d <- dim(ambDF)[2]

    
    
    
    
    ##################################################################
    
    
    
    mod.list <- unique(ambDF$ModNmae)
    
    require(gridExtra)
    
    
    
    ### calculate multi-model means and then plot a vector with arrow to show directional change under eCO2
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    mgDF <- rbind(ambDF, eleDF)
    sumDF <- summaryBy(Aleaf+GPP+LAI+NL+PL+NL_leaf+PL_leaf+LCN+LCP+LNP~ModName+Trt, FUN=c(mean,sd), data=mgDF,
                        keep.names=T, na.rm=T)
    
    
    ### plot
    p1 <- ggplot() +
      geom_segment(aes(x=sumDF$PL_leaf.mean[sumDF$Trt=="amb"], xend = sumDF$PL_leaf.mean[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$PL_leaf.mean[sumDF$Trt=="ele"], xend = sumDF$PL_leaf.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$PL_leaf.mean[sumDF$Trt=="amb"]+sumDF$PL_leaf.sd[sumDF$Trt=="amb"], 
                       xend = sumDF$PL_leaf.mean[sumDF$Trt=="amb"]-sumDF$PL_leaf.sd[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$PL_leaf.mean[sumDF$Trt=="ele"]+sumDF$PL_leaf.sd[sumDF$Trt=="ele"], 
                       xend = sumDF$PL_leaf.mean[sumDF$Trt=="ele"]-sumDF$PL_leaf.sd[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$PL_leaf.mean[sumDF$Trt=="amb"], xend = sumDF$PL_leaf.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=2.0)+
      geom_point(data=sumDF, aes(PL_leaf.mean, Aleaf.mean, fill=ModName, pch=Trt), color="black", size=4)+
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
      xlab(expression("Leaf P content (g P " * m^-2 * " leaf)"))
    
    
    
    p2 <- ggplot() +
      geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="amb"], xend = sumDF$NL_leaf.mean[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"]+sumDF$Aleaf.sd[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]-sumDF$Aleaf.sd[sumDF$Trt=="amb"]),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="ele"], xend = sumDF$NL_leaf.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"]+sumDF$Aleaf.sd[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]-sumDF$Aleaf.sd[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="amb"]+sumDF$NL_leaf.sd[sumDF$Trt=="amb"], 
                       xend = sumDF$NL_leaf.mean[sumDF$Trt=="amb"]-sumDF$NL_leaf.sd[sumDF$Trt=="amb"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="amb"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="ele"]+sumDF$NL_leaf.sd[sumDF$Trt=="ele"], 
                       xend = sumDF$NL_leaf.mean[sumDF$Trt=="ele"]-sumDF$NL_leaf.sd[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="ele"], 
                       yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF$NL_leaf.mean[sumDF$Trt=="amb"], xend = sumDF$NL_leaf.mean[sumDF$Trt=="ele"],
                       y=sumDF$Aleaf.mean[sumDF$Trt=="amb"], yend=sumDF$Aleaf.mean[sumDF$Trt=="ele"]), 
                   lwd=2.0)+
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
    
    
    ### add data
    
    
    
    
    ### plotting
    p1 <- ggplot() +
      geom_segment(aes(x=sumDF3$LCP.mean, xend = sumDF3$LCP.mean,
                       y=sumDF3$Aleaf.mean+sumDF3$Aleaf.sd, 
                       yend=sumDF3$Aleaf.mean-sumDF3$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF3$LCP.mean+sumDF3$LCP.sd, 
                       xend = sumDF3$LCP.mean-sumDF3$LCP.sd,
                       y=sumDF3$Aleaf.mean, 
                       yend=sumDF3$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(LCP.mean, Aleaf.mean, fill=ModName), 
                 pch=21,color="black", size=4)+
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
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf CP ratio (%)"))); p1
    
    
    
    
    
    p2 <- ggplot() +
      geom_segment(aes(x=sumDF3$LCN.mean, xend = sumDF3$LCN.mean,
                       y=sumDF3$Aleaf.mean+sumDF3$Aleaf.sd, 
                       yend=sumDF3$Aleaf.mean-sumDF3$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF3$LCN.mean+sumDF3$LCN.sd, 
                       xend = sumDF3$LCN.mean-sumDF3$LCN.sd,
                       y=sumDF3$Aleaf.mean, 
                       yend=sumDF3$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(LCN.mean, Aleaf.mean, fill=ModName), 
                 pch=21,color="black", size=4)+
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
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf CN ratio (%)"))); p2
    
    
    p3 <- ggplot() +
      geom_segment(aes(x=sumDF3$LNP.mean, xend = sumDF3$LNP.mean,
                       y=sumDF3$Aleaf.mean+sumDF3$Aleaf.sd, 
                       yend=sumDF3$Aleaf.mean-sumDF3$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF3$LNP.mean+sumDF3$LNP.sd, 
                       xend = sumDF3$LNP.mean-sumDF3$LNP.sd,
                       y=sumDF3$Aleaf.mean, 
                       yend=sumDF3$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(LNP.mean, Aleaf.mean, fill=ModName), 
                 pch=21,color="black", size=4)+
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
      #xlim(-10, 50)+
      #ylim(-10, 50)+
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf NP ratio (%)"))); p3
    
    

    p4 <- ggplot() +
      geom_segment(aes(x=sumDF3$PL_leaf.mean, xend = sumDF3$PL_leaf.mean,
                       y=sumDF3$Aleaf.mean+sumDF3$Aleaf.sd, 
                       yend=sumDF3$Aleaf.mean-sumDF3$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF3$PL_leaf.mean+sumDF3$PL_leaf.sd, 
                       xend = sumDF3$PL_leaf.mean-sumDF3$PL_leaf.sd,
                       y=sumDF3$Aleaf.mean, 
                       yend=sumDF3$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(PL_leaf.mean, Aleaf.mean, fill=ModName), 
                 pch=21,color="black", size=4)+
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
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf P content (%)"))); p4
    
    
    
    
    
    p5 <- ggplot() +
      geom_segment(aes(x=sumDF3$NL_leaf.mean, xend = sumDF3$NL_leaf.mean,
                       y=sumDF3$Aleaf.mean+sumDF3$Aleaf.sd, 
                       yend=sumDF3$Aleaf.mean-sumDF3$Aleaf.sd),
                   lwd=0.5, color="grey")+
      geom_segment(aes(x=sumDF3$NL_leaf.mean+sumDF3$NL_leaf.sd, 
                       xend = sumDF3$NL_leaf.mean-sumDF3$NL_leaf.sd,
                       y=sumDF3$Aleaf.mean, 
                       yend=sumDF3$Aleaf.mean), 
                   lwd=0.5, color="grey")+
      geom_point(data=sumDF3, aes(NL_leaf.mean, Aleaf.mean, fill=ModName), 
                 pch=21,color="black", size=4)+
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
      ylab(expression(paste(CO[2] * " effect on " * A[leaf]* " (%)")))+
      guides(fill = guide_legend(override.aes = list(col = c(col.values))),
             color = guide_legend(nrow=12, byrow=F))+
      xlab(expression(paste(CO[2] * " effect on leaf N content (%)"))); p5
    
    
    
  
}    

