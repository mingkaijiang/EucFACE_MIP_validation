plot_normalized_GPP_response <- function(scenario) {
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
    
    
    ambDF <- ambDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI")]
    
    eleDF <- eleDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI")]
    
    ambDF$Aleaf <- with(ambDF, GPP/LAI)
    eleDF$Aleaf <- with(eleDF, GPP/LAI)
    
    ### calculate annual mean
    ambDF2 <- summaryBy(GPP+LAI+Aleaf~YEAR+ModName, FUN=c(mean,sd),
                        na.rm=T, keep.names=T, data=ambDF)
    
    eleDF2 <- summaryBy(GPP+LAI+Aleaf~YEAR+ModName, FUN=c(mean,sd),
                        na.rm=T, keep.names=T, data=eleDF)
    
    
    ambDF3 <- summaryBy(GPP+Aleaf~YEAR+ModName, FUN=sum,
                       na.rm=T, keep.names=T, data=ambDF)
    
    eleDF3 <- summaryBy(GPP+Aleaf~YEAR+ModName, FUN=sum,
                        na.rm=T, keep.names=T, data=eleDF)
    
    
    ambDF4 <- summaryBy(LAI~YEAR+ModName, FUN=max,
                        na.rm=T, keep.names=T, data=ambDF)
    
    eleDF4 <- summaryBy(LAI+Aleaf~YEAR+ModName, FUN=max,
                        na.rm=T, keep.names=T, data=eleDF)
    
    
    
    ##################################################################
    mod.list <- unique(ambDF2$ModNmae)
    
    #require(gridExtra)
  
    ### Aleaf
    p1 <- ggplot() +
      geom_point(data=ambDF2, aes(YEAR, Aleaf.mean, col=ModName))+
      geom_line(data=ambDF2, aes(YEAR, Aleaf.mean, col=ModName, lty=ModName))+
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
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1,4))
    
    
    p2 <- ggplot() +
      geom_point(data=eleDF2, aes(YEAR, Aleaf.mean, col=ModName))+
      geom_line(data=eleDF2, aes(YEAR, Aleaf.mean, col=ModName, lty=ModName))+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1,4))
    
    ### LAI
    p3 <- ggplot() +
      geom_point(data=ambDF2, aes(YEAR, LAI.mean, col=ModName))+
      geom_line(data=ambDF2, aes(YEAR,LAI.mean, col=ModName, lty=ModName))+
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
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("LAI")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1,4.5))
    
    
    p4 <- ggplot() +
      geom_point(data=eleDF2, aes(YEAR, LAI.mean, col=ModName))+
      geom_line(data=eleDF2, aes(YEAR, LAI.mean, col=ModName, lty=ModName))+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste("LAI")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1,4.5))
    
    
    p5 <- ggplot() +
      geom_point(data=ambDF3, aes(YEAR, GPP, col=ModName))+
      geom_line(data=ambDF3, aes(YEAR, GPP, col=ModName, lty=ModName))+
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
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(GPP* " (g C " * m^2 * " " * yr^-1, ")")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1000,3500))
    
    
    p6 <- ggplot() +
      geom_point(data=eleDF3, aes(YEAR, GPP, col=ModName))+
      geom_line(data=eleDF3, aes(YEAR, GPP, col=ModName, lty=ModName))+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.text.x=element_text(size=12),
            axis.title.x=element_blank(),
            axis.text.y=element_text(size=12),
            axis.title.y=element_blank(),
            legend.text=element_text(size=12),
            legend.title=element_text(size=14),
            panel.grid.major=element_blank(),
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            legend.key.width = unit(1.5,"cm"),
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(GPP * " (g C " * m^2 * " " * yr^-1, ")")))+
      scale_color_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      scale_linetype_manual(name="Model", 
                            values=linetype.values,
                            labels=model.labels)+
      guides(fill = guide_legend(override.aes = list(col = col.values,
                                                     lty = linetype.values),
                                 nrow=2, byrow=F))+
      xlab(expression(paste("Year")))+
      ylim(c(1000,3500))
    
    legend_top_row <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    plots_top_row <- plot_grid(p1, p2, p3, p4, p5, p6, 
                               labels=c("(a)", "(b)", "(c)", "(d)",
                                        "(e)", "(f)"),
                               ncol=2, align="vh", axis = "l",
                               label_x=0.86, label_y=0.95,
                               label_size = 18)
    
    
    pdf(paste0(out.dir, "/MIP_normalized_photosynthesis_response_obs_", scenario, "_amb_comparison.pdf"), 
        width=12, height=16)
    plot_grid(plots_top_row,
              legend_top_row,
              ncol=1, rel_heights=c(1,0.2))
    
    dev.off()
    
    
    ### plot time-averaged comparison
    ambDF2$Trt <- "amb"
    eleDF2$Trt <- "ele"
    mgDF1 <- rbind(ambDF2, eleDF2)
    
    sumDF1 <- summaryBy(Aleaf.mean+LAI.mean~ModName+Trt, FUN=c(mean, sd),
                       keep.names=T, na.rm=T, data=mgDF1)
    
    ambDF3$Trt <- "amb"
    eleDF3$Trt <- "ele"
    mgDF2 <- rbind(ambDF3, eleDF3)
    
    sumDF2 <- summaryBy(GPP~ModName+Trt, FUN=c(mean, sd),
                        keep.names=T, na.rm=T, data=mgDF2)
    
    mod.list <- unique(sumDF2$ModName)
    
    ### plotting
    p1 <- ggplot(data=sumDF1, 
                 aes(ModName, Aleaf.mean.mean, group=Trt)) +
      geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(5.5, 7.5, 9.5, 11.5), lty=2)+
      geom_errorbar(aes(x=ModName, ymin=Aleaf.mean.mean-Aleaf.mean.sd, 
                        ymax=Aleaf.mean.mean+Aleaf.mean.sd), 
                    position=position_dodge(width=1)) +
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      scale_alpha_manual(name="Treatment",
                         values=c("amb" = 0.3, 
                                  "ele" = 1.0),
                         label=c("AMB", "ELE"))+
      scale_fill_manual(name="Model",
                         values=col.values,
                         labels=model.labels)+
      guides(alpha=guide_legend("Treatment"), fill = FALSE)+
      scale_x_discrete(limit=c(mod.list),
                       label=c(model.labels)); p1
    
    
    
    p2 <- ggplot(data=sumDF1, 
                 aes(ModName, LAI.mean.mean, group=Trt)) +
      geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(5.5, 7.5, 9.5, 11.5), lty=2)+
      geom_errorbar(aes(x=ModName, ymin=LAI.mean.mean-LAI.mean.sd, 
                        ymax=LAI.mean.mean+LAI.mean.sd), 
                    position=position_dodge(width=1)) +
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
      ylab(expression(paste("LAI")))+
      scale_alpha_manual(name="Treatment",
                         values=c("amb" = 0.3, 
                                  "ele" = 1.0),
                         label=c("AMB", "ELE"))+
      scale_fill_manual(name="Model",
                        values=col.values,
                        labels=model.labels)+
      guides(alpha=guide_legend("Treatment"), fill = FALSE)+
      scale_x_discrete(limit=c(mod.list),
                       label=c(model.labels))
    
    
    p3 <- ggplot(data=sumDF2, 
                 aes(ModName, GPP.mean, group=Trt)) +
      geom_bar(stat = "identity", aes(fill=ModName, alpha=Trt), 
               position=position_dodge(), col="black") +
      geom_vline(xintercept=c(5.5, 7.5, 9.5, 11.5), lty=2)+
      geom_errorbar(aes(x=ModName, ymin=GPP.mean-GPP.sd, 
                        ymax=GPP.mean+GPP.sd), 
                    position=position_dodge(width=1)) +
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
      ylab(expression(paste("GPP (g C " * m^2 * " " * yr^-1, ")")))+
      scale_alpha_manual(name="Treatment",
                         values=c("amb" = 0.3, 
                                  "ele" = 1.0),
                         label=c("AMB", "ELE"))+
      scale_fill_manual(name="Model",
                        values=col.values,
                        labels=model.labels)+
      guides(alpha=guide_legend("Treatment"), fill = FALSE)+
      scale_x_discrete(limit=c(mod.list),
                       label=c(model.labels))
    
    
    legend_top_row <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    plots_top_row <- plot_grid(p1, p2, p3, 
                               labels=c("(a)", "(b)", "(c)"),
                               ncol=1, align="vh", axis = "l",
                               label_x=0.86, label_y=0.95,
                               label_size = 18)
    
    
    pdf(paste0(out.dir, "/MIP_normalized_photosynthesis_response_obs_", scenario, "_comparison.pdf"), 
        width=12, height=16)
    plot_grid(plots_top_row,
              legend_top_row,
              ncol=1, rel_heights=c(1,0.1))
    
    dev.off()
    
    

}    

