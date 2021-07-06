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
    
    #### Model
    i <- "CABLP"
    
    ### leaf NC ratio
    p1 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p2 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
 
      
    #### Model
    i <- "OCHDP"
    
    ### leaf NC ratio
    p3 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p4 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "GDAYP"
    
    ### leaf NC ratio
    p5 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p6 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "LPJGP"
    
    ### leaf NC ratio
    p7 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p8 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "OCHDX"
    
    ### leaf NC ratio
    p9 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p10 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "QUINC"
    
    ### leaf NC ratio
    p11 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p12 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "QUJSM"
    
    ### leaf NC ratio
    p13 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p14 <- ggplot(data=ambDF[ambDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
 
    
    
    pdf(paste0(out.dir, "/MIP_photosynthesis_relationships_obs_", scenario, "_amb_comparison.pdf"), 
        width=6, height=20)
    grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
              p11, p12, p13, p14,
              ncol = 2)
    
    dev.off()
    
    
    
    
    ##################################################################
    
    
    
    #### Model
    i <- "CABLP"
    
    ### leaf NC ratio
    p1 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p2 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    
    #### Model
    i <- "OCHDP"
    
    ### leaf NC ratio
    p3 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p4 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "GDAYP"
    
    ### leaf NC ratio
    p5 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p6 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "LPJGP"
    
    ### leaf NC ratio
    p7 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p8 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "OCHDX"
    
    ### leaf NC ratio
    p9 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p10 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "QUINC"
    
    ### leaf NC ratio
    p11 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p12 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "QUJSM"
    
    ### leaf NC ratio
    p13 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p14 <- ggplot(data=eleDF[eleDF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    
    pdf(paste0(out.dir, "/MIP_photosynthesis_relationships_obs_", scenario, "_ele_comparison.pdf"), 
        width=6, height=20)
    grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                 p11, p12, p13, p14,
                 ncol = 2)
    
    dev.off()
    
    
    ##################################################################
    ### change in nutrient concentration
    
    #sumDF <- summaryBy(Aleaf+LCN+LCP~ModName, FUN=c(mean, sd),
    #                   data=co2DF, na.rm=T, keep.names=T)
    
    #### Model
    i <- "CABLP"
    
    ### leaf NC ratio
    p1 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p2 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    
    #### Model
    i <- "OCHDP"
    
    ### leaf NC ratio
    p3 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p4 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "GDAYP"
    
    ### leaf NC ratio
    p5 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p6 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "LPJGP"
    
    ### leaf NC ratio
    p7 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p8 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "OCHDX"
    
    ### leaf NC ratio
    p9 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p10 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "QUINC"
    
    ### leaf NC ratio
    p11 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p12 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "QUJSM"
    
    ### leaf NC ratio
    p13 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCN, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p14 <- ggplot(data=co2DF[co2DF$ModName==i,], aes(LCP, Aleaf)) +
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
            legend.position="none",
            legend.box = 'horizontal',
            legend.box.just = 'left',
            plot.title = element_text(size=14, face="bold.italic", 
                                      hjust = 0.5))+
      ylab(expression(paste(Delta * A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste(Delta * "Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    
    pdf(paste0(out.dir, "/MIP_photosynthesis_relationships_obs_", scenario, "_co2_abs_comparison.pdf"), 
        width=6, height=20)
    grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                 p11, p12, p13, p14,
                 ncol = 2)
    
    dev.off()
    
    
    ##################################################################
    ### arrow plot
    mgDF <- merge(ambDF, eleDF, by=c("YEAR", "DOY", "Date", "ModName"))
    
    mgDF$LCNgroup <- round(mgDF$LCN.x, 1)
    mgDF$LCPgroup <- round(mgDF$LCP.x, 0)
    
    #mgDF$LCPgroup[mgDF$ModName%in%c("GDAYP","LPJGP", "QUINC", "QUJSM")]
    
    
    
    mgDF.lcn <- summaryBy(LCN.x+LCN.y+Aleaf.x+Aleaf.y~ModName+LCNgroup, 
                          FUN=mean, data=mgDF,
                          na.rm=T, keep.names=T)
    
    mgDF.lcp <- summaryBy(LCP.x+LCP.y+Aleaf.x+Aleaf.y~ModName+LCPgroup, 
                          FUN=mean, data=mgDF,
                          na.rm=T, keep.names=T)
    
    
    #### Model
    i <- "CABLP"
    
    ### leaf NC ratio
    p1 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    
    p2 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    
    
    #### Model
    i <- "OCHDP"
    
    ### leaf NC ratio
    p3 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    
    ### leaf PC ratio
    p4 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "GDAYP"
    
    ### leaf NC ratio
    p5 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p6 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "LPJGP"
    
    ### leaf NC ratio
    p7 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p8 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "OCHDX"
    
    ### leaf NC ratio
    p9 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p10 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "QUINC"
    
    ### leaf NC ratio
    p11 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p12 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    #### Model
    i <- "QUJSM"
    
    ### leaf NC ratio
    p13 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcn[mgDF.lcn$ModName==i,], 
                   aes(x = LCN.x, y = Aleaf.x, 
                       xend = LCN.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CN ratio")))+
      ggtitle(paste0(i))
    
    ### leaf PC ratio
    p14 <- ggplot() +
      geom_segment(data=mgDF[mgDF$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = alpha("yellow", 0.5),
                   arrow = arrow(length = unit(0.03, "npc")))+
      geom_segment(data=mgDF.lcp[mgDF.lcp$ModName==i,], 
                   aes(x = LCP.x, y = Aleaf.x, 
                       xend = LCP.y, yend = Aleaf.y),
                   colour = "black",
                   arrow = arrow(length = unit(0.03, "npc")))+
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
      ylab(expression(paste(A[leaf]* " (g C " * m^2 * " " * d^-1, ")")))+
      xlab(expression(paste("Leaf CP ratio")))+
      ggtitle(paste0(i))
    
    
    
    pdf(paste0(out.dir, "/MIP_photosynthesis_relationships_obs_", scenario, "_co2_arrow_comparison.pdf"), 
        width=6, height=20)
    grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                 p11, p12, p13, p14,
                 ncol = 2)
    
    dev.off()
    
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

