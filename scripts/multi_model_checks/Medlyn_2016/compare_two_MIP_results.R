compare_two_MIP_results <- function() {
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/FIX")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    
    ### calculate 2016 multi-model means for CNP and CN models
    medDF <- calculate_Medlyn_2016_ModVersion_means(time.period=c(2013, 2016))
    medDF2 <- calculate_Medlyn_2016_individual_model_means(time.period=c(2013, 2016))
    
    ### prepare the current MIP output
    thisDF <- calculate_this_MIP_ModVersion_means(time.period=c(2013, 2016))
    thisDF2 <- calculate_this_MIP_individual_model_means(time.period=c(2013, 2016))
    
    
    ### compare the two
    myDF <- merge_this_MIP_and_Medlyn_2016(medDF=medDF, thisDF=thisDF)
    myDF2 <- merge_this_MIP_and_Medlyn_2016_individual_model(medDF=medDF2, thisDF=thisDF2)
    
    
    col.values <- c("A_GDAYP" = SpectralPalette[1],
                    "B_ELMV1" = SpectralPalette[2],
                    "C_CABLP" = SpectralPalette[3],
                    "D_LPJGP" = SpectralPalette[4],
                    "E_OCHDP" = SpectralPalette[5],
                    "F_QUINC" = SpectralPalette[6],
                    "G_OCHDX" = SpectralPalette[7],
                    "H_QUJSM" = SpectralPalette[8],
                    "I_GDAYN" = SpectralPalette[1],
                    "J_LPJGN" = SpectralPalette[4])
    
    ### compare the two with plotting
    plotDF1 <- myDF[myDF$variable=="GPP",]
    subDF1 <- myDF2[myDF2$variable=="GPP",]
    
    p1 <- ggplot(data=plotDF1, 
                 aes(ModVersion, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position=position_dodge(), col="black") +
        geom_point(data=subDF1, aes(ModVersion, meanvalue, group=Trt,
                                    col=ModName), 
                   position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9), size=4)+
        geom_errorbar(aes(x=ModVersion, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge(width=0.9), width=0.2)+
        xlab("")+
        theme_linedraw() +
        facet_wrap(~MIP)+
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
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("GPP (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_fill_manual(name="Trt",
                          values=c("amb"="white", "ele"="grey"))+
        scale_color_manual(name="Model",
                           values=c(col.values, 
                                    "CLM4"=Diverge_hsv_Palette[1],
                                    "CLMP"=Diverge_hsv_Palette[2],
                                    "CABL"=Diverge_hsv_Palette[3],
                                    "GDAY"=Diverge_hsv_Palette[4],
                                    "LPJW"=Diverge_hsv_Palette[5],
                                    "LPJX"=Diverge_hsv_Palette[6],
                                    "OCNX"=Diverge_hsv_Palette[7],
                                    "SDVM"=Diverge_hsv_Palette[8]))
    

    #plot(p1)    

    plotDF2 <- myDF[myDF$variable=="NPP",]
    subDF2 <- myDF2[myDF2$variable=="NPP",]
    
    p2 <- ggplot(data=plotDF2, 
                 aes(ModVersion, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position=position_dodge(), col="black") +
        geom_point(data=subDF2, aes(ModVersion, meanvalue, group=Trt,
                                    col=ModName), 
                   position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9), size=4)+
        geom_errorbar(aes(x=ModVersion, ymin=meanvalue-sdvalue,
                         ymax=meanvalue+sdvalue), 
                     col="black", 
                     position=position_dodge(width=0.9), width=0.2)+
        xlab("")+
        theme_linedraw() +
        facet_wrap(~MIP)+
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
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("NPP (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_fill_manual(name="Trt",
                          values=c("amb"="white", "ele"="grey"))+
        scale_color_manual(name="Model",
                           values=c(col.values, 
                                    "CLM4"=Diverge_hsv_Palette[1],
                                    "CLMP"=Diverge_hsv_Palette[2],
                                    "CABL"=Diverge_hsv_Palette[3],
                                    "GDAY"=Diverge_hsv_Palette[4],
                                    "LPJW"=Diverge_hsv_Palette[5],
                                    "LPJX"=Diverge_hsv_Palette[6],
                                    "OCNX"=Diverge_hsv_Palette[7],
                                    "SDVM"=Diverge_hsv_Palette[8]))
    
    
    plotDF3 <- myDF[myDF$variable=="NEP",]
    subDF3 <- myDF2[myDF2$variable=="NEP",]
    
    p3 <- ggplot(data=plotDF3, 
                 aes(ModVersion, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position=position_dodge(), col="black") +
        geom_point(data=subDF3, aes(ModVersion, meanvalue, group=Trt,
                                    col=ModName), 
                   position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9), size=4)+
        geom_errorbar(aes(x=ModVersion, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge(width=0.9), width=0.2)+
        xlab("")+
        theme_linedraw() +
        facet_wrap(~MIP)+
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
              legend.box.just = 'left')+
        ylab(expression(paste("NEP (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_fill_manual(name="",
                          values=c("amb"="white", "ele"="grey"))+
        scale_color_manual(name="Model",
                           values=c(col.values, 
                                    "CLM4"=Diverge_hsv_Palette[1],
                                    "CLMP"=Diverge_hsv_Palette[2],
                                    "CABL"=Diverge_hsv_Palette[3],
                                    "GDAY"=Diverge_hsv_Palette[4],
                                    "LPJW"=Diverge_hsv_Palette[5],
                                    "LPJX"=Diverge_hsv_Palette[6],
                                    "OCNX"=Diverge_hsv_Palette[7],
                                    "SDVM"=Diverge_hsv_Palette[8]))
    
    
    
    plotDF4 <- myDF[myDF$variable=="LAI",]
    subDF4 <- myDF2[myDF2$variable=="LAI",]
    
    p4 <- ggplot(data=plotDF4, 
                 aes(ModVersion, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position=position_dodge(), col="black") +
        geom_point(data=subDF4, aes(ModVersion, meanvalue, group=Trt,
                                    col=ModName), 
                   position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9), size=4)+
        geom_errorbar(aes(x=ModVersion, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge(width=0.9), width=0.2)+
        xlab("")+
        theme_linedraw() +
        facet_wrap(~MIP)+
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
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("LAI")))+
        scale_fill_manual(name="Trt",
                          values=c("amb"="white", "ele"="grey"))+
        scale_color_manual(name="Model",
                           values=c(col.values, 
                                    "CLM4"=Diverge_hsv_Palette[1],
                                    "CLMP"=Diverge_hsv_Palette[2],
                                    "CABL"=Diverge_hsv_Palette[3],
                                    "GDAY"=Diverge_hsv_Palette[4],
                                    "LPJW"=Diverge_hsv_Palette[5],
                                    "LPJX"=Diverge_hsv_Palette[6],
                                    "OCNX"=Diverge_hsv_Palette[7],
                                    "SDVM"=Diverge_hsv_Palette[8]))
    
    
    plotDF5 <- myDF[myDF$variable=="deltaC",]
    subDF5 <- myDF2[myDF2$variable=="deltaC",]
    
    p5 <- ggplot(data=plotDF5, 
                 aes(ModVersion, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position=position_dodge(), col="black") +
        geom_point(data=subDF5, aes(ModVersion, meanvalue, group=Trt,
                                    col=ModName), 
                   position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9), size=4)+
        geom_errorbar(aes(x=ModVersion, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge(width=0.9), width=0.2)+
        xlab("")+
        theme_linedraw() +
        facet_wrap(~MIP)+
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
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("BP (g C " * m^2 * " " * yr^-1 * ")")))+
        scale_fill_manual(name="Trt",
                          values=c("amb"="white", "ele"="grey"))+
        scale_color_manual(name="Model",
                           values=c(col.values, 
                                    "CLM4"=Diverge_hsv_Palette[1],
                                    "CLMP"=Diverge_hsv_Palette[2],
                                    "CABL"=Diverge_hsv_Palette[3],
                                    "GDAY"=Diverge_hsv_Palette[4],
                                    "LPJW"=Diverge_hsv_Palette[5],
                                    "LPJX"=Diverge_hsv_Palette[6],
                                    "OCNX"=Diverge_hsv_Palette[7],
                                    "SDVM"=Diverge_hsv_Palette[8]))
    
    
    legend_top_row <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    plots_top_row <- plot_grid(p1, p5, p3, 
                               labels=NA,
                               ncol=1, align="vh", axis = "l",
                               label_x=0.1, label_y=0.95,
                               label_size = 18)
    
    ### Plotting
    pdf(paste0(out.dir, "/Comparison_to_Medlyn_2016.pdf"), width=6, height=10)
    
    #grid.arrange(p1, p5, p3, nrow=3, heights=c(1,1,1.2))
    plot_grid(plots_top_row,
              legend_top_row,
              ncol=1, rel_heights=c(1,0.1))
    
    dev.off()
    
    
    
}