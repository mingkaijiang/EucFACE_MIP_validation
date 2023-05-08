theoretical_analysis_of_leaf_nutrient_effect_on_leaf_physiology <- function(scenario) {
    
    ### this is a theoretical analysis on how leaf nutrient concentrations
    ### affect Vcmax and Jmax parameters. 
    ### Currently we know:
    ### Walker et al. 2014: GDAYN, GDAYP, CABLP (also coordination hypothesis)
    ### Ellsworth unpublished: OCHDY, OCHDX, LPJGP (with Haxeltine & Prentice, 1996)
    ### Haxeltine and Prentice 1996: LPJGN
    ### P only downregulate biomass growth: ELMXX, QUINC, QUJSM
    
    
    ### prepare leaf N and P matrix
    npDF <- data.frame("leafN"=rep(seq(1.0, 6.0, by=0.01), each=291),
                       "leafP"=rep(seq(0.01, 0.3, by=0.001), 501))
    
    #npDF <- data.frame("leafN"=rep(seq(0.5, 10.0, by=0.005), each=491),
    #                   "leafP"=rep(seq(0.01, 0.5, by=0.001), 1901))
    
    
    ### 1. Walker relationship (area basis)
    ###    ln(Vcmax) = 3.946 + 0.921ln(N) + 0.121ln(P) +0.282ln(N)ln(P)
    ###    ln(Jmax) = 1.246 + 0.886ln(Vcmax) + 0.089ln(P)
    ### if ignoring p > 0.5,
    ###    ln(Vcmax) = 3.946 + 0.921ln(N)
    ###    ln(Jmax) = 1.246 + 0.886ln(Vcmax) + 0.089ln(P)
    
    ### 2. Ellsworth relationship (area basis)
    ###    ln(Vcmax) = 4.308 + 0.298ln(P) + 0.197 ln(N)
    ###    ln(Jmax) = 5.139 + 0.325ln(P) + 0.112ln(N)
    
    
    ### add walker Jmax, Vcmax data
    npDF$Walker_Vcmax <- exp(3.946 + 0.921 * log(npDF$leafN) + 0.121 * log(npDF$leafP) + 0.282 * log(npDF$leafN) * log(npDF$leafP))
    npDF$Walker_Jmax <- exp(1.246 + 0.886 * log(npDF$Walker_Vcmax) + 0.089 * log(npDF$leafP))
    
    
    ### add Ellsworth Jmax, Vcmax
    npDF$Ellsworth_Vcmax <- exp(4.308 + 0.298 * log(npDF$leafP) + 0.197 * log(npDF$leafN)) 
    npDF$Ellsworth_Jmax <- exp(5.139 + 0.325 * log(npDF$leafP) + 0.112 * log(npDF$leafN))
    
    
    ### calculate Jmax/Vcmax
    npDF$Walker_JVratio <- with(npDF, Walker_Jmax/Walker_Vcmax)
    npDF$Ellsworth_JVratio <- with(npDF, Ellsworth_Jmax/Ellsworth_Vcmax)
    
    ### 
    vcmax.range <- c(plyr::round_any(min(c(npDF$Walker_Vcmax, npDF$Ellsworth_Vcmax)), 10, f=floor), 
                     plyr::round_any(max(c(npDF$Walker_Vcmax, npDF$Ellsworth_Vcmax)), 10, f=ceiling))
    jmax.range <- c(plyr::round_any(min(c(npDF$Walker_Jmax, npDF$Ellsworth_Jmax)), 10, f=floor),
                    plyr::round_any(max(c(npDF$Walker_Jmax, npDF$Ellsworth_Jmax)), 10, f=ceiling))
    
    
    
    ### prepare the plot labels
    npDF$Walker_Vcmax_discrete <- ifelse(npDF$Walker_Vcmax>20 & npDF$Walker_Vcmax<=30, 1, 
                                         ifelse(npDF$Walker_Vcmax>30 & npDF$Walker_Vcmax<=40,2,
                                                ifelse(npDF$Walker_Vcmax>40 & npDF$Walker_Vcmax<=50,3,
                                                       ifelse(npDF$Walker_Vcmax>50 & npDF$Walker_Vcmax<=60,4,
                                                              ifelse(npDF$Walker_Vcmax>60 & npDF$Walker_Vcmax<=70,5,
                                                                     ifelse(npDF$Walker_Vcmax>70 & npDF$Walker_Vcmax<=80,6,
                                                                            ifelse(npDF$Walker_Vcmax>80 & npDF$Walker_Vcmax<=90,7,
                                                                                   ifelse(npDF$Walker_Vcmax>90 & npDF$Walker_Vcmax<=100,8,
                                                                                          ifelse(npDF$Walker_Vcmax>100 & npDF$Walker_Vcmax<=110,9,NA)))))))))
    
    
    npDF$Ellsworth_Vcmax_discrete <- ifelse(npDF$Ellsworth_Vcmax>20 & npDF$Ellsworth_Vcmax<=30, 1, 
                                         ifelse(npDF$Ellsworth_Vcmax>30 & npDF$Ellsworth_Vcmax<=40,2,
                                                ifelse(npDF$Ellsworth_Vcmax>40 & npDF$Ellsworth_Vcmax<=50,3,
                                                       ifelse(npDF$Ellsworth_Vcmax>50 & npDF$Ellsworth_Vcmax<=60,4,
                                                              ifelse(npDF$Ellsworth_Vcmax>60 & npDF$Ellsworth_Vcmax<=70,5,
                                                                     ifelse(npDF$Ellsworth_Vcmax>70 & npDF$Ellsworth_Vcmax<=80,6,
                                                                            ifelse(npDF$Ellsworth_Vcmax>80 & npDF$Ellsworth_Vcmax<=90,7,
                                                                                   ifelse(npDF$Ellsworth_Vcmax>90 & npDF$Ellsworth_Vcmax<=100,8,
                                                                                          ifelse(npDF$Ellsworth_Vcmax>100 & npDF$Ellsworth_Vcmax<=110,9,NA)))))))))
    
    
    
    
    npDF$Walker_Jmax_discrete <- ifelse(npDF$Walker_Jmax>60 & npDF$Walker_Jmax<=80, 1, 
                                         ifelse(npDF$Walker_Jmax>80 & npDF$Walker_Jmax<=100,2,
                                                ifelse(npDF$Walker_Jmax>100 & npDF$Walker_Jmax<=120,3,
                                                       ifelse(npDF$Walker_Jmax>120 & npDF$Walker_Jmax<=140,4,
                                                              ifelse(npDF$Walker_Jmax>140 & npDF$Walker_Jmax<=160,5,
                                                                     ifelse(npDF$Walker_Jmax>160 & npDF$Walker_Jmax<=180,6,
                                                                            ifelse(npDF$Walker_Jmax>180 & npDF$Walker_Jmax<=200,7,
                                                                                   ifelse(npDF$Walker_Jmax>200 & npDF$Walker_Jmax<=220,8,NA))))))))
    
    
    npDF$Ellsworth_Jmax_discrete <- ifelse(npDF$Ellsworth_Jmax>60 & npDF$Ellsworth_Jmax<=80, 1, 
                                        ifelse(npDF$Ellsworth_Jmax>80 & npDF$Ellsworth_Jmax<=100,2,
                                               ifelse(npDF$Ellsworth_Jmax>100 & npDF$Ellsworth_Jmax<=120,3,
                                                      ifelse(npDF$Ellsworth_Jmax>120 & npDF$Ellsworth_Jmax<=140,4,
                                                             ifelse(npDF$Ellsworth_Jmax>140 & npDF$Ellsworth_Jmax<=160,5,
                                                                    ifelse(npDF$Ellsworth_Jmax>160 & npDF$Ellsworth_Jmax<=180,6,
                                                                           ifelse(npDF$Ellsworth_Jmax>180 & npDF$Ellsworth_Jmax<=200,7,
                                                                                  ifelse(npDF$Ellsworth_Jmax>200 & npDF$Ellsworth_Jmax<=220,8,NA))))))))
   
    
    npDF <- na.omit(npDF)
    
    
    ### plotting color range
    rdbu9 <- brewer.pal(n = 9, name = "Reds")
    rdbu8 <- brewer.pal(n = 8, name = "Reds")
    
    
    ############################################################################
    ### get leaf N and P content from each model under ambient and elevated CO2
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_ELE_annual.rds"))
    
    d <- dim(ambDF)[2]
    
    ### remove N models
    ambDF <- ambDF[ambDF$ModName!="I_GDAYN",]
    ambDF <- ambDF[ambDF$ModName!="J_LPJGN",]
    eleDF <- eleDF[eleDF$ModName!="I_GDAYN",]
    eleDF <- eleDF[eleDF$ModName!="J_LPJGN",]
    
    #### calculate 4-yr means in the simulation datasets
    ambDF <- subset(ambDF, YEAR>2012 & YEAR<2017)
    eleDF <- subset(eleDF, YEAR>2012 & YEAR<2017)
    
    
    ### normalize by leaf area
    ambDF$NL_norm <- ambDF$NL/ambDF$LAI
    ambDF$PL_norm <- ambDF$PL/ambDF$LAI
    
    eleDF$NL_norm <- eleDF$NL/eleDF$LAI
    eleDF$PL_norm <- eleDF$PL/eleDF$LAI
    
    ### calculate average
    ambDF.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                           data=ambDF,
                           keep.names=T, na.rm=T)
    
    eleDF.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                           data=eleDF,
                           keep.names=T, na.rm=T)
    
    ### get the list of models
    mod.list <- unique(ambDF.sum$ModName)
    nmod <- length(mod.list)
    
    ## merge amb and ele dataframes
    ambDF.sum <- ambDF.sum[,c("ModName", "GPP.mean", "NL_norm.mean", "PL_norm.mean", 
                              "NL.mean", "PL.mean", "LAI.mean")]
    eleDF.sum <- eleDF.sum[,c("ModName", "GPP.mean", "NL_norm.mean", "PL_norm.mean", 
                              "NL.mean", "PL.mean", "LAI.mean")]
    
    
    
    ambDF.sum$Trt <- "amb"
    eleDF.sum$Trt <- "ele"
    
    modDF <- rbind(ambDF.sum, eleDF.sum)
    arrowDF <- data.frame("ModName"=modDF$ModName[modDF$Trt=="amb"],
                          "GPP_amb"=modDF$GPP.mean[modDF$Trt=="amb"],
                          "GPP_ele"=modDF$GPP.mean[modDF$Trt=="ele"],
                          "NL_norm_amb"=modDF$NL_norm.mean[modDF$Trt=="amb"],
                          "NL_norm_ele"=modDF$NL_norm.mean[modDF$Trt=="ele"],
                          "PL_norm_amb"=modDF$PL_norm.mean[modDF$Trt=="amb"],
                          "PL_norm_ele"=modDF$PL_norm.mean[modDF$Trt=="ele"],
                          "NL_amb"=modDF$NL.mean[modDF$Trt=="amb"],
                          "NL_ele"=modDF$NL.mean[modDF$Trt=="ele"],
                          "PL_amb"=modDF$PL.mean[modDF$Trt=="amb"],
                          "PL_ele"=modDF$PL.mean[modDF$Trt=="ele"],
                          "LAI_amb"=modDF$LAI.mean[modDF$Trt=="amb"],
                          "LAI_ele"=modDF$LAI.mean[modDF$Trt=="ele"])
    
    
    
    ############################################################################
    ### plot
    
    p1 <- ggplot(npDF, aes(leafN, leafP)) +
        geom_tile(aes(fill=as.character(Walker_Vcmax_discrete)))+
        scale_fill_manual(name=expression(V[cmax]), values=rdbu9,
                          labels=c("(20,30]",
                                   "(30,40]",
                                   "(40,50]",
                                   "(50,60]",
                                   "(60,70]",
                                   "(70,80]",
                                   "(80,90]",
                                   "(90,100]",
                                   "(100,110]"))+
        geom_segment(arrowDF, mapping=aes(x=NL_norm_amb, 
                                  xend = NL_norm_ele,
                                  y=PL_norm_amb, 
                                  yend=PL_norm_ele),#arrow=arrow(length=unit(0.03, "npc")),
                     lwd=0.5, col="black")+
        geom_point(data=modDF, aes(NL_norm.mean, PL_norm.mean, col=ModName, pch=Trt), size=4)+
        scale_shape_manual(name=expression(CO[2] * " treatment"),
                           values=c("amb"=19, "ele"=17),
                           labels=c("amb", "ele"))+
        scale_color_manual(name="Model",
                          values=c(col.values),
                          labels=c(model.labels))+
        geom_abline(slope=0.05, intercept=0.0, lty=2)+
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
        xlab(expression("Leaf N (g N " * m^-2 * ")"))+
        ylab(expression("Leaf P (g P " * m^-2 * ")"))+
        guides(color="none",
               fill=guide_legend(nrow=8, byrow=T),
               pch="none")
    
    
    
    
    p2 <- ggplot(npDF, aes(leafN, leafP)) +
        geom_tile(aes(fill=as.character(Ellsworth_Vcmax_discrete)))+
        scale_fill_manual(name=expression(V[cmax]), values=rdbu9,
                          labels=c("(20,30]",
                                   "(30,40]",
                                   "(40,50]",
                                   "(50,60]",
                                   "(60,70]",
                                   "(70,80]",
                                   "(80,90]",
                                   "(90,100]",
                                   "(100,110]"))+
        geom_segment(arrowDF, mapping=aes(x=NL_norm_amb, 
                                          xend = NL_norm_ele,
                                          y=PL_norm_amb, 
                                          yend=PL_norm_ele),
                     lwd=0.5, col="black")+
        geom_point(data=modDF, aes(NL_norm.mean, PL_norm.mean, col=ModName, pch=Trt), size=4)+
        scale_shape_manual(name=expression(CO[2] * " treatment"),
                           values=c("amb"=19, "ele"=17),
                           labels=c("amb", "ele"))+
        scale_color_manual(name="Model",
                           values=c(col.values),
                           labels=c(model.labels))+
        geom_abline(slope=0.05, intercept=0.0, lty=2)+
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
        xlab(expression("Leaf N (g N " * m^-2 * ")"))+
        ylab(expression("Leaf P (g P " * m^-2 * ")"))
    
    
    p3 <- ggplot(npDF, aes(leafN, leafP)) +
        geom_tile(aes(fill=as.character(Walker_Jmax_discrete)))+
        scale_fill_manual(name=expression(J[max]), values=rdbu8,
                          labels=c("(60,80]",
                                   "(80,100]",
                                   "(100,120]",
                                   "(120,140]",
                                   "(140,160]",
                                   "(160,180]",
                                   "(180,200]",
                                   "(200,220]"))+
        geom_segment(arrowDF, mapping=aes(x=NL_norm_amb, 
                                          xend = NL_norm_ele,
                                          y=PL_norm_amb, 
                                          yend=PL_norm_ele),
                     lwd=0.5, col="black")+
        geom_point(data=modDF, aes(NL_norm.mean, PL_norm.mean, col=ModName, pch=Trt), size=4)+
        scale_shape_manual(name=expression(CO[2] * " treatment"),
                           values=c("amb"=19, "ele"=17),
                           labels=c("amb", "ele"))+
        scale_color_manual(name="Model",
                           values=c(col.values),
                           labels=c(model.labels))+
        geom_abline(slope=0.05, intercept=0.0, lty=2)+
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
        xlab(expression("Leaf N (g N " * m^-2 * ")"))+
        ylab(expression("Leaf P (g P " * m^-2 * ")"))+
        guides(color="none",
               fill=guide_legend(nrow=8, byrow=T),
               pch="none")
    
    

    
    p4 <- ggplot(npDF, aes(leafN, leafP)) +
        geom_tile(aes(fill=as.character(Ellsworth_Jmax_discrete)))+
        scale_fill_manual(name=expression(J[max]), values=rdbu8,
                          labels=c("(60,80]",
                                   "(80,100]",
                                   "(100,120]",
                                   "(120,140]",
                                   "(140,160]",
                                   "(160,180]",
                                   "(180,200]",
                                   "(200,220]"))+
        geom_segment(arrowDF, mapping=aes(x=NL_norm_amb, 
                                          xend = NL_norm_ele,
                                          y=PL_norm_amb, 
                                          yend=PL_norm_ele),
                     lwd=0.5, col="black")+
        geom_point(data=modDF, aes(NL_norm.mean, PL_norm.mean, col=ModName, pch=Trt), size=4)+
        scale_shape_manual(name=expression(CO[2] * " treatment"),
                           values=c("amb"=19, "ele"=17),
                           labels=c("amb", "ele"))+
        scale_color_manual(name="Model",
                           values=c(col.values),
                           labels=c(model.labels))+
        geom_abline(slope=0.05, intercept=0.0, lty=2)+
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
        xlab(expression("Leaf N (g N " * m^-2 * ")"))+
        ylab(expression("Leaf P (g P " * m^-2 * ")"))
    
    
    
    common_legend <- get_legend(p1 + theme(legend.position="bottom")+
                                    guides(color=guide_legend(nrow=4, byrow=T),
                                           fill="none",
                                           pch=guide_legend(nrow=2, byrow=T)))
    
    plot_row <- plot_grid(p2, p1, p4, p3,
                          labels=NA,rel_widths=c(1,1.3),
                          ncol=2, align="vh", axis = "l",
                          label_x=0.1, label_y=0.95,
                          label_size = 18)
    
    
    ### Plotting
    pdf(paste0("output/MIP_output/theoretical_photosynthesis.pdf"), width=12, height=12)
    
    plot_grid(plot_row, common_legend,
              ncol=1, rel_heights=c(1,0.5))

    dev.off()
    
    
    
}

