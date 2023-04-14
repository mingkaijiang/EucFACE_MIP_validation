theoretical_analysis_of_leaf_nutrient_effect_on_leaf_physiology <- function() {
    
    ### this is a theoretical analysis on how leaf nutrient concentrations
    ### affect Vcmax and Jmax parameters. 
    ### Currently we know:
    ### Walker et al. 2014: GDAYN, GDAYP, CABLP (also coordination hypothesis)
    ### Ellsworth unpublished: OCHDY, OCHDX, LPJGP (with Haxeltine & Prentice, 1996)
    ### Haxeltine and Prentice 1996: LPJGN
    ### P only downregulate biomass growth: ELMXX, QUINC, QUJSM
    
    
    ### prepare leaf N and P matrix
    npDF <- data.frame("leafN"=rep(seq(0.5, 5.0, by=0.005), each=291),
                       "leafP"=rep(seq(0.01, 0.3, by=0.001), 901))
    
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
    
    ### plot
    
    p1 <- ggplot(npDF, aes(leafN, leafP)) +
        #geom_contour_filled()+
        geom_tile(aes(fill=as.character(Walker_Vcmax_discrete)))+
        #scale_fill_brewer(name=expression(V[cmax]), palette=5, type="div")+
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
        geom_abline(slope=0.05, intercept=0.0)+
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
        ylab(expression("Leaf P (g N " * m^-2 * ")")); p1
    
    
    
    
    p2 <- ggplot(npDF, aes(leafN, leafP)) +
        #geom_contour_filled()+
        geom_tile(aes(fill=as.character(Ellsworth_Vcmax_discrete)))+
        #scale_fill_brewer(name=expression(V[cmax]), palette=5, type="div")+
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
        geom_abline(slope=0.05, intercept=0.0)+
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
        ylab(expression("Leaf P (g N " * m^-2 * ")")); p2
    
    
    p3 <- ggplot(npDF, aes(leafN, leafP)) +
        #geom_contour_filled()+
        geom_tile(aes(fill=as.character(Walker_Jmax_discrete)))+
        #scale_fill_brewer(name=expression(V[cmax]), palette=5, type="div")+
        scale_fill_manual(name=expression(J[max]), values=rdbu8,
                          labels=c("(60,80]",
                                   "(80,100]",
                                   "(100,120]",
                                   "(120,140]",
                                   "(140,160]",
                                   "(160,180]",
                                   "(180,200]",
                                   "(200,220]"))+
        geom_abline(slope=0.05, intercept=0.0)+
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
        ylab(expression("Leaf P (g N " * m^-2 * ")")); p3
    
    
    
    p4 <- ggplot(npDF, aes(leafN, leafP)) +
        #geom_contour_filled()+
        geom_tile(aes(fill=as.character(Ellsworth_Jmax_discrete)))+
        #scale_fill_brewer(name=expression(V[cmax]), palette=5, type="div")+
        scale_fill_manual(name=expression(J[max]), values=rdbu8,
                          labels=c("(60,80]",
                                   "(80,100]",
                                   "(100,120]",
                                   "(120,140]",
                                   "(140,160]",
                                   "(160,180]",
                                   "(180,200]",
                                   "(200,220]"))+
        geom_abline(slope=0.05, intercept=0.0)+
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
        ylab(expression("Leaf P (g N " * m^-2 * ")")); p4
    
    
    
    
    

    
    ### Plotting
    pdf(paste0("output/MIP_output/theoretical_photosynthesis.pdf"), width=12, height=8)
    

    plot_grid(p2, p1, p4, p3,
              labels=NA,rel_widths=c(1,1.3),
              ncol=2, align="vh", axis = "l",
              label_x=0.1, label_y=0.95,
              label_size = 18)
    dev.off()
    
    
    
}

