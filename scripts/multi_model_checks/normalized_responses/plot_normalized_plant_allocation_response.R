plot_normalized_plant_allocation_response <- function(scenario="fix") {
    
    ###################################################################
    #### Set up basics
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario, "/")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_AMB_daily.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_ELE_daily.rds"))
    
    ### ignore NAs
    ambDF[ambDF<=-999] <- NA
    eleDF[eleDF<=-999] <- NA
    
    ### add delta pools at daily timesteps
    mod <- unique(ambDF$ModName)

    outDF1 <- outDF2 <- c()
    
    for (i in mod) {
        subDF1 <- ambDF[ambDF$ModName==i,]
        subDF2 <- eleDF[eleDF$ModName==i,]
        
        d <- dim(subDF1)[1]
        
        for (j in 2:d) {
            ## CL
            subDF1$deltaCL[j] <- subDF1$CL[j] - subDF1$CL[j-1]
            subDF2$deltaCL[j] <- subDF2$CL[j] - subDF2$CL[j-1]
            
            ## CW
            subDF1$deltaCW[j] <- subDF1$CW[j] - subDF1$CW[j-1]
            subDF2$deltaCW[j] <- subDF2$CW[j] - subDF2$CW[j-1]
            
            ## CFR
            subDF1$deltaCFR[j] <- subDF1$CFR[j] - subDF1$CFR[j-1]
            subDF2$deltaCFR[j] <- subDF2$CFR[j] - subDF2$CFR[j-1]
            
            ## CCR
            subDF1$deltaCCR[j] <- subDF1$CCR[j] - subDF1$CCR[j-1]
            subDF2$deltaCCR[j] <- subDF2$CCR[j] - subDF2$CCR[j-1]
            
            ## CSTOR
            subDF1$deltaCSTOR[j] <- subDF1$CSTOR[j] - subDF1$CSTOR[j-1]
            subDF2$deltaCSTOR[j] <- subDF2$CSTOR[j] - subDF2$CSTOR[j-1]
            
            ## CSOIL
            subDF1$deltaCSOIL[j] <- subDF1$CSOIL[j] - subDF1$CSOIL[j-1]
            subDF2$deltaCSOIL[j] <- subDF2$CSOIL[j] - subDF2$CSOIL[j-1]
            
            ## CFLIT
            subDF1$deltaCFLIT[j] <- subDF1$CFLIT[j] - subDF1$CFLIT[j-1]
            subDF2$deltaCFLIT[j] <- subDF2$CFLIT[j] - subDF2$CFLIT[j-1]
            
            ## CCLITB
            subDF1$deltaCCLITB[j] <- subDF1$CCLITB[j] - subDF1$CCLITB[j-1]
            subDF2$deltaCCLITB[j] <- subDF2$CCLITB[j] - subDF2$CCLITB[j-1]
        }
        
        
        outDF1 <- rbind(outDF1, subDF1)
        outDF2 <- rbind(outDF2, subDF2)
        
        
    }
    
    ### calculate annual totals
    ambDF1 <- summaryBy(NEP+GPP+NPP+RAU+RHET+
                            CGL+CGW+CGCR+CGFR+CREPR+CEX+
                            RL+RW+RCR+RFR+RGR+
                            deltaCL+deltaCW+deltaCFR+deltaCCR+
                            deltaCSTOR+deltaCSOIL+deltaCFLIT+
                            deltaCCLITB~ModName+YEAR,
                        FUN=sum, data=outDF1, keep.names=T,
                        na.rm=T)
    
    eleDF1 <- summaryBy(NEP+GPP+NPP+RAU+RHET+
                            CGL+CGW+CGCR+CGFR+CREPR+CEX+
                            RL+RW+RCR+RFR+RGR+
                            deltaCL+deltaCW+deltaCFR+deltaCCR+
                            deltaCSTOR+deltaCSOIL+deltaCFLIT+
                            deltaCCLITB~ModName+YEAR,
                        FUN=sum, data=outDF2, keep.names=T,
                        na.rm=T)
    
    
    ###################################################################
    ### we need to check mass balance first
    
    ## GPP = NPP+RAU
    p1 <- ggplot(ambDF1, aes(x=GPP, y=NPP+RAU, group=ModName))+
        geom_point(aes(fill=ModName), pch=21)+
        geom_abline(slope=1,intercept=0)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))
    
    
    ## NEP = NPP-RHET
    p2 <- ggplot(ambDF1, aes(x=NEP, y=NPP-RHET, group=ModName))+
        geom_point(aes(fill=ModName), pch=21)+
        geom_abline(slope=1,intercept=0)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))

    
    ## NEP = all delta pools
    p3 <- ggplot(ambDF1, aes(x=NEP, y=deltaCL+deltaCW+deltaCFR+deltaCCR+deltaCSTOR+deltaCSOIL+deltaCFLIT+deltaCCLITB,
                             group=ModName))+
        geom_point(aes(fill=ModName), pch=21)+
        geom_abline(slope=1,intercept=0)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
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
        ylab("delta pools")
    
    ## NPP = CGL + CGW + CGCR + CGFR + CREPR + CEX
    p4 <- ggplot(ambDF1, aes(x=NPP, y=CGL+CGW+CGCR+CGFR+CREPR+CEX,
                             group=ModName))+
        geom_point(aes(fill=ModName), pch=21)+
        geom_abline(slope=1,intercept=0)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
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
        ylab("CG fluxes + CEX + REPR")
    
    
    ### plot
    legend_top_row <- get_legend(p1 + theme(legend.position="bottom",
                                            legend.box = 'horizontal',
                                            legend.box.just = 'left'))
    
    plots_top_row <- plot_grid(p1, p2, p3, p4,
                               labels="AUTO",
                               ncol=2, align="vh", axis = "l",
                               label_x=0.1, label_y=0.95,
                               label_size = 18)
    
    
    pdf(paste0(out.dir, "/MIP_allocation_mass_balance_OBS_", scenario, "_check.pdf"), 
        width=8, height=8)
    plot_grid(plots_top_row,
              legend_top_row,
              ncol=1, rel_heights=c(1,0.1))
    
    dev.off()
    
    ### Note:
    ### 1. For LPJ-GUESS, there is no delta CSTOR involved in the original
    ###    mass balance check.
    ### 2. For CABLE-POP, 
    
    
    ###################################################################
    ### then we can work out C allocation and fate of C
    
    
    
    
    
    
}
