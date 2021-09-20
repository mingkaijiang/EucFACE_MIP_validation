plot_normalized_plant_allocation_response <- function(scenario="fix") {
    
    ###################################################################
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
    
    
    ### we need to check mass balance first
    ### then we can work out C allocation and fate of C
    
    
    
    
    
}
