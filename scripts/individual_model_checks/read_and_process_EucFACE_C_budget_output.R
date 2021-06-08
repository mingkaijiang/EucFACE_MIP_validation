read_and_process_EucFACE_C_budget_output <- function() {
    
    ### read all data
    deltaDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/delta_pool.csv")
    inoutDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/inout.csv")
    nppDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/npp.csv")
    poolDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/pool.csv")
    nepDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/nep_normalized_summary_with_NPPmyco.csv")
    
    ### prepare a list to store all variables
    c.var.list <- c("GPP", "LAI", "NPP", "CGL", "CGW", "CGFR", "CGCR", "CEX",
                    "RL", "RW", "RFR", "RGR", "RAU", "CVOC", "RHET", "RECO",
                    "CL", "CW", "CFR", "CCR", "CFLITA", "CMIC", "CSOIL", "CMYC",
                    "deltaCL", "deltaCW", "deltaCFR", "deltaCCR", "deltaCFLIT", 
                    "deltaCMIC", "deltaCSOIL", "deltaCMYC")
        
    ### prepare storage DF
    outDF <- data.frame(c("mean", "mean", "sd", "sd", "mean", "sd"),
                        c("aCO2", "eCO2", "aCO2", "eCO2", "pct_diff", "pct_diff"))
    
    colnames(outDF) <- c("Group", "Trt")
        
    ### assign values
    ## GPP
    outDF$GPP[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="GPP overstorey"]
    outDF$GPP[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="GPP overstorey"]
    outDF$GPP[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="GPP overstorey"]
    outDF$GPP[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="GPP overstorey"]
    outDF$GPP[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="GPP overstorey"]
    
    ## CL
    outDF$CL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Overstorey leaf"]
    outDF$CL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Overstorey leaf"]
    outDF$CL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Overstorey leaf"]
    outDF$CL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Overstorey leaf"]
    outDF$CL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Overstorey leaf"]
    
    ## CW
    outDF$CW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Overstorey wood"]
    outDF$CW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Overstorey wood"]
    outDF$CW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Overstorey wood"]
    outDF$CW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Overstorey wood"]
    outDF$CW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Overstorey wood"]
    
    
    ## CFR
    tmp <- colSums(poolDF[poolDF$term%in%c("Fine Root", "Intermediate Root"),2:7])
    amean <- mean(c(tmp[2], tmp[3], tmp[6]))
    emean <- mean(c(tmp[1], tmp[4], tmp[5]))
    asd <- sd(c(tmp[2], tmp[3], tmp[6]))
    esd <- sd(c(tmp[1], tmp[4], tmp[5]))
    diff <- (emean/amean - 1) * 100
    
    outDF$CFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$CFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$CFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$CFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$CFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    
    ## CCR
    outDF$CCR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Coarse Root"]
    outDF$CCR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Coarse Root"]
    outDF$CCR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Coarse Root"]
    outDF$CCR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Coarse Root"]
    outDF$CCR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Coarse Root"]
        
    ## CFLITA
    outDF$CFLITA[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Litter"]
    outDF$CFLITA[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Litter"]
    outDF$CFLITA[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Litter"]
    outDF$CFLITA[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Litter"]
    outDF$CFLITA[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Litter"]
    
    
    ## CMIC
    outDF$CMIC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Microbial biomass"]
    outDF$CMIC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Microbial biomass"]
    outDF$CMIC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Microbial biomass"]
    outDF$CMIC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Microbial biomass"]
    outDF$CMIC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Microbial biomass"]
    
    ## CSOIL
    outDF$CSOIL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Soil C"]
    outDF$CSOIL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Soil C"]
    outDF$CSOIL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Soil C"]
    outDF$CSOIL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Soil C"]
    outDF$CSOIL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Soil C"]
    
    ## CMYC
    outDF$CMYC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Mycorrhizae"]
    outDF$CMYC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Mycorrhizae"]
    outDF$CMYC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Mycorrhizae"]
    outDF$CMYC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Mycorrhizae"]
    outDF$CMYC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Mycorrhizae"]
    
    
    
    
    ## deltaCL
    outDF$deltaCL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Overstorey leaf"]
    outDF$deltaCL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Overstorey leaf"]
    outDF$deltaCL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Overstorey leaf"]
    outDF$deltaCL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Overstorey leaf"]
    outDF$deltaCL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Overstorey leaf"]
    
    ## deltaCW
    outDF$deltaCW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Overstorey wood"]
    outDF$deltaCW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Overstorey wood"]
    outDF$deltaCW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Overstorey wood"]
    outDF$deltaCW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Overstorey wood"]
    outDF$deltaCW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Overstorey wood"]
    
    
    ## deltaCFR
    tmp <- colSums(deltaDF[deltaDF$term%in%c("Fine Root", "Intermediate Root"),2:7])
    amean <- mean(c(tmp[2], tmp[3], tmp[6]))
    emean <- mean(c(tmp[1], tmp[4], tmp[5]))
    asd <- sd(c(tmp[2], tmp[3], tmp[6]))
    esd <- sd(c(tmp[1], tmp[4], tmp[5]))
    diff <- (emean/amean - 1) * 100
    
    outDF$deltaCFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$deltaCFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$deltaCFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$deltaCFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$deltaCFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    
    ## CCR
    outDF$deltaCCR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Coarse Root"]
    outDF$deltaCCR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Coarse Root"]
    outDF$deltaCCR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Coarse Root"]
    outDF$deltaCCR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Coarse Root"]
    outDF$deltaCCR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Coarse Root"]
    
    ## CFLITA
    outDF$deltaCFLITA[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Litter"]
    outDF$deltaCFLITA[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Litter"]
    outDF$deltaCFLITA[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Litter"]
    outDF$deltaCFLITA[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Litter"]
    outDF$deltaCFLITA[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Litter"]
    
    
    ## CMIC
    outDF$deltaCMIC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Microbial biomass"]
    outDF$deltaCMIC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Microbial biomass"]
    outDF$deltaCMIC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Microbial biomass"]
    outDF$deltaCMIC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Microbial biomass"]
    outDF$deltaCMIC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Microbial biomass"]
    
    ## CSOIL
    outDF$deltaCSOIL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Soil C"]
    outDF$deltaCSOIL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Soil C"]
    outDF$deltaCSOIL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Soil C"]
    outDF$deltaCSOIL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Soil C"]
    outDF$deltaCSOIL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Soil C"]
    
    ## CMYC
    outDF$deltaCMYC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Mycorrhizae"]
    outDF$deltaCMYC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Mycorrhizae"]
    outDF$deltaCMYC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Mycorrhizae"]
    outDF$deltaCMYC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Mycorrhizae"]
    outDF$deltaCMYC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Mycorrhizae"]
    
        
    ### CGL
    tmp <- colSums(nppDF[nppDF$term%in%c("Leaf NPP", "Leaf consumption"),2:7])
    amean <- mean(c(tmp[2], tmp[3], tmp[6]))
    emean <- mean(c(tmp[1], tmp[4], tmp[5]))
    asd <- sd(c(tmp[2], tmp[3], tmp[6]))
    esd <- sd(c(tmp[1], tmp[4], tmp[5]))
    diff <- (emean/amean - 1) * 100
    
    outDF$CGL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$CGL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$CGL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$CGL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$CGL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    
    ### CGW
    tmp <- colSums(nppDF[nppDF$term%in%c("Stem NPP", "Other NPP"),2:7])
    amean <- mean(c(tmp[2], tmp[3], tmp[6]))
    emean <- mean(c(tmp[1], tmp[4], tmp[5]))
    asd <- sd(c(tmp[2], tmp[3], tmp[6]))
    esd <- sd(c(tmp[1], tmp[4], tmp[5]))
    diff <- (emean/amean - 1) * 100
    
    outDF$CGW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$CGW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$CGW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$CGW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$CGW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    
    ### CGFR
    tmp <- colSums(nppDF[nppDF$term%in%c("Fine Root NPP", "Intermediate Root NPP"),2:7])
    amean <- mean(c(tmp[2], tmp[3], tmp[6]))
    emean <- mean(c(tmp[1], tmp[4], tmp[5]))
    asd <- sd(c(tmp[2], tmp[3], tmp[6]))
    esd <- sd(c(tmp[1], tmp[4], tmp[5]))
    diff <- (emean/amean - 1) * 100
    
    outDF$CGFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$CGFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$CGFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$CGFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$CGFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    
    ### CGCR
    outDF$CGCR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- nppDF$aCO2[nppDF$term=="Coarse Root NPP"]
    outDF$CGCR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- nppDF$eCO2[nppDF$term=="Coarse Root NPP"]
    outDF$CGCR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- nppDF$aCO2_sd[nppDF$term=="Coarse Root NPP"]
    outDF$CGCR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- nppDF$eCO2_sd[nppDF$term=="Coarse Root NPP"]
    outDF$CGCR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- nppDF$percent_diff[nppDF$term=="Coarse Root NPP"]
    
    
    ### NPP - does not include exudation yet
    tmp <- colSums(nppDF[nppDF$term%in%c("Leaf NPP", "Stem NPP", 
                                         "Fine Root NPP", "Intermediate Root NPP",
                                         "Coarse Root NPP", "Other NPP",
                                         "Leaf consumption"),2:7])
    amean <- mean(c(tmp[2], tmp[3], tmp[6]))
    emean <- mean(c(tmp[1], tmp[4], tmp[5]))
    asd <- sd(c(tmp[2], tmp[3], tmp[6]))
    esd <- sd(c(tmp[1], tmp[4], tmp[5]))
    diff <- (emean/amean - 1) * 100
    
    outDF$NPP[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$NPP[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$NPP[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$NPP[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$NPP[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    
    
    ### RHET
    outDF$RHET[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- nppDF$aCO2[nppDF$term=="R hetero"]
    outDF$RHET[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- nppDF$eCO2[nppDF$term=="R hetero"]
    outDF$RHET[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- nppDF$aCO2_sd[nppDF$term=="R hetero"]
    outDF$RHET[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- nppDF$eCO2_sd[nppDF$term=="R hetero"]
    outDF$RHET[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- nppDF$percent_diff[nppDF$term=="R hetero"]
    
    
    ### RL
    outDF$RL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="Ra leaf"]
    outDF$RL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="Ra leaf"]
    outDF$RL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="Ra leaf"]
    outDF$RL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="Ra leaf"]
    outDF$RL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="Ra leaf"]
    
    ### RW
    outDF$RW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="Ra stem"]
    outDF$RW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="Ra stem"]
    outDF$RW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="Ra stem"]
    outDF$RW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="Ra stem"]
    outDF$RW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="Ra stem"]
    
    ### RFR
    outDF$RFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="Ra root"]
    outDF$RFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="Ra root"]
    outDF$RFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="Ra root"]
    outDF$RFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="Ra root"]
    outDF$RFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="Ra root"]
    
    ### CVOC
    outDF$CVOC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="VOC"]
    outDF$CVOC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="VOC"]
    outDF$CVOC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="VOC"]
    outDF$CVOC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="VOC"]
    outDF$CVOC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="VOC"]
    
    ### RGR
    outDF$RGR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="Rgrowth"]
    outDF$RGR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="Rgrowth"]
    outDF$RGR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="Rgrowth"]
    outDF$RGR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="Rgrowth"]
    outDF$RGR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="Rgrowth"]
    
    
    ### RAU
    tmp <- colSums(inoutDF[inoutDF$term%in%c("Ra leaf", "Ra stem", 
                                         "Ra root", "Rgrowth"),2:7])
    amean <- mean(c(tmp[2], tmp[3], tmp[6]))
    emean <- mean(c(tmp[1], tmp[4], tmp[5]))
    asd <- sd(c(tmp[2], tmp[3], tmp[6]))
    esd <- sd(c(tmp[1], tmp[4], tmp[5]))
    diff <- (emean/amean - 1) * 100
    
    outDF$RAU[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$RAU[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$RAU[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$RAU[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$RAU[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    
    
    ### RECO
    tmp <- colSums(inoutDF[inoutDF$term%in%c("Ra leaf", "Ra stem", 
                                             "Rsoil", "Rgrowth"),2:7])
    amean <- mean(c(tmp[2], tmp[3], tmp[6]))
    emean <- mean(c(tmp[1], tmp[4], tmp[5]))
    asd <- sd(c(tmp[2], tmp[3], tmp[6]))
    esd <- sd(c(tmp[1], tmp[4], tmp[5]))
    diff <- (emean/amean - 1) * 100
    
    outDF$RECO[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$RECO[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$RECO[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$RECO[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$RECO[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    
    
    ### CEX
    outDF$CEX[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- NA
    outDF$CEX[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- NA
    outDF$CEX[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- NA
    outDF$CEX[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- NA
    outDF$CEX[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- NA
    
    ### LAI
    outDF$LAI[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- NA
    outDF$LAI[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- NA
    outDF$LAI[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- NA
    outDF$LAI[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- NA
    outDF$LAI[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- NA
    
    return(outDF)
    
}