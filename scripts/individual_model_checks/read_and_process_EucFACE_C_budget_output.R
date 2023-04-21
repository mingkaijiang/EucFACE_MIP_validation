read_and_process_EucFACE_C_budget_output <- function() {
    
    ### read all data
    deltaDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/delta_pool.csv")
    inoutDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/inout.csv")
    nppDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/npp.csv")
    poolDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/pool.csv")
    nepDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/nep_normalized_summary_with_NPPmyco.csv")
    
    ### revise Ra root to exclude understorey contribution (assume 50%)
    newDF <- inoutDF[inoutDF$term=="Ra root",]
    newDF$term <- "Ra overstorey root"
    newDF[,2:9] <- newDF[,2:9]/2
    inoutDF <- rbind(inoutDF, newDF)
    
    ### add NPP allocation to mycorrhizae flux
    tmpDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/NEP_normalized_method_comparison.csv")
    
    nppDF[nppDF$term=="Mycorrhizal production", 2:9] <- tmpDF[tmpDF$Category=="Figure3inset"&tmpDF$Method=="NPP-Rh",3:10]-tmpDF[tmpDF$Category=="Figure3"&tmpDF$Method=="NPP-Rh",3:10]
    nppDF$diff[nppDF$term=="Mycorrhizal production"] <- nppDF$eCO2[nppDF$term=="Mycorrhizal production"] - nppDF$aCO2[nppDF$term=="Mycorrhizal production"] 
    nppDF$aCO2_sd[nppDF$term=="Mycorrhizal production"] <- sqrt((tmpDF$aCO2_sd[tmpDF$Category=="Figure3inset"&tmpDF$Method=="NPP-Rh"]^2+tmpDF$aCO2_sd[tmpDF$Category=="Figure3"&tmpDF$Method=="NPP-Rh"]^2)/2)
    nppDF$eCO2_sd[nppDF$term=="Mycorrhizal production"] <- sqrt((tmpDF$eCO2_sd[tmpDF$Category=="Figure3inset"&tmpDF$Method=="NPP-Rh"]^2+tmpDF$eCO2_sd[tmpDF$Category=="Figure3"&tmpDF$Method=="NPP-Rh"]^2)/2)
    
    nppDF$percent_diff[nppDF$term=="Mycorrhizal production"] <- (nppDF$diff[nppDF$term=="Mycorrhizal production"])/nppDF$aCO2[nppDF$term=="Mycorrhizal production"] *100.0
    
    
    ### revise NEP to remove understorey contributions
    tmpDF1 <- data.frame(inoutDF[inoutDF$term%in%c("GPP overstorey",
                                                   "GPP understorey",
                                                   "CH4 efflux",
                                                   "Ra leaf",
                                                   "Ra stem",
                                                   "Ra root",
                                                   "Ra understorey",
                                                   "Rsoil",
                                                   "Ra overstorey root",
                                                   "VOC",
                                                   "DOC",
                                                   "Rherbivore",
                                                   "Rgrowth"),1:7])
    tmpDF2 <- tmpDF1[tmpDF1$term=="GPP overstorey",2:7] -
        abs(tmpDF1[tmpDF1$term=="CH4 efflux",2:7]) - tmpDF1[tmpDF1$term=="Ra leaf",2:7] -
        tmpDF1[tmpDF1$term=="Ra stem",2:7] - 
        (tmpDF1[tmpDF1$term=="Rsoil",2:7] - (tmpDF1[tmpDF1$term=="Ra root",2:7] - tmpDF1[tmpDF1$term=="Ra overstorey root",2:7])) - 
        tmpDF1[tmpDF1$term=="Rgrowth",2:7] -
        tmpDF1[tmpDF1$term=="VOC",2:7] - tmpDF1[tmpDF1$term=="Rherbivore",2:7]
    
    v1 <- mean(c(tmpDF2$Ring_2, tmpDF2$Ring_3, tmpDF2$Ring_6))
    v2 <- mean(c(tmpDF2$Ring_1, tmpDF2$Ring_4, tmpDF2$Ring_5))
    v3 <- sd(c(tmpDF2$Ring_2, tmpDF2$Ring_3, tmpDF2$Ring_6))
    v4 <- sd(c(tmpDF2$Ring_1, tmpDF2$Ring_4, tmpDF2$Ring_5))
    v5 <- v2-v1
    v6 <- sqrt((v3^2+v4^2)/2)
    
    nepDF$NEP[nepDF$Method=="In-out"&nepDF$Trt=="aCO2"] <- v1
    nepDF$NEP[nepDF$Method=="In-out"&nepDF$Trt=="eCO2"] <- v2
    nepDF$NEP_conf[nepDF$Method=="In-out"&nepDF$Trt=="aCO2"] <- v3
    nepDF$NEP_conf[nepDF$Method=="In-out"&nepDF$Trt=="eCO2"] <- v4
    
    
    ## npp - rh
    tmpDF1 <- data.frame(nppDF[nppDF$term%in%c("Leaf NPP",
                                               "Stem NPP",
                                               "Fine Root NPP",
                                               "Intermediate Root NPP",
                                               "Coarse Root NPP",
                                               "Other NPP",
                                               "Leaf consumption",
                                               "Mycorrhizal production",
                                               "R hetero"),1:7])
    tmpDF2 <- tmpDF1[tmpDF1$term=="Leaf NPP",2:7] +
        tmpDF1[tmpDF1$term=="Stem NPP",2:7] + tmpDF1[tmpDF1$term=="Fine Root NPP",2:7] /2 +
        tmpDF1[tmpDF1$term=="Intermediate Root NPP",2:7] / 2 + tmpDF1[tmpDF1$term=="Coarse Root NPP",2:7] +
        tmpDF1[tmpDF1$term=="Other NPP",2:7] + tmpDF1[tmpDF1$term=="Leaf consumption",2:7] +
        tmpDF1[tmpDF1$term=="Mycorrhizal production",2:7] - tmpDF1[tmpDF1$term=="R hetero",2:7] 
    
    v1 <- mean(c(tmpDF2$Ring_2, tmpDF2$Ring_3, tmpDF2$Ring_6))
    v2 <- mean(c(tmpDF2$Ring_1, tmpDF2$Ring_4, tmpDF2$Ring_5))
    v3 <- sd(c(tmpDF2$Ring_2, tmpDF2$Ring_3, tmpDF2$Ring_6))
    v4 <- sd(c(tmpDF2$Ring_1, tmpDF2$Ring_4, tmpDF2$Ring_5))
    v5 <- v2-v1
    v6 <- sqrt((v3^2+v4^2)/2)
    
    nepDF$NEP[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"] <- v1
    nepDF$NEP[nepDF$Method=="NPP-Rh"&nepDF$Trt=="eCO2"] <- v2
    nepDF$NEP_conf[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"] <- v3
    nepDF$NEP_conf[nepDF$Method=="NPP-Rh"&nepDF$Trt=="eCO2"] <- v4
    
    
    ## delta Cveg
    tmpDF1 <- data.frame(deltaDF[deltaDF$term%in%c("Overstorey leaf",
                                               "Overstorey wood",
                                               "Fine Root",
                                               "Intermediate Root",
                                               "Coarse Root",
                                               "Litter",
                                               "Microbial biomass",
                                               "Mycorrhizae",
                                               "Soil C",
                                               "Insects"),1:7])
    tmpDF2 <- tmpDF1[tmpDF1$term=="Overstorey leaf",2:7] +
        tmpDF1[tmpDF1$term=="Overstorey wood",2:7] + tmpDF1[tmpDF1$term=="Fine Root",2:7] /2 +
        tmpDF1[tmpDF1$term=="Intermediate Root",2:7] / 2 + tmpDF1[tmpDF1$term=="Coarse Root",2:7] +
        tmpDF1[tmpDF1$term=="Litter",2:7] + tmpDF1[tmpDF1$term=="Microbial biomass",2:7] +
        tmpDF1[tmpDF1$term=="Mycorrhizae",2:7] + tmpDF1[tmpDF1$term=="Soil C",2:7] +
        tmpDF1[tmpDF1$term=="Insects",2:7] 
    
    v1 <- mean(c(tmpDF2$Ring_2, tmpDF2$Ring_3, tmpDF2$Ring_6))
    v2 <- mean(c(tmpDF2$Ring_1, tmpDF2$Ring_4, tmpDF2$Ring_5))
    v3 <- sd(c(tmpDF2$Ring_2, tmpDF2$Ring_3, tmpDF2$Ring_6))
    v4 <- sd(c(tmpDF2$Ring_1, tmpDF2$Ring_4, tmpDF2$Ring_5))
    v5 <- v2-v1
    v6 <- sqrt((v3^2+v4^2)/2)
    
    nepDF$NEP[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"] <- v1
    nepDF$NEP[nepDF$Method=="Pool"&nepDF$Trt=="eCO2"] <- v2
    nepDF$NEP_conf[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"] <- v3
    nepDF$NEP_conf[nepDF$Method=="Pool"&nepDF$Trt=="eCO2"] <- v4
    
    
    ### update
    
    nepDF$pos <- nepDF$NEP + nepDF$NEP_conf
    nepDF$neg <- nepDF$NEP - nepDF$NEP_conf
    
    

    
    ### prepare a list to store all variables
    c.var.list <- c("GPP", "LAI", "NPP", "CGL", "CGW", "CGFR", "CGCR", "CEX",
                    "RL", "RW", "RFR", "RGR", "RAU", "CVOC", "RHET", "RECO",
                    "NEP_inout", "NEP_npprh", "NEP_pool", "NEP",
                    "CL", "CW", "CFR", "CCR", "CFLITA", "CMIC", "CSOIL", "CMYC",
                    "deltaCL", "deltaCW", "deltaCFR", "deltaCCR", "deltaCFLIT", 
                    "deltaCMIC", "deltaCSOIL", "deltaCMYC", "BP")
        
    ### prepare storage DF
    outDF <- data.frame(c("mean", "mean", "sd", "sd", "mean", "sd", "mean", "sd"),
                        c("aCO2", "eCO2", "aCO2", "eCO2", "diff", "diff", "pct_diff", "pct_diff"))
    
    colnames(outDF) <- c("Group", "Trt")
        
    ### assign values
    ## GPP
    outDF$GPP[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="GPP overstorey"]
    outDF$GPP[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="GPP overstorey"]
    outDF$GPP[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="GPP overstorey"]
    outDF$GPP[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="GPP overstorey"]
    outDF$GPP[outDF$Group=="mean"&outDF$Trt=="diff"] <- inoutDF$diff[inoutDF$term=="GPP overstorey"]
    outDF$GPP[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="GPP overstorey"]^2+
                                                                    inoutDF$eCO2_sd[inoutDF$term=="GPP overstorey"]^2)/2)
    outDF$GPP[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="GPP overstorey"]
    outDF$GPP[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="GPP overstorey"]^2+inoutDF$aCO2_sd[inoutDF$term=="GPP overstorey"]^2+
                                                                   inoutDF$eCO2_sd[inoutDF$term=="GPP overstorey"]^2)/3)/inoutDF$aCO2[inoutDF$term=="GPP overstorey"]*100 
    
    ## CL
    outDF$CL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Overstorey leaf"]
    outDF$CL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Overstorey leaf"]
    outDF$CL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Overstorey leaf"]
    outDF$CL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Overstorey leaf"]
    outDF$CL[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$percent_diff[poolDF$term=="Overstorey leaf"]
    outDF$CL[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Overstorey leaf"]^2+
                                                                   poolDF$eCO2_sd[poolDF$term=="Overstorey leaf"]^2)/2)
    
    outDF$CL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Overstorey leaf"]
    outDF$CL[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Overstorey leaf"]^2+poolDF$aCO2_sd[poolDF$term=="Overstorey leaf"]^2+
                                                                  poolDF$eCO2_sd[poolDF$term=="Overstorey leaf"]^2)/3)/poolDF$aCO2[poolDF$term=="Overstorey leaf"]*100 
    
    ## CW
    outDF$CW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Overstorey wood"]
    outDF$CW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Overstorey wood"]
    outDF$CW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Overstorey wood"]
    outDF$CW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Overstorey wood"]
    outDF$CW[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$percent_diff[poolDF$term=="Overstorey wood"]
    outDF$CW[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Overstorey wood"]^2+
                                                                   poolDF$eCO2_sd[poolDF$term=="Overstorey wood"]^2)/2)
    
    outDF$CW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Overstorey wood"]
    outDF$CW[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Overstorey wood"]^2+poolDF$aCO2_sd[poolDF$term=="Overstorey wood"]^2+
                                                                  poolDF$eCO2_sd[poolDF$term=="Overstorey wood"]^2)/3)/poolDF$aCO2[poolDF$term=="Overstorey wood"]*100 
    
    
    ## CFR - partition into understorey and overstorey
    tmp <- colSums(poolDF[poolDF$term%in%c("Fine Root", "Intermediate Root"),2:7], na.rm=T)/2
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    
    outDF$CFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$CFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$CFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$CFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$CFR[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$CFR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+asd^2+esd^2)/2) 
    outDF$CFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$CFR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+esd^2)/2)/amean*100 
    
    ## CCR
    outDF$CCR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Coarse Root"]
    outDF$CCR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Coarse Root"]
    outDF$CCR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Coarse Root"]
    outDF$CCR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Coarse Root"]
    outDF$CCR[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$term=="Coarse Root"]
    outDF$CCR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Coarse Root"]^2+
                                                                    poolDF$eCO2_sd[poolDF$term=="Coarse Root"]^2)/2)
    
    outDF$CCR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Coarse Root"]
    outDF$CCR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Coarse Root"]^2+poolDF$aCO2_sd[poolDF$term=="Coarse Root"]^2+
                                                                  poolDF$eCO2_sd[poolDF$term=="Coarse Root"]^2)/3)/poolDF$aCO2[poolDF$term=="Coarse Root"]*100 
    
    
    ## CFLITA
    outDF$CFLITA[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Litter"]
    outDF$CFLITA[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Litter"]
    outDF$CFLITA[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Litter"]
    outDF$CFLITA[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Litter"]
    outDF$CFLITA[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$term=="Litter"]
    outDF$CFLITA[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Litter"]^2+
                                                                       poolDF$eCO2_sd[poolDF$term=="Litter"]^2)/3)
    outDF$CFLITA[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Litter"]
    outDF$CFLITA[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Litter"]^2+poolDF$aCO2_sd[poolDF$term=="Litter"]^2+
                                                                   poolDF$eCO2_sd[poolDF$term=="Litter"]^2)/3)/poolDF$aCO2[poolDF$term=="Litter"]*100 
    
    ## CMIC
    outDF$CMIC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Microbial biomass"]
    outDF$CMIC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Microbial biomass"]
    outDF$CMIC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Microbial biomass"]
    outDF$CMIC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Microbial biomass"]
    outDF$CMIC[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$term=="Microbial biomass"]
    outDF$CMIC[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Microbial biomass"]^2+
                                                                     poolDF$eCO2_sd[poolDF$term=="Microbial biomass"]^2)/2)
    
    
    outDF$CMIC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Microbial biomass"]
    outDF$CMIC[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Microbial biomass"]^2+poolDF$aCO2_sd[poolDF$term=="Microbial biomass"]^2+
                                                                      poolDF$eCO2_sd[poolDF$term=="Microbial biomass"]^2)/3)/poolDF$aCO2[poolDF$term=="Microbial biomass"]*100 
    
    
    ## CSOIL
    outDF$CSOIL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Soil C"]
    outDF$CSOIL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Soil C"]
    outDF$CSOIL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Soil C"]
    outDF$CSOIL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Soil C"]
    outDF$CSOIL[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$term=="Soil C"]
    outDF$CSOIL[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Soil C"]^2+
                                                                      poolDF$eCO2_sd[poolDF$term=="Soil C"]^2)/2)
    
    outDF$CSOIL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Soil C"]
    outDF$CSOIL[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Soil C"]^2+poolDF$aCO2_sd[poolDF$term=="Soil C"]^2+
                                                                    poolDF$eCO2_sd[poolDF$term=="Soil C"]^2)/3)/poolDF$aCO2[poolDF$term=="Soil C"]*100
    
    
    ## CMYC
    outDF$CMYC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$term=="Mycorrhizae"]
    outDF$CMYC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$term=="Mycorrhizae"]
    outDF$CMYC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$term=="Mycorrhizae"]
    outDF$CMYC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$term=="Mycorrhizae"]
    outDF$CMYC[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$term=="Mycorrhizae"]
    outDF$CMYC[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Mycorrhizae"]^2+
                                                                     poolDF$eCO2_sd[poolDF$term=="Mycorrhizae"]^2)/2)
    
    outDF$CMYC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$term=="Mycorrhizae"]
    outDF$CMYC[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$term=="Mycorrhizae"]^2+poolDF$aCO2_sd[poolDF$term=="Mycorrhizae"]^2+
                                                                     poolDF$eCO2_sd[poolDF$term=="Mycorrhizae"]^2)/3)/poolDF$aCO2[poolDF$term=="Mycorrhizae"]*100
    
    
    
    ## deltaCL
    outDF$deltaCL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Overstorey leaf"]
    outDF$deltaCL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Overstorey leaf"]
    outDF$deltaCL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Overstorey leaf"]
    outDF$deltaCL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Overstorey leaf"]
    outDF$deltaCL[outDF$Group=="mean"&outDF$Trt=="diff"] <- deltaDF$diff[deltaDF$term=="Overstorey leaf"]
    outDF$deltaCL[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Overstorey leaf"]^2+
                                                                        deltaDF$eCO2_sd[deltaDF$term=="Overstorey leaf"]^2)/2)
    
    outDF$deltaCL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Overstorey leaf"]
    outDF$deltaCL[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Overstorey leaf"]^2+deltaDF$aCO2_sd[deltaDF$term=="Overstorey leaf"]^2+
                                                                       deltaDF$eCO2_sd[deltaDF$term=="Overstorey leaf"]^2)/3)/abs(deltaDF$aCO2[deltaDF$term=="Overstorey leaf"])*100
    
    
    ## deltaCW
    outDF$deltaCW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Overstorey wood"]
    outDF$deltaCW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Overstorey wood"]
    outDF$deltaCW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Overstorey wood"]
    outDF$deltaCW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Overstorey wood"]
    outDF$deltaCW[outDF$Group=="mean"&outDF$Trt=="diff"] <- deltaDF$diff[deltaDF$term=="Overstorey wood"]
    outDF$deltaCW[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Overstorey wood"]^2+
                                                                        deltaDF$eCO2_sd[deltaDF$term=="Overstorey wood"]^2)/2)
    
    outDF$deltaCW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Overstorey wood"]
    outDF$deltaCW[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Overstorey wood"]^2+deltaDF$aCO2_sd[deltaDF$term=="Overstorey wood"]^2+
                                                                       deltaDF$eCO2_sd[deltaDF$term=="Overstorey wood"]^2)/3)/abs(deltaDF$aCO2[deltaDF$term=="Overstorey wood"])*100
    
    ## deltaCFR
    tmp <- colSums(deltaDF[deltaDF$term%in%c("Fine Root", "Intermediate Root"),2:7], na.rm=T)/2
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    
    outDF$deltaCFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$deltaCFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$deltaCFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$deltaCFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$deltaCFR[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$deltaCFR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/2)
    outDF$deltaCFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$deltaCFR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/abs(amean)*100
    
    
    ## CCR
    outDF$deltaCCR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Coarse Root"]
    outDF$deltaCCR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Coarse Root"]
    outDF$deltaCCR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Coarse Root"]
    outDF$deltaCCR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Coarse Root"]
    outDF$deltaCCR[outDF$Group=="mean"&outDF$Trt=="diff"] <- deltaDF$diff[deltaDF$term=="Coarse Root"]
    outDF$deltaCCR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Coarse Root"]^2+
                                                                         deltaDF$eCO2_sd[deltaDF$term=="Coarse Root"]^2)/2)
    
    outDF$deltaCCR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Coarse Root"]
    outDF$deltaCCR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Coarse Root"]^2+deltaDF$aCO2_sd[deltaDF$term=="Coarse Root"]^2+
                                                                       deltaDF$eCO2_sd[deltaDF$term=="Coarse Root"]^2)/3)/abs(deltaDF$aCO2[deltaDF$term=="Coarse Root"])*100
    
    
    ## CFLITA
    outDF$deltaCFLITA[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Litter"]
    outDF$deltaCFLITA[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Litter"]
    outDF$deltaCFLITA[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Litter"]
    outDF$deltaCFLITA[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Litter"]
    outDF$deltaCFLITA[outDF$Group=="mean"&outDF$Trt=="diff"] <- deltaDF$diff[deltaDF$term=="Litter"]
    outDF$deltaCFLITA[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Litter"]^2+
                                                                            deltaDF$eCO2_sd[deltaDF$term=="Litter"]^2)/2)
    
    outDF$deltaCFLITA[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Litter"]
    outDF$deltaCFLITA[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Litter"]^2+deltaDF$aCO2_sd[deltaDF$term=="Litter"]^2+
                                                                        deltaDF$eCO2_sd[deltaDF$term=="Litter"]^2)/3)/abs(deltaDF$aCO2[deltaDF$term=="Litter"])*100
    
    ## CMIC
    outDF$deltaCMIC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Microbial biomass"]
    outDF$deltaCMIC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Microbial biomass"]
    outDF$deltaCMIC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Microbial biomass"]
    outDF$deltaCMIC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Microbial biomass"]
    outDF$deltaCMIC[outDF$Group=="mean"&outDF$Trt=="diff"] <- deltaDF$diff[deltaDF$term=="Microbial biomass"]
    outDF$deltaCMIC[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Microbial biomass"]^2+
                                                                          deltaDF$eCO2_sd[deltaDF$term=="Microbial biomass"]^2)/2)
    
    outDF$deltaCMIC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Microbial biomass"]
    outDF$deltaCMIC[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Microbial biomass"]^2+deltaDF$aCO2_sd[deltaDF$term=="Microbial biomass"]^2+
                                                                           deltaDF$eCO2_sd[deltaDF$term=="Microbial biomass"]^2)/3)/abs(deltaDF$aCO2[deltaDF$term=="Microbial biomass"])*100
    
    
    ## CSOIL
    outDF$deltaCSOIL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Soil C"]
    outDF$deltaCSOIL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Soil C"]
    outDF$deltaCSOIL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Soil C"]
    outDF$deltaCSOIL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Soil C"]
    outDF$deltaCSOIL[outDF$Group=="mean"&outDF$Trt=="diff"] <- deltaDF$diff[deltaDF$term=="Soil C"]
    outDF$deltaCSOIL[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Soil C"]^2+
                                                                           deltaDF$eCO2_sd[deltaDF$term=="Soil C"]^2)/2)
    
    outDF$deltaCSOIL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Soil C"]
    outDF$deltaCSOIL[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Soil C"]^2+deltaDF$aCO2_sd[deltaDF$term=="Soil C"]^2+
                                                                         deltaDF$eCO2_sd[deltaDF$term=="Soil C"]^2)/3)/abs(deltaDF$aCO2[deltaDF$term=="Soil C"])*100
    
    
    ## CMYC
    outDF$deltaCMYC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- deltaDF$aCO2[deltaDF$term=="Mycorrhizae"]
    outDF$deltaCMYC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- deltaDF$eCO2[deltaDF$term=="Mycorrhizae"]
    outDF$deltaCMYC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- deltaDF$aCO2_sd[deltaDF$term=="Mycorrhizae"]
    outDF$deltaCMYC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- deltaDF$eCO2_sd[deltaDF$term=="Mycorrhizae"]
    outDF$deltaCMYC[outDF$Group=="mean"&outDF$Trt=="diff"] <- deltaDF$diff[deltaDF$term=="Mycorrhizae"]
    outDF$deltaCMYC[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Mycorrhizae"]^2+
                                                                          deltaDF$eCO2_sd[deltaDF$term=="Mycorrhizae"]^2)/2)
    
    outDF$deltaCMYC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- deltaDF$percent_diff[deltaDF$term=="Mycorrhizae"]
    outDF$deltaCMYC[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((deltaDF$aCO2_sd[deltaDF$term=="Mycorrhizae"]^2+deltaDF$aCO2_sd[deltaDF$term=="Mycorrhizae"]^2+
                                                                          deltaDF$eCO2_sd[deltaDF$term=="Mycorrhizae"]^2)/3)/abs(deltaDF$aCO2[deltaDF$term=="Mycorrhizae"])*100
    
        
    ### CGL
    tmp <- colSums(nppDF[nppDF$term%in%c("Leaf NPP", "Leaf consumption"),2:7], na.rm=T)
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    
    outDF$CGL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$CGL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$CGL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$CGL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$CGL[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$CGL[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/2)
    outDF$CGL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$CGL[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/amean*100
    
    ### CGW
    tmp <- colSums(nppDF[nppDF$term%in%c("Stem NPP", "Other NPP"),2:7], na.rm=T)
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    
    outDF$CGW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$CGW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$CGW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$CGW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$CGW[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$CGW[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/2)
    outDF$CGW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$CGW[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/amean*100
    
    
    ### CGFR
    tmp <- colSums(nppDF[nppDF$term%in%c("Fine Root NPP", "Intermediate Root NPP"),2:7], na.rm=T)/2
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    
    outDF$CGFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$CGFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$CGFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$CGFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$CGFR[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$CGFR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/2)
    outDF$CGFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$CGFR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/amean*100
    
    ### CGCR
    outDF$CGCR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- nppDF$aCO2[nppDF$term=="Coarse Root NPP"]
    outDF$CGCR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- nppDF$eCO2[nppDF$term=="Coarse Root NPP"]
    outDF$CGCR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- nppDF$aCO2_sd[nppDF$term=="Coarse Root NPP"]
    outDF$CGCR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- nppDF$eCO2_sd[nppDF$term=="Coarse Root NPP"]
    outDF$CGCR[outDF$Group=="mean"&outDF$Trt=="diff"] <- nppDF$diff[nppDF$term=="Coarse Root NPP"]
    outDF$CGCR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((nppDF$aCO2_sd[nppDF$term=="Coarse Root NPP"]^2+
                                                                     nppDF$eCO2_sd[nppDF$term=="Coarse Root NPP"]^2)/2)
    outDF$CGCR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- nppDF$percent_diff[nppDF$term=="Coarse Root NPP"]
    outDF$CGCR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((nppDF$aCO2_sd[nppDF$term=="Coarse Root NPP"]^2+nppDF$aCO2_sd[nppDF$term=="Coarse Root NPP"]^2+
                                                                    nppDF$eCO2_sd[nppDF$term=="Coarse Root NPP"]^2)/3)/nppDF$aCO2[nppDF$term=="Coarse Root NPP"]*100
    
    ### NPP - include exudation 
    tmpDF <- nppDF[nppDF$term%in%c("Leaf NPP", "Stem NPP", 
                                   "Fine Root NPP", "Intermediate Root NPP",
                                   "Coarse Root NPP", "Other NPP",
                                   "Leaf consumption", "Mycorrhizal production"),1:7]
    tmpDF[tmpDF$term=="Fine Root NPP",2:7] <- tmpDF[tmpDF$term=="Fine Root NPP",2:7] / 2
    tmpDF[tmpDF$term=="Intermediate Root NPP",2:7] <- tmpDF[tmpDF$term=="Intermediate Root NPP",2:7] / 2
    
    tmp <- colSums(tmpDF[tmpDF$term%in%c("Leaf NPP", "Stem NPP", 
                                         "Fine Root NPP", "Intermediate Root NPP",
                                         "Coarse Root NPP", "Other NPP",
                                         "Leaf consumption", "Mycorrhizal production"),2:7], na.rm=T)
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    
    outDF$NPP[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$NPP[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$NPP[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$NPP[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$NPP[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$NPP[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/2)
    outDF$NPP[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$NPP[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/amean*100
    
    
    ### RHET
    outDF$RHET[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- nppDF$aCO2[nppDF$term=="R hetero"]
    outDF$RHET[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- nppDF$eCO2[nppDF$term=="R hetero"]
    outDF$RHET[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- nppDF$aCO2_sd[nppDF$term=="R hetero"]
    outDF$RHET[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- nppDF$eCO2_sd[nppDF$term=="R hetero"]
    outDF$RHET[outDF$Group=="mean"&outDF$Trt=="diff"] <- nppDF$diff[nppDF$term=="R hetero"]
    outDF$RHET[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((nppDF$aCO2_sd[nppDF$term=="R hetero"]^2+
                                                                     nppDF$eCO2_sd[nppDF$term=="R hetero"]^2)/2)
    outDF$RHET[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- nppDF$percent_diff[nppDF$term=="R hetero"]
    outDF$RHET[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((nppDF$aCO2_sd[nppDF$term=="R hetero"]^2+nppDF$aCO2_sd[nppDF$term=="R hetero"]^2+
                                                                    nppDF$eCO2_sd[nppDF$term=="R hetero"]^2)/3)/nppDF$aCO2[nppDF$term=="R hetero"]*100
    
    
    ### RL
    outDF$RL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="Ra leaf"]
    outDF$RL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="Ra leaf"]
    outDF$RL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="Ra leaf"]
    outDF$RL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="Ra leaf"]
    outDF$RL[outDF$Group=="mean"&outDF$Trt=="diff"] <- inoutDF$diff[inoutDF$term=="Ra leaf"]
    outDF$RL[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="Ra leaf"]^2+
                                                                   inoutDF$eCO2_sd[inoutDF$term=="Ra leaf"]^2)/2)
    
    outDF$RL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="Ra leaf"]
    outDF$RL[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="Ra leaf"]^2+inoutDF$aCO2_sd[inoutDF$term=="Ra leaf"]^2+
                                                                  inoutDF$eCO2_sd[inoutDF$term=="Ra leaf"]^2)/3)/inoutDF$aCO2[inoutDF$term=="Ra leaf"]*100
    
    
    ### RW
    outDF$RW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="Ra stem"]
    outDF$RW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="Ra stem"]
    outDF$RW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="Ra stem"]
    outDF$RW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="Ra stem"]
    outDF$RW[outDF$Group=="mean"&outDF$Trt=="diff"] <- inoutDF$diff[inoutDF$term=="Ra stem"]
    outDF$RW[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="Ra stem"]^2+
                                                                   inoutDF$eCO2_sd[inoutDF$term=="Ra stem"]^2)/2)
    
    outDF$RW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="Ra stem"]
    outDF$RW[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="Ra stem"]^2+inoutDF$aCO2_sd[inoutDF$term=="Ra stem"]^2+
                                                                  inoutDF$eCO2_sd[inoutDF$term=="Ra stem"]^2)/3)/inoutDF$aCO2[inoutDF$term=="Ra stem"]*100
    
    
    ### RFR
    outDF$RFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="Ra overstorey root"]
    outDF$RFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="Ra overstorey root"]
    outDF$RFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="Ra overstorey root"]
    outDF$RFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="Ra overstorey root"]
    outDF$RFR[outDF$Group=="mean"&outDF$Trt=="diff"] <- inoutDF$diff[inoutDF$term=="Ra overstorey root"]
    outDF$RFR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="Ra overstorey root"]^2+
                                                                    inoutDF$eCO2_sd[inoutDF$term=="Ra overstorey root"]^2)/2)
    outDF$RFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="Ra overstorey root"]
    outDF$RFR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="Ra overstorey root"]^2+inoutDF$aCO2_sd[inoutDF$term=="Ra overstorey root"]^2+
                                                                  inoutDF$eCO2_sd[inoutDF$term=="Ra overstorey root"]^2)/3)/inoutDF$aCO2[inoutDF$term=="Ra overstorey root"]*100
    
    
    #outDF$RFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="Ra root"]
    #outDF$RFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="Ra root"]
    #outDF$RFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="Ra root"]
    #outDF$RFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="Ra root"]
    #outDF$RFR[outDF$Group=="mean"&outDF$Trt=="diff"] <- inoutDF$diff[inoutDF$term=="Ra root"]
    #outDF$RFR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="Ra root"]^2+
    #                                                            inoutDF$eCO2_sd[inoutDF$term=="Ra root"]^2)/2)
    #outDF$RFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="Ra root"]
    #outDF$RFR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="Ra root"]^2+inoutDF$aCO2_sd[inoutDF$term=="Ra root"]^2+
    #                                                                inoutDF$eCO2_sd[inoutDF$term=="Ra root"]^2)/3)/inoutDF$aCO2[inoutDF$term=="Ra root"]*100
    
    
    
    ### CVOC
    outDF$CVOC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="VOC"]
    outDF$CVOC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="VOC"]
    outDF$CVOC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="VOC"]
    outDF$CVOC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="VOC"]
    outDF$CVOC[outDF$Group=="mean"&outDF$Trt=="diff"] <- inoutDF$diff[inoutDF$term=="VOC"]
    outDF$CVOC[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="VOC"]^2+
                                                                     inoutDF$eCO2_sd[inoutDF$term=="VOC"]^2)/2)
    
    outDF$CVOC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="VOC"]
    outDF$CVOC[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="VOC"]^2+inoutDF$aCO2_sd[inoutDF$term=="VOC"]^2+
                                                                   inoutDF$eCO2_sd[inoutDF$term=="VOC"]^2)/3)/inoutDF$aCO2[inoutDF$term=="VOC"]*100
    
    
    ### RGR
    outDF$RGR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- inoutDF$aCO2[inoutDF$term=="Rgrowth"]
    outDF$RGR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- inoutDF$eCO2[inoutDF$term=="Rgrowth"]
    outDF$RGR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- inoutDF$aCO2_sd[inoutDF$term=="Rgrowth"]
    outDF$RGR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- inoutDF$eCO2_sd[inoutDF$term=="Rgrowth"]
    outDF$RGR[outDF$Group=="mean"&outDF$Trt=="diff"] <- inoutDF$diff[inoutDF$term=="Rgrowth"]
    outDF$RGR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="Rgrowth"]^2+
                                                                    inoutDF$eCO2_sd[inoutDF$term=="Rgrowth"]^2)/2)
    outDF$RGR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- inoutDF$percent_diff[inoutDF$term=="Rgrowth"]
    outDF$RGR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((inoutDF$aCO2_sd[inoutDF$term=="Rgrowth"]^2+inoutDF$aCO2_sd[inoutDF$term=="Rgrowth"]^2+
                                                                    inoutDF$eCO2_sd[inoutDF$term=="Rgrowth"]^2)/3)/inoutDF$aCO2[inoutDF$term=="Rgrowth"]*100
    
    ### RAU, assume half of root respiration is overstorey
    tmp <- colSums(inoutDF[inoutDF$term%in%c("Ra leaf", "Ra stem", 
                                         "Ra overstorey root", "Rgrowth"),2:7], na.rm=T)
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    
    outDF$RAU[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$RAU[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$RAU[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$RAU[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$RAU[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$RAU[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/2)
    outDF$RAU[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$RAU[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/amean*100
    
    
    ### RECO
    tmp <- colSums(rbind(inoutDF[inoutDF$term%in%c("Ra leaf", "Ra stem", "Ra overstorey root",
                                                   "Rgrowth"),2:7], 
                         nppDF[nppDF$term=="R hetero",2:7]), na.rm=T)
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    
    outDF$RECO[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$RECO[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$RECO[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$RECO[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$RECO[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$RECO[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/3)
    outDF$RECO[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$RECO[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/amean*100
    
    
    ### CEX
    outDF$CEX[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- nppDF$aCO2[nppDF$term=="Mycorrhizal production"]
    outDF$CEX[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- nppDF$eCO2[nppDF$term=="Mycorrhizal production"]
    outDF$CEX[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- nppDF$aCO2_sd[nppDF$term=="Mycorrhizal production"]
    outDF$CEX[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- nppDF$eCO2_sd[nppDF$term=="Mycorrhizal production"]
    outDF$CEX[outDF$Group=="mean"&outDF$Trt=="diff"] <- nppDF$diff[nppDF$term=="Mycorrhizal production"]
    outDF$CEX[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((nppDF$aCO2_sd[nppDF$term=="Mycorrhizal production"]^2+nppDF$eCO2_sd[nppDF$term=="Mycorrhizal production"]^2)/2)
    
    outDF$CEX[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- nppDF$percent_diff[nppDF$term=="Mycorrhizal production"]
    
    
    ### NEP in - out
    outDF$NEP_inout[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- nepDF$NEP[nepDF$Method=="In-out"&nepDF$Trt=="aCO2"]
    outDF$NEP_inout[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- nepDF$NEP[nepDF$Method=="In-out"&nepDF$Trt=="eCO2"]
    outDF$NEP_inout[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- nepDF$NEP_conf[nepDF$Method=="In-out"&nepDF$Trt=="aCO2"]
    outDF$NEP_inout[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- nepDF$NEP_conf[nepDF$Method=="In-out"&nepDF$Trt=="eCO2"]
    outDF$NEP_inout[outDF$Group=="mean"&outDF$Trt=="diff"] <- (nepDF$NEP[nepDF$Method=="In-out"&nepDF$Trt=="eCO2"]-nepDF$NEP[nepDF$Method=="In-out"&nepDF$Trt=="aCO2"])
    outDF$NEP_inout[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((nepDF$NEP_conf[nepDF$Method=="In-out"&nepDF$Trt=="aCO2"]^2+
                                                                          nepDF$NEP_conf[nepDF$Method=="In-out"&nepDF$Trt=="eCO2"]^2)/2)
    
    outDF$NEP_inout[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (nepDF$NEP[nepDF$Method=="In-out"&nepDF$Trt=="eCO2"]-nepDF$NEP[nepDF$Method=="In-out"&nepDF$Trt=="aCO2"])/nepDF$NEP[nepDF$Method=="In-out"&nepDF$Trt=="aCO2"]*100
    outDF$NEP_inout[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((nepDF$NEP_conf[nepDF$Method=="In-out"&nepDF$Trt=="aCO2"]^2+nepDF$NEP_conf[nepDF$Method=="In-out"&nepDF$Trt=="aCO2"]^2+
                                                                          nepDF$NEP_conf[nepDF$Method=="In-out"&nepDF$Trt=="eCO2"]^2)/3)/nepDF$NEP[nepDF$Method=="In-out"&nepDF$Trt=="aCO2"]*100
    
    ### NEP npp - rh
    outDF$NEP_npprh[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- nepDF$NEP[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"]
    outDF$NEP_npprh[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- nepDF$NEP[nepDF$Method=="NPP-Rh"&nepDF$Trt=="eCO2"]
    outDF$NEP_npprh[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- nepDF$NEP_conf[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"]
    outDF$NEP_npprh[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- nepDF$NEP_conf[nepDF$Method=="NPP-Rh"&nepDF$Trt=="eCO2"]
    outDF$NEP_npprh[outDF$Group=="mean"&outDF$Trt=="diff"] <- (nepDF$NEP[nepDF$Method=="NPP-Rh"&nepDF$Trt=="eCO2"]-nepDF$NEP[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"])
    outDF$NEP_npprh[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((nepDF$NEP_conf[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"]^2+nepDF$NEP_conf[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"]^2+
                                                                          nepDF$NEP_conf[nepDF$Method=="NPP-Rh"&nepDF$Trt=="eCO2"]^2)/2)
    
    outDF$NEP_npprh[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (nepDF$NEP[nepDF$Method=="NPP-Rh"&nepDF$Trt=="eCO2"]-nepDF$NEP[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"])/nepDF$NEP[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"]*100
    outDF$NEP_npprh[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((nepDF$NEP_conf[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"]^2+nepDF$NEP_conf[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"]^2+
                                                                          nepDF$NEP_conf[nepDF$Method=="NPP-Rh"&nepDF$Trt=="eCO2"]^2)/3)/nepDF$NEP[nepDF$Method=="NPP-Rh"&nepDF$Trt=="aCO2"]*100
    
    ### NEP, delta pools
    outDF$NEP_pools[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- nepDF$NEP[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"]
    outDF$NEP_pools[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- nepDF$NEP[nepDF$Method=="Pool"&nepDF$Trt=="eCO2"]
    outDF$NEP_pools[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- nepDF$NEP_conf[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"]
    outDF$NEP_pools[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- nepDF$NEP_conf[nepDF$Method=="Pool"&nepDF$Trt=="eCO2"]
    outDF$NEP_pools[outDF$Group=="mean"&outDF$Trt=="diff"] <- (nepDF$NEP[nepDF$Method=="Pool"&nepDF$Trt=="eCO2"]-nepDF$NEP[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"])
    outDF$NEP_pools[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((nepDF$NEP_conf[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"]^2+nepDF$NEP_conf[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"]^2+
                                                                          nepDF$NEP_conf[nepDF$Method=="Pool"&nepDF$Trt=="eCO2"]^2)/2)
    
    outDF$NEP_pools[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (nepDF$NEP[nepDF$Method=="Pool"&nepDF$Trt=="eCO2"]-nepDF$NEP[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"])/nepDF$NEP[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"]*100
    outDF$NEP_pools[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((nepDF$NEP_conf[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"]^2+nepDF$NEP_conf[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"]^2+
                                                                          nepDF$NEP_conf[nepDF$Method=="Pool"&nepDF$Trt=="eCO2"]^2)/3)/nepDF$NEP[nepDF$Method=="Pool"&nepDF$Trt=="aCO2"]*100
    
    
    ### NEP all
    outDF$NEP[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- (outDF$NEP_inout[outDF$Group=="mean"&outDF$Trt=="aCO2"]+
                                                                 outDF$NEP_npprh[outDF$Group=="mean"&outDF$Trt=="aCO2"]+
                                                                 outDF$NEP_pools[outDF$Group=="mean"&outDF$Trt=="aCO2"])/3
    
    outDF$NEP[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- (outDF$NEP_inout[outDF$Group=="mean"&outDF$Trt=="eCO2"]+
                                                                 outDF$NEP_npprh[outDF$Group=="mean"&outDF$Trt=="eCO2"]+
                                                                 outDF$NEP_pools[outDF$Group=="mean"&outDF$Trt=="eCO2"])/3
    
    outDF$NEP[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- sqrt((outDF$NEP_inout[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                               outDF$NEP_npprh[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                               outDF$NEP_pools[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2)/3)
    
    outDF$NEP[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- sqrt((outDF$NEP_inout[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                                    outDF$NEP_npprh[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                                    outDF$NEP_pools[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/3)
    
    outDF$NEP[outDF$Group=="mean"&outDF$Trt=="diff"] <- (outDF$NEP_inout[outDF$Group=="mean"&outDF$Trt=="diff"]+
                                                                 outDF$NEP_npprh[outDF$Group=="mean"&outDF$Trt=="diff"]+
                                                                 outDF$NEP_pools[outDF$Group=="mean"&outDF$Trt=="diff"])/3
    
    outDF$NEP[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$NEP_inout[outDF$Group=="sd"&outDF$Trt=="diff"]^2+
                                                                    outDF$NEP_npprh[outDF$Group=="sd"&outDF$Trt=="diff"]^2+
                                                                    outDF$NEP_pools[outDF$Group=="sd"&outDF$Trt=="diff"]^2)/3)
    
    outDF$NEP[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (outDF$NEP_inout[outDF$Group=="mean"&outDF$Trt=="pct_diff"]+
                                                                     outDF$NEP_npprh[outDF$Group=="mean"&outDF$Trt=="pct_diff"]+
                                                                     outDF$NEP_pools[outDF$Group=="mean"&outDF$Trt=="pct_diff"])/3
    
    outDF$NEP[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((outDF$NEP_inout[outDF$Group=="sd"&outDF$Trt=="pct_diff"]^2+
                                                                        outDF$NEP_npprh[outDF$Group=="sd"&outDF$Trt=="pct_diff"]^2+
                                                                        outDF$NEP_pools[outDF$Group=="sd"&outDF$Trt=="pct_diff"]^2)/3)
    
    ### read in laiDF
    laiDF <- read.csv("validation_dataset/EucFACE_LAI_2012_2016.csv")
    
    laisumDF <- summaryBy(lai~Trt, data=laiDF, FUN=c(mean,sd), 
                          keep.names=T, na.rm=T)
    
    
    ### LAI
    outDF$LAI[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- laisumDF$lai.mean[laisumDF$Trt=="aCO2"]
    outDF$LAI[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- laisumDF$lai.mean[laisumDF$Trt=="eCO2"]
    outDF$LAI[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- laisumDF$lai.sd[laisumDF$Trt=="aCO2"]
    outDF$LAI[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- laisumDF$lai.sd[laisumDF$Trt=="eCO2"]
    outDF$LAI[outDF$Group=="mean"&outDF$Trt=="diff"] <- (laisumDF$lai.mean[laisumDF$Trt=="eCO2"]-laisumDF$lai.mean[laisumDF$Trt=="aCO2"])
    outDF$LAI[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((laisumDF$lai.sd[laisumDF$Trt=="eCO2"]^2+laisumDF$lai.sd[laisumDF$Trt=="aCO2"]^2)/2)
    
    outDF$LAI[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (laisumDF$lai.mean[laisumDF$Trt=="eCO2"]-laisumDF$lai.mean[laisumDF$Trt=="aCO2"])/laisumDF$lai.mean[laisumDF$Trt=="aCO2"]*100
    outDF$LAI[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((laisumDF$lai.sd[laisumDF$Trt=="eCO2"]^2+laisumDF$lai.sd[laisumDF$Trt=="aCO2"]^2+laisumDF$lai.sd[laisumDF$Trt=="aCO2"]^2)/3)/laisumDF$lai.mean[laisumDF$Trt=="aCO2"]*100
    
    
    ### BP
    outDF$BP[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- outDF$deltaCL[outDF$Group=="mean"&outDF$Trt=="aCO2"]+
        outDF$deltaCW[outDF$Group=="mean"&outDF$Trt=="aCO2"]+
        outDF$deltaCFR[outDF$Group=="mean"&outDF$Trt=="aCO2"]+
        outDF$deltaCCR[outDF$Group=="mean"&outDF$Trt=="aCO2"]
        
    outDF$BP[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- outDF$deltaCL[outDF$Group=="mean"&outDF$Trt=="eCO2"]+
        outDF$deltaCW[outDF$Group=="mean"&outDF$Trt=="eCO2"]+
        outDF$deltaCFR[outDF$Group=="mean"&outDF$Trt=="eCO2"]+
        outDF$deltaCCR[outDF$Group=="mean"&outDF$Trt=="eCO2"]
    
    outDF$BP[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- sqrt((outDF$deltaCL[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                               outDF$deltaCW[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                               outDF$deltaCFR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                               outDF$deltaCCR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2)/4)
    
    outDF$BP[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- sqrt((outDF$deltaCL[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                               outDF$deltaCW[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                               outDF$deltaCFR[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                               outDF$deltaCCR[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/4)
    
    outDF$BP[outDF$Group=="mean"&outDF$Trt=="diff"] <- outDF$BP[outDF$Group=="mean"&outDF$Trt=="eCO2"]-outDF$BP[outDF$Group=="mean"&outDF$Trt=="aCO2"]
    outDF$BP[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$BP[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+outDF$BP[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/2)
    
    outDF$BP[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (outDF$BP[outDF$Group=="mean"&outDF$Trt=="eCO2"]-outDF$BP[outDF$Group=="mean"&outDF$Trt=="aCO2"])/outDF$BP[outDF$Group=="mean"&outDF$Trt=="aCO2"]*100
    outDF$BP[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((outDF$BP[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+outDF$BP[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+outDF$BP[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2)/3)/outDF$BP[outDF$Group=="mean"&outDF$Trt=="aCO2"]*100
    
    
    
    
    return(outDF)
    
}
