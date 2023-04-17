import_MAESPA_GPP <- function() {
    
    
    ### LAI
    laiDF <- read.csv("validation_dataset/EucFACE_LAI_2012_2016.csv")
    
    
    outDF2 <- summaryBy(lai~Trt, data=laiDF, FUN=c(mean,sd), 
                        keep.names=T, na.rm=T)
    
    outDF2$ModName <- "OBS"
    
    colnames(outDF2) <- c("Trt", "LAI.mean.mean", "LAI.mean.sd", "ModName")
    
    
    ### GPP
    gppDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/inout.csv")
    #read.csv("validation_dataset/EucFACE_C_Budget_data/MAESPA_output/maespa.year.ring.csv")
    
    
    ### process GPP dataset
    aDF <- gppDF[gppDF$term=="GPP overstorey", c("Ring_2", "Ring_3", "Ring_6")]
    eDF <- gppDF[gppDF$term=="GPP overstorey", c("Ring_1", "Ring_4", "Ring_5")]
    
    a1 <- rowMeans(aDF)
    a2 <- sd(c(aDF[1,1], aDF[1,2], aDF[1,3]))
    
    a3 <- rowMeans(eDF)
    a4 <- sd(c(eDF[1,1], eDF[1,2], eDF[1,3]))
    
    
    outDF1 <- outDF2
    colnames(outDF1) <- c("Trt", "GPP.mean", "GPP.sd", "ModName")
    outDF1$GPP.mean[outDF1$Trt=="aCO2"] <- a1
    outDF1$GPP.sd[outDF1$Trt=="aCO2"] <- a2
    
    outDF1$GPP.mean[outDF1$Trt=="eCO2"] <- a3
    outDF1$GPP.sd[outDF1$Trt=="eCO2"] <- a4
    
    outDF1$Trt[outDF1$Trt=="aCO2"] <- "amb"
    outDF2$Trt[outDF2$Trt=="aCO2"] <- "amb"
    
    outDF1$Trt[outDF1$Trt=="eCO2"] <- "ele"
    outDF2$Trt[outDF2$Trt=="eCO2"] <- "ele"
    
    
    ### prepare Aleaf dataset
    gppDF2 <- read.csv("validation_dataset/EucFACE_C_Budget_data/MAESPA_output/maespa.year.ring.csv")
    gppDF2$GPP550_leaf <- with(gppDF2, GPP.sum.550/LAI.mean/365)
    gppDF2$GPP400_leaf <- with(gppDF2, GPP.sum.400/LAI.mean/365)
    
    a1 <- mean(gppDF2$GPP400_leaf)
    a2 <- sd(gppDF2$GPP400_leaf)
    
    a3 <- mean(gppDF2$GPP550_leaf)
    a4 <- sd(gppDF2$GPP550_leaf)
    
    
    outDF3 <- outDF1
    colnames(outDF3) <- c("Trt", "Aleaf.mean.mean", "Aleaf.mean.sd", "ModName")
    outDF3$Aleaf.mean.mean[outDF3$Trt=="amb"] <- a1
    outDF3$Aleaf.mean.sd[outDF3$Trt=="amb"] <- a2
    
    outDF3$Aleaf.mean.mean[outDF3$Trt=="ele"] <- a3
    outDF3$Aleaf.mean.sd[outDF3$Trt=="ele"] <- a4
    
    
    
    
    out <- list(gppDF=outDF1,
                laiDF=outDF2,
                aleafDF=outDF3)
    
    return(out)
    
}