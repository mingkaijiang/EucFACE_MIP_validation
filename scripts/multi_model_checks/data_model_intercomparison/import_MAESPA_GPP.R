import_MAESPA_GPP <- function() {
    
    gppDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/MAESPA_output/maespa.year.ring.csv")
    laiDF <- read.csv("validation_dataset/EucFACE_LAI_2012_2016.csv")
    
    ### process GPP dataset
    aDF <- gppDF[gppDF$Ring%in%c("R2", "R3", "R6"),]
    eDF <- gppDF[gppDF$Ring%in%c("R1", "R4", "R5"),]
    
    asDF <- summaryBy(GPP.sum.400~year, data=aDF, FUN=mean, na.rm=T, keep.names=T)
    esDF <- summaryBy(GPP.sum.550~year, data=eDF, FUN=mean, na.rm=T, keep.names=T)
    
    colnames(asDF) <- colnames(esDF) <- c("YEAR", "GPP")
    asDF$Trt <- "amb"
    esDF$Trt <- "ele"
    
    gppDF <- rbind(asDF, esDF)
    
    ### process LAI data
    laiDF$YEAR <- year(laiDF$Date)
    laiDF <- subset(laiDF, YEAR>2012 & YEAR <= 2016)
    laisDF <- summaryBy(lai~YEAR+Trt, FUN=mean, data=laiDF, na.rm=T, keep.names=T)
    
    laisDF$Trt[laisDF$Trt=="aCO2"] <- "amb"
    laisDF$Trt[laisDF$Trt=="eCO2"] <- "ele"
    

    outDF1 <- summaryBy(GPP~Trt, FUN=c(mean, sd), data=gppDF, keep.names=T,
                        na.rm=T)
    
    outDF2 <- summaryBy(lai~Trt, FUN=c(mean, sd), data=laisDF, keep.names=T,
                        na.rm=T)
    
    outDF1$ModName <- outDF2$ModName <- "OBS"
    
    colnames(outDF1) <- c("Trt", "GPP.mean", "GPP.sd", "ModName")
    colnames(outDF2) <- c("Trt", "LAI.mean.mean", "LAI.mean.sd", "ModName")
    
    outDF1 <- outDF1[,c("ModName","Trt", "GPP.mean", "GPP.sd")]
    outDF2 <- outDF2[,c("ModName","Trt", "LAI.mean.mean", "LAI.mean.sd")]
    
    
    out <- list(gppDF=outDF1,
                laiDF=outDF2)
    
    return(out)
    
}