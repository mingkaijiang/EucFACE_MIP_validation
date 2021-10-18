prepare_LAI_for_taylor_diagram <- function(ambDF, eleDF) {
    
    ### input
    ambDF <- ambDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI")]
    
    eleDF <- eleDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI")]
    
    
    #### prepare taylor diagram for GPP
    laiDF <- read.csv("validation_dataset/EucFACE_LAI_2012_2016.csv")
    
    #laiDF <- laiDF[laiDF$Trt=="aCO2",]
    laiDF$Date <- as.Date(as.character(laiDF$Date))
    
    names(laiDF)[names(laiDF)=="lai"] <- "OBS"

    laiDF1 <- laiDF[laiDF$Trt=="aCO2",]
    laiDF2 <- laiDF[laiDF$Trt=="eCO2",]
    
    laiDF1$Trt <- NULL
    laiDF2$Trt <- NULL
    
    
    ### merge
    myDF1 <- merge(ambDF, laiDF1, by="Date")
    myDF2 <- merge(eleDF, laiDF2, by="Date")
    
    ### add information
    myDF1$Trt <- "amb"
    myDF2$Trt <- "ele"
    
    outDF <- rbind(myDF1, myDF2)
    
    return(outDF)
    
}