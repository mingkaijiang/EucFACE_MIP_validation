prepare_RSOIL_for_taylor_diagram <- function(ambDF, eleDF) {
    
    ### input
    subDF1 <- ambDF[,c("YEAR", "DOY", "Date", "RHET", "RCR", "RFR", "ModName")]
    subDF2 <- eleDF[,c("YEAR", "DOY", "Date", "RHET", "RCR", "RFR", "ModName")]
    
    subDF1$Trt <- "amb"
    subDF2$Trt <- "ele"

    subDF1$Rsoil<- rowSums(data.frame(subDF1$RHET, subDF1$RCR, subDF1$RFR), na.rm=T)
    subDF2$Rsoil<- rowSums(data.frame(subDF2$RHET, subDF2$RCR, subDF2$RFR), na.rm=T)
    
    ambDF <- subDF1[,c("Date", "ModName", "Rsoil")]
    eleDF <- subDF2[,c("Date", "ModName", "Rsoil")]
    
    
    #### prepare taylor diagram for GPP
    rsoilDF <- read.csv("validation_dataset/EucFACE_daily_soil_respiration_flux_2013_2015.csv")
    rsoilDF$Date <- as.Date(as.character(rsoilDF$Date))
    
    ### convert unit, from mg m-2 d-1 to g m-2 d-1
    rsoilDF$OBS <- rsoilDF$Rsoil_mg_m2_d / 1000.0
    
    rsoilDF1 <- rsoilDF[rsoilDF$Trt=="aCO2",]
    rsoilDF2 <- rsoilDF[rsoilDF$Trt=="eCO2",]
    
    rsoilDF1$Trt <- NULL
    rsoilDF2$Trt <- NULL
    
    
    ### merge
    myDF1 <- merge(ambDF, rsoilDF1, by="Date")
    myDF2 <- merge(eleDF, rsoilDF2, by="Date")
    
    ### add information
    myDF1$Trt <- "amb"
    myDF2$Trt <- "ele"
    
    outDF <- rbind(myDF1, myDF2)
    
    return(outDF)
    
}