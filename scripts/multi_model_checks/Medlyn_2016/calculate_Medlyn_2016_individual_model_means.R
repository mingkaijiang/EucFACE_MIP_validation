
calculate_Medlyn_2016_individual_model_means <- function(time.period) {
    
    
    ambDF <- readRDS(paste0(getwd(), "/output/MIP_output/processed_simulation/Medyn2016/MIP_Medlyn_AMB_annual.rds"))
    eleDF <- readRDS(paste0(getwd(), "/output/MIP_output/processed_simulation/Medyn2016/MIP_Medlyn_ELE_annual.rds"))
    
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    
    myDF <- rbind(ambDF, eleDF)
    
    
    ### subset a period equivalent to the current MIP
    myDF <- subset(myDF, YEAR >= time.period[1] & YEAR <= time.period[2])
    
    
    outDF <- summaryBy(.~ModName+ModVersion+Trt, FUN=c(mean, sd), data=myDF,
                       na.rm=T, keep.names=T)
    
    
    return(outDF)
    
}
