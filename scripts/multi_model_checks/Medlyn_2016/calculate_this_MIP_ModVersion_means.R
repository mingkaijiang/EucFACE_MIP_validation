
calculate_this_MIP_ModVersion_means <- function(time.period) {
    
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_FIX_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_FIX_ELE_annual.rds"))
    
    
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    
    myDF <- rbind(ambDF, eleDF)
    
    
    ### subset a period equivalent to the current MIP
    myDF <- subset(myDF, YEAR >= time.period[1] & YEAR <= time.period[2])
    
    
    ### add model version
    myDF$ModVersion <- ifelse(myDF$ModName%in%c("I_GDAYN", "J_LPJGN"), "CN", "CNP")
    
    
    outDF <- summaryBy(.~ModVersion+Trt, FUN=c(mean, sd), data=myDF,
                       na.rm=T, keep.names=T)
    
    
    return(outDF)
    
}
