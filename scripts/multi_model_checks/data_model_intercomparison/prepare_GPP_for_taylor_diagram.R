prepare_GPP_for_taylor_diagram <- function(ambDF, eleDF) {
    
    ### input
    ambDF <- ambDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI")]
    
    eleDF <- eleDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI")]
    
    ### calculate annual total
    ambDF3 <- summaryBy(GPP~YEAR+ModName, FUN=sum,
                        na.rm=T, keep.names=T, data=ambDF)
    
    eleDF3 <- summaryBy(GPP~YEAR+ModName, FUN=sum,
                        na.rm=T, keep.names=T, data=eleDF)
    
    
    
    #### prepare taylor diagram for GPP
    maespaDF <- read.csv("simulation_output/maespa.year.ring.csv")
    
    mambDF <- summaryBy(GPP.sum.400~year, data=maespaDF[maespaDF$Ring%in%c("R2","R3","R6"),],
                        FUN=mean, keep.names=T, na.rm=T)
    meleDF <- summaryBy(GPP.sum.550~year, data=maespaDF[maespaDF$Ring%in%c("R1","R4","R5"),],
                        FUN=mean, keep.names=T, na.rm=T)
    
    colnames(mambDF) <- colnames(meleDF) <- c("YEAR", "GPP")
    
    
    names(mambDF)[names(mambDF)=="GPP"] <- "OBS"
    names(meleDF)[names(meleDF)=="GPP"] <- "OBS"
    
    
    ### merge
    myDF1 <- merge(ambDF3, mambDF, by="YEAR")
    myDF2 <- merge(eleDF3, meleDF, by="YEAR")
    
    ### add information
    myDF1$Trt <- "amb"
    myDF2$Trt <- "ele"
    
    outDF <- rbind(myDF1, myDF2)
    
    return(outDF)
    
}