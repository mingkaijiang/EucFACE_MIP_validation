calculate_proportionality_of_WUE <- function(ambDF, eleDF) {
    
    ### calculate WUE
    ambDF$WUE <- with(ambDF, GPP / TRANS)
    eleDF$WUE <- with(eleDF, GPP / TRANS)
    
    
    ### subset
    ambDF <- ambDF[,c("ModName", "YEAR", "CO2", "WUE")]
    eleDF <- eleDF[,c("ModName", "YEAR", "CO2", "WUE")]
    
    ### missing information
    ambDF$CO2 <- ifelse(is.nan(ambDF$CO2), ambDF$CO2[ambDF$ModName=="A_GDAYP"], ambDF$CO2)
    eleDF$CO2 <- ifelse(is.nan(eleDF$CO2), eleDF$CO2[eleDF$ModName=="A_GDAYP"], eleDF$CO2)
    
    
    ### merge
    myDF <- merge(ambDF, eleDF, by=c("ModName", "YEAR"))
    
    
    myDF$Norm_response <- with(myDF, ((WUE.y/WUE.x)/(CO2.y/CO2.x)))
    
    
    ### ignore inf
    is.na(myDF) <- sapply(myDF, is.nan)
    
    
    return(myDF)
}