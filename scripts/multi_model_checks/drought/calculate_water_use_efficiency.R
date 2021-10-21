calculate_water_use_efficiency <- function (ambDF, eleDF) {
    
    ### calculate WUE
    ambDF$WUE <- with(ambDF, GPP / TRANS)
    eleDF$WUE <- with(eleDF, GPP / TRANS)

    
    ### assign variable
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    
    myDF <- rbind(ambDF, eleDF)
    
    ### calculate means
    outDF <- summaryBy(WUE~ModName+Trt, FUN=c(mean, sd), keep.names=T,
                       data=myDF, na.rm=T)
    
    ### ignore inf
    is.na(outDF) <- sapply(outDF, is.infinite)
    
    
    return(outDF)
    
    
}