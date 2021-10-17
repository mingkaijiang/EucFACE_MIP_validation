compile_cumulative_water_budget <- function (ambDF, eleDF) {
    
    ### select only water variables
    ambDF <- ambDF[,c("ModName", "YEAR", "PREC", "ET",
                      "TRANS", "ES", "EC", "RO", "DRAIN", #"SW",
                      "deltaSW")]
    
    eleDF <- eleDF[,c("ModName", "YEAR", "PREC", "ET",
                      "TRANS", "ES", "EC", "RO", "DRAIN", #"SW",
                      "deltaSW")]
    
    ### calculate all year totals for all water fluxes
    myDF1 <- summaryBy(PREC+ET+TRANS+ES+EC+RO+DRAIN+deltaSW~ModName, 
                       FUN=sum, data=ambDF, keep.names=T, na.rm=T)
    
    myDF2 <- summaryBy(PREC+ET+TRANS+ES+EC+RO+DRAIN+deltaSW~ModName, 
                       FUN=sum, data=eleDF, keep.names=T, na.rm=T)
    
    ### assign variable
    myDF1$Trt <- "amb"
    myDF2$Trt <- "ele"
    
    myDF <- rbind(myDF1, myDF2)
    
    ### ignore several variables
    myDF <- myDF[,c("ModName", "Trt", "PREC", "ET",
                    "RO", "DRAIN", "deltaSW")]
    
    ### convert from wide to long
    outDF <- reshape2::melt(myDF, id.vars = c("ModName", "Trt"))
    
    
    ### ignore PREC under eCO2, because it is the same as aCO2
    outDF <- outDF[outDF$Trt!="ele"|outDF$variable!="PREC",]
    
    ### Label groups
    outDF$Group <- "1_PREC"
    outDF$Group[outDF$Trt=="amb"&outDF$variable%in%c("ET","RO","DRAIN","deltaSW")] <- "2_amb"
    outDF$Group[outDF$Trt=="ele"&outDF$variable%in%c("ET","RO","DRAIN","deltaSW")] <- "3_ele"
    
    return(outDF)
    
}