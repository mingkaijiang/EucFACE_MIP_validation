prepare_basic_GPP_plot <- function (ambDF, eleDF) {
    
    ### Purpose: this function prepares GPP = NPP + Ra plot
    ###          for aCO2 and eCO2 treatment
    
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    
    myDF <- rbind(ambDF, eleDF)
    
    
    ### get the subsets
    subDF1 <- subset(myDF, variable%in%c("GPP"))
    
    subDF2 <- subset(myDF, variable%in%c("NPP", "RAU"))
    
    sumDF1 <- summaryBy(value.mean~ModName+Trt, data=subDF2, FUN=sum,
                       keep.names=T, na.rm=T)
    
    for (i in c("amb","ele")) {
        for (j in unique(sumDF1$ModName)) {
            sumDF1$value.sd[sumDF1$ModName==j&sumDF1$Trt==i] <- sqrt(sum(c(subDF2$value.sd[subDF2$Trt==i&subDF2$ModName==j&subDF2$variable=="NPP"],
                                                                           subDF2$value.sd[subDF2$Trt==i&subDF2$ModName==j&subDF2$variable=="RAU"]), 
                                                                         na.rm=T)/2)
        }
    }
    
    
    
    out <- list("nppraDF" = subDF2,
                "totDF" = sumDF1)
    
    return(out)
    
}