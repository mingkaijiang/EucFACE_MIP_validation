prepare_basic_GPP_plot <- function (ambDF, eleDF) {
    
    ### Purpose: this function prepares GPP = NPP + Ra plot
    ###          for aCO2 and eCO2 treatment
    
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    
    myDF <- rbind(ambDF, eleDF)
    
    
    ### get the subsets
    subDF1 <- subset(myDF, variable%in%c("GPP"))
    
    subDF2 <- subset(myDF, variable%in%c("NPP", "RAU"))
    
    ### delta C pools to be added later
    subDF3 <- subset(myDF, variable%in%c("RAU", "RHET"))
    
    
    ### calculate NPP + RAU sums
    sumDF1 <- summaryBy(value.mean~ModName+Trt, data=subDF2, FUN=sum,
                       keep.names=T, na.rm=T)
    
    for (i in c("amb","ele")) {
        for (j in unique(sumDF1$ModName)) {
            sumDF1$value.sd[sumDF1$ModName==j&sumDF1$Trt==i] <- sqrt(sum(c(subDF2$value.sd[subDF2$Trt==i&subDF2$ModName==j&subDF2$variable=="NPP"]^2,
                                                                           subDF2$value.sd[subDF2$Trt==i&subDF2$ModName==j&subDF2$variable=="RAU"]^2), 
                                                                         na.rm=T)/2)
        }
    }
    
    
    ### process delta Cveg (including litter) and delta Csoil
    tmpDF1 <- subset(myDF, variable%in%c("deltaCL", "deltaCW", "deltaCFR",
                                         "deltaCCR", "deltaCSOIL", "deltaCFLIT", "deltaCSTOR"))
    
    tmpDF1$variable2[tmpDF1$variable%in%c("deltaCL", "deltaCW", "deltaCFR",
                                          "deltaCCR", "deltaCFLIT", "deltaCSTOR")] <- "deltaCVEG"
    
    tmpDF1$variable2[tmpDF1$variable%in%c("deltaCSOIL")] <- "deltaCSOIL"
    
    ### calculate means and sd of the new variable
    sumDF2 <- summaryBy(value.mean~ModName+Trt+variable2, data=tmpDF1, FUN=sum,
                        keep.names=T, na.rm=T)
    
    for (i in c("amb","ele")) {
        for (j in unique(sumDF2$ModName)) {
            sumDF2$value.sd[sumDF2$ModName==j&sumDF2$Trt==i&sumDF2$variable2=="deltaCVEG"] <- sqrt(sum(c(tmpDF1$value.sd[tmpDF1$Trt==i&tmpDF1$ModName==j&tmpDF1$variable=="deltaCL"]^2,
                                                                                                         tmpDF1$value.sd[tmpDF1$Trt==i&tmpDF1$ModName==j&tmpDF1$variable=="deltaCW"]^2,
                                                                                                         tmpDF1$value.sd[tmpDF1$Trt==i&tmpDF1$ModName==j&tmpDF1$variable=="deltaCFR"]^2,
                                                                                                         tmpDF1$value.sd[tmpDF1$Trt==i&tmpDF1$ModName==j&tmpDF1$variable=="deltaCCR"]^2,
                                                                                                         tmpDF1$value.sd[tmpDF1$Trt==i&tmpDF1$ModName==j&tmpDF1$variable=="deltaCSTOR"]^2,
                                                                                                         tmpDF1$value.sd[tmpDF1$Trt==i&tmpDF1$ModName==j&tmpDF1$variable=="deltaCFLIT"]^2), 
                                                                                                       na.rm=T)/6)
            
            sumDF2$value.sd[sumDF2$ModName==j&sumDF2$Trt==i&sumDF2$variable2=="deltaCSOIL"] <- tmpDF1$value.sd[tmpDF1$Trt==i&tmpDF1$ModName==j&tmpDF1$variable=="deltaCSOIL"]
        }
    }
    
    sumDF2 <- sumDF2[,c("ModName", "variable2", "value.mean", "value.sd", "Trt")]
    names(sumDF2)[names(sumDF2)=="variable2"] <- "variable"
    
    subDF3 <- rbind(subDF3, sumDF2)
    
    ### calculate total GPP inferred from subDF3
    totDF1 <- summaryBy(value.mean~ModName+Trt, FUN=sum, data=subDF3,
                        na.rm=T, keep.names=T)
    
    
    for (i in c("amb","ele")) {
        for (j in unique(totDF1$ModName)) {
            totDF1$value.sd[totDF1$ModName==j&totDF1$Trt==i] <- sqrt(sum(c(subDF3$value.sd[subDF3$Trt==i&subDF3$ModName==j&subDF3$variable=="RHET"]^2,
                                                                           subDF3$value.sd[subDF3$Trt==i&subDF3$ModName==j&subDF3$variable=="RAU"]^2,
                                                                           subDF3$value.sd[subDF3$Trt==i&subDF3$ModName==j&subDF3$variable=="deltaCVEG"]^2,
                                                                           subDF3$value.sd[subDF3$Trt==i&subDF3$ModName==j&subDF3$variable=="deltaCSOIL"]^2), 
                                                                         na.rm=T)/4)
        }
    }
    
    
    ### prepare outDF - means and CIs of the total
    ciDF1 <- subDF1[,c("ModName", "Trt", "value.mean", "value.sd")]
    ciDF2 <- sumDF1[,c("ModName", "Trt", "value.mean", "value.sd")]
    ciDF3 <- totDF1[,c("ModName", "Trt", "value.mean", "value.sd")]
    
    ciDF1$Method <- "GPP"
    ciDF2$Method <- "NPP+RAU"
    ciDF3$Method <- "R+deltaC"
    
    ciDF <- rbind(ciDF1, rbind(ciDF2, ciDF3))
    
    ### prepare outDF - stacked of individual fluxes
    stackDF1 <- subDF1[,c("ModName", "Trt", "variable", "value.mean", "value.sd")]
    stackDF2 <- subDF2[,c("ModName", "Trt", "variable", "value.mean", "value.sd")]
    stackDF3 <- subDF3[,c("ModName", "Trt", "variable", "value.mean", "value.sd")]
    
    
    stackDF1$Method <- "GPP"
    stackDF2$Method <- "NPP+RAU"
    stackDF3$Method <- "R+deltaC"
    
    stackDF <- rbind(stackDF1, rbind(stackDF2, stackDF3))
    
    
    
    ### out list
    out <- list("ciDF" = ciDF,
                "stackDF" = stackDF)
    
    return(out)
    
}