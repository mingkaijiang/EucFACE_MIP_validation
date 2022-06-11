merge_this_MIP_and_Medlyn_2016 <- function(medDF, thisDF) {
    
    ### variables to select
    medDF <- medDF[,c("ModVersion", "Trt", "CO2.mean", "PREC.mean",
                      #"CO2.sd", "PREC.sd",
                      "NEP.mean", "GPP.mean", "NPP.mean", 
                      "NEP.sd", "GPP.sd", "NPP.sd",
                      "LAI.mean", "LAI.sd",
                      "deltaCL.mean","deltaCL.sd",
                      "deltaCW.mean","deltaCW.sd",
                      "deltaCFR.mean","deltaCFR.sd",
                      "deltaCCR.mean","deltaCCR.sd",
                      "deltaCSTOR.mean","deltaCSTOR.sd",
                      "deltaCSOIL.mean","deltaCSOIL.sd")]
    
    ### variables to select
    thisDF <- thisDF[,c("ModVersion", "Trt", "CO2.mean", "PREC.mean",
                        #"CO2.sd", "PREC.sd",
                      "NEP.mean", "GPP.mean", "NPP.mean", 
                      "NEP.sd", "GPP.sd", "NPP.sd",
                      "LAI.mean", "LAI.sd",
                      "deltaCL.mean","deltaCL.sd",
                      "deltaCW.mean","deltaCW.sd",
                      "deltaCFR.mean","deltaCFR.sd",
                      "deltaCCR.mean","deltaCCR.sd",
                      "deltaCSTOR.mean","deltaCSTOR.sd",
                      "deltaCSOIL.mean","deltaCSOIL.sd")]
    
    ### add MIP version
    medDF$MIP <- "Medlyn_2016"
    thisDF$MIP <- "Current"
    
    ### merge
    myDF <- rbind(medDF, thisDF)
    
    ### calculate delta C
    myDF$deltaC.mean <- rowSums(data.frame(myDF$deltaCL.mean,myDF$deltaCW.mean,
                                           myDF$deltaCFR.mean,myDF$deltaCCR.mean),
                                           #myDF$deltaCSTOR.mean),#,myDF$deltaCSOIL.mean),
                                na.rm=T)
    
    
    myDF$deltaC.sd <- sqrt(rowSums(data.frame(myDF$deltaCL.sd^2,myDF$deltaCW.sd^2,
                                         myDF$deltaCFR.sd^2,myDF$deltaCCR.sd^2),
                                         #myDF$deltaCSTOR.sd^2),#,myDF$deltaCSOIL.sd^2),
                                   na.rm=T)/4)
    
    ### split
    myDF1 <- myDF[,c("MIP", "ModVersion", "Trt", "CO2.mean", "PREC.mean",
                     "NEP.mean", "GPP.mean", "NPP.mean","LAI.mean", "deltaC.mean")]
    
    myDF2 <- myDF[,c("MIP", "ModVersion", "Trt", 
                       "NEP.sd", "GPP.sd", "NPP.sd", "LAI.sd", "deltaC.sd")]
    
    ### convert from wide to long
    outDF1 <- reshape::melt(myDF1, id.vars=c("MIP", "ModVersion", "Trt", 
                                             "CO2.mean", "PREC.mean"))
    outDF2 <- reshape::melt(myDF2, id.vars=c("MIP", "ModVersion", "Trt"))
    
    ### revise names
    names(outDF1)[names(outDF1)=="value"] <- "meanvalue"
    names(outDF2)[names(outDF2)=="value"] <- "sdvalue"
    
    outDF1$variable <- gsub(".mean", "", outDF1$variable)
    outDF2$variable <- gsub(".sd", "", outDF2$variable)
    
    outDF <- merge(outDF1, outDF2, by=c("MIP", "ModVersion", "Trt", "variable"))
    
    return(outDF)
}