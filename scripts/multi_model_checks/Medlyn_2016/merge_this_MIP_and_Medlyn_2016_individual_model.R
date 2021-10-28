merge_this_MIP_and_Medlyn_2016_individual_model <- function(medDF, thisDF) {
    
    ### variables to select
    medDF <- medDF[,c("ModVersion", "ModName", "Trt", "CO2.mean", "PREC.mean",
                      #"CO2.sd", "PREC.sd",
                      "NEP.mean", "GPP.mean", "NPP.mean", 
                      "NEP.sd", "GPP.sd", "NPP.sd")]
    
    ### variables to select
    thisDF <- thisDF[,c("ModVersion", "ModName", "Trt", "CO2.mean", "PREC.mean",
                        #"CO2.sd", "PREC.sd",
                      "NEP.mean", "GPP.mean", "NPP.mean", 
                      "NEP.sd", "GPP.sd", "NPP.sd")]
    
    ### add MIP version
    medDF$MIP <- "Medlyn_2016"
    thisDF$MIP <- "Current"
    
    ### merge
    myDF <- rbind(medDF, thisDF)
    
    ### split
    myDF1 <- myDF[,c("MIP", "ModVersion", "ModName", "Trt", "CO2.mean", "PREC.mean",
                     "NEP.mean", "GPP.mean", "NPP.mean")]
    
    myDF2 <- myDF[,c("MIP", "ModVersion","ModName", "Trt", 
                       "NEP.sd", "GPP.sd", "NPP.sd")]
    
    ### convert from wide to long
    outDF1 <- reshape::melt(myDF1, id.vars=c("MIP", "ModVersion", "ModName", "Trt", 
                                             "CO2.mean", "PREC.mean"))
    outDF2 <- reshape::melt(myDF2, id.vars=c("MIP", "ModVersion", "ModName", "Trt"))
    
    ### revise names
    names(outDF1)[names(outDF1)=="value"] <- "meanvalue"
    names(outDF2)[names(outDF2)=="value"] <- "sdvalue"
    
    outDF1$variable <- gsub(".mean", "", outDF1$variable)
    outDF2$variable <- gsub(".sd", "", outDF2$variable)
    
    outDF <- merge(outDF1, outDF2, by=c("MIP", "ModVersion", "ModName", "Trt", "variable"))
    
    return(outDF)
}