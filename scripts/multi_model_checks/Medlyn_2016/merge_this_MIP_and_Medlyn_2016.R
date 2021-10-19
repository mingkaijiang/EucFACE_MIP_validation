merge_this_MIP_and_Medlyn_2016 <- function(medDF, thisDF) {
    
    ### variables to select
    medDF <- medDF[,c("ModVersion", "Trt", "CO2.mean", "PREC.mean",
                      "CO2.sd", "PREC.sd",
                      "NEP.mean", "GPP.mean", "NPP.mean", 
                      "NEP.sd", "GPP.sd", "NPP.sd")]
    
    ### variables to select
    thisDF <- thisDF[,c("ModVersion", "Trt", "CO2.mean", "PREC.mean",
                        "CO2.sd", "PREC.sd",
                      "NEP.mean", "GPP.mean", "NPP.mean", 
                      "NEP.sd", "GPP.sd", "NPP.sd")]
    
    ### add MIP version
    medDF$MIP <- "Medlyn_2016"
    thisDF$MIP <- "Current"
    
    ### merge
    myDF <- rbind(medDF, thisDF)
    
    ### split
    myDF1 <- myDF[,c("ModVersion", "Trt", "CO2.mean", "PREC.mean",
                     "NEP.mean", "GPP.mean", "NPP.mean")]
    
    myDF2 <- myDF[,c("ModVersion", "Trt", "CO2.sd", "PREC.sd",
                       "NEP.sd", "GPP.sd", "NPP.sd")]
    
    ### convert from wide to long
    
    
    
}