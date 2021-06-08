prepare_simulation_results_for_comparison_against_data <- function(source.dir, 
                                                                   mod.abb) {
    
    ### over observed period (2012-2019)
    simDF1 <- read.csv(paste0(source.dir, "/EUC_", mod.abb, "_OBS_VAR_AMB_NOP_D.csv"))  # dry
    simDF2 <- read.csv(paste0(source.dir, "/EUC_", mod.abb, "_OBS_VAR_ELE_NOP_D.csv"))  # dry
    
    ### obtain means, sums for stocks and fluxes
    if (mod.abb == "CABLP") {
        simDF1 <- convert_into_annual_CABLP(simDF1)
        simDF2 <- convert_into_annual_CABLP(simDF2)
    } else if (mod.abb == "LPJGP") {
        simDF1 <- convert_into_annual_LPJGP(simDF1)
        simDF2 <- convert_into_annual_LPJGP(simDF2)
    } else if (mod.abb == "LPJGN") {
        simDF1 <- convert_into_annual_LPJGN(simDF1)
        simDF2 <- convert_into_annual_LPJGN(simDF2)
    } else if (mod.abb == "GDAYN") {
        simDF1 <- convert_into_annual_GDAYN(simDF1)
        simDF2 <- convert_into_annual_GDAYN(simDF2)
    } else {
        simDF1 <- convert_into_annual(simDF1)
        simDF2 <- convert_into_annual(simDF2)
    }
    
    ### calculate observed period means and sds
    simDF1 <- simDF1[simDF1$YEAR%in%c(2013, 2014, 2015, 2016),]
    simDF2 <- simDF2[simDF2$YEAR%in%c(2013, 2014, 2015, 2016),]
    
    ### get dimension
    d <- dim(simDF1)
    n <- d[2]
    
    ### calculate means and sds
    meanDF1 <- colMeans(simDF1[2:n])
    sdDF1 <- apply(simDF1[,2:n], 2, sd)
    
    meanDF2 <- colMeans(simDF2[2:n])
    sdDF2 <- apply(simDF2[,2:n], 2, sd)
    
    ### calculate CO2 response ratio
    CO2DF <- simDF1
    CO2DF[,2:n] <- (simDF2[,2:n]/simDF1[,2:n]  - 1.0) * 100
    
    ### calculate means and sds of the CO2 response signal (%)
    meanDF3 <- colMeans(CO2DF[2:n])
    sdDF3 <- apply(CO2DF[,2:n], 2, sd)
    
    ### assign information
    outDF <- data.frame(c("mean", "mean", "sd", "sd", "mean", "sd"),
                        c("aCO2", "eCO2", "aCO2", "eCO2", "pct_diff", "pct_diff"))
    colnames(outDF) <- c("Group", "Trt")
    
    outDF[outDF$Group=="mean" & outDF$Trt=="aCO2", 3:(n+1)] <- meanDF1
    
    names(outDF)[3:(n+1)] <- paste0(names(simDF1[,2:n]))
    
    outDF[outDF$Group=="mean" & outDF$Trt=="eCO2", 3:(n+1)] <- meanDF2
    outDF[outDF$Group=="sd" & outDF$Trt=="aCO2", 3:(n+1)] <- sdDF1
    outDF[outDF$Group=="sd" & outDF$Trt=="eCO2", 3:(n+1)] <- sdDF2
    outDF[outDF$Group=="mean" & outDF$Trt=="pct_diff", 3:(n+1)] <- meanDF3
    outDF[outDF$Group=="sd" & outDF$Trt=="pct_diff", 3:(n+1)] <- sdDF3
    
    return(outDF)
    
}