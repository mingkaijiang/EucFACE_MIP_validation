prepare_coef_for_traceability <- function() {
    
    r2DF <- colMeans(pChain_aCO2_1[,1:no.var])
    r3DF <- colMeans(pChain_aCO2_2[,1:no.var])
    r6DF <- colMeans(pChain_aCO2_3[,1:no.var])
    
    r1DF <- colMeans(pChain_eCO2_1[,1:no.var])
    r4DF <- colMeans(pChain_eCO2_2[,1:no.var])
    r5DF <- colMeans(pChain_eCO2_3[,1:no.var])
    
    outDF <- rbind(r2DF, r3DF, r6DF, r1DF, r4DF, r5DF)
    outDF <- as.data.frame(outDF)
    
    outDF$tau.ag.lit <- 365 * c(0.0059, 0.0035, 0.0072, 
                                0.0056, 0.0079, 0.0083)

    outDF$tau.wood <- c(rep(107 / (4966 + 777), 3), 
                        rep(118 / (5091 + 821), 3))
    
    return(outDF)
}