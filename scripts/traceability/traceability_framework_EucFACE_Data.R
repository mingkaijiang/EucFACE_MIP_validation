traceability_framework_EucFACE_data <- function(NPP,
                                                coefDF) {
    
    #### data below is based on Xia et al. 2013 Supplementary Materials
    #### based on Table S5. PFT evergreen broadleaf forest
    
    ### A matrix: Carbon transfer matrix among pools
    A2 <- diag(8)
    A2[5,1] <- -1.0
    A2[6,3] <- -1.0
    A2[7,4] <- -(coefDF$frac.myco)
    A2[7,5] <- -(coefDF$frac.ag.lit)
    A2[7,6] <- -(coefDF$frac.bg.lit)
    A2[8,7] <- -(coefDF$frac.micr)
    
    ### B vector: partitioning coefficients of NPP into different plant pools
    B2 <- t(matrix(c(coefDF$alloc.leaf, 
                     coefDF$alloc.wood, 
                     coefDF$alloc.froot,
                     coefDF$alloc.myco, 
                     0, 
                     0, 
                     0, 
                     0), nrow=1))
    
    ### C: diagonal matrix, quantifies the fraction of carbon left from pool X after each time step. 
    C2 <- diag(c(coefDF$tau.leaf, 
                 coefDF$tau.wood, 
                 coefDF$tau.froot, 
                 coefDF$tau.myco, 
                 coefDF$tau.ag.lit, 
                 coefDF$tau.bg.lit, 
                 coefDF$tau.micr, 
                 coefDF$tau.soil))
    
    ### E: diagonal matrix, environmental scalar matrix that 
    ###    quantifies the environmental scalar on carbon decay rate of pool X at each time step
    E2 <- diag(c(1.0, 
                 1.0, 
                 1.0, 
                 1.0, 
                 1.0, 
                 1.0, 
                 1.0, 
                 1.0))
    
    
    ### ecosystem C residence time, = C-1 * A-1 * B
    tauE_t <- solve(C2) %*% solve(A2) %*% B2
    
    ### E2-1 * tauE_t
    tauE <- solve(E2) %*% tauE_t
    
    ### U: NPP input at ambient CO2
    U <- NPP
    
    ### ecosystem carbon storage capacity
    Xss <- tauE * U
    
    ### total ecosystem carbon storage capacity (kg m-2)
    tot_C <- round(sum(Xss) / 1000,2)
    print(paste0("C storage = ", tot_C, " kg m-2" ))
    
    tot_tau <- round(sum(tauE),2)
    print(paste0("C residence time = ", tot_tau, " yr" ))
    
    return(cbind(tot_C, tot_tau))
    
}