traceability_framework_CABLE <- function(NPP,
                                         CN.couple) {
    
    #### data below is based on Xia et al. 2013 Supplementary Materials
    #### based on Table S5. PFT evergreen broadleaf forest
    
    if (CN.couple == "C only") {
        
        print("C only model")
        
        ### A matrix: Carbon transfer matrix among pools
        A2 <- diag(9)
        A2[4,1] <- -0.67
        A2[5,1] <- -0.33
        A2[4,2] <- -0.58
        A2[5,2] <- -0.42
        A2[6,3] <- -1.0
        A2[7,4] <- -0.45
        A2[7,5] <- -0.36
        A2[8,5] <- -0.14
        A2[7,6] <- -0.24
        A2[8,6] <- -0.28
        A2[8,7] <- -0.39
        A2[9,7] <- -0.006
        A2[9,8] <- -0.003
        
        
        ### B vector: partitioning coefficients of NPP into different plant pools
        B2 <- t(matrix(c(0.249, 0.551, 0.2, 0, 0, 0, 0, 0, 0), nrow=1))
        
        ### C: diagonal matrix, quantifies the fraction of carbon left from pool X after each time step. 
        C2 <- diag(c(1.12, 0.1, 0.025, 10, 0.95, 0.49, 1.97, 0.108, 0.0024))
        
        ### E: diagonal matrix, environmental scalar matrix that 
        ###    quantifies the environmental scalar on carbon decay rate of pool X at each time step
        E2 <- diag(c(1.01, 1, 1, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4))
        
        
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
        
    } else if (CN.couple == "CN model") {
        print("CN model")
        
        
        ### A matrix: Carbon transfer matrix among pools
        A2 <- diag(9)
        A2[4,1] <- -0.69
        A2[5,1] <- -0.31
        A2[4,2] <- -0.6
        A2[5,2] <- -0.4
        A2[6,3] <- -1.0
        A2[7,4] <- -0.45
        A2[7,5] <- -0.36
        A2[8,5] <- -0.14
        A2[7,6] <- -0.24
        A2[8,6] <- -0.28
        A2[8,7] <- -0.39
        A2[9,7] <- -0.006
        A2[9,8] <- -0.003
        
        
        ### B vector: partitioning coefficients of NPP into different plant pools
        B2 <- t(matrix(c(0.249, 0.551, 0.2, 0, 0, 0, 0, 0, 0), nrow=1))
        
        ### C: diagonal matrix, quantifies the fraction of carbon left from pool X after each time step. 
        C2 <- diag(c(1.12, 0.1, 0.025, 10, 0.95, 0.49, 2.19, 0.12, 0.0027))
        
        ### E: diagonal matrix, environmental scalar matrix that 
        ###    quantifies the environmental scalar on carbon decay rate of pool X at each time step
        E2 <- diag(c(1.01, 1, 1, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4))
        
        
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
        
    }
    
    return(cbind(tot_C, tot_tau))
    
}