prepare_coef_for_traceability_treatment_mean <- function() {
    inDF <- read.csv("output/set_aside_runs/parameter_summary_table.csv")
    
    inDF$tau.ag.lit[1] <- 365 * mean(c(0.0059, 0.0035, 0.0072))
    inDF$tau.ag.lit[2] <- 365 * mean(c(0.0056, 0.0079, 0.0083))
    
    inDF$tau.wood[1] <- 107 / (4966+777)
    inDF$tau.wood[2] <- 118 / (5091+821)
    
    return(inDF)
}