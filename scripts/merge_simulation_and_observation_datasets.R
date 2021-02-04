merge_simulation_and_observation_datasets <- function(eucDF, simDF) {
    
    ### merge the two datasets according to column names (variables) in eucDF
    eucDF$Source <- "Obs"
    simDF$Source <- "Sim"
    
    ### find common variable names
    name1 <- names(eucDF)
    name2 <- names(simDF)
    
    dup.names <- duplicates(c(name1, name2))
    
    ### subset
    eucDF2 <- eucDF[,dup.names]
    simDF2 <- simDF[,dup.names]
    
    ### merge
    outDF <- rbindlist(list(eucDF2, simDF2), fill = TRUE)    
    
    ### organize colnames
    tmp <- c("Source","Group", "Trt", dup.names)
    
    outDF1 <- outDF[,c("Source", "Group", "Trt")]
    outDF2 <- outDF
    outDF2$Source <- NULL
    outDF2$Group <- NULL
    outDF2$Trt <- NULL
    
    outDF <- cbind(outDF1, outDF2)
    
    ### return
    return(outDF)
    
    
}