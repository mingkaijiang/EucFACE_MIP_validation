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
    
    ### return
    return(outDF)
}