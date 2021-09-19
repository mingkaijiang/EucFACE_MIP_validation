calculate_LPJG_daily_N_and_P_fluxes <- function (modDF, mod.abb) {
    
    if (mod.abb == "LPJGP") {
        
        myDF <- modDF[,c("YEAR", "DOY", 
                         "NL", "NW", "NFR",
                         "NGL", "NGW", "NGFR",
                         "NLITIN", "NWLIN", "NFRLIN",
                         "NLRETR", "NWRETR", "NFRRETR",
                         "PL", "PW", "PFR",
                         "PGL", "PGW", "PGFR",
                         "PLITIN", "PWLIN", "PFRLIN",
                         "PLRETR", "PWRETR", "PFRRETR")]
        
        l <- dim(myDF)[1]
        
        for (i in 2:l) {
            myDF$deltaNL[i] <- myDF$NL[i] - myDF$NL[i-1]
            myDF$deltaNW[i] <- myDF$NW[i] - myDF$NW[i-1]
            myDF$deltaNFR[i] <- myDF$NFR[i] - myDF$NFR[i-1]
            
            myDF$deltaPL[i] <- myDF$PL[i] - myDF$PL[i-1]
            myDF$deltaPW[i] <- myDF$PW[i] - myDF$PW[i-1]
            myDF$deltaPFR[i] <- myDF$PFR[i] - myDF$PFR[i-1]
            
        }
        
    } else if (mod.abb == "LPJGN") {
        myDF <- modDF[,c("YEAR", "DOY", 
                         "NL", "NW", "NFR",
                         "NGL", "NGW", "NGFR",
                         "NLITIN", "NWLIN", "NFRLIN",
                         "NLRETR", "NWRETR", "NFRRETR")]
        
        
        for (i in 2:l) {
            myDF$deltaNL[i] <- myDF$NL[i] - myDF$NL[i-1]
            myDF$deltaNW[i] <- myDF$NW[i] - myDF$NW[i-1]
            myDF$deltaNFR[i] <- myDF$NFR[i] - myDF$NFR[i-1]
            
        }
    }
    
    return(myDF)
    
}