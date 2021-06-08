convert_into_annual_LPJGN <- function (inDF) {
    
    ### sum fluxes
    ### summarize all fluxes first to obain annual rate
    fluxDF <- summaryBy(PREC+ET+TRANS+ES+EC+RO+DRAIN+NEP+GPP+NPP+
                            GPPno+GPPns+CLEST+CWEST+CFREST+CDEBTEST+         ### model specific variables
                            #CWLINDEBT+
                            RHET+RAU+RECO+CGL+CGFR+CGCR+CGW+
                            NGL+NGFR+NGCR+NGW+
                            NUP+NGMIN+NMIN+NLEACH+NLRETR+PLRETR+RCR+RFR+CREPR+CEX+CVOC+
                            RL+RW+RGR+CLITIN+CCRLIN+CFRLIN+CWLIN+NLITIN+
                            NCRLIN+NFRLIN+NWLIN+
                            NWRETR+PWRETR+NCRRETR+NFRRETR+
                            CLEACH+NNEP+                                     ### Model specific variables
                            NDEP+NFIX+NVOL~YEAR, 
                        data=inDF, FUN=sum, keep.names=T, na.rm=T)
    
    
    ### subset first day within a year of all pools
    poolDF <- inDF[,c("YEAR", "DOY", "SW","CL","LAI","CW","CFR","CCR","NL","NW","NFR","NCR","CSTOR","NSTOR",
                       "CSOIL","NSOIL","NPMIN",
                       "CFLIT","CFLITA","CFLITB",
                       "NFLITA","NFLITB","CCLITB","NCLITB","NFLIT","NPORG",  
                       "MAXNSTORE", "SWtot", "SWPAtot", "CSOILtot", "NSOILtot", "NPMINtot",          ## model specific output
                       "NPORGtot","CDEBT", "CEXCESS", "CWLINDEBT",                                   ## model specific output
                       "PHENL", "PHENFR")]     
    
    ### calculate change in pools for mass balance
    deltaDF <- subset(poolDF, DOY==1)
    
    l <- dim(deltaDF)[2]
    
    ### get year
    yr.start <- min(unique(poolDF$YEAR))
    yr.end <- max(unique(poolDF$YEAR))
    
    for (i in c(yr.start:yr.end)) {
        deltaDF[deltaDF$YEAR==i,3:l] <- poolDF[poolDF$YEAR==i&poolDF$DOY==365,3:l]-poolDF[poolDF$YEAR==i&poolDF$DOY==1,3:l]
    }
    
    ### add delta column name to deltaDF
    names(deltaDF)[3:l] <- paste0("delta", names(deltaDF[3:l]))
    
    ### poolDF
    poolDF <- subset(poolDF, DOY==1)
    poolDF$DOY <- NULL
    deltaDF$DOY <- NULL
    
    
    ### merge all dataframe together
    annDF <- merge(fluxDF, poolDF, by="YEAR")
    annDF <- merge(annDF, deltaDF, by="YEAR", all.x=T)
    
    ### remve redundant variables
    annDF[annDF<=-9999.] <- NA
    
    return(annDF)
    
}