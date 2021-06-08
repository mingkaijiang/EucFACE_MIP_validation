convert_into_annual <- function (inDF) {
    
    ### sum fluxes
    fluxDF <- summaryBy(PREC+ET+TRANS+ES+EC+RO+DRAIN+NEP+GPP+NPP+RHET+RAU+RECO+CGL+CGFR+CGCR+CGW+NGL+NGFR+NGCR+NGW+PGL+PGFR+PGCR+PGW+NUP+NGMIN+NMIN+NLEACH+PUP+PGMIN+PMIN+PLEACH+PBIOCHMIN+NLRETR+PLRETR+RCR+RFR+CREPR+CEX+CVOC+RL+RW+RGR+CLITIN+CCRLIN+CFRLIN+CWLIN+NLITIN+NCRLIN+NFRLIN+NWLIN+PLITIN+PCRLIN+PFRLIN+PWLIN+NWRETR+PWRETR+NCRRETR+PCRRETR+NFRRETR+PFRRETR+NDEP+NFIX+NVOL+PDEP+PWEA~YEAR, 
                        data=inDF, FUN=sum, keep.names=T, na.rm=T)
    
    
    ### subset first day within a year of all pools
    poolDF <- inDF[,c("YEAR", "DOY", "SW", "CL","LAI","CW","CFR","CCR","NL","NW","NFR","NCR","PL","PW","PFR","PCR","CSTOR","NSTOR","PSTOR",
                       "CSOIL","NSOIL","PSOIL","NPMIN","PPMIN","PLAB","PSEC","POCC","PPAR","CFLIT","CFLITA","CFLITB",
                       "NFLITA","NFLITB","PFLITA","PFLITB","CCLITB","NCLITB","PCLITB","NFLIT","PFLIT", "NPORG", "PPORG")]
    
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