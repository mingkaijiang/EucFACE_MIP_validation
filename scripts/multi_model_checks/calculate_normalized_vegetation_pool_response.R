calculate_normalized_vegetation_pool_response <- function(inDF, pcycle) {
    
    ### treatment
    trt <- c("amb", "ele")
    
    ### mod
    modlist <- unique(inDF$ModName)
    
    ### year start and end
    yr <- range(inDF$YEAR)
    
    ### outDF - ignore soil for now
    if (pcycle == F) {
        outDF <- data.frame("ModName"=rep(modlist, each=2),
                            "Trt"=rep(trt),
                            "CL"=NA,"CW"=NA,"CFR"=NA,"CCR"=NA,"CSTOR"=NA,
                            "NL"=NA,"NW"=NA,"NFR"=NA,"NCR"=NA,"NSTOR"=NA,
                            "CVEG"=NA, "NVEG"=NA)
        
        ## intermediate DF
        tmpDF <- outDF
        
        ### calculate the difference
        for (i in trt) {
            for (j in modlist) {
                tmpDF$CL[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$CL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$CL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$CW[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$CW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$CW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$CFR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$CFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$CFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$CCR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$CCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$CCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$CSTOR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$CSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$CSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                
                tmpDF$NL[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$NL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$NL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$NW[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$NW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$NW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$NFR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$NFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$NFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$NCR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$NCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$NCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$NSTOR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$NSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$NSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                
            }
        }
        
        ### calculate veg total
        tmpDF$CVEG <- rowSums(tmpDF[,c("CL","CW","CFR","CCR","CSTOR")], na.rm=T)
        tmpDF$NVEG <- rowSums(tmpDF[,c("NL","NW","NFR","NCR","NSTOR")], na.rm=T)
        
        
        ### calculate pct 
        for (i in trt) {
            for (j in modlist) {
                outDF$CL[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CL[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$CL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$CW[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CW[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$CW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$CFR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CFR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$CFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$CCR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CCR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$CCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$CSTOR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CSTOR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$CSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                
                outDF$CVEG[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CVEG[tmpDF$Trt==i&tmpDF$ModName==j] / sum(inDF$CL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                              inDF$CW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                              inDF$CFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                              inDF$CCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]], 
                                                                                                              inDF$CSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]], 
                                                                                                             na.rm=T)
                
                outDF$NL[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NL[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$NL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$NW[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NW[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$NW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$NFR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NFR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$NFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$NCR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NCR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$NCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$NSTOR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NSTOR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$NSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                
                outDF$NVEG[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NVEG[tmpDF$Trt==i&tmpDF$ModName==j] / sum(inDF$NL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                              inDF$NW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                              inDF$NFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                              inDF$NCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                              inDF$NSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],na.rm=T)
            
            } # j
        } # i
        
    } else {
        outDF <- data.frame("ModName"=rep(modlist, each=2),
                            "Trt"=rep(trt),
                            "CL"=NA,"CW"=NA,"CFR"=NA,"CCR"=NA,"CSTOR"=NA,
                            "NL"=NA,"NW"=NA,"NFR"=NA,"NCR"=NA,"NSTOR"=NA,
                            "PL"=NA,"PW"=NA,"PFR"=NA,"PCR"=NA,"PSTOR"=NA,
                            "CVEG"=NA, "NVEG"=NA, "PVEG"=NA)
        
        
        ## intermediate DF
        tmpDF <- outDF
        
        ### calculate the difference
        for (i in trt) {
            for (j in modlist) {
                tmpDF$CL[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$CL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$CL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$CW[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$CW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$CW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$CFR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$CFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$CFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$CCR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$CCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$CCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$CSTOR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$CSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$CSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                
                tmpDF$NL[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$NL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$NL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$NW[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$NW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$NW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$NFR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$NFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$NFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$NCR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$NCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$NCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$NSTOR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$NSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$NSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                
                tmpDF$PL[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$PL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$PL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$PW[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$PW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$PW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$PFR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$PFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$PFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$PCR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$PCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$PCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                tmpDF$PSTOR[tmpDF$Trt==i&tmpDF$ModName==j] <- inDF$PSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[2]] - inDF$PSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                
            }
        }
        
        ### calculate veg total
        tmpDF$CVEG <- rowSums(tmpDF[,c("CL","CW","CFR","CCR","CSTOR")], na.rm=T)
        tmpDF$NVEG <- rowSums(tmpDF[,c("NL","NW","NFR","NCR","NSTOR")], na.rm=T)
        tmpDF$PVEG <- rowSums(tmpDF[,c("PL","PW","PFR","PCR","PSTOR")], na.rm=T)
        
        
        ### calculate pct 
        for (i in trt) {
            for (j in modlist) {
                outDF$CL[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CL[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$CL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$CW[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CW[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$CW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$CFR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CFR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$CFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$CCR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CCR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$CCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$CSTOR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CSTOR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$CSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$CVEG[outDF$Trt==i&outDF$ModName==j] <- tmpDF$CVEG[tmpDF$Trt==i&tmpDF$ModName==j] / sum(inDF$CL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                             inDF$CW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                             inDF$CFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                             inDF$CCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                             inDF$CSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],na.rm=T)
                
                outDF$NL[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NL[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$NL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$NW[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NW[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$NW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$NFR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NFR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$NFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$NCR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NCR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$NCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$NSTOR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NSTOR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$NSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$NVEG[outDF$Trt==i&outDF$ModName==j] <- tmpDF$NVEG[tmpDF$Trt==i&tmpDF$ModName==j] / sum(inDF$NL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                             inDF$NW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                             inDF$NFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                             inDF$NSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]], na.rm=T)
                
                outDF$PL[outDF$Trt==i&outDF$ModName==j] <- tmpDF$PL[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$PL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$PW[outDF$Trt==i&outDF$ModName==j] <- tmpDF$PW[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$PW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$PFR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$PFR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$PFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$PCR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$PCR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$PCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                outDF$PSTOR[outDF$Trt==i&outDF$ModName==j] <- tmpDF$PSTOR[tmpDF$Trt==i&tmpDF$ModName==j] / inDF$PSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]]
                
                outDF$PVEG[outDF$Trt==i&outDF$ModName==j] <- tmpDF$PVEG[tmpDF$Trt==i&tmpDF$ModName==j] / sum(inDF$PL[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                             inDF$PW[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                             inDF$PFR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]],
                                                                                                             inDF$PCR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]], 
                                                                                                             inDF$PSTOR[inDF$Trt==i&inDF$ModName==j&inDF$YEAR==yr[1]], na.rm=T)
                
            } # j
        } # i
    }
    
    ### return data list
    out <- list(absDF=tmpDF, pctDF=outDF)
    
    return(out)
    
}
