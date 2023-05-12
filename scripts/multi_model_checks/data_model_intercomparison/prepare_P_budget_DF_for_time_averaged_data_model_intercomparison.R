prepare_P_budget_DF_for_time_averaged_data_model_intercomparison <- function(eucDF,
                                                                             ambDF,
                                                                             eleDF,
                                                                             difDF) {
    
    ### Purpose:
    ### to prepare P budget variables
    
    ### get model list
    mod.list <- unique(ambDF$ModName)
    nmod <- length(mod.list)
    
    ### get dimensions of different variables
    var.list1 <- c("PL", "PW", "PFR", "PCR", "PSTOR", 
                  "PGL", "PGW", "PGFR", "PGCR", 
                  "PLRETR", "PWRETR", "PCRRETR", "PFRRETR",
                  "PUP", "GPP_use")
    
    var.list2 <- c("PL", "PW", "PFR", "PCR", "PVEG", 
                   "PGL", "PGW", "PGFR", "PGCR", "PGVEG", 
                   "PLRETR", "PWRETR", "PCRRETR", "PFRRETR", "PRETR", 
                   "PMINTOT", "PUP", 
                   "CPL", "CPW", "CPFR", "CPSOIL", "CPFLIT",
                   "PUE", "PMRT", "PUPREQ")
    
    nvar1 <- length(var.list1)
    nvar2 <- length(var.list2)
    
    
    ### create a DF for aCO2 and eCO2
    myDF1 <- data.frame(rep(var.list2, (1+nmod)*2), 
                        rep(c("obs", mod.list), each=(nvar2*2)), 
                        rep(c("aCO2", "eCO2"), each=nvar2), 
                        NA, NA)
    colnames(myDF1) <- c("Variable", 
                         "Group",
                         "Trt",
                         "meanvalue",
                         "sdvalue")
    
    
    ### add obs data
    for (i in c("aCO2", "eCO2")) {
        for (j in var.list2) {
            
            tryCatch({
            myDF1$meanvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable==j] <- eucDF[eucDF$Group=="mean"&eucDF$Trt==i,j]
            myDF1$sdvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable==j] <- eucDF[eucDF$Group=="sd"&eucDF$Trt==i,j]
            }, error=function(e){})
        }
        
    }
    
    ### add model output
    for (i in mod.list) {
        for (j in var.list1) {
            ### means
            myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable==j] <- ambDF[ambDF$ModName==i, paste0(j, ".mean")]
            myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable==j] <- eleDF[eleDF$ModName==i, paste0(j, ".mean")]
            
            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable==j] <- ambDF[ambDF$ModName==i, paste0(j, ".sd")]
            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable==j] <- eleDF[eleDF$ModName==i, paste0(j, ".sd")]
            
        }
    }
    
    ### convert nan to na
    myDF1$meanvalue <- ifelse(is.nan(myDF1$meanvalue), NA, myDF1$meanvalue)
    
    
    ### calculate all the totals
    for (i in unique(myDF1$Group)) {
        for (j in c("aCO2", "eCO2")) {
            
            ### PVEG
            myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PVEG"] <- sum(c(myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PL"],
                                                                                         myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PW"],
                                                                                         myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PFR"],
                                                                                         myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PCR"],
                                                                                         myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PSTOR"]), 
                                                                                       na.rm=T)
            
            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PVEG"] <- sqrt(sum(c(myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PL"]^2,
                                                                                            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PW"]^2,
                                                                                            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PFR"]^2,
                                                                                            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PCR"]^2,
                                                                                            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PSTOR"]^2), 
                                                                                          na.rm=T)/5)
            
            ### PGVEG
            myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PGVEG"] <- sum(c(myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PGL"],
                                                                                         myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PGW"],
                                                                                         myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PGFR"],
                                                                                         myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PGCR"]), 
                                                                                       na.rm=T)
            
            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PGVEG"] <- sqrt(sum(c(myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PGL"]^2,
                                                                                            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PGW"]^2,
                                                                                            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PGFR"]^2,
                                                                                            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PGCR"]^2), 
                                                                                          na.rm=T)/4)
            
            ### PRETR
            myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PRETR"] <- sum(c(myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PLRETR"],
                                                                                         myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PWRETR"],
                                                                                         myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PFRRETR"],
                                                                                         myDF1$meanvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PCRRETR"]), 
                                                                                       na.rm=T)
            
            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PRETR"] <- sqrt(sum(c(myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PLRETR"]^2,
                                                                                            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PWRETR"]^2,
                                                                                            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PFRRETR"]^2,
                                                                                            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j&myDF1$Variable=="PCRRETR"]^2), 
                                                                                          na.rm=T)/4)
        }
    }
    
    
    ### add other variables
    ## PMINTOT
    for (i in c("aCO2", "eCO2")) {
        myDF1$meanvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable=="PMINTOT"] <- eucDF$PMIN[eucDF$Group=="mean"&eucDF$Trt==i]
        myDF1$sdvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable=="PMINTOT"] <- eucDF$PMIN[eucDF$Group=="mean"&eucDF$Trt==i]
    }
    
    for (i in mod.list) {
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="PMINTOT"] <- ambDF$PMIN.mean[ambDF$ModName==i] + ambDF$PBIOCHMIN.mean[ambDF$ModName==i]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="PMINTOT"] <- eleDF$PMIN.mean[eleDF$ModName==i] + eleDF$PBIOCHMIN.mean[eleDF$ModName==i]
        
        myDF1$sdvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="PMINTOT"] <- sqrt(sum(c(ambDF$PMIN.sd[ambDF$ModName==i]^2,
                                                                                                ambDF$PMIN.sd[ambDF$ModName==i]^2), na.rm=T)/2) 
        myDF1$sdvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="PMINTOT"] <- sqrt(sum(c(eleDF$PMIN.sd[eleDF$ModName==i]^2,
                                                                                                eleDF$PMIN.sd[eleDF$ModName==i]^2), na.rm=T)/2) 
        
    }
    
    
    ## CPL, CPW, CPFR, CPSOIL, CPFLIT
    for (i in c("aCO2", "eCO2")) {
        for (j in c("CPL", "CPW", "CPFR", "CPSOIL", "CPFLIT", "PUE", "PMRT", "PUPREQ")) {
            myDF1$meanvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable==j] <- eucDF[eucDF$Group=="mean"&eucDF$Trt==i, j]
 
        }
    }
    
    for (i in mod.list) {
        ### CPL
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="CPL"] <- ambDF$CL.mean[ambDF$ModName==i] / ambDF$PL.mean[ambDF$ModName==i]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="CPL"] <- eleDF$CL.mean[eleDF$ModName==i] / eleDF$PL.mean[eleDF$ModName==i]
        
        ### CPW
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="CPW"] <- ambDF$CW.mean[ambDF$ModName==i] / ambDF$PW.mean[ambDF$ModName==i]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="CPW"] <- eleDF$CW.mean[eleDF$ModName==i] / eleDF$PW.mean[eleDF$ModName==i]
        
        ###CPFR
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="CPFR"] <- ambDF$CFR.mean[ambDF$ModName==i] / ambDF$PFR.mean[ambDF$ModName==i]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="CPFR"] <- eleDF$CFR.mean[eleDF$ModName==i] / eleDF$PFR.mean[eleDF$ModName==i]
        
        ### CPSOIL
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="CPSOIL"] <- ambDF$CSOIL.mean[ambDF$ModName==i] / ambDF$PSOIL.mean[ambDF$ModName==i]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="CPSOIL"] <- eleDF$CSOIL.mean[eleDF$ModName==i] / eleDF$PSOIL.mean[eleDF$ModName==i]
        
        ### CPFLIT
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="CPFLIT"] <- ambDF$CFLIT.mean[ambDF$ModName==i] / ambDF$PFLIT.mean[ambDF$ModName==i]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="CPFLIT"] <- eleDF$CFLIT.mean[eleDF$ModName==i] / eleDF$PFLIT.mean[eleDF$ModName==i]  
        
    }
    
    
    for (i in mod.list) {
        
        ### PUE
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="PUE"] <- ambDF$NPP.mean[ambDF$ModName==i] / myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="PUP"]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="PUE"] <- eleDF$NPP.mean[eleDF$ModName==i] / myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="PUP"]
        
        ### PMRT
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="PMRT"] <- myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="PVEG"] / myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="PUP"]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="PMRT"] <- myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="PVEG"] / myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="PUP"]
        
        
        ### PUPREQ
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="PUPREQ"] <- myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="PUP"] / myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="PGVEG"]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="PUPREQ"] <- myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="PUP"] / myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="PGVEG"]
        
    }
    
    
    
    
    ### get the diff and % diff DF
    myDF2 <- data.frame(rep(var.list2, (1+nmod)*2), 
                        rep(c("obs", mod.list), each=(nvar2*2)), 
                        rep(c("diff", "pct_diff"), each=nvar2), 
                        NA, NA)
    colnames(myDF2) <- c("Variable", 
                         "Group",
                         "Trt",
                         "meanvalue",
                         "sdvalue")
    
    ### add obs data
    for (i in unique(myDF1$Group)) {
        for (j in var.list2) {
            tryCatch({
                myDF2$meanvalue[myDF2$Group==i&myDF2$Trt=="diff"&myDF2$Variable==j] <- myDF1$meanvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="eCO2"] - myDF1$meanvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="aCO2"]
                myDF2$meanvalue[myDF2$Group==i&myDF2$Trt=="pct_diff"&myDF2$Variable==j] <- myDF2$meanvalue[myDF2$Group==i&myDF2$Trt=="diff"&myDF2$Variable==j]/myDF1$meanvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="aCO2"] * 100.0
                
                myDF2$sdvalue[myDF2$Group==i&myDF2$Trt=="diff"&myDF2$Variable==j] <- sqrt(sum(c(myDF1$sdvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="eCO2"]^2,
                                                                                                myDF1$sdvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="aCO2"]^2), na.rm=T)/2)
                
                myDF2$sdvalue[myDF2$Group==i&myDF2$Trt=="pct_diff"&myDF2$Variable==j] <- sqrt(sum(c(myDF1$sdvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="eCO2"]^2,
                                                                                                    myDF1$sdvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="aCO2"]^2,
                                                                                                    myDF1$sdvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="aCO2"]^2), na.rm=T)/3)/myDF1$meanvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="aCO2"]*100
                
                
            }, error=function(e){})
        }
    }    
    
    
    
    ### convert nan to na
    myDF2$meanvalue <- ifelse(is.nan(myDF2$meanvalue), NA, myDF2$meanvalue)
    
    
    ### return
    outDF <- rbind(myDF1, myDF2)
    

    return(outDF)
}
