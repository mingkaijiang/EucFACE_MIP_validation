read_and_process_EucFACE_P_budget_output <- function() {
    
    ### read all data
    concDF <- read.csv("validation_dataset/EucFACE_P_Budget/summary_table_P_concentration_unnormalized.csv")
    fluxDF <- read.csv("validation_dataset/EucFACE_P_Budget/summary_table_P_flux_unnormalized.csv")
    poolDF <- read.csv("validation_dataset/EucFACE_P_Budget/summary_table_P_pool_unnormalized.csv")
    budgetDF <- read.csv("validation_dataset/EucFACE_P_Budget/total_p_budget_unnormalized.csv")
    cpDF <- read.csv("validation_dataset/EucFACE_P_Budget/summary_cp_ratios.csv")
    
    inoutDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/inout.csv")
    nppDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/npp.csv")
    
    tmpDF <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/NEP_normalized_method_comparison.csv")
    
    nppDF[nppDF$term=="Mycorrhizal production", 2:9] <- tmpDF[tmpDF$Category=="Figure3inset"&tmpDF$Method=="NPP-Rh",3:10]-tmpDF[tmpDF$Category=="Figure3"&tmpDF$Method=="NPP-Rh",3:10]
    
    
    ### cut out fineroot that is assumed to be understorey
    ## fluxes
    fluxDF[fluxDF$terms%in%c("Fine Root P flux", "Fineroot retrans P flux",
                             "Fineroot Litter P flux"), 2:9] <- fluxDF[fluxDF$terms%in%c("Fine Root P flux", 
                                                                                         "Fineroot retrans P flux",
                                                                                         "Fineroot Litter P flux"), 2:9] * 0.5
    
    fluxDF[fluxDF$terms=="Total vegetation production P flux",2:9] <- fluxDF[fluxDF$terms=="Canopy P flux",2:9] +
        fluxDF[fluxDF$terms=="Wood P flux",2:9] +
        fluxDF[fluxDF$terms=="Fine Root P flux",2:9] +
        fluxDF[fluxDF$terms=="Coarse Root P flux",2:9] +
        fluxDF[fluxDF$terms=="Twig litter P flux",2:9] +
        fluxDF[fluxDF$terms=="Bark litter P flux",2:9] +
        fluxDF[fluxDF$terms=="Seed litter P flux",2:9]
    
    fluxDF[fluxDF$terms=="Total vegetation retranslocation P flux",2:9] <- fluxDF[fluxDF$terms=="Canopy retrans P flux",2:9] +
        fluxDF[fluxDF$terms=="Sapwood retrans P flux",2:9] +
        fluxDF[fluxDF$terms=="Fineroot retrans P flux",2:9] +
        fluxDF[fluxDF$terms=="Coarseroot retrans P flux",2:9] 
    
    fluxDF[fluxDF$terms=="Total vegetation uptake P flux",2:9] <- fluxDF[fluxDF$terms=="Total vegetation production P flux",2:9] -
        fluxDF[fluxDF$terms=="Total vegetation retranslocation P flux",2:9]
    
    
    fluxDF$diff <- fluxDF$eCO2 - fluxDF$aCO2
    fluxDF$percent_diff <- (fluxDF$eCO2 - fluxDF$aCO2)/fluxDF$aCO2 * 100
    
    fluxDF$aCO2_sd <- matrixStats::rowSds(as.matrix(subset(fluxDF, select=c(R2, R3, R6)), na.rm=T)) 
    fluxDF$eCO2_sd <- matrixStats::rowSds(as.matrix(subset(fluxDF, select=c(R1, R4, R5)), na.rm=T)) 
    
    
    
    ## stocks
    poolDF[poolDF$terms=="Fine Root P Pool",2:9] <- poolDF[poolDF$terms=="Fine Root P Pool",2:9] * 0.5
    poolDF$diff <- poolDF$eCO2 - poolDF$aCO2
    poolDF$percent_diff <- (poolDF$eCO2 - poolDF$aCO2)/poolDF$aCO2 * 100
    
    poolDF$aCO2_sd <- matrixStats::rowSds(as.matrix(subset(poolDF, select=c(R2, R3, R6)), na.rm=T)) 
    poolDF$eCO2_sd <- matrixStats::rowSds(as.matrix(subset(poolDF, select=c(R1, R4, R5)), na.rm=T)) 
    
    
    ## budgets
    budgetDF[budgetDF$terms=="Total plant P stock",2:9] <- poolDF[poolDF$terms=="Canopy P Pool",2:9] +
        poolDF[poolDF$terms=="Total Wood P Pool",2:9] + poolDF[poolDF$terms=="Fine Root P Pool",2:9] + 
        poolDF[poolDF$terms=="Coarse Root P Pool",2:9] 
    
    budgetDF[budgetDF$terms=="Total plant P requirement flux",2:9] <- fluxDF[fluxDF$terms=="Total vegetation production P flux",2:9]
    
    budgetDF[budgetDF$terms=="Total plant P retranslocation flux",2:9] <- fluxDF[fluxDF$terms=="Total vegetation retranslocation P flux",2:9]
    
    budgetDF[budgetDF$terms=="Plant P uptake flux",2:9] <- fluxDF[fluxDF$terms=="Total vegetation uptake P flux",2:9]
    
    budgetDF[budgetDF$terms=="Overstorey GPP efficiency",2:9] <- inoutDF[inoutDF$term=="GPP overstorey",2:9] / fluxDF[fluxDF$terms=="Canopy P flux",2:9]
    
    budgetDF[budgetDF$terms=="Plant PUE",2:9] <- (nppDF[nppDF$term=="Leaf NPP",2:9] +
                                                      nppDF[nppDF$term=="Stem NPP",2:9] + 
                                                      nppDF[nppDF$term=="Fine Root NPP",2:9] /2 +
                                                      nppDF[nppDF$term=="Intermediate Root NPP",2:9] / 2 + 
                                                      nppDF[nppDF$term=="Coarse Root NPP",2:9] +
                                                      nppDF[nppDF$term=="Other NPP",2:9] + 
                                                      nppDF[nppDF$term=="Leaf consumption",2:9] +
                                                      nppDF[nppDF$term=="Mycorrhizal production",2:9]) / fluxDF[fluxDF$terms=="Total vegetation uptake P flux",2:9]
    
    
    budgetDF$aCO2 <- rowMeans(as.matrix(subset(budgetDF, select=c(R2, R3, R6)), na.rm=T)) 
    budgetDF$eCO2 <- rowMeans(as.matrix(subset(budgetDF, select=c(R1, R4, R5)), na.rm=T)) 
    budgetDF$aCO2_sd <- matrixStats::rowSds(as.matrix(subset(budgetDF, select=c(R2, R3, R6)), na.rm=T)) 
    budgetDF$eCO2_sd <- matrixStats::rowSds(as.matrix(subset(budgetDF, select=c(R1, R4, R5)), na.rm=T)) 
    
    budgetDF$diff <- budgetDF$eCO2 - budgetDF$aCO2
    budgetDF$percent_diff <- (budgetDF$eCO2 - budgetDF$aCO2)/budgetDF$aCO2 * 100
    

    
    
    ### prepare a list to store all variables, using the model names
    p.var.list <- c("PL", "PW", "PCR", "PFR", "PSTOR",
                    "PFLIT", "PFLITA", "PFLITB", "PCLITB",
                    "PSOIL", "PPORG", "PPMIN", 
                    "PLAB", "PSEC", "POCC", 
                    "PGL", "PGW", "PGCR", "PGFR", "PGUOA",
                    "PLITIN", "PWLIN", "PCRLIN", "PFRLIN",
                    "PUP", "PMIN", "PBIOCHMIN", "PLEACH",
                    "PLRETR", "PWRETR", "PCRRETR", "PFRRETR", 
                    "PRETR",
                    "PUE", "GPP_use",
                    "PMRT", "PUPREQ",
                    "CPL", "CPW", "CPFR", "CPSOIL", "CPFLIT",
                    "PDEM")
        
    ### prepare storage DF
    outDF <- data.frame(c("mean", "mean", "sd", "sd", "mean", "sd", "mean", "sd"),
                        c("aCO2", "eCO2", "aCO2", "eCO2", "diff", "diff", "pct_diff", "pct_diff"))
    
    colnames(outDF) <- c("Group", "Trt")
        
    ### assign values
    ## PL
    outDF$PL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$terms=="Canopy P Pool"]
    outDF$PL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$terms=="Canopy P Pool"]
    outDF$PL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$terms=="Canopy P Pool"]
    outDF$PL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$terms=="Canopy P Pool"]
    outDF$PL[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$terms=="Canopy P Pool"]
    outDF$PL[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Canopy P Pool"]^2+
                                                                   poolDF$eCO2_sd[poolDF$terms=="Canopy P Pool"]^2)/2)
    
    outDF$PL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$terms=="Canopy P Pool"]
    outDF$PL[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Canopy P Pool"]^2+poolDF$aCO2_sd[poolDF$terms=="Canopy P Pool"]^2+
                                                                   poolDF$eCO2_sd[poolDF$terms=="Canopy P Pool"]^2)/3)/poolDF$aCO2[poolDF$terms=="Canopy P Pool"]*100 
    
    
    ## PW
    outDF$PW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$terms=="Total Wood P Pool"]
    outDF$PW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$terms=="Total Wood P Pool"]
    outDF$PW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$terms=="Total Wood P Pool"]
    outDF$PW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$terms=="Total Wood P Pool"]
    outDF$PW[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$terms=="Total Wood P Pool"]
    outDF$PW[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Total Wood P Pool"]^2+
                                                                   poolDF$eCO2_sd[poolDF$terms=="Total Wood P Pool"]^2)/2)
    
    outDF$PW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$terms=="Total Wood P Pool"]
    outDF$PW[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Total Wood P Pool"]^2+poolDF$aCO2_sd[poolDF$terms=="Total Wood P Pool"]^2+
                                                                   poolDF$eCO2_sd[poolDF$terms=="Total Wood P Pool"]^2)/3)/poolDF$aCO2[poolDF$terms=="Total Wood P Pool"]*100 
    
    
    ## PFR
    outDF$PFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$terms=="Fine Root P Pool"]
    outDF$PFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$terms=="Fine Root P Pool"]
    outDF$PFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$terms=="Fine Root P Pool"]
    outDF$PFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$terms=="Fine Root P Pool"]
    outDF$PFR[outDF$Group=="mean"&outDF$Trt=="diff"] <- outDF$PFR[outDF$Group=="mean"&outDF$Trt=="eCO2"]-outDF$PFR[outDF$Group=="mean"&outDF$Trt=="aCO2"]
    outDF$PFR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Fine Root P Pool"]^2+
                                                                    poolDF$eCO2_sd[poolDF$terms=="Fine Root P Pool"]^2)/2)
    
    outDF$PFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$terms=="Fine Root P Pool"]
    outDF$PFR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Fine Root P Pool"]^2+poolDF$aCO2_sd[poolDF$terms=="Fine Root P Pool"]^2+
                                                                   poolDF$eCO2_sd[poolDF$terms=="Fine Root P Pool"]^2)/3)/poolDF$aCO2[poolDF$terms=="Fine Root P Pool"]*100 
    
    
    ## PCR
    outDF$PCR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$terms=="Coarse Root P Pool"]
    outDF$PCR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$terms=="Coarse Root P Pool"]
    outDF$PCR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$terms=="Coarse Root P Pool"]
    outDF$PCR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$terms=="Coarse Root P Pool"]
    outDF$PCR[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$terms=="Coarse Root P Pool"]
    outDF$PCR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Coarse Root P Pool"]^2+
                                                                    poolDF$eCO2_sd[poolDF$terms=="Coarse Root P Pool"]^2)/2) 
    
    outDF$PCR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$terms=="Coarse Root P Pool"]
    outDF$PCR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Coarse Root P Pool"]^2+poolDF$aCO2_sd[poolDF$terms=="Coarse Root P Pool"]^2+
                                                                   poolDF$eCO2_sd[poolDF$terms=="Coarse Root P Pool"]^2)/3)/poolDF$aCO2[poolDF$terms=="Coarse Root P Pool"]*100 
    
    
    ## PSTOR
    #outDF$PCR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- NA
    #outDF$PCR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- NA
    #outDF$PCR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- NA
    #outDF$PCR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- NA
    #outDF$PCR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- NA
    #outDF$PCR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- NA
    
    
    ## Forestfloor Leaf Litter P Pool
    outDF$PFLITA[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$terms=="Forestfloor Leaf Litter P Pool"]
    outDF$PFLITA[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$terms=="Forestfloor Leaf Litter P Pool"]
    outDF$PFLITA[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$terms=="Forestfloor Leaf Litter P Pool"]
    outDF$PFLITA[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$terms=="Forestfloor Leaf Litter P Pool"]
    outDF$PFLITA[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$terms=="Forestfloor Leaf Litter P Pool"]
    outDF$PFLITA[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Forestfloor Leaf Litter P Pool"]^2+
                                                                       poolDF$eCO2_sd[poolDF$terms=="Forestfloor Leaf Litter P Pool"]^2)/2)
    
    outDF$PFLITA[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$terms=="Forestfloor Leaf Litter P Pool"]
    outDF$PFLITA[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Forestfloor Leaf Litter P Pool"]^2+poolDF$aCO2_sd[poolDF$terms=="Forestfloor Leaf Litter P Pool"]^2+
                                                                    poolDF$eCO2_sd[poolDF$terms=="Forestfloor Leaf Litter P Pool"]^2)/3)/poolDF$aCO2[poolDF$terms=="Forestfloor Leaf Litter P Pool"]*100 
    
    
    ## Forestfloor Leaf Litter P Pool
    outDF$PFLITB[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- NA
    outDF$PFLITB[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- NA
    outDF$PFLITB[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- NA
    outDF$PFLITB[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- NA
    outDF$PFLITB[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- NA
    outDF$PFLITB[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- NA
    
    
    ## PCLITB
    outDF$PCLITB[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$terms=="Standing Dead Wood P Pool"]
    outDF$PCLITB[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$terms=="Standing Dead Wood P Pool"]
    outDF$PCLITB[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$terms=="Standing Dead Wood P Pool"]
    outDF$PCLITB[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$terms=="Standing Dead Wood P Pool"]
    outDF$PCLITB[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$terms=="Standing Dead Wood P Pool"]
    outDF$PCLITB[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Standing Dead Wood P Pool"]^2+
                                                                       poolDF$eCO2_sd[poolDF$terms=="Standing Dead Wood P Pool"]^2)/2)
    
    outDF$PCLITB[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$terms=="Standing Dead Wood P Pool"]
    outDF$PCLITB[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Standing Dead Wood P Pool"]^2+poolDF$aCO2_sd[poolDF$terms=="Standing Dead Wood P Pool"]^2+
                                                                       poolDF$eCO2_sd[poolDF$terms=="Standing Dead Wood P Pool"]^2)/3)/poolDF$aCO2[poolDF$terms=="Standing Dead Wood P Pool"]*100 
    
    
    ## PSOIL
    outDF$PSOIL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$terms=="Soil P Pool 0-10cm"]
    outDF$PSOIL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$terms=="Soil P Pool 0-10cm"]
    outDF$PSOIL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$terms=="Soil P Pool 0-10cm"]
    outDF$PSOIL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$terms=="Soil P Pool 0-10cm"]
    outDF$PSOIL[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$terms=="Soil P Pool 0-10cm"]
    outDF$PSOIL[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Soil P Pool 0-10cm"]^2+
                                                                      poolDF$eCO2_sd[poolDF$terms=="Soil P Pool 0-10cm"]^2)/2)
    
    outDF$PSOIL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$terms=="Soil P Pool 0-10cm"]
    outDF$PSOIL[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Soil P Pool 0-10cm"]^2+poolDF$aCO2_sd[poolDF$terms=="Soil P Pool 0-10cm"]^2+
                                                                       poolDF$eCO2_sd[poolDF$terms=="Soil P Pool 0-10cm"]^2)/3)/poolDF$aCO2[poolDF$terms=="Soil P Pool 0-10cm"]*100 
    
    
    ## PPORG
    outDF$PPORG[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$terms=="Soil Org P Pool 0-10cm"]
    outDF$PPORG[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$terms=="Soil Org P Pool 0-10cm"]
    outDF$PPORG[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$terms=="Soil Org P Pool 0-10cm"]
    outDF$PPORG[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$terms=="Soil Org P Pool 0-10cm"]
    outDF$PPORG[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$terms=="Soil Org P Pool 0-10cm"]
    outDF$PPORG[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Soil Org P Pool 0-10cm"]^2+
                                                                      poolDF$eCO2_sd[poolDF$terms=="Soil Org P Pool 0-10cm"]^2)/2)
    
    outDF$PPORG[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$terms=="Soil Org P Pool 0-10cm"]
    outDF$PPORG[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Soil Org P Pool 0-10cm"]^2+poolDF$aCO2_sd[poolDF$terms=="Soil Org P Pool 0-10cm"]^2+
                                                                      poolDF$eCO2_sd[poolDF$terms=="Soil Org P Pool 0-10cm"]^2)/3)/poolDF$aCO2[poolDF$terms=="Soil Org P Pool 0-10cm"]*100 
    
    
    ## PPMIN
    outDF$PPMIN[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$terms=="Soil Inorg P Pool 0-10cm"]
    outDF$PPMIN[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$terms=="Soil Inorg P Pool 0-10cm"]
    outDF$PPMIN[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$terms=="Soil Inorg P Pool 0-10cm"]
    outDF$PPMIN[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$terms=="Soil Inorg P Pool 0-10cm"]
    outDF$PPMIN[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$terms=="Soil Inorg P Pool 0-10cm"]
    outDF$PPMIN[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Soil Inorg P Pool 0-10cm"]^2+
                                                                      poolDF$eCO2_sd[poolDF$terms=="Soil Inorg P Pool 0-10cm"]^2)/2)
    
    outDF$PPMIN[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$terms=="Soil Inorg P Pool 0-10cm"]
    outDF$PPMIN[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Soil Inorg P Pool 0-10cm"]^2+poolDF$aCO2_sd[poolDF$terms=="Soil Inorg P Pool 0-10cm"]^2+
                                                                      poolDF$eCO2_sd[poolDF$terms=="Soil Inorg P Pool 0-10cm"]^2)/3)/poolDF$aCO2[poolDF$terms=="Soil Inorg P Pool 0-10cm"]*100 
    
    
    ## PLAB
    outDF$PLAB[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]
    outDF$PLAB[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]
    outDF$PLAB[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]
    outDF$PLAB[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]
    outDF$PLAB[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]
    outDF$PLAB[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]^2+
                                                                     poolDF$eCO2_sd[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]^2)/2)
    
    outDF$PLAB[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]
    outDF$PLAB[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]^2+poolDF$aCO2_sd[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]^2+
                                                                      poolDF$eCO2_sd[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]^2)/3)/poolDF$aCO2[poolDF$terms=="Soil Phosphate P Pool 0-10cm"]*100 
    
    
    ## PSEC
    tmp <- colSums(poolDF[poolDF$term%in%c("Secondary Fe bound Pi Pool", "Primary Ca bound Pi Pool"),2:7], na.rm=T)
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    
    outDF$PSEC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$PSEC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$PSEC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$PSEC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$PSEC[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$PSEC[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/2) 
    
    outDF$PSEC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$PSEC[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/amean*100 
    
    
    ## POCC
    outDF$POCC[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- poolDF$aCO2[poolDF$terms=="Occluded P Pool"]
    outDF$POCC[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- poolDF$eCO2[poolDF$terms=="Occluded P Pool"]
    outDF$POCC[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- poolDF$aCO2_sd[poolDF$terms=="Occluded P Pool"]
    outDF$POCC[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- poolDF$eCO2_sd[poolDF$terms=="Occluded P Pool"]
    outDF$POCC[outDF$Group=="mean"&outDF$Trt=="diff"] <- poolDF$diff[poolDF$terms=="Occluded P Pool"]
    outDF$POCC[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Occluded P Pool"]^2+
                                                                     poolDF$eCO2_sd[poolDF$terms=="Occluded P Pool"]^2)/2)
    
    outDF$POCC[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- poolDF$percent_diff[poolDF$terms=="Occluded P Pool"]
    outDF$POCC[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((poolDF$aCO2_sd[poolDF$terms=="Occluded P Pool"]^2+poolDF$aCO2_sd[poolDF$terms=="Occluded P Pool"]^2+
                                                                     poolDF$eCO2_sd[poolDF$terms=="Occluded P Pool"]^2)/3)/poolDF$aCO2[poolDF$terms=="Occluded P Pool"]*100 
    
    
    ## PGL
    tmp <- colSums(fluxDF[fluxDF$term%in%c(#"Frass P flux", 
                                           "Canopy P flux"),2:7], na.rm=T)
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    outDF$PGL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$PGL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$PGL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$PGL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$PGL[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$PGL[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/2) 
    
    outDF$PGL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$PGL[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/amean*100 
    
    
    ## PGW
    tmp <- colSums(fluxDF[fluxDF$term%in%c("Wood P flux", "Bark litter P flux",
                                           "Twig litter P flux", "Seed litter P flux"),2:7], na.rm=T)
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    outDF$PGW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$PGW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$PGW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$PGW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$PGW[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$PGW[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/2) 
    
    outDF$PGW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$PGW[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/amean*100 
    
    #outDF$PGW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- fluxDF$aCO2[fluxDF$terms=="Wood P flux"]
    #outDF$PGW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- fluxDF$eCO2[fluxDF$terms=="Wood P flux"]
    #outDF$PGW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- fluxDF$aCO2_sd[fluxDF$terms=="Wood P flux"]
    #outDF$PGW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- fluxDF$eCO2_sd[fluxDF$terms=="Wood P flux"]
    #outDF$PGW[outDF$Group=="mean"&outDF$Trt=="diff"] <- fluxDF$diff[fluxDF$terms=="Wood P flux"]
    #outDF$PGW[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Wood P flux"]^2+
    #                                                                fluxDF$eCO2_sd[fluxDF$terms=="Wood P flux"]^2)/2)
    #
    #outDF$PGW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- fluxDF$percent_diff[fluxDF$terms=="Wood P flux"]
    #outDF$PGW[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Wood P flux"]^2+fluxDF$aCO2_sd[fluxDF$terms=="Wood P flux"]^2+
    #                                                                fluxDF$eCO2_sd[fluxDF$terms=="Wood P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Wood P flux"]*100 
    #
    
    ## PGFR
    outDF$PGFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- fluxDF$aCO2[fluxDF$terms=="Fine Root P flux"]
    outDF$PGFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- fluxDF$eCO2[fluxDF$terms=="Fine Root P flux"]
    outDF$PGFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- fluxDF$aCO2_sd[fluxDF$terms=="Fine Root P flux"]
    outDF$PGFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- fluxDF$eCO2_sd[fluxDF$terms=="Fine Root P flux"]
    outDF$PGFR[outDF$Group=="mean"&outDF$Trt=="diff"] <- fluxDF$diff[fluxDF$terms=="Fine Root P flux"]
    outDF$PGFR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Fine Root P flux"]^2+
                                                                     fluxDF$eCO2_sd[fluxDF$terms=="Fine Root P flux"]^2)/2)
    
    outDF$PGFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- fluxDF$percent_diff[fluxDF$terms=="Fine Root P flux"]
    outDF$PGFR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Fine Root P flux"]^2+fluxDF$aCO2_sd[fluxDF$terms=="Fine Root P flux"]^2+
                                                                    fluxDF$eCO2_sd[fluxDF$terms=="Fine Root P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Fine Root P flux"]*100 
    
    ## PGCR
    outDF$PGCR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- fluxDF$aCO2[fluxDF$terms=="Coarse Root P flux"]
    outDF$PGCR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- fluxDF$eCO2[fluxDF$terms=="Coarse Root P flux"]
    outDF$PGCR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- fluxDF$aCO2_sd[fluxDF$terms=="Coarse Root P flux"]
    outDF$PGCR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- fluxDF$eCO2_sd[fluxDF$terms=="Coarse Root P flux"]
    outDF$PGCR[outDF$Group=="mean"&outDF$Trt=="diff"] <- fluxDF$diff[fluxDF$terms=="Coarse Root P flux"]
    outDF$PGCR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Coarse Root P flux"]^2+
                                                                     fluxDF$eCO2_sd[fluxDF$terms=="Coarse Root P flux"]^2)/2)
    
    outDF$PGCR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- fluxDF$percent_diff[fluxDF$terms=="Coarse Root P flux"]
    outDF$PGCR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Coarse Root P flux"]^2+fluxDF$aCO2_sd[fluxDF$terms=="Coarse Root P flux"]^2+
                                                                    fluxDF$eCO2_sd[fluxDF$terms=="Coarse Root P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Coarse Root P flux"]*100 
    
    
    
    ## PGUOA
    #outDF$PGCR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- fluxDF$aCO2[fluxDF$terms=="Understorey P flux"]
    #outDF$PGCR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- fluxDF$eCO2[fluxDF$terms=="Understorey P flux"]
    #outDF$PGCR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- fluxDF$aCO2_sd[fluxDF$terms=="Understorey P flux"]
    #outDF$PGCR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- fluxDF$eCO2_sd[fluxDF$terms=="Understorey P flux"]
    #outDF$PGCR[outDF$Group=="mean"&outDF$Trt=="diff"] <- fluxDF$diff[fluxDF$terms=="Understorey P flux"]
    #outDF$PGCR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Understorey P flux"]^2+
    #                                                             fluxDF$eCO2_sd[fluxDF$terms=="Understorey P flux"]^2)/2)
    #
    #outDF$PGCR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- fluxDF$percent_diff[fluxDF$terms=="Understorey P flux"]
    #outDF$PGCR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Understorey P flux"]^2+fluxDF$aCO2_sd[fluxDF$terms=="Understorey P flux"]^2+
    #                                                                 fluxDF$eCO2_sd[fluxDF$terms=="Understorey P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Understorey P flux"]*100 
    
    
    
    ## PDEM
    outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- outDF$PGL[outDF$Group=="mean"&outDF$Trt=="aCO2"]+
        outDF$PGW[outDF$Group=="mean"&outDF$Trt=="aCO2"]+
        outDF$PGFR[outDF$Group=="mean"&outDF$Trt=="aCO2"]+
        outDF$PGCR[outDF$Group=="mean"&outDF$Trt=="aCO2"]
    
    outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- outDF$PGL[outDF$Group=="mean"&outDF$Trt=="eCO2"]+
        outDF$PGW[outDF$Group=="mean"&outDF$Trt=="eCO2"]+
        outDF$PGFR[outDF$Group=="mean"&outDF$Trt=="eCO2"]+
        outDF$PGCR[outDF$Group=="mean"&outDF$Trt=="eCO2"]
    
    outDF$PDEM[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- sqrt((outDF$PGL[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                 outDF$PGW[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                 outDF$PGFR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                 outDF$PGCR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2)/4)
    
    
    outDF$PDEM[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- sqrt((outDF$PGL[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                                 outDF$PGW[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                                 outDF$PGFR[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                                 outDF$PGCR[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/4)
    
    outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="diff"] <- outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="eCO2"] - outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="aCO2"]
    outDF$PDEM[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$PDEM[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                 outDF$PDEM[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/2)
    
    outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="eCO2"] - outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="aCO2"])/outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="aCO2"]*100
    outDF$PDEM[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((outDF$PDEM[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+outDF$PDEM[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                     outDF$PDEM[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2)/3)/outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="aCO2"]*100 
    
    
    
    ## PLITIN
    outDF$PLITIN[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- fluxDF$aCO2[fluxDF$terms=="Leaflitter P flux"]
    outDF$PLITIN[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- fluxDF$eCO2[fluxDF$terms=="Leaflitter P flux"]
    outDF$PLITIN[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- fluxDF$aCO2_sd[fluxDF$terms=="Leaflitter P flux"]
    outDF$PLITIN[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- fluxDF$eCO2_sd[fluxDF$terms=="Leaflitter P flux"]
    outDF$PLITIN[outDF$Group=="mean"&outDF$Trt=="diff"] <- fluxDF$diff[fluxDF$terms=="Leaflitter P flux"]
    outDF$PLITIN[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Leaflitter P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Leaflitter P flux"]^2)/2)
    
    outDF$PLITIN[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- fluxDF$percent_diff[fluxDF$terms=="Leaflitter P flux"]
    outDF$PLITIN[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Leaflitter P flux"]^2+fluxDF$aCO2_sd[fluxDF$terms=="Leaflitter P flux"]^2+
                                                                     fluxDF$eCO2_sd[fluxDF$terms=="Leaflitter P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Leaflitter P flux"]*100 
    
    
    ## PWLIN
    tmp <- colSums(fluxDF[fluxDF$term%in%c("Bark litter P flux", "Twig litter P flux"),2:7], na.rm=T)
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    outDF$PWLIN[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$PWLIN[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$PWLIN[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$PWLIN[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$PWLIN[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$PWLIN[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/2)
    
    outDF$PWLIN[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$PWLIN[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/amean*100 
    
    
    ## PFRLIN
    outDF$PFRLIN[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- fluxDF$aCO2[fluxDF$terms=="Fineroot Litter P flux"]
    outDF$PFRLIN[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- fluxDF$eCO2[fluxDF$terms=="Fineroot Litter P flux"]
    outDF$PFRLIN[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- fluxDF$aCO2_sd[fluxDF$terms=="Fineroot Litter P flux"]
    outDF$PFRLIN[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- fluxDF$eCO2_sd[fluxDF$terms=="Fineroot Litter P flux"]
    outDF$PFRLIN[outDF$Group=="mean"&outDF$Trt=="diff"] <- fluxDF$diff[fluxDF$terms=="Fineroot Litter P flux"]
    outDF$PFRLIN[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Fineroot Litter P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Fineroot Litter P flux"]^2)/2)
    
    outDF$PFRLIN[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- fluxDF$percent_diff[fluxDF$terms=="Fineroot Litter P flux"]
    outDF$PFRLIN[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Fineroot Litter P flux"]^2+fluxDF$aCO2_sd[fluxDF$terms=="Fineroot Litter P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Fineroot Litter P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Fineroot Litter P flux"]*100 
    
    
    ## PCRLIN
    outDF$PCRLIN[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- NA
    outDF$PCRLIN[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- NA
    outDF$PCRLIN[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- NA
    outDF$PCRLIN[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- NA
    outDF$PCRLIN[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- NA
    outDF$PCRLIN[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- NA
    

    ## PMIN
    tmp <- colSums(fluxDF[fluxDF$term%in%c("Mineralization P flux 0-10cm", 
                                           "Mineralization P flux 10-30cm",
                                           "Mineralization P flux 30-60cm"),2:7], na.rm=T)
    amean <- mean(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    emean <- mean(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    asd <- sd(c(tmp[2], tmp[3], tmp[6]), na.rm=T)
    esd <- sd(c(tmp[1], tmp[4], tmp[5]), na.rm=T)
    diff <- (emean/amean - 1) * 100
    outDF$PMIN[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- amean
    outDF$PMIN[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- emean
    outDF$PMIN[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- asd
    outDF$PMIN[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- esd
    outDF$PMIN[outDF$Group=="mean"&outDF$Trt=="diff"] <- emean-amean
    outDF$PMIN[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((asd^2+esd^2)/2) 
    
    outDF$PMIN[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- diff
    outDF$PMIN[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((asd^2+asd^2+esd^2)/3)/amean*100 
    
    
    ## PBIOCHMIN
    outDF$PBIOCHMIN[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- NA
    outDF$PBIOCHMIN[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- NA
    outDF$PBIOCHMIN[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- NA
    outDF$PBIOCHMIN[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- NA
    outDF$PBIOCHMIN[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- NA
    outDF$PBIOCHMIN[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- NA
    
    
    ## PLEACH
    outDF$PLEACH[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- fluxDF$aCO2[fluxDF$terms=="Leaching P flux"]
    outDF$PLEACH[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- fluxDF$eCO2[fluxDF$terms=="Leaching P flux"]
    outDF$PLEACH[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- fluxDF$aCO2_sd[fluxDF$terms=="Leaching P flux"]
    outDF$PLEACH[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- fluxDF$eCO2_sd[fluxDF$terms=="Leaching P flux"]
    outDF$PLEACH[outDF$Group=="mean"&outDF$Trt=="diff"] <- fluxDF$diff[fluxDF$terms=="Leaching P flux"]
    outDF$PLEACH[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Leaching P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Leaching P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Leaching P flux"]*100 
    
    outDF$PLEACH[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- fluxDF$percent_diff[fluxDF$terms=="Leaching P flux"]
    outDF$PLEACH[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Leaching P flux"]^2+fluxDF$aCO2_sd[fluxDF$terms=="Leaching P flux"]^2+
                                                                    fluxDF$eCO2_sd[fluxDF$terms=="Leaching P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Leaching P flux"]*100 
    

    ## PLRETR
    outDF$PLRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- fluxDF$aCO2[fluxDF$terms=="Canopy retrans P flux"]
    outDF$PLRETR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- fluxDF$eCO2[fluxDF$terms=="Canopy retrans P flux"]
    outDF$PLRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- fluxDF$aCO2_sd[fluxDF$terms=="Canopy retrans P flux"]
    outDF$PLRETR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- fluxDF$eCO2_sd[fluxDF$terms=="Canopy retrans P flux"]
    outDF$PLRETR[outDF$Group=="mean"&outDF$Trt=="diff"] <- fluxDF$diff[fluxDF$terms=="Canopy retrans P flux"]
    outDF$PLRETR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Canopy retrans P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Canopy retrans P flux"]^2)/2)
    
    outDF$PLRETR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- fluxDF$percent_diff[fluxDF$terms=="Canopy retrans P flux"]
    outDF$PLRETR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Canopy retrans P flux"]^2+fluxDF$aCO2_sd[fluxDF$terms=="Canopy retrans P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Canopy retrans P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Canopy retrans P flux"]*100 
    
    
    ## PWRETR
    outDF$PWRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- fluxDF$aCO2[fluxDF$terms=="Sapwood retrans P flux"]
    outDF$PWRETR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- fluxDF$eCO2[fluxDF$terms=="Sapwood retrans P flux"]
    outDF$PWRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- fluxDF$aCO2_sd[fluxDF$terms=="Sapwood retrans P flux"]
    outDF$PWRETR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- fluxDF$eCO2_sd[fluxDF$terms=="Sapwood retrans P flux"]
    outDF$PWRETR[outDF$Group=="mean"&outDF$Trt=="diff"] <- fluxDF$diff[fluxDF$terms=="Sapwood retrans P flux"]
    outDF$PWRETR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Sapwood retrans P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Sapwood retrans P flux"]^2)/2)
    
    outDF$PWRETR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- fluxDF$percent_diff[fluxDF$terms=="Sapwood retrans P flux"]
    outDF$PWRETR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Sapwood retrans P flux"]^2+fluxDF$aCO2_sd[fluxDF$terms=="Sapwood retrans P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Sapwood retrans P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Sapwood retrans P flux"]*100 
    
    
    ## PCRETR
    outDF$PCRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- fluxDF$aCO2[fluxDF$terms=="Coarseroot retrans P flux"]
    outDF$PCRETR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- fluxDF$eCO2[fluxDF$terms=="Coarseroot retrans P flux"]
    outDF$PCRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- fluxDF$aCO2_sd[fluxDF$terms=="Coarseroot retrans P flux"]
    outDF$PCRETR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- fluxDF$eCO2_sd[fluxDF$terms=="Coarseroot retrans P flux"]
    outDF$PCRETR[outDF$Group=="mean"&outDF$Trt=="diff"] <- fluxDF$diff[fluxDF$terms=="Coarseroot retrans P flux"]
    outDF$PCRETR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Coarseroot retrans P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Coarseroot retrans P flux"]^2)/2)
    
    outDF$PCRETR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- fluxDF$percent_diff[fluxDF$terms=="Coarseroot retrans P flux"]
    outDF$PCRETR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Coarseroot retrans P flux"]^2+fluxDF$aCO2_sd[fluxDF$terms=="Coarseroot retrans P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Coarseroot retrans P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Coarseroot retrans P flux"]*100 
    
    
    ## PFRETR
    outDF$PFRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- fluxDF$aCO2[fluxDF$terms=="Fineroot retrans P flux"]
    outDF$PFRETR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- fluxDF$eCO2[fluxDF$terms=="Fineroot retrans P flux"]
    outDF$PFRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- fluxDF$aCO2_sd[fluxDF$terms=="Fineroot retrans P flux"]
    outDF$PFRETR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- fluxDF$eCO2_sd[fluxDF$terms=="Fineroot retrans P flux"]
    outDF$PFRETR[outDF$Group=="mean"&outDF$Trt=="diff"] <- fluxDF$diff[fluxDF$terms=="Fineroot retrans P flux"]
    outDF$PFRETR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Fineroot retrans P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Fineroot retrans P flux"]^2)/2)
    
    outDF$PFRETR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- fluxDF$percent_diff[fluxDF$terms=="Fineroot retrans P flux"]
    outDF$PFRETR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((fluxDF$aCO2_sd[fluxDF$terms=="Fineroot retrans P flux"]^2+fluxDF$aCO2_sd[fluxDF$terms=="Fineroot retrans P flux"]^2+
                                                                       fluxDF$eCO2_sd[fluxDF$terms=="Fineroot retrans P flux"]^2)/3)/fluxDF$aCO2[fluxDF$terms=="Fineroot retrans P flux"]*100 
    
    
    ## PRETR
    outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- outDF$PLRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"]+
        outDF$PWRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"]+
        outDF$PFRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"]
    
    outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- outDF$PLRETR[outDF$Group=="mean"&outDF$Trt=="eCO2"]+
        outDF$PWRETR[outDF$Group=="mean"&outDF$Trt=="eCO2"]+
        outDF$PFRETR[outDF$Group=="mean"&outDF$Trt=="eCO2"]
    
    outDF$PRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- sqrt((outDF$PLRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                 outDF$PWRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                 outDF$PFRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2)/3)
    
    
    outDF$PRETR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- sqrt((outDF$PLRETR[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                                  outDF$PWRETR[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                                  outDF$PFRETR[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/3)
    
    
    outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="diff"] <- outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="eCO2"] - outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"]
    outDF$PRETR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$PRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                 outDF$PRETR[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/2)
    
    outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="eCO2"] - outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"])/outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"]*100
    outDF$PRETR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((outDF$PRETR[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+outDF$PRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                     outDF$PRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2)/3)/outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"]*100 
    
    
    ## PUP
    outDF$PUP[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="aCO2"] - outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="aCO2"]
    outDF$PUP[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- outDF$PDEM[outDF$Group=="mean"&outDF$Trt=="eCO2"] - outDF$PRETR[outDF$Group=="mean"&outDF$Trt=="eCO2"]
    outDF$PUP[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- sqrt((outDF$PDEM[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2 + outDF$PRETR[outDF$Group=="sd"&outDF$Trt=="aCO2"])/2)
    outDF$PUP[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- sqrt((outDF$PDEM[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2 + outDF$PRETR[outDF$Group=="sd"&outDF$Trt=="eCO2"])/2)
    outDF$PUP[outDF$Group=="mean"&outDF$Trt=="diff"] <- outDF$PUP[outDF$Group=="mean"&outDF$Trt=="eCO2"] - outDF$PUP[outDF$Group=="mean"&outDF$Trt=="aCO2"]
    outDF$PUP[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$PUP[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                outDF$PUP[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/2) 
    
    outDF$PUP[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (outDF$PUP[outDF$Group=="mean"&outDF$Trt=="eCO2"] - outDF$PUP[outDF$Group=="mean"&outDF$Trt=="aCO2"]) / outDF$PUP[outDF$Group=="mean"&outDF$Trt=="aCO2"]
    outDF$PUP[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((outDF$PUP[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+outDF$PUP[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                                    outDF$PUP[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/3)/outDF$PUP[outDF$Group=="mean"&outDF$Trt=="eCO2"]*100 
    
    

    ## PUE
    outDF$PUE[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- budgetDF$aCO2[budgetDF$terms=="Plant PUE"]
    outDF$PUE[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- budgetDF$eCO2[budgetDF$terms=="Plant PUE"]
    outDF$PUE[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- budgetDF$aCO2_sd[budgetDF$terms=="Plant PUE"]
    outDF$PUE[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- budgetDF$eCO2_sd[budgetDF$terms=="Plant PUE"]
    outDF$PUE[outDF$Group=="mean"&outDF$Trt=="diff"] <- outDF$PUE[outDF$Group=="mean"&outDF$Trt=="eCO2"]-outDF$PUE[outDF$Group=="mean"&outDF$Trt=="aCO2"]
    outDF$PUE[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$PUE[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                                outDF$PUE[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2)/2)
    
    outDF$PUE[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- NA
    outDF$PUE[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- NA
    
    
    ## GPP_use
    outDF$GPP_use[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- budgetDF$aCO2[budgetDF$terms=="Overstorey GPP efficiency"]
    outDF$GPP_use[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- budgetDF$eCO2[budgetDF$terms=="Overstorey GPP efficiency"]
    outDF$GPP_use[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- budgetDF$aCO2_sd[budgetDF$terms=="Overstorey GPP efficiency"]
    outDF$GPP_use[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- budgetDF$eCO2_sd[budgetDF$terms=="Overstorey GPP efficiency"]
    outDF$GPP_use[outDF$Group=="mean"&outDF$Trt=="diff"] <- outDF$GPP_use[outDF$Group=="mean"&outDF$Trt=="eCO2"]-outDF$GPP_use[outDF$Group=="mean"&outDF$Trt=="aCO2"]
    outDF$GPP_use[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$GPP_use[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2+
                                                                outDF$GPP_use[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2)/2)
    
    outDF$GPP_use[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- NA
    outDF$GPP_use[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- NA
    
    
    ## PMRT
    outDF$PMRT[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- budgetDF$aCO2[budgetDF$terms=="Plant P MRT"]
    outDF$PMRT[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- budgetDF$eCO2[budgetDF$terms=="Plant P MRT"]
    outDF$PMRT[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- budgetDF$aCO2_sd[budgetDF$terms=="Plant P MRT"]
    outDF$PMRT[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- budgetDF$eCO2_sd[budgetDF$terms=="Plant P MRT"]
    outDF$PMRT[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- NA
    outDF$PMRT[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- NA
    
    
    ## PUPREQ
    outDF$PUPREQ[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- budgetDF$aCO2[budgetDF$terms=="Plant P uptake over requirement"]
    outDF$PUPREQ[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- budgetDF$eCO2[budgetDF$terms=="Plant P uptake over requirement"]
    outDF$PUPREQ[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- budgetDF$aCO2_sd[budgetDF$terms=="Plant P uptake over requirement"]
    outDF$PUPREQ[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- budgetDF$eCO2_sd[budgetDF$terms=="Plant P uptake over requirement"]
    outDF$PUPREQ[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- NA
    outDF$PUPREQ[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- NA 
    
    
    ## CPL
    outDF$CPL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- mean(cpDF$canopy[cpDF$Ring%in%c(2,3,6)], na.rm=T)
    outDF$CPL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- mean(cpDF$canopy[cpDF$Ring%in%c(1,4,5)], na.rm=T)
    outDF$CPL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- sd(cpDF$canopy[cpDF$Ring%in%c(2,3,6)], na.rm=T)
    outDF$CPL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- sd(cpDF$canopy[cpDF$Ring%in%c(1,4,5)], na.rm=T)
    outDF$CPL[outDF$Group=="mean"&outDF$Trt=="diff"] <- (mean(cpDF$canopy[cpDF$Ring%in%c(1,4,5)], na.rm=T) - mean(cpDF$canopy[cpDF$Ring%in%c(2,3,6)], na.rm=T) ) 
    outDF$CPL[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$CPL[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                    outDF$CPL[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/2)
    
    outDF$CPL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (mean(cpDF$canopy[cpDF$Ring%in%c(1,4,5)], na.rm=T) - mean(cpDF$canopy[cpDF$Ring%in%c(2,3,6)], na.rm=T) ) / mean(cpDF$canopy[cpDF$Ring%in%c(2,3,6)], na.rm=T) * 100
    outDF$CPL[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((outDF$CPL[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+outDF$CPL[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                    outDF$CPL[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/3)/mean(cpDF$canopy[cpDF$Ring%in%c(2,3,6)], na.rm=T)*100 
    
    
    ## CPW
    outDF$CPW[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- mean(cpDF$wood[cpDF$Ring%in%c(2,3,6)], na.rm=T)
    outDF$CPW[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- mean(cpDF$wood[cpDF$Ring%in%c(1,4,5)], na.rm=T)
    outDF$CPW[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- sd(cpDF$wood[cpDF$Ring%in%c(2,3,6)], na.rm=T)
    outDF$CPW[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- sd(cpDF$wood[cpDF$Ring%in%c(1,4,5)], na.rm=T)
    outDF$CPW[outDF$Group=="mean"&outDF$Trt=="diff"] <- (mean(cpDF$wood[cpDF$Ring%in%c(1,4,5)], na.rm=T) - mean(cpDF$wood[cpDF$Ring%in%c(2,3,6)], na.rm=T) ) 
    outDF$CPW[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$CPW[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                    outDF$CPW[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/2)
    
    outDF$CPW[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (mean(cpDF$wood[cpDF$Ring%in%c(1,4,5)], na.rm=T) - mean(cpDF$wood[cpDF$Ring%in%c(2,3,6)], na.rm=T) ) / mean(cpDF$wood[cpDF$Ring%in%c(2,3,6)], na.rm=T) * 100
    outDF$CPW[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((outDF$CPW[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+outDF$CPW[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                    outDF$CPW[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/3)/mean(cpDF$wood[cpDF$Ring%in%c(2,3,6)], na.rm=T)*100 
    
    
    ## CPFR
    outDF$CPFR[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- mean(cpDF$fineroot[cpDF$Ring%in%c(2,3,6)], na.rm=T)
    outDF$CPFR[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- mean(cpDF$fineroot[cpDF$Ring%in%c(1,4,5)], na.rm=T)
    outDF$CPFR[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- sd(cpDF$fineroot[cpDF$Ring%in%c(2,3,6)], na.rm=T)
    outDF$CPFR[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- sd(cpDF$fineroot[cpDF$Ring%in%c(1,4,5)], na.rm=T)
    outDF$CPFR[outDF$Group=="mean"&outDF$Trt=="diff"] <- (mean(cpDF$fineroot[cpDF$Ring%in%c(1,4,5)], na.rm=T) - mean(cpDF$fineroot[cpDF$Ring%in%c(2,3,6)], na.rm=T) ) 
    outDF$CPFR[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$CPFR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                     outDF$CPFR[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/2)
    
    
    outDF$CPFR[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (mean(cpDF$fineroot[cpDF$Ring%in%c(1,4,5)], na.rm=T) - mean(cpDF$fineroot[cpDF$Ring%in%c(2,3,6)], na.rm=T) ) / mean(cpDF$fineroot[cpDF$Ring%in%c(2,3,6)], na.rm=T) * 100
    outDF$CPFR[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((outDF$CPFR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+outDF$CPFR[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                    outDF$CPFR[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/3)/mean(cpDF$fineroot[cpDF$Ring%in%c(2,3,6)], na.rm=T)*100 
    
    
    ## CPSOIL
    outDF$CPSOIL[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- mean(cpDF$soil[cpDF$Ring%in%c(2,3,6)], na.rm=T)
    outDF$CPSOIL[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- mean(cpDF$soil[cpDF$Ring%in%c(1,4,5)], na.rm=T)
    outDF$CPSOIL[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- sd(cpDF$soil[cpDF$Ring%in%c(2,3,6)], na.rm=T)
    outDF$CPSOIL[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- sd(cpDF$soil[cpDF$Ring%in%c(1,4,5)], na.rm=T)
    outDF$CPSOIL[outDF$Group=="mean"&outDF$Trt=="diff"] <- (mean(cpDF$soil[cpDF$Ring%in%c(1,4,5)], na.rm=T) - mean(cpDF$soil[cpDF$Ring%in%c(2,3,6)], na.rm=T) ) 
    outDF$CPSOIL[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$CPSOIL[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                       outDF$CPSOIL[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/2)
    
    outDF$CPSOIL[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (mean(cpDF$soil[cpDF$Ring%in%c(1,4,5)], na.rm=T) - mean(cpDF$soil[cpDF$Ring%in%c(2,3,6)], na.rm=T) ) / mean(cpDF$soil[cpDF$Ring%in%c(2,3,6)], na.rm=T) * 100
    outDF$CPSOIL[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((outDF$CPSOIL[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+outDF$CPSOIL[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                     outDF$CPSOIL[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/3)/mean(cpDF$soil[cpDF$Ring%in%c(2,3,6)], na.rm=T)*100 
    
    
    ## CPFLIT
    outDF$CPFLIT[outDF$Group=="mean"&outDF$Trt=="aCO2"] <- mean(cpDF$leaflitter[cpDF$Ring%in%c(2,3,6)], na.rm=T)
    outDF$CPFLIT[outDF$Group=="mean"&outDF$Trt=="eCO2"] <- mean(cpDF$leaflitter[cpDF$Ring%in%c(1,4,5)], na.rm=T)
    outDF$CPFLIT[outDF$Group=="sd"&outDF$Trt=="aCO2"] <- sd(cpDF$leaflitter[cpDF$Ring%in%c(2,3,6)], na.rm=T)
    outDF$CPFLIT[outDF$Group=="sd"&outDF$Trt=="eCO2"] <- sd(cpDF$leaflitter[cpDF$Ring%in%c(1,4,5)], na.rm=T)
    outDF$CPFLIT[outDF$Group=="mean"&outDF$Trt=="diff"] <- (mean(cpDF$leaflitter[cpDF$Ring%in%c(1,4,5)], na.rm=T) - mean(cpDF$leaflitter[cpDF$Ring%in%c(2,3,6)], na.rm=T) ) 
    outDF$CPFLIT[outDF$Group=="sd"&outDF$Trt=="diff"] <- sqrt((outDF$CPFLIT[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                       outDF$CPFLIT[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/2)
    
    outDF$CPFLIT[outDF$Group=="mean"&outDF$Trt=="pct_diff"] <- (mean(cpDF$leaflitter[cpDF$Ring%in%c(1,4,5)], na.rm=T) - mean(cpDF$leaflitter[cpDF$Ring%in%c(2,3,6)], na.rm=T) ) / mean(cpDF$leaflitter[cpDF$Ring%in%c(2,3,6)], na.rm=T) * 100
    outDF$CPFLIT[outDF$Group=="sd"&outDF$Trt=="pct_diff"] <- sqrt((outDF$CPFLIT[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+outDF$CPFLIT[outDF$Group=="sd"&outDF$Trt=="aCO2"]^2+
                                                                       outDF$CPFLIT[outDF$Group=="sd"&outDF$Trt=="eCO2"]^2)/3)/mean(cpDF$leaflitter[cpDF$Ring%in%c(2,3,6)], na.rm=T)*100 
    
    

    return(outDF)
    
}
