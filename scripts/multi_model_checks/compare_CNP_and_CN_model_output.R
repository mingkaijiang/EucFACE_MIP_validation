compare_CNP_and_CN_model_output <- function() {
    
    
    ##################################################################
    #### Set up basics
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/obs_var_output")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0(out.dir, "/MIP_obs_var_amb_annual.rds"))
    eleDF <- readRDS(paste0(out.dir, "/MIP_obs_var_ele_annual.rds"))
    
    ### select GDAYN, GDAYP, LPJGN, LPJGP model output
    ambDF <- subset(ambDF, ModName%in%c("GDAYN", "GDAYP",
                                        "LPJGN", "LPJGP"))
    
    
    eleDF <- subset(eleDF, ModName%in%c("GDAYN", "GDAYP",
                                        "LPJGN", "LPJGP"))
    
    
    #### calculate 4-yr means in the simulation datasets
    ambDF <- subset(ambDF, YEAR>2012 & YEAR<2017)
    eleDF <- subset(eleDF, YEAR>2012 & YEAR<2017)
    
    d <- dim(ambDF)[2]
    
    ### calculate the effect of P limitation as difference of CNP - CN
    diffDF <- ambDF[ambDF$ModName%in%c("GDAYN", "LPJGN"),]
    
    diffDF$ModName <- gsub("GDAYN", "GDAY", diffDF$ModName)
    diffDF$ModName <- gsub("LPJGN", "LPJG", diffDF$ModName)
    
    diffDF[diffDF$ModName=="GDAY",3:d] <- ambDF[ambDF$ModName=="GDAYP",3:d] - ambDF[ambDF$ModName=="GDAYN",3:d]
    diffDF[diffDF$ModName=="LPJG",3:d] <- ambDF[ambDF$ModName=="LPJGP",3:d] - ambDF[ambDF$ModName=="LPJGN",3:d]
    
    
    ### calculate the effect of P limitation as % difference of (CNP - CN)/CN
    pctdiffDF <- diffDF
    
    pctdiffDF[pctdiffDF$ModName=="GDAY",3:d] <- diffDF[diffDF$ModName=="GDAY",3:d]/ambDF[ambDF$ModName=="GDAYN",3:d] * 100.0
    pctdiffDF[pctdiffDF$ModName=="LPJG",3:d] <- diffDF[diffDF$ModName=="LPJG",3:d]/ambDF[ambDF$ModName=="LPJGN",3:d] * 100.0
    
    
    ### calculate the effect of CO2 effect in real magnitude, in both versions of model
    co2DF <- ambDF
    co2DF[,3:d] <- eleDF[,3:d]-ambDF[,3:d]
    
    
    ### calculate the effect of CO2 effect in %, in both versions of model
    pctco2DF <- co2DF
    pctco2DF[,3:d] <- co2DF[,3:d]/ambDF[,3:d]*100.0
    
    
    ### calculate multi-year means and sds across datasets
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    tmpDF <- rbind(ambDF, eleDF)
    
    myDF1 <- summaryBy(.~ModName+Trt, FUN=c(mean, sd), data=tmpDF,
                       keep.names=T, na.rm=T)
    
    
    myDF2 <- summaryBy(.~ModName, FUN=c(mean, sd), data=diffDF,
                       keep.names=T, na.rm=T)
    
    
    myDF3 <- summaryBy(.~ModName, FUN=c(mean, sd), data=pctdiffDF,
                       keep.names=T, na.rm=T)
    
    
    myDF4 <- summaryBy(.~ModName, FUN=c(mean, sd), data=co2DF,
                       keep.names=T, na.rm=T)
    
    
    myDF5 <- summaryBy(.~ModName, FUN=c(mean, sd), data=pctco2DF,
                       keep.names=T, na.rm=T)
    
    
    ##################################################################
    #### Plotting
    mod.list1 <- c("GDAYN", "GDAYP", "LPJGN", "LPJGP")
    mod.list2 <- c("GDAY", "LPJG")
    
    ### prepare vegetation biomass datasets
    vegDF1 <- data.frame(rep(c("CL", "CW", "CFR", "CCR", "Total"), 8), 
                         rep(c("GDAYN", "GDAYP", "LPJGN", "LPJGP"), each=10), 
                         rep(c("amb", "ele"), each = 5), NA, NA)
    colnames(vegDF1) <- c("Variable", 
                         "Model",
                         "Trt",
                         "meanvalue",
                         "sdvalue")
    
    for (i in c("amb", "ele")) {
        vegDF1$meanvalue[vegDF1$Variable=="CL"&vegDF1$Trt==i] <- myDF1$CL.mean[myDF1$Trt==i]
        vegDF1$sdvalue[vegDF1$Variable=="CL"&vegDF1$Trt==i] <- myDF1$CL.sd[myDF1$Trt==i]
        
        vegDF1$meanvalue[vegDF1$Variable=="CW"&vegDF1$Trt==i] <- myDF1$CW.mean[myDF1$Trt==i]
        vegDF1$sdvalue[vegDF1$Variable=="CW"&vegDF1$Trt==i] <- myDF1$CW.sd[myDF1$Trt==i]
        
        vegDF1$meanvalue[vegDF1$Variable=="CCR"&vegDF1$Trt==i] <- myDF1$CCR.mean[myDF1$Trt==i]
        vegDF1$sdvalue[vegDF1$Variable=="CCR"&vegDF1$Trt==i] <- myDF1$CCR.sd[myDF1$Trt==i]
        
        vegDF1$meanvalue[vegDF1$Variable=="CFR"&vegDF1$Trt==i] <- myDF1$CFR.mean[myDF1$Trt==i]
        vegDF1$sdvalue[vegDF1$Variable=="CFR"&vegDF1$Trt==i] <- myDF1$CFR.sd[myDF1$Trt==i]
    }
    
    for (i in c("amb", "ele")) {
        for (j in c("GDAYN", "GDAYP", "LPJGN", "LPJGP")) {
            
            ### calculate means
            v1 <- sum(vegDF1$meanvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable%in%c("CL", "CW", "CCR", "CFR")],
                      na.rm=T)
            
            ### calculate sd
            v2 <- sqrt(sum(vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CL"]^2,
                            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CW"]^2,
                            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CFR"]^2,
                            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Trt==i&vegDF1$Variable=="CCR"]^2, na.rm=T)/3)
            
            ### assign values
            vegDF1$meanvalue[vegDF1$Model==j&vegDF1$Variable=="Total"&vegDF1$Trt==i] <- v1
            vegDF1$sdvalue[vegDF1$Model==j&vegDF1$Variable=="Total"&vegDF1$Trt==i] <- v2
            
        }
    }
    
    plotDF1 <- subset(vegDF1, Variable%in%c("CL", "CW", "CFR", "CCR") & Trt=="amb")
    plotDF2 <- subset(vegDF1, Variable%in%c("Total") & Trt=="amb")
    
    
    ### Plotting C pools in ambient CO2
    p1 <- ggplot(data=plotDF1, 
                 aes(Model, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable, alpha=Model), 
                 position="stack", col="black") +
        annotate("text", x=2, y=5500, label=("-32.4%"), size=10)+
        annotate("text", x=4, y=2600, label=("-23.0%"), size=10)+
        
        #geom_errorbar(data=plotDF2,
        #              aes(x=Model, 
        #                  ymin=meanvalue-sdvalue, 
        #                  ymax=meanvalue+sdvalue), 
        #              position="dodge", width=1.0) +
        ggtitle("Major ecosystem carbon pools")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Carbon pools (g C " * m^2*")")))+
        scale_x_discrete(limit=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"),
                         label=c("GDAYN","GDAYP", 
                                 "LPJGN","LPJGP"))+
        xlab("")+
        scale_alpha_manual(values=c("GDAYN" = 0.3, "GDAYP" = 1.0,
                            "LPJGN" = 0.3, "LPJGP" = 1.0)); p1
    
    
    
    

    
### end    
}