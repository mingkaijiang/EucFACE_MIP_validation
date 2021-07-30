plot_taylor_diagram <- function() {
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/obs_var_output")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0(out.dir, "/MIP_obs_var_amb_daily.rds"))
    eleDF <- readRDS(paste0(out.dir, "/MIP_obs_var_ele_daily.rds"))
    
    ### ignore NAs
    ambDF[ambDF<=-999] <- NA
    eleDF[eleDF<=-999] <- NA
    
    
    ambDF <- ambDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI")]
    
    eleDF <- eleDF[,c("YEAR", "DOY", "Date", "ModName", "GPP", "LAI")]
    
    
    ambDF3 <- summaryBy(GPP~YEAR+ModName, FUN=sum,
                        na.rm=T, keep.names=T, data=ambDF)
    
    eleDF3 <- summaryBy(GPP~YEAR+ModName, FUN=sum,
                        na.rm=T, keep.names=T, data=eleDF)
    
    
    
    #### prepare taylor diagram for GPP
    maespaDF <- read.csv("simulation_output/maespa.year.ring.csv")
    
    mambDF <- summaryBy(GPP.sum.400~year, data=maespaDF[maespaDF$Ring%in%c("R2","R3","R6"),],
                       FUN=mean, keep.names=T, na.rm=T)
    meleDF <- summaryBy(GPP.sum.550~year, data=maespaDF[maespaDF$Ring%in%c("R1","R4","R5"),],
                        FUN=mean, keep.names=T, na.rm=T)
    
    colnames(mambDF) <- colnames(meleDF) <- c("YEAR", "GPP")
    
    ambDF3 <- subset(ambDF3, YEAR<2017&YEAR>2012)
    eleDF3 <- subset(eleDF3, YEAR<2017&YEAR>2012)
    
    tmpDF1 <- ambDF3$GPP[ambDF3$ModName=="A_CABLP"]
    tmpDF2 <- ambDF3$GPP[ambDF3$ModName=="B_GDAYP"]
    tmpDF3 <- ambDF3$GPP[ambDF3$ModName=="C_LPJGP"]
    tmpDF4 <- ambDF3$GPP[ambDF3$ModName=="D_OCHDP"]
    tmpDF5 <- ambDF3$GPP[ambDF3$ModName=="E_QUINC"]
    #tmpDF6 <- ambDF3$GPP[ambDF3$ModName=="F_ELMXX"]
    tmpDF7 <- ambDF3$GPP[ambDF3$ModName=="G_OCHDX"]
    tmpDF8 <- ambDF3$GPP[ambDF3$ModName=="H_QUJSM"]
    tmpDF9 <- ambDF3$GPP[ambDF3$ModName=="I_GDAYN"]
    tmpDF10 <- ambDF3$GPP[ambDF3$ModName=="J_LPJGN"]
    tmpDF11 <- ambDF3$GPP[ambDF3$ModName=="K_CABLP-VD"]
    tmpDF12 <- ambDF3$GPP[ambDF3$ModName=="L_LPJGP-VD"]
    
    taylor.diagram(mambDF$GPP,tmpDF1, col=col.values[1])    
    taylor.diagram(mambDF$GPP,tmpDF2, col=col.values[2], add=T)   
    taylor.diagram(mambDF$GPP,tmpDF3, col=col.values[3], add=T)   
    taylor.diagram(mambDF$GPP,tmpDF4, col=col.values[4], add=T)   
    taylor.diagram(mambDF$GPP,tmpDF5, col=col.values[5], add=T)   
    
    
    
    
    # get approximate legend position
    #lpos<-1.5*sd(ref)
    # add a legend
    #legend(lpos,lpos,legend=c("Better","Worse"),pch=19,col=c("red","blue"))
    
    # now restore par values
    #par(oldpar)
    ## show the "all correlation" display
    #taylor.diagram(maeDF$GPP.sum.400,tmpDF1,pos.cor=FALSE)
    #
    #for (i in 2:length(mod.list)){
    #    tmpDF2 <- ambDF3$GPP[ambDF3$ModName==mod.list[i]]
    #    taylor.diagram(maeDF$GPP.sum.400,tmpDF2, col=col.values[i], add=T)   
    #}
    

    
    
}