plot_annual_rainfall_deficit <- function (fixDF, varDF, out.dir) {
    
    
    ### plot varDF rainfall and the deficit
    fixDF <- fixDF[,c("ModName", "YEAR", "PREC")]
    varDF <- varDF[,c("ModName", "YEAR", "PREC")]
    
    ### merge
    myDF <- merge(fixDF, varDF, by=c("ModName", "YEAR"))
    colnames(myDF) <- c("ModName", "YEAR", "FIX", "VAR")
    
    myDF <- myDF[myDF$YEAR <= 2019,]
    
    ### deficit
    myDF$Deficit <- myDF$FIX - myDF$VAR
    
    
    ### plotting
    #p1 <- ggplot(myDF, aes(YEAR, Deficit, group=ModName))+
    #    geom_bar(stat="identity", aes(fill=ModName),
    #             position=position_dodge()); p1
    #
    #p2 <- ggplot(myDF, aes(YEAR, FIX, group=ModName))+
    #    geom_bar(stat="identity",  aes(fill=ModName),
    #             position=position_dodge()); p2
    #
    #p3 <- ggplot(myDF, aes(YEAR, VAR, group=ModName))+
    #    geom_bar(stat="identity",  aes(fill=ModName),
    #             position=position_dodge()); p3
    
    
    
    #### merge deficit and var rainfall
    plotDF <- myDF
    plotDF$FIX <- NULL
    plotDF <- reshape::melt(plotDF, id.var=c("ModName", "YEAR"))
    plotDF <- plotDF[plotDF$ModName=="A_GDAYP",]
    
    
    p1 <- ggplot(plotDF, aes(YEAR, value, group=variable))+
        geom_bar(stat="identity", aes(fill=variable),
                 position="stack")+
        xlab("")+
        ylab("Rainfall (mm)")+
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
        scale_fill_manual(name="",
                          values=c("Deficit"="orange",
                                   "VAR"="blue1"),
                          labels=c("Deficit"="Deficit",
                                   "VAR"="Variable rainfall"))
    
    
    pdf(paste0(out.dir, "/annual_rainfall.pdf"), width=6, height=6)
    plot(p1)
    dev.off()
    
    
    
    
    
    
    
}
