compare_data_model_traceability <- function() {
    ### step 1: prepare NPP at ambient CO2 as input
    NPP.aCO2 <- obsDF$GPP.mean[4] - obsDF$Ra.mean[4]
    NPP.eCO2 <- eco2DF$GPP.mean[4] - eco2DF$Ra.mean[4]
    
    ### step 2: run traceability for CABLE at aCO2 and eCO2 NPP
    cbDF1 <- traceability_framework_CABLE(NPP=NPP.aCO2,
                                          CN.couple="CN model")
    
    cbDF2 <- traceability_framework_CABLE(NPP=NPP.eCO2,
                                          CN.couple="CN model")
    
    ### step 3: run traceability for EucFACE data, based on treatment mean
    inDF <- prepare_coef_for_traceability_treatment_mean()
    
    euc.aco2 <- traceability_framework_EucFACE_data(NPP=NPP.aCO2,
                                        coefDF=inDF[1,])
    
    euc.eco2 <- traceability_framework_EucFACE_data(NPP=NPP.eCO2,
                                        coefDF=inDF[2,])
    
    
    ### step 4: run traceability for EucFACE data, based on individual ring
    inDF <- prepare_coef_for_traceability()
    
    ## aCO2
    r2 <- traceability_framework_EucFACE_data(NPP=(obsDF$GPP.mean[1] - obsDF$Ra.mean[1]),
                                        coefDF=inDF[1,])
    
    r3 <- traceability_framework_EucFACE_data(NPP=(obsDF$GPP.mean[2] - obsDF$Ra.mean[2]),
                                        coefDF=inDF[2,])
    
    r6 <- traceability_framework_EucFACE_data(NPP=(obsDF$GPP.mean[3] - obsDF$Ra.mean[3]),
                                        coefDF=inDF[3,])
    
    ## eCO2 
    r1 <- traceability_framework_EucFACE_data(NPP=(eco2DF$GPP.mean[1] - eco2DF$Ra.mean[1]),
                                        coefDF=inDF[4,])
    
    r4 <- traceability_framework_EucFACE_data(NPP=(eco2DF$GPP.mean[2] - eco2DF$Ra.mean[2]),
                                        coefDF=inDF[5,])
    
    r5 <- traceability_framework_EucFACE_data(NPP=(eco2DF$GPP.mean[3] - eco2DF$Ra.mean[3]),
                                        coefDF=inDF[6,])
    
    ### step 5: combine the output
    outDF <- as.data.frame(rbind(cbDF1, cbDF2, euc.aco2, euc.eco2,
                                 r2, r3, r6, r1, r4, r5))
    
    outDF$Source <- c("CABL.aCO2", "CABL.eCO2",
                      "Euc.aCO2", "Euc.eCO2",
                      "R2", "R3", "R6", "R1", "R4", "R5")
    
    ### step6: prepare plotDF
    plotDF <- outDF[1:4,]
    plotDF$Trt <- c("amb", "ele", "amb", "ele")
    plotDF$Source <- c("CABL", "CABL", "Data", "Data")
    plotDF$tot_C.sd <- NA
    plotDF$tot_tau.sd <- NA
    
    subDF <- outDF[5:10,]
    subDF$Trt <- c("amb", "amb", "amb", "ele", "ele", "ele")
    smDF <- summaryBy(tot_C+tot_tau~Trt, FUN=c(mean,sd), data=subDF,keep.names=T)
    
    plotDF$tot_C[plotDF$Trt=="amb"&plotDF$Source=="Data"] <- smDF$tot_C.mean[smDF$Trt=="amb"]
    plotDF$tot_C[plotDF$Trt=="ele"&plotDF$Source=="Data"] <- smDF$tot_C.mean[smDF$Trt=="ele"]
    
    plotDF$tot_C.sd[plotDF$Trt=="amb"&plotDF$Source=="Data"] <- smDF$tot_C.sd[smDF$Trt=="amb"]
    plotDF$tot_C.sd[plotDF$Trt=="ele"&plotDF$Source=="Data"] <- smDF$tot_C.sd[smDF$Trt=="ele"]
    
    plotDF$tot_tau[plotDF$Trt=="amb"&plotDF$Source=="Data"] <- smDF$tot_tau.mean[smDF$Trt=="amb"]
    plotDF$tot_tau[plotDF$Trt=="ele"&plotDF$Source=="Data"] <- smDF$tot_tau.mean[smDF$Trt=="ele"]
    
    plotDF$tot_tau.sd[plotDF$Trt=="amb"&plotDF$Source=="Data"] <- smDF$tot_tau.sd[smDF$Trt=="amb"]
    plotDF$tot_tau.sd[plotDF$Trt=="ele"&plotDF$Source=="Data"] <- smDF$tot_tau.sd[smDF$Trt=="ele"]
    
    ### step 7: make plot
    ### make the bar plot
    p1 <- ggplot(plotDF,
                 aes(x=Source, y=tot_C, group=Trt)) + 
        geom_errorbar(aes(x=Source, ymin=tot_C-tot_C.sd, ymax=tot_C+tot_C.sd), 
                      position = position_dodge(0.6), width=0.2)+
        geom_point(aes(x=Source, y=tot_C,fill=Trt), 
                   size=4, shape = 21, position = position_dodge(0.6))+
        xlab("") + ylab("Ecosystem carbon (kg C)")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_y_continuous(limits=c(5, 25), 
                           breaks=c(5, 10, 15, 20, 25),
                           labels=c(5, 10, 15, 20, 25))+
        scale_fill_manual(values=c("amb"="blue3", "ele"="red3"))+
        guides(fill = guide_legend(override.aes = list(shape=21)))
    
    p2 <- ggplot(plotDF,
                 aes(x=Source, y=tot_tau, group=Trt)) + 
        geom_errorbar(aes(x=Source, ymin=tot_tau-tot_tau.sd, ymax=tot_tau+tot_tau.sd), 
                      position = position_dodge(0.6), width=0.2)+
        geom_point(aes(x=Source, y=tot_tau,fill=Trt), 
                   size=4, shape = 21, position = position_dodge(0.6))+
        xlab("") + ylab("Carbon residence time (yr)")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=16),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_y_continuous(limits=c(5, 25), 
                           breaks=c(5, 10, 15, 20, 25),
                           labels=c(5, 10, 15, 20, 25))+
        scale_fill_manual(values=c("amb"="blue3", "ele"="red3"))+
        guides(fill = guide_legend(override.aes = list(shape=21)))
    
    legend_shared <- get_legend(p1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    ### output
    pdf("output/traceability_data_model_comparison.pdf", width=4, height=10)
    plot_grid(p1, p2, legend_shared, 
              labels=c("A","B",""),
              ncol=1, rel_heights=c(1,1,0.3))
    dev.off()    
    
    
}
