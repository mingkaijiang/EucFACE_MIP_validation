investigate_gross_mineralization <- function(scenario) {
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario, "/")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_ELE_annual.rds"))
    
    ### subset 
    ambDF <- ambDF[,c("ModName", "YEAR", 
                      "NUP", "NGMIN", "NMIN",
                      "PUP", "PGMIN", "PMIN", "PBIOCHMIN")]
    
    eleDF <- eleDF[,c("ModName", "YEAR", 
                      "NUP", "NGMIN", "NMIN",
                      "PUP", "PGMIN", "PMIN", "PBIOCHMIN")]
    
    ### calculate ratios
    ambDF$NUP_NGMIN <- with(ambDF, NUP/NGMIN)
    ambDF$PUP_PGMIN <- with(ambDF, PUP/(PGMIN+PBIOCHMIN))
    
    eleDF$NUP_NGMIN <- with(eleDF, NUP/NGMIN)
    eleDF$PUP_PGMIN <- with(eleDF, PUP/(PGMIN+PBIOCHMIN))
    
    ambDF$PGMIN_PBIOCHMIN <- with(ambDF, PGMIN+PBIOCHMIN)
    eleDF$PGMIN_PBIOCHMIN <- with(eleDF, PGMIN+PBIOCHMIN)
    
    ### add label
    ambDF$Trt <- "amb"
    eleDF$Trt <- "ele"
    
    ### merge
    mgDF <- rbind(ambDF, eleDF)
    
    myDF <- summaryBy(.~ModName+Trt, FUN=c(mean, sd), data=mgDF,
                      keep.names=T, na.rm=T)
    
    ### remove inf numbers
    is.na(myDF) <- sapply(myDF, is.infinite)
    
    
    ### plotting aCO2 and eCO2 comparison
    p1 <- ggplot(data=myDF, 
                 aes(ModName, NUP_NGMIN.mean, group=Trt)) +
        geom_bar(stat = "identity", 
                 aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=NUP_NGMIN.mean-NUP_NGMIN.sd, 
                          ymax=NUP_NGMIN.mean+NUP_NGMIN.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(N[up] * " / " * N[gmin]))+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    
    p2 <- ggplot(data=myDF, 
                 aes(ModName, PUP_PGMIN.mean, group=Trt)) +
        geom_bar(stat = "identity", 
                 aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=PUP_PGMIN.mean-PUP_PGMIN.sd, 
                          ymax=PUP_PGMIN.mean+PUP_PGMIN.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(P[up] * " / (" * P[gmin] * " + " * P[biochem] * ")"))+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    
    p3 <- ggplot(data=myDF, 
                 aes(ModName, NUP.mean, group=Trt)) +
        geom_bar(stat = "identity", 
                 aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=NUP.mean-NUP.sd, 
                          ymax=NUP.mean+NUP.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(N[up] * " (g N " * m^-2 * " " * yr^-1 * ")"))+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    
    p4 <- ggplot(data=myDF, 
                 aes(ModName, PUP.mean, group=Trt)) +
        geom_bar(stat = "identity", 
                 aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=PUP.mean-PUP.sd, 
                          ymax=PUP.mean+PUP.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(P[up] * " (g P " * m^-2 * " " * yr^-1 * ")"))+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    p5 <- ggplot(data=myDF, 
                 aes(ModName, NGMIN.mean, group=Trt)) +
        geom_bar(stat = "identity", 
                 aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=NGMIN.mean-NGMIN.sd, 
                          ymax=NGMIN.mean+NGMIN.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(N[gmin] * " (g N " * m^-2 * " " * yr^-1 * ")"))+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    
    p6 <- ggplot(data=myDF, 
                 aes(ModName, PGMIN_PBIOCHMIN.mean, group=Trt)) +
        geom_bar(stat = "identity", 
                 aes(fill=ModName, alpha=Trt), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=PGMIN_PBIOCHMIN.mean-PGMIN_PBIOCHMIN.sd, 
                          ymax=PGMIN_PBIOCHMIN.mean+PGMIN_PBIOCHMIN.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(P[gmin] * " (g P " * m^-2 * " " * yr^-1 * ")"))+
        scale_alpha_manual(name="Treatment",
                           values=c("amb" = 0.3, 
                                    "ele" = 1.0),
                           label=c("AMB", "ELE"))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    
    
    pdf(paste0(out.dir, "/gross_mineralization_fluxes_", scenario, ".pdf"), 
        width=14, height=10)
    
    grid.arrange(p1, p2, p3, 
                 p4, p5, p6, nrow=3, ncol=2)
    dev.off()
    
    
    
    ### merge
    diffDF <- ambDF
    diffDF[,2:10] <- eleDF[,2:10]/ambDF[,2:10]
    
    myDF <- summaryBy(.~ModName, FUN=c(mean, sd), data=diffDF,
                      keep.names=T, na.rm=T)
    
    ### remove inf numbers
    is.na(myDF) <- sapply(myDF, is.infinite)
    
    
    ### plotting aCO2 and eCO2 comparison
    p1 <- ggplot(data=myDF, 
                 aes(ModName, NUP_NGMIN.mean)) +
        geom_hline(yintercept=1, lty=1)+
        geom_bar(stat = "identity", 
                 aes(fill=ModName), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=NUP_NGMIN.mean-NUP_NGMIN.sd, 
                          ymax=NUP_NGMIN.mean+NUP_NGMIN.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * "effect on " * N[up] * " / " * N[gmin]))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    
    p2 <- ggplot(data=myDF, 
                 aes(ModName, PUP_PGMIN.mean)) +
        geom_hline(yintercept=1, lty=1)+
        geom_bar(stat = "identity", 
                 aes(fill=ModName), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=PUP_PGMIN.mean-PUP_PGMIN.sd, 
                          ymax=PUP_PGMIN.mean+PUP_PGMIN.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * "effect on " * P[up] * " / (" * P[gmin] * " + " * P[biochem] * ")"))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    
    p3 <- ggplot(data=myDF, 
                 aes(ModName, NUP.mean)) +
        geom_hline(yintercept=1, lty=1)+
        geom_bar(stat = "identity", 
                 aes(fill=ModName), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=NUP.mean-NUP.sd, 
                          ymax=NUP.mean+NUP.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * "effect on " * N[up]))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    
    p4 <- ggplot(data=myDF, 
                 aes(ModName, PUP.mean)) +
        geom_hline(yintercept=1, lty=1)+
        geom_bar(stat = "identity", 
                 aes(fill=ModName), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=PUP.mean-PUP.sd, 
                          ymax=PUP.mean+PUP.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * "effect on " * P[up]))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    p5 <- ggplot(data=myDF, 
                 aes(ModName, NGMIN.mean)) +
        geom_hline(yintercept=1, lty=1)+
        geom_bar(stat = "identity", 
                 aes(fill=ModName), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=NGMIN.mean-NGMIN.sd, 
                          ymax=NGMIN.mean+NGMIN.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * "effect on " * N[gmin]))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    
    p6 <- ggplot(data=myDF, 
                 aes(ModName, PGMIN_PBIOCHMIN.mean)) +
        geom_hline(yintercept=1, lty=1)+
        geom_bar(stat = "identity", 
                 aes(fill=ModName), 
                 position=position_dodge(), col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        geom_errorbar(aes(x=ModName, ymin=PGMIN_PBIOCHMIN.mean-PGMIN_PBIOCHMIN.sd, 
                          ymax=PGMIN_PBIOCHMIN.mean+PGMIN_PBIOCHMIN.sd), width=0.4,
                      position=position_dodge(width=1)) +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * "effect on " * P[gmin]))+
        scale_fill_manual(name="Model",
                          values=col.values,
                          labels=model.labels)+
        guides(alpha=guide_legend("Treatment"), fill = FALSE)+
        scale_x_discrete(limit=mod.list,
                         label=model.labels)
    
    
    
    pdf(paste0(out.dir, "/gross_mineralization_fluxes_co2_effect_", scenario, ".pdf"), 
        width=14, height=10)
    
    grid.arrange(p1, p2, p3, 
                 p4, p5, p6, nrow=3, ncol=2)
    dev.off()
    
}