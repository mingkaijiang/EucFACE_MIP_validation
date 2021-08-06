trace_fate_of_carbon_MIP_plot <- function() {
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/obs_var_output")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0(out.dir, "/MIP_obs_var_amb_annual.rds"))
    eleDF <- readRDS(paste0(out.dir, "/MIP_obs_var_ele_annual.rds"))
    
    
    ### calculate 4-yr means in the simulation datasets
    ambDF <- subset(ambDF, YEAR>2012 & YEAR<2017)
    eleDF <- subset(eleDF, YEAR>2012 & YEAR<2017)
    
    d<-dim(ambDF)[2]
    
    ### summaryby
    co2DF <- ambDF
    co2DF[,3:d] <- eleDF[,3:d]-ambDF[,3:d]
    
    ### extract only a subset of variables
    outDF <- co2DF[,c("ModName", "YEAR", 
                      "GPP", 
                      "RAU", "RHET", "CVOC",
                      "CGL", "CGW", "CGFR", "CGCR", "CEX", "CREPR", 
                      "CLITIN", "CWLIN", "CFRLIN", "CCRLIN", 
                      "CL", "CW", "CFR", "CCR", "CSOIL", "CFLIT", "CSTOR",
                      "deltaCL", "deltaCW", "deltaCFR",
                      "deltaCCR", "deltaCSOIL", "deltaCFLIT", "deltaCSTOR")]
    
    d <- dim(outDF)[2]
    mod.list <- unique(outDF$ModName)
    nmod <- length(mod.list)
    
    longDF <- reshape2::melt(outDF, id.vars = c("ModName", "YEAR"))
    
    sumDF <- summaryBy(value~ModName+variable, FUN=c(mean, sd),
                      data=longDF, na.rm=T, keep.names=T)
    
    
    #### exclude all pools
    sumDF$Category <- rep(c(rep("gpp", 1),  
                           rep("resp", 3), 
                           rep("prod", 6),
                           rep("litter", 4),
                           rep("pool", 7), 
                           rep("change_in_pool", 7)), 12)
    
    
    sumDF$conf_low <- sumDF$value.mean - sumDF$value.sd
    sumDF$conf_high <- sumDF$value.mean + sumDF$value.sd
    
    ### Subset GPP, NPP, change in pools, and out fluxes
    plotDF1 <- subset(sumDF, variable %in% c("ModName", 
                                             "GPP", 
                                             "RAU", "RHET", "CVOC",
                                             "CGL", "CGW", "CGFR", "CGCR", "CEX", "CREPR", 
                                             "deltaCL", "deltaCW", "deltaCFR",
                                             "deltaCCR", "deltaCSOIL", "deltaCFLIT", "deltaCSTOR"))
    
    ### Add plot category
    plotDF1$plot.cat[plotDF1$Category=="gpp"] <- "Influxes"
    plotDF1$plot.cat[plotDF1$Category=="change_in_pool"] <- "Change_in_pools"
    plotDF1$plot.cat[plotDF1$Category=="resp"] <- "Outfluxes"
    plotDF1$plot.cat[plotDF1$Category=="prod"] <- "NPP"

    ### obtain overall fluxes, i.e. influx, outflux, change in pools and npp sums
    plotDF2 <- summaryBy(value.mean~plot.cat+ModName, data=plotDF1,
                         FUN=sum, keep.names=T, na.rm=T)
    
    ### obtain their sd
    for (i in mod.list) {
        for (j in c("Influxes", "Change_in_pools", "Outfluxes", "NPP")) {
            tmpDF <- plotDF1$value.sd[plotDF1$ModName==i&plotDF1$plot.cat==j]
            l <- length(tmpDF)
            val <- sqrt(sum(tmpDF^2, na.rm=T)/l)
            plotDF2$value.sd[plotDF2$ModName==i&plotDF2$plot.cat==j] <- val
            
        }
    }
    
    ### Assign plot cat 2
    plotDF2$plot.cat2[plotDF2$plot.cat=="Influxes"] <- "A"
    plotDF2$plot.cat2[plotDF2$plot.cat=="NPP"] <- "B"
    plotDF2$plot.cat2[plotDF2$plot.cat=="Outfluxes"] <- "C"
    plotDF2$plot.cat2[plotDF2$plot.cat=="Change_in_pools"] <- "C"
    
    ### need to add Ra as B
    tmpDF <- plotDF1[plotDF1$variable=="RAU",]
    tmpDF$plot.cat <- "Total_Ra"
    tmpDF$plot.cat2 <- "B"
    tmpDF <- tmpDF[,c("plot.cat", "ModName", "value.mean", "value.sd", "plot.cat2")]
    
    plotDF2 <- rbind(plotDF2, tmpDF)
    
    ### next to include individual fluxes on NPP
    tmpDF <- plotDF1[plotDF1$plot.cat=="NPP",]
    tmpDF$plot.cat <- tmpDF$variable
    tmpDF$plot.cat2 <- "D"
    tmpDF <- tmpDF[,c("plot.cat", "ModName", "value.mean", "value.sd", "plot.cat2")]
    
    plotDF2 <- rbind(plotDF2, tmpDF)
    
    
    ### next to include individual fluxes on outfluxes
    tmpDF <- plotDF1[plotDF1$plot.cat=="Outfluxes",]
    tmpDF$plot.cat <- tmpDF$variable
    tmpDF$plot.cat2 <- "E"
    tmpDF <- tmpDF[,c("plot.cat", "ModName", "value.mean", "value.sd", "plot.cat2")]
    
    plotDF2 <- rbind(plotDF2, tmpDF)
    
    
    ### next to include individual fluxes on change in pools
    tmpDF <- plotDF1[plotDF1$plot.cat=="Change_in_pools",]
    tmpDF$plot.cat <- tmpDF$variable
    tmpDF$plot.cat2 <- "F"
    tmpDF <- tmpDF[,c("plot.cat", "ModName", "value.mean", "value.sd", "plot.cat2")]
    
    plotDF2 <- rbind(plotDF2, tmpDF)
    
    
    ### Order plot DF
    plotDF2 <- plotDF2[order(plotDF2$plot.cat2),]
    
    
    library(viridis)
    library(RColorBrewer)
    
    ## gpp
    A.col.list <- viridis(6)[1] 
    
    # 2nd column bar - NPP + Ra 
    B.col.list <- viridis(6)[3:4] 
    
    # 3rd column bar - Change in pools + all R
    C.col.list <- viridis(6)[5:6] 
    
    ## NPP
    D.col.list <- brewer.pal(6,"Greens")[1:6]
    
    ## R
    E.col.list <- brewer.pal(6,"YlOrRd")[4:6]
    
    ### Change in pools
    F.col.list <- brewer.pal(7,"Blues")[1:7]
    
    
    ### Combine all color list
    col.list2 <- c("Influxes"=A.col.list[1],                    
                   "NPP"=B.col.list[1],   
                   "Total_Ra"=B.col.list[2],      
                   "Change_in_pools"=C.col.list[1],   
                   "Outfluxes"=C.col.list[2],
                   "CGL"=D.col.list[1],                
                   "CGW"=D.col.list[2],
                   "CGFR"=D.col.list[3],           
                   "CGCR"=D.col.list[4],
                   "CEX"=D.col.list[5], 
                   "CREPR"=D.col.list[6], 
                   "RAU"=E.col.list[1],           
                   "RHET"=E.col.list[2],           
                   "CVOC"=E.col.list[3],
                   "deltaCL"=F.col.list[1],        
                   "deltaCW"=F.col.list[2],
                   "deltaCFR"=F.col.list[3],    
                   "deltaCCR"=F.col.list[4],
                   "deltaCSOIL"=F.col.list[5],
                   "deltaCFLIT"=F.col.list[6],
                   "deltaCSTOR"=F.col.list[7])  
    
    
    # y label
    y.lab2 <- c("Influxes"=expression(GPP[o]),                    # 1
                "NPP"="NPP",                                      # 3
                "Total_Ra"=expression(R[a]),                      # 5
                "Change_in_pools"=expression(Delta*C[pools]),
                "Outfluxes"="R",                              # 4
                "CGL"=expression(NPP[ol]),                # 7
                "CGW"=expression(NPP[stem]),                # 11
                "CGFR"=expression(NPP[froot]),           
                "CGCR"=expression(NPP[croot]),               
                "CEX"=expression(NPP[exud]),          
                "CREPR"=expression(C[rprod]),           
                "RAU"=expression(R[au]),           
                "RHET"=expression(R[het]),      
                "CVOC"=expression(R[voc]),      
                "deltaCL"=expression(Delta*C[leaf]),        
                "deltaCW"=expression(Delta*C[stem]),         
                "deltaCFR"=expression(Delta*C[froot]),    
                "deltaCCR"=expression(Delta*C[croot]),
                "deltaCSOIL"=expression(Delta*C[soil]),
                "deltaCFLIT"=expression(Delta*C[lit]),
                "deltaCSTOR"=expression(Delta*C[store]))
    
    
    require(grid)
    require(cowplot)
    
    for (i in mod.list) {
        myDF <- plotDF2[plotDF2$ModName==i,]
        
        ### split the groups and make separate plots
        plotDF.sub1 <- subset(myDF, plot.cat2%in%c("A", "B", "C"))
        
        plotDF.sub2 <- subset(myDF, plot.cat2=="D")

        plotDF.sub3 <- subset(myDF, plot.cat2=="E")

        plotDF.sub4 <- subset(myDF, plot.cat2=="F")

        
        
        ### make plots
        p1 <- ggplot(plotDF.sub1,
                     aes(plot.cat2, value.mean)) +  
            geom_hline(yintercept=0)+
            geom_bar(stat = "identity", aes(fill=plot.cat),
                     position="stack") +
            #geom_errorbar(data=confDF.sub1, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
            #              width=0.1, size=0.6, color="black") + 
            #geom_point(data=confDF.sub1, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
            xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
            scale_x_discrete(labels=c("GPP", 
                                      expression(paste("NPP+", R[a])),
                                      expression(paste("R+",Delta*C[pools]))))+
            scale_fill_manual(name="", 
                              breaks = plotDF2$plot.cat,
                              values = col.list2,
                              labels=y.lab2) +
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.title.x = element_text(size=16), 
                  axis.text.x = element_text(size=20),
                  axis.text.y=element_text(size=14),
                  axis.title.y=element_text(size=20),
                  legend.text=element_text(size=18),
                  legend.title=element_text(size=16),
                  panel.grid.major=element_blank(),
                  legend.position="bottom",
                  legend.text.align=0)+
            scale_y_continuous(limits=c(-205, 600), 
                               breaks=c(-200, -100, 0, 100, 200, 400, 600),
                               labels=c(-200, -100, 0, 100, 200, 400, 600))+
            guides(fill=guide_legend(ncol=3),legend.justification = c(0, 1))+
            annotate(geom="text", x=0.6, y=500, label="a", size=7)
        
        
        p2 <- ggplot(plotDF.sub2,
                     aes(plot.cat2, value.mean)) +  
            geom_hline(yintercept=0)+
            geom_bar(stat = "identity", aes(fill=plot.cat),
                     position="stack") +
            #geom_errorbar(data=confDF.sub2, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
            #              width=0.1, size=0.6, color="black") + 
            #geom_point(data=confDF.sub2, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
            xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
            scale_x_discrete(labels=c("NPP", 
                                      "R",
                                      expression(Delta*C[pools])))+
            scale_fill_manual(name="", 
                              breaks = plotDF2$plot.cat,
                              values = col.list2,
                              labels=y.lab2) +
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.title.x = element_text(size=16), 
                  axis.text.x = element_text(size=20),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=18),
                  legend.title=element_text(size=16),
                  panel.grid.major=element_blank(),
                  legend.position="bottom",
                  legend.text.align=0)+
            scale_y_continuous(limits=c(-205, 600), 
                               breaks=c(-200, -100, 0, 100, 200, 400, 600),
                               labels=c(-200, -100, 0, 100, 200, 400, 600))+
            guides(fill=guide_legend(ncol=2))+
            annotate(geom="text", x=0.5, y=500, label="b", size=7)
        
        
        p3 <- ggplot(plotDF.sub3,
                     aes(plot.cat2, value.mean)) +  
            geom_hline(yintercept=0)+
            geom_bar(stat = "identity", aes(fill=plot.cat),
                     position="stack") +
            #geom_errorbar(data=confDF.sub3, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
            #              width=0.1, size=0.6, color="black") + 
            #geom_point(data=confDF.sub3, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
            xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
            scale_x_discrete(labels=c("R"))+
            scale_fill_manual(name="", 
                              breaks = plotDF2$plot.cat,
                              values = col.list2,
                              labels=y.lab2) +
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.title.x = element_text(size=16), 
                  axis.text.x = element_text(size=20),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=18),
                  legend.title=element_text(size=16),
                  panel.grid.major=element_blank(),
                  legend.position="bottom",
                  legend.text.align=0)+
            scale_y_continuous(limits=c(-205, 600), 
                               breaks=c(-200, -100, 0, 100, 200, 400, 600),
                               labels=c(-200, -100, 0, 100, 200, 400, 600))+
            guides(fill=guide_legend(ncol=2, nrow=3))+
            annotate(geom="text",x=0.5, y=500, label="c", size=7)
        
        
        p4 <- ggplot(plotDF.sub4,
                     aes(plot.cat2, value.mean)) +  
            geom_hline(yintercept=0)+
            geom_bar(stat = "identity", aes(fill=plot.cat),
                     position="stack") +
            #geom_errorbar(data=confDF.sub4, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
            #              width=0.1, size=0.6, color="black") + 
            #geom_point(data=confDF.sub4, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
            xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
            scale_x_discrete(labels=c(expression(Delta*C[pools])))+
            scale_fill_manual(name="", 
                              breaks = plotDF2$plot.cat,
                              values = col.list2,
                              labels=y.lab2) +
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.title.x = element_text(size=16), 
                  axis.text.x = element_text(size=20),
                  axis.text.y=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text=element_text(size=18),
                  legend.title=element_text(size=16),
                  panel.grid.major=element_blank(),
                  legend.position="bottom",
                  legend.text.align=0)+
            scale_y_continuous(limits=c(-205, 600), 
                               breaks=c(-200, -100, 0, 100, 200, 400, 600),
                               labels=c(-200, -100, 0, 100, 200, 400, 600))+
            guides(fill=guide_legend(ncol=2))+
            annotate(geom="text", x=0.5, y=500, label="d", size=7)
        
        ### Plotting
        pdf(paste0(out.dir, "/MIP_fate_of_carbon_", i, ".pdf"), width=16, height=6)
        #plot_grid(p1, p2, p3, p4, labels="", ncol=4, align="h", axis="l",
        #          rel_widths=c(1.3, 0.5, 0.5, 0.5))
        grid.arrange(p1, p2, p3, p4, 
                     widths=c(1.3, 0.5, 0.5, 0.5),
                     #heights=c(1,1.2,1.2,1.4),
                     ncol = 4)
        dev.off()
        
    }
    
    
    
    
    
    #  
}