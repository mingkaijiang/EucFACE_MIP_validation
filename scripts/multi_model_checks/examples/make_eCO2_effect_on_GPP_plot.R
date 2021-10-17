make_eCO2_effect_on_GPP_plot <- function(inDF) {
    
    ######### Plot abs, no interactions, and change in pools
    ### read in the csv file to plot the treatment effect and confidence interval
    myDF <- rbind(inDF$inout, inDF$npp, inDF$pool, inDF$delta_pool)
    
    myDF$diff_mean <- myDF$eCO2 - myDF$aCO2
    myDF$diff_sd <- sqrt((myDF$aCO2_sd^2 + myDF$eCO2_sd^2)/2)
    
    myDF <- myDF[complete.cases(myDF$diff_mean),]
    
    myDF$term <- c("over_gpp", "understorey_gpp", "ch4",
                   "over_leaf_respiration", "wood_respiration", "root_respiration",
                   "understorey_respiration","voc", "herbivory_respiration",
                   "doc", "soil_respiration", "growth_respiration",
                   "leaf_prod", "wood_prod", "fineroot_prod",
                   "intermediate_root_prod", "coarse_root_prod",
                   "other_prod", "understorey_prod", 
                   "understorey_lit", "frass_prod", "herb_consump", 
                   "hetero_respiration", "mycorrhizal_prod", "over_leaf", "wood", "und_aboveground",
                   "fineroot", "intermediate_root", "coarse_root", "litter", "microbe",
                   "soil", "mycorrhizae", "insects", 
                   "delta_leaf_c", "delta_wood_c", "delta_understorey_c",
                   "delta_fineroot_c", "delta_intermediate_root_c", "delta_coarse_root_c",
                   "delta_litter_c","delta_microbial_c","delta_soil_c",
                   "delta_mycorrhizal_c", "delta_insect_c")
    
    #### exclude all pools
    myDF$Category <- c(rep("gpp", 2),  
                       rep("resp", 10), 
                       rep("prod", 7),
                       rep("litter", 2), 
                       rep("prod", 1), 
                       rep("resp", 1), 
                       rep("prod", 1),
                       rep("pool", 11), 
                       rep("change_in_pool", 11))  
    
    ### Drop redundant pools and fluxes
    myDF <- subset(myDF, term != c("understorey_lit"))
    
    ### Drop CWD - confidence interval too large
    myDF <- subset(myDF, term != c("cwd"))
    
    myDF <- subset(myDF, term != "mycorrhizal_prod")
    
    myDF <- myDF[,c("term", "Ring_1", "Ring_2", "Ring_3", "Ring_4", "Ring_5", "Ring_6", 
                    "diff_mean", "diff_sd", "Category")]
    colnames(myDF) <- c("Variable", "R1", "R2", "R3", "R4", "R5", "R6",
                        "effect_size", "sd", "Category")
    myDF$conf_low <- myDF$effect_size - myDF$sd
    myDF$conf_high <- myDF$effect_size + myDF$sd
    
    ### Subset GPP, NPP, change in pools, and out fluxes
    plotDF1 <- subset(myDF, Variable %in% c("over_gpp", "understorey_gpp", "ch4",
                                            "leaf_prod", "other_prod", "wood_prod", "fineroot_prod",
                                            "intermediate_root_prod", "coarse_root_prod",
                                            "understorey_prod", "herb_consump", 
                                            "over_leaf_respiration", "wood_respiration", "root_respiration",
                                            "understorey_respiration", "hetero_respiration","doc", "voc", "growth_respiration",
                                            "delta_leaf_c", "delta_wood_c", "delta_fineroot_c", 
                                            "delta_intermediate_root_c", "delta_coarse_root_c",
                                            "delta_understorey_c", 
                                            "delta_litter_c","delta_soil_c","delta_microbial_c",
                                            "delta_mycorrhizal_c", "delta_litter_c", "delta_insect_c"))
    
    ### Add plot category
    plotDF1$plot.cat[plotDF1$Category=="gpp"] <- "Influxes"
    plotDF1$plot.cat[plotDF1$Category=="change_in_pool"] <- "Change_in_pools"
    plotDF1$plot.cat[plotDF1$Category=="resp"] <- "Outfluxes"
    plotDF1$plot.cat[plotDF1$Category=="prod"] <- "NPP"
    plotDF1$plot.cat[plotDF1$Variable=="ch4"] <- "Influxes"
    
    #plotDF1 <- plotDF1[,c("Variable", "effect_size", "conf_low", "conf_high", "plot.cat", "sd")]
    
    plotDF1$plot.cat[plotDF1$Variable%in%c("root_respiration", "understorey_respiration", "growth_respiration",
                                           "over_leaf_respiration", "wood_respiration", "voc")] <- "Ra"
    plotDF1$plot.cat[plotDF1$Variable=="herb_consump"] <- "NPP"
    
    
    ### Calculate totals of each plot.cat
    plotDF2 <- summaryBy(effect_size+R1+R2+R3+R4+R5+R6~plot.cat, data=plotDF1, FUN=sum, keep.names=T, na.rm=T)
    names(plotDF2)[1] <- "Variable"
    
    plotDF2$aCO2 <- rowMeans(subset(plotDF2, select=c(R2, R3, R6)), na.rm=T)
    plotDF2$aCO2_sd <- rowSds(as.matrix(subset(plotDF2, select=c(R2, R3, R6)), na.rm=T))
    
    plotDF2$eCO2 <- rowMeans(subset(plotDF2, select=c(R1, R4, R5)), na.rm=T)
    plotDF2$eCO2_sd <- rowSds(as.matrix(subset(plotDF2, select=c(R1, R4, R5)), na.rm=T))
    
    plotDF2$sd <- sqrt((plotDF2$aCO2_sd^2 + plotDF2$eCO2_sd^2)/2)
    
    
    ### Total of Ra and Rh and the conf. interval
    tot_out <- plotDF1[plotDF1$Variable%in%c("over_leaf_respiration", "wood_respiration", "root_respiration",
                                             "understorey_respiration", "voc", "doc", "growth_respiration",
                                             "hetero_respiration"),]
    tot_out$plot.cat <- "total_outflux"
    totoutDF <- summaryBy(effect_size+R1+R2+R3+R4+R5+R6~plot.cat, data=tot_out, FUN=sum, keep.names=T, na.rm=T)
    names(totoutDF)[1] <- "Variable"
    
    totoutDF$aCO2 <- rowMeans(subset(totoutDF, select=c(R2, R3, R6)), na.rm=T)
    totoutDF$aCO2_sd <- rowSds(as.matrix(subset(totoutDF, select=c(R2, R3, R6)), na.rm=T))
    
    totoutDF$eCO2 <- rowMeans(subset(totoutDF, select=c(R1, R4, R5)), na.rm=T)
    totoutDF$eCO2_sd <- rowSds(as.matrix(subset(totoutDF, select=c(R1, R4, R5)), na.rm=T))
    
    totoutDF$sd <- sqrt((totoutDF$aCO2_sd^2 + totoutDF$eCO2_sd^2)/2)
    
    plotDF2 <- rbind(plotDF2, totoutDF)
    
    plotDF2$plot.cat <- c("total_change_in_pool", "total_influx", "total_npp", "total_rh", "total_ra", "total_outflux")
    
    plotDF2$conf_low <- plotDF2$effect_size - plotDF2$sd
    plotDF2$conf_high <- plotDF2$effect_size + plotDF2$sd
    
    plotDF2 <- plotDF2[,c("Variable", "effect_size", "conf_low", "conf_high", "sd", "plot.cat")]
    
    ### Combine the plots
    plotDF3 <- plotDF1[,c("Variable", "effect_size", "conf_low", "conf_high", "sd", "plot.cat")]
    plotDF <- rbind(plotDF3, plotDF2[c(1,3,5,6),])
    
    ### Assign plot cat 2
    plotDF$plot.cat2[plotDF$plot.cat=="Influxes"] <- "A"
    plotDF$plot.cat2[plotDF$plot.cat=="total_npp"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="total_ra"] <- "B"
    plotDF$plot.cat2[plotDF$plot.cat=="total_outflux"] <- "C"
    plotDF$plot.cat2[plotDF$plot.cat=="total_change_in_pool"] <- "C"
    plotDF$plot.cat2[plotDF$plot.cat=="NPP"] <- "D"
    plotDF$plot.cat2[plotDF$plot.cat=="Ra"] <- "E"
    plotDF$plot.cat2[plotDF$plot.cat=="Outfluxes"] <- "E"
    plotDF$plot.cat2[plotDF$plot.cat=="Change_in_pools"] <- "F"
    
    
    confDF <- summaryBy(effect_size+sd~plot.cat2, keep.names=T, na.rm=T, data=plotDF, FUN=sum)
    
    ## update A influxes
    influxDF <- subset(plotDF1, plot.cat=="Influxes")
    tmp1 <- colSums(subset(influxDF, select=c(R1, R2, R3, R4, R5, R6)), na.rm=T)
    aC <- mean(c(tmp1[2], tmp1[3], tmp1[6]), na.rm=T)
    eC <- mean(c(tmp1[1], tmp1[4], tmp1[5]), na.rm=T)
    aC_sd <- sd(c(tmp1[2], tmp1[3], tmp1[6]), na.rm=T)
    eC_sd <- sd(c(tmp1[1], tmp1[4], tmp1[5]), na.rm=T)
    
    confDF$sd[confDF$plot.cat2 == "A"] <- sqrt((aC_sd^2+eC_sd^2)/2)
    
    ## update D NPP
    confDF$sd[confDF$plot.cat2 == "D"] <- plotDF2$sd[plotDF2$Variable=="NPP"]
    confDF$conf_low[confDF$plot.cat2 == "D"] <- plotDF2$conf_low[plotDF2$Variable=="NPP"]
    confDF$conf_high[confDF$plot.cat2 == "D"] <- plotDF2$conf_high[plotDF2$Variable=="NPP"]
    
    ## update E total outfluxes
    confDF$sd[confDF$plot.cat2 == "E"] <- plotDF2$sd[plotDF2$Variable=="total_outflux"]
    confDF$conf_low[confDF$plot.cat2 == "E"] <- plotDF2$conf_low[plotDF2$Variable=="total_outflux"]
    confDF$conf_high[confDF$plot.cat2 == "E"] <- plotDF2$conf_high[plotDF2$Variable=="total_outflux"]
    
    ## update F change in pools
    confDF$sd[confDF$plot.cat2 == "F"] <- plotDF2$sd[plotDF2$Variable=="Change_in_pools"]
    confDF$conf_low[confDF$plot.cat2 == "F"] <- plotDF2$conf_low[plotDF2$Variable=="Change_in_pools"]
    confDF$conf_high[confDF$plot.cat2 == "F"] <- plotDF2$conf_high[plotDF2$Variable=="Change_in_pools"]
    
    ## update B NPP + Ra
    tmp2 <- plotDF1[plotDF1$Variable%in%c("leaf_prod", "wood_prod", "fineroot_prod", "intermediate_root_prod", "coarse_root_prod",
                                          "other_prod", "understorey_prod", "herb_consump", "over_leaf_respiration",
                                          "wood_respiration", "root_respiration", "understorey_respiration",
                                          "voc", "growth_respiration"), ]
    tmp1 <- colSums(subset(tmp2, select=c(R1, R2, R3, R4, R5, R6)), na.rm=T)
    aC <- mean(c(tmp1[2], tmp1[3], tmp1[6]), na.rm=T)
    eC <- mean(c(tmp1[1], tmp1[4], tmp1[5]), na.rm=T)
    aC_sd <- sd(c(tmp1[2], tmp1[3], tmp1[6]), na.rm=T)
    eC_sd <- sd(c(tmp1[1], tmp1[4], tmp1[5]), na.rm=T)
    
    confDF$sd[confDF$plot.cat2 == "B"] <- sqrt((aC_sd^2+eC_sd^2)/2)
    
    
    ## update C change in pool + total outfluxes
    
    tmp2 <- plotDF1[plotDF1$Variable%in%c("delta_leaf_c","delta_wood_c","delta_understorey_c",
                                          "delta_fineroot_c","delta_intermediate_root_c","delta_coarse_root_c",
                                          "delta_litter_c",
                                          "delta_microbial_c","delta_soil_c","delta_mycorrhizal_c",
                                          "delta_insect_c","over_leaf_respiration","wood_respiration",
                                          "root_respiration","understorey_respiration","voc",
                                          "growth_respiration","doc","hetero_respiration"), ]
    tmp1 <- colSums(subset(tmp2, select=c(R1, R2, R3, R4, R5, R6)), na.rm=T)
    aC <- mean(c(tmp1[2], tmp1[3], tmp1[6]), na.rm=T)
    eC <- mean(c(tmp1[1], tmp1[4], tmp1[5]), na.rm=T)
    aC_sd <- sd(c(tmp1[2], tmp1[3], tmp1[6]), na.rm=T)
    eC_sd <- sd(c(tmp1[1], tmp1[4], tmp1[5]), na.rm=T)
    
    confDF$sd[confDF$plot.cat2 == "C"] <- sqrt((aC_sd^2+eC_sd^2)/2)
    
    
    
    confDF$conf_low  <- confDF$effect_size - confDF$sd
    confDF$conf_high  <- confDF$effect_size + confDF$sd
    
    
    ### Simplify the plot by eleminating fluxes that has CO2 effect < 1 g m-2 yr-1
    plotDF2 <- subset(plotDF, abs(effect_size) >= 5)
    #subDF <- subset(plotDF, Variable == "Change_in_pools")
    #plotDF2 <- rbind(subDF, plotDF2)
    
    ### Order plot DF
    plotDF2 <- plotDF2[order(plotDF2$plot.cat2),]
    
    ### Plot variable ordering
    plotDF2$var.order <- 1:length(plotDF2$plot.cat2)
    
    library(viridis)
    library(RColorBrewer)
    
    ## gpp
    A.col.list <- viridis(6)[1:2] 
    
    # 2nd column bar - NPP + Ra 
    B.col.list <- viridis(6)[3:4] 
    
    # 3rd column bar - Change in pools + all R
    C.col.list <- viridis(6)[5:6] 
    
    ## NPP
    D.col.list <- brewer.pal(5,"Greens")[2:5]
    
    ## R
    E.col.list <- brewer.pal(6,"YlOrRd")[2:6]
    
    ### Change in pools
    F.col.list <- brewer.pal(6,"Blues")[2:6]
    
    
    ### Combine all color list
    col.list2 <- c("over_gpp"=A.col.list[1],                    
                   "understorey_gpp"=A.col.list[2], 
                   "NPP"=B.col.list[1],   
                   "Ra"=B.col.list[2],      
                   "Change_in_pools"=C.col.list[1],   
                   "total_outflux"=C.col.list[2],
                   "leaf_prod"=D.col.list[1],                
                   "other_prod"=D.col.list[2],
                   "fineroot_prod"=D.col.list[3],           
                   "understorey_prod"=D.col.list[4], 
                   "wood_respiration"=E.col.list[1],           
                   "root_respiration"=E.col.list[2],           
                   "understorey_respiration"=E.col.list[3], 
                   "growth_respiration"=E.col.list[4],   
                   "hetero_respiration"=E.col.list[5],    
                   "delta_wood_c"=F.col.list[1],        
                   "delta_understorey_c"=F.col.list[2],
                   "delta_fineroot_c"=F.col.list[3],    
                   "delta_soil_c"=F.col.list[4],
                   "delta_litter_c"=F.col.list[5])  
    
    
    # y label
    y.lab2 <- c("over_gpp"=expression(GPP[o]),                    # 1
                "understorey_gpp"=expression(GPP[u]),             # 2
                "NPP"="NPP",                                      # 3
                "Ra"=expression(R[a]),                            # 5
                "Change_in_pools"=expression(Delta*C[pools]),
                "total_outflux"="R",                              # 4
                "leaf_prod"=expression(NPP[ol]),                # 7
                "wood_prod"=expression(NPP[stem]),                # 11
                "fineroot_prod"=expression(NPP[froot]),           # 12
                "other_prod"=expression(NPP[other]),                # 8
                "understorey_prod"=expression(NPP[ua]),           # 14
                "wood_respiration"=expression(R[stem]),           # 19
                "root_respiration"=expression(R[root]),           # 15
                "understorey_respiration"=expression(R[ua]),      # 16
                "growth_respiration"=expression(R[grow]),      # 16
                "hetero_respiration"=expression(R[hetero]),            # 17
                "delta_wood_c"=expression(Delta*C[stem]),         # 21
                "delta_fineroot_c"=expression(Delta*C[froot]),    # 22
                "delta_understorey_c"=expression(Delta*C[ua]),
                "delta_soil_c"=expression(Delta*C[soil]),
                "delta_litter_c"=expression(Delta*C[lit]))
    
    #plotDF2[plotDF2$Variable=="fineroot_prod", "var.order"] <- 10
    #plotDF2[plotDF2$Variable=="other_prod", "var.order"] <- 9
    plotDF2 <- plotDF2[order(plotDF2$var.order),]
    
    ### split the groups and make separate plots
    plotDF.sub1 <- subset(plotDF2, plot.cat2%in%c("A", "B", "C"))
    confDF.sub1 <- subset(confDF, plot.cat2%in%c("A", "B", "C"))
    
    plotDF.sub2 <- subset(plotDF2, plot.cat2=="D")
    confDF.sub2 <- subset(confDF, plot.cat2=="D")
    
    plotDF.sub3 <- subset(plotDF2, plot.cat2=="E")
    confDF.sub3 <- subset(confDF, plot.cat2=="E")
    
    plotDF.sub4 <- subset(plotDF2, plot.cat2=="F")
    confDF.sub4 <- subset(confDF, plot.cat2=="F")
    
    ### make plots
    p1 <- ggplot(plotDF.sub1,
                 aes(plot.cat2, effect_size)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        geom_errorbar(data=confDF.sub1, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
                      width=0.1, size=0.6, color="black") + 
        geom_point(data=confDF.sub1, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("GPP", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste("R+",Delta*C[pools]))))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$Variable,
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
        scale_y_continuous(limits=c(-205, 500), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=3),legend.justification = c(0, 1))+
        annotate(geom="text", x=0.6, y=500, label="a", size=7)
    
    
    p2 <- ggplot(plotDF.sub2,
                 aes(plot.cat2, effect_size)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        geom_errorbar(data=confDF.sub2, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
                      width=0.1, size=0.6, color="black") + 
        geom_point(data=confDF.sub2, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$Variable,
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
        scale_y_continuous(limits=c(-205, 500), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))+
        annotate(geom="text", x=0.5, y=500, label="b", size=7)
    
    
    p3 <- ggplot(plotDF.sub3,
                 aes(plot.cat2, effect_size)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        geom_errorbar(data=confDF.sub3, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
                      width=0.1, size=0.6, color="black") + 
        geom_point(data=confDF.sub3, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("R"))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$Variable,
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
        scale_y_continuous(limits=c(-205, 500), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2, nrow=3))+
        annotate(geom="text",x=0.5, y=500, label="c", size=7)
    
    
    p4 <- ggplot(plotDF.sub4,
                 aes(plot.cat2, effect_size)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=Variable),
                 position="stack") +
        geom_errorbar(data=confDF.sub4, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
                      width=0.1, size=0.6, color="black") + 
        geom_point(data=confDF.sub4, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$Variable,
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
        scale_y_continuous(limits=c(-205, 500), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))+
        annotate(geom="text", x=0.5, y=500, label="d", size=7)
    
    require(grid)
    require(cowplot)
    
    ### Plotting
    pdf("Output/Figure_2.pdf", width=16, height=6)
    plot_grid(p1, p2, p3, p4, labels="", ncol=4, align="h", axis="l",
              rel_widths=c(1.3, 0.5, 0.5, 0.5))
    dev.off()
    
    
    write.csv(plotDF.sub1, "output/Figure_2a1.csv", row.names=F)
    write.csv(plotDF.sub2, "output/Figure_2b1.csv", row.names=F)
    write.csv(plotDF.sub3, "output/Figure_2c1.csv", row.names=F)
    write.csv(plotDF.sub4, "output/Figure_2d1.csv", row.names=F)
    
    write.csv(confDF.sub1, "output/Figure_2a2.csv", row.names=F)
    write.csv(confDF.sub2, "output/Figure_2b2.csv", row.names=F)
    write.csv(confDF.sub3, "output/Figure_2c2.csv", row.names=F)
    write.csv(confDF.sub4, "output/Figure_2d2.csv", row.names=F)
    
    
    #  
}