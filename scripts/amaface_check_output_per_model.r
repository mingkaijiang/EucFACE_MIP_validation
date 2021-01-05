# script to check model simulations for AmazonFACE
# sanity and quality check for each model 

# Katrin Fleischer, Anthony Walker, 27 Juli 2017
#-----------------------------------------------------------------------------------------------

#clean workspace and load libraries
rm(list=ls())
library(ggplot2)
library(reshape2)
library(lattice)

# fill in your model abbreviation here, example: ORCHIDEE=ORC, LPJ-GUESS=LPJ
modcod<-"LPJ"

# READING and FORMATTING ----------------------------------------------------------------------------

# upload file with output names, units and type of output variable (change path to your directory)
setwd("C:/Users/FALW/Dropbox/AmaFACE_MEI/model_runs_input/")
# load RData file
units<-readRDS("units_frame.rds")

# #alternatively upload csv file and format (uncommented here)
# units<-read.table("output_names_units.csv",sep=";",header=T,stringsAsFactors = F)
# units<-data.frame(t(units))
# units$vars<-rownames(units);rownames(units)<-NULL
# units$X1<-as.character(units$X1);units$X2<-as.character(units$X2);units$X3<-as.character(units$X3)
# names(units)<-c("units","type","agg","vars")
# # add row of units for after aggreagtion, annual fluxes then in yr-1
# units$units_agg<-gsub("d-1","yr-1",units$units)

# set working directory to path where your output sits (this is mine)
setwd("C:/Users/FALW/Dropbox/AmaFACE_MEI_output/LPJ-GUESS")

# decide which set of meteorology simulations to check
mets<-c("OBS")  #can be set to c("OBS","DRY","WET")
print(paste("checking simulations AMB and ELE for:",mets,sep=" "))

# read in the output files for ambient and elevated simulation
# if you havent deleted the header rows (as specified) you need to add "skip=x", with x being the number of rows to skip
out<-read.table(paste("AmaFACE1_D_",modcod,"_AMB_",mets,"_US.csv",sep=""),sep=",")
outco2<-read.table(paste("AmaFACE1_D_",modcod,"_ELE_",mets,"_US.csv",sep=""),sep=",")

# check length of frames, need to be TRUE, otherwise you have too many or little days
# 102 years with 365 days plus 25 years with a leap day! (for those models that do not model leap days, please repeat Feb 28 to fill Feb 29)
if (nrow(out)!=(102*365)+25|nrow(outco2)!=(102*365)+25) 
  {print(paste("number of rows of model output does not match!!"))}

# paste them together by rows, remove original
out<-rbind(out,outco2)
rm(outco2)

# add the 130 column names 
colnames(out)<-units$vars

# add the names for any extra variables that you may have added and need for the QC
# e.g. colnames(out)[131:132]<-c("xxxxx","yyyyy"), otherwise remove them 
out<-out[,c(1:130)]

# add the column for "simulation", and move it forward
out$SIM<-c(rep("amb",nrow(out)/2),rep("ele",nrow(out)/2))
out<-out[,c(1:2,ncol(out),3:(ncol(out)-1))]

# have a look at the created data frames
print(str(out))

#factorize simulation column, and set missing values (-9999) to NA
out$SIM<-as.factor(out$SIM)
out[out==-9999]<-NA

# ANNUAL AGGREGATION of daily output -----------------------------------------------------------------

#define aggregation column, by calendar year
aggcol="YEAR"

#aggregate columns with mean/sum/ or first entry per year (defined in "units" frame)
ann<-cbind(aggregate(out[,units$vars[units$agg=="sum"]],by=list(out[,aggcol],out$SIM),FUN=sum),
           aggregate(out[,units$vars[units$agg=="mean"]],by=list(out[,aggcol],out$SIM),FUN=mean)[,-c(1:2)],
           aggregate(out[,units$vars[units$agg=="first"]],by=list(out[,aggcol],out$SIM),FUN=function(x) (x[1]))[,-c(1:2)])
    
ann<-ann[,c("Group.1","Group.2",units$vars[-c(1:2)])]  #rename sorting variables and reorder aggregated ones
names(ann)[1:2]<-c(aggcol,"SIM")
ann$YEAR<-as.numeric(ann$YEAR)

# add row of units after aggreagtion, annual fluxes now in yr-1
units$unitsann<-gsub("d-1","yr-1",units$units)

#check missing variables from output, make sure these are really not applicable/outputted by your model
vars_miss<-NULL
yvars<-names(ann)[-c(1:2)]
for (y in yvars) {
  if(sum(is.na(ann[,y]))==length(ann[,y]))  vars_miss<-c(vars_miss,y)
}
print(paste(c("variables missing from output: ",vars_miss)))

# PLOT OUTPUT PER VARIABLE (for checking individually) -----------------------------------------------------------------------
setwd("~/SpiderOak Hive/AMAZONAS/FACE-MEI/model_output/LPJ-GUESS")

ofile <- paste('OUT_abs_',modcod,'.pdf',sep='')
pdf(ofile,width=10,height=8)

for (y in yvars) {
  indy<-which(units$vars==y)
  
  if (sum(is.finite(ann[,y]))>0) {
  print(y)
  timeplot<-xyplot(get(y)~YEAR,ann,groups=SIM,pch=19,cex=2,type="a",lwd=2.5,
        main=paste(y,"in",units$unitsann[units$vars==y],sep=" "),col=c("blue","red"),ylab=y,
        scales=list(tck=c(-0.5,0),alternating=F,relation='free'),
        panel=function(...){
        panel.xyplot(...)
        panel.abline(v=seq(2000,2100,5),col="grey75")
        },
        key=list(space='inside',border=T,columns=2,text=list(levels(ann$SIM)),
        lines=list(col=c("blue","red"),lty=1,lwd=3)))
print(timeplot)
  }
  else print(paste(y, " no finite values"))
}

dev.off()

#CALCULATE CHANGE in POOLS (delta) for mass balance check-------------------------------------------------------------
delta<-ann[,units$vars[units$type=="pool"]]
delta[,]<-NA

for(y in names(delta)) {
  for (s in levels(ann$SIM)) {
    for (yr in unique(ann$YEAR)[1:length(unique(ann$YEAR))-1]) {
    ind<-ann$SIM==s&ann$YEAR==yr
    ind1<-ann$SIM==s&ann$YEAR==yr+1
    delta[ind,y]<-ann[ind1,y]-ann[ind,y]
}}}
names(delta)<-paste("delta",names(delta),sep="")
ann<-cbind(ann,delta) 

#calculate annual maximum for some variables for mass balance check
peak<-ann[,c("LAI","NCON","CL","CFR","CCR","CW","CSTOR")]
peak[,]<-NA

for(y in c("LAI","NCON","CL","CFR","CCR","CW","CSTOR")) {
  for (s in levels(ann$SIM)) {
    for (yr in unique(ann$YEAR)) {
      
      ind<-ann$SIM==s&ann$YEAR==yr
      peak[ind,y]<-max(out[out$YEAR==yr&out$SIM==s,y])
}}}
names(peak)<-paste("peak",names(peak),sep="")
ann<-cbind(ann,peak) 

#set missing variables to 0 for plotting (there should only be complete variables missing, not individual values)
sum(is.na(ann))
df0<-ann
df0[is.na(df0)]<-0


# PLOT MASS BALANCE CHECKS----------------------------------------------------------------------------------------------------

#comment out plots that do not apply for your model with an explanation
#add other plots that are applicable to your model with an explanation


#carbon balance
p1<-xyplot(I(NPP+RAU)~GPP,df0,groups=SIM,
           main='NPP+RAU~GPP',auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
             panel.xyplot(...)
             panel.abline(a=0,b=1)}) 

p2<-xyplot(I(NEP+RECO)~GPP,df0,groups=SIM,
           main='NEP+RECO~GPP',auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
             panel.xyplot(...)
             panel.abline(a=0,b=1)})

p3<-xyplot(I(RHET+RAU)~RECO,df0,groups=SIM,
           main='RHET+RAUTO~RECO',auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
             panel.xyplot(...)
             panel.abline(a=0,b=1)})

p4<-xyplot(I(RL+RW+RCR+RFR+RGR)~RAU,df0,groups=SIM,
           main='RL+RW+RCR+RFR+RGR~RAU',auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
             panel.xyplot(...)
             panel.abline(a=0,b=1)})

#scaling leaf and leaf carbon
p5<-xyplot(LAI~CL,df0,groups=SIM,
           main='LAI~CL',auto.key=T,
           type=c('p','r'),
           scales=list(relation='free'),
           panel=function(...){
             panel.xyplot(...)
             panel.abline(a=0,b=1)})

p6<-xyplot(LAI*LMA~CL,df0,groups=SIM,
           main='LAI*LMA~CL',auto.key=T,
           type=c('p','r'),
           scales=list(relation='free'),
           panel=function(...){
             panel.xyplot(...)
             panel.abline(a=0,b=1)})

p7<-xyplot(peakLAI~peakCL,df0,groups=SIM,
           main='peak LAI~peak CL',auto.key=T,
           type=c('p','r'),
           scales=list(relation='free'),
           panel=function(...){
             panel.xyplot(...)
             panel.abline(a=0,b=1)})

#water balance
p8<-xyplot(I(ES+EC+T)~ET,df0,groups=SIM,
           main='ES+EC+T~ET',auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
             panel.xyplot(...)
             panel.abline(a=0,b=1)})

p9<-xyplot(I(PREC-(RO+DRAIN+ET))~deltaSW,df0,groups=SIM,
           main='PREC-RO-DRAIN-ET~deltaSW',auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
             panel.xyplot(...)
             panel.abline(a=0,b=1)})

# carbon growth rates
p10<-xyplot(I(CGL-CLITIN)~deltaCL,df0,groups=SIM,
            main='CGL-CLITIN~deltaCL',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p11<-xyplot(I(CGW-CWLIN)~deltaCW,df0,groups=SIM,
            main='CGW-CWLIN~deltaCW',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p12<-xyplot(I(CGFR-CFRLIN)~deltaCFR,df0,groups=SIM,
            main='CGFR-CFRLIN~deltaCFR',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p13<-xyplot(I(CGCR-CCRLIN)~deltaCCR,df0,groups=SIM,
            main='CGCR-CCRLIN~deltaCCR',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

#GPP and NPP components
p14<-xyplot(I(RAU+CGL+CGFR+CGCR+CGW+CREPR)~GPP,df0,groups=SIM,
            main='I(RAU+CGL+CGFR+CGCR+CGW+CREPR)~GPP',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p15<-xyplot(I(CGW+CGL+CGFR+CGCR+CREPR)~NPP,df0,groups=SIM,
            main='CGW+CGL+CGFR+CGCR+CREPR~NPP this or plot with CSTOR on 1:1',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p16<-xyplot(I(CGW+CGL+CGFR+CGCR+CREPR+deltaCSTOR)~NPP,df0,groups=SIM,
            main='CGW+CGL+CGFR+CGCR+CREPR+deltaCSTOR~NPP',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

#Litter pools #Litter and repiration fluxes  
p17<-xyplot(I(CFLITA+CFLITB)~CFLIT,df0,groups=SIM,
            main='CFLITA+CFLITB~CFLIT',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p18<-xyplot(I(CLITIN+CWLIN+CFRLIN+CCRLIN+CREPR-RHET)~I(deltaCSOIL+deltaCCLITB+deltaCFLIT),df0,groups=SIM,
            main='CLITIN+CWLIN+CFRLIN+CCRLIN+CREPR-RHET~deltaCSOIL+deltaCCLITB+deltaCFLIT',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

# N cycle
#N turnover in pools
p19<-xyplot(I(NGL-NLITIN-NLRETR)~deltaNL,df0,groups=SIM,
            main='NGL-NLITIN-NLRETR~deltaNL',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p20<-xyplot(I(NGW-NWLIN-NWRETR)~deltaNW,df0,groups=SIM,
            main='NGW-NWLIN-NWRETR~deltaNW',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p21<-xyplot(I(NGFR-NFRLIN-NFRRETR)~deltaNFR,df0,groups=SIM,
            main='NGFR-NFRLIN-NFRRETR~deltaNFR',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p22<-xyplot(I(NGCR-NCRLIN-NCRRETR)~deltaNCR,df0,groups=SIM,
            main='NGCR-NCRLIN-NCRRETR~deltaNCR',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})


p23<-xyplot(I(NFLITA+NFLITB)~NFLIT,df0,groups=SIM,
            main='NFLITA+NFLITB~NFLIT',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

#N balance for plant, plant N storage, and soil+litter pool   

#plant N balance depends on whether NFIX is added to the plant N pool or soil and whether NSTORE exists
#four options here, comment out what does not apply for your model
p24<-xyplot(I(NUP+NLRETR+NWRETR+NFRRETR+NCRRETR)~I(NGL+NGFR+NGCR+NGW),df0,groups=SIM,
            main='I(NUP+NLRETR+NWRETR+NFRRETR+NCRRETR)~I(NGL+NGFR+NGCR+NGW) (this or the 3 plots below on 1:1)',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p25<-xyplot(I(NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGFR-NGCR-NGW)~deltaNSTOR,df0,groups=SIM,
            main='NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGR-NGCR-NGW~deltaNSTOR',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p26<-xyplot(I(NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR)~I(NGL+NGFR+NGCR+NGW),df0,groups=SIM,
            main='I(NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR)~I(NGL+NGFR+NGCR+NGW)',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p27<-xyplot(I(NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGFR-NGCR-NGW)~deltaNSTOR,df0,groups=SIM,
            main='NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGR-NGCR-NGW~deltaNSTOR',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

#N balance ecosystem
p28<-xyplot(I(NDEP+NFIX-NLEACH-NVOL)~(I(deltaNL+deltaNW+deltaNCR+deltaNFR+deltaNSOIL+deltaNFLIT+deltaNCLITB)),df0,groups=SIM,
            main='I(NDEP+NFIX-NLEACH-NVOL)~(I(deltaNL+deltaNW+deltaNCR+deltaNFR+deltaNSOIL',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

#N balance soil, with and without NFIX added to soil pool
p29<-xyplot(I(NDEP+NFIX+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL)~I(deltaNSOIL+deltaNFLIT+deltaNCLITB),df0,groups=SIM,
            main='NDEP+NFIX+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL~deltaNSOIL+deltaNFLIT+deltaNCLITB this or plot below',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p30<-xyplot(I(NDEP+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL)~I(deltaNSOIL+deltaNFLIT+deltaNCLITB),df0,groups=SIM,
            main='NDEP+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL~deltaNSOIL+deltaNFLIT+deltaNCLITB',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p31<-xyplot(I(NPMIN+NPORG)~NSOIL,df0,groups=SIM,
            main='NPMIN+NPORG~NSOIL',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

#P budget for the ecosystem
p32<-xyplot(I(PDEP+PWEA-PLEACH)~(I(deltaPL+deltaPW+deltaPCR+deltaPFR+deltaPSOIL+deltaPFLIT+deltaPCLITB)),df0,groups=SIM,
            main='I(PDEP+PWEA-PLEACH)~(I(deltaPL+deltaPW+deltaPCR+deltaPFR+deltaPSOIL',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

# P turnover in pools
p33<-xyplot(I(PGL-PLITIN-PLRETR)~deltaPL,df0,groups=SIM,
            main='PGL-PLITIN-PLRETR~deltaPL',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p34<-xyplot(I(PGW-PWLIN-PWRETR)~deltaPW,df0,groups=SIM,
            main='PGW-PWLIN-PWRETR~deltaPW',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p35<-xyplot(I(PGFR-PFRLIN-PFRRETR)~deltaPFR,df0,groups=SIM,
            main='PGFR-PFRLIN-PFRRETR~deltaPFR',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p36<-xyplot(I(PGCR-PCRLIN-PCRRETR)~deltaPCR,df0,groups=SIM,
            main='PGCR-PCRLIN-PCRRETR~deltaPCR',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p37<-xyplot(I(PFLITA+PFLITB)~PFLIT,df0,groups=SIM,
            main='PFLITA+PFLITB~PFLIT',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})


#P balance for plant, plant P storage, and soil+litter pool   
p38<-xyplot(I(PUP+PLRETR+PWRETR+PFRRETR+PCRRETR)~I(PGL+PGFR+PGCR+PGW),df0,groups=SIM,
            main='PUP+PLRETR+PWRETR+PFRRETR+PCRRETR~PGL+PGFR+PGCR+PGW (this or plot below on 1:1)',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p39<-xyplot(I(PUP+PLRETR+PWRETR+PFRRETR+PCRRETR-PGL-PGFR-PGCR-PGW)~deltaPSTOR,df0,groups=SIM,
            main='PUP+PLRETR+PWRETR+PFRRETR+PCRRETR-PGL-PGFR-PGCR-PGW~deltaPSTOR',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p40<-xyplot(I(PDEP+PWEA+PLITIN+PWLIN+PCRLIN+PFRLIN-PUP-PLEACH)~I(deltaPSOIL+deltaPFLIT+deltaPCLITB),df0,groups=SIM,
            main='PLITIN+PWLIN+PCRLIN+PFRLIN-PUP-PLEACH~I(deltaPSOIL+deltaPFLIT+deltaPCLITB)',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

#Inorganic P pools
p41<-xyplot(I(PPMIN+PPORG)~PSOIL,df0,groups=SIM,
            main='PPMIN+PPORG~PSOIL',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p42<-xyplot(I(PLAB+PSEC+POCC+PPAR)~PPMIN,df0,groups=SIM,
            main='PLAB+PSEC+POCC+PPAR~PPMIN',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

p43<-xyplot(I(PLAB+PSEC+POCC+PPAR+PPORG)~PSOIL,df0,groups=SIM,
            main='PLAB+PSEC+POCC+PPAR+PPORG~PSOIL',auto.key=T,
            scales=list(relation='free'),
            panel=function(...){
              panel.xyplot(...)
              panel.abline(a=0,b=1)})

# print plots to file, change numbering if needed
pdf(paste('QC_',modcod,'.pdf',sep=''),width=10,height=8)
for (i in 1:43) {
  print(get(paste("p",i,sep="")))
}
dev.off()