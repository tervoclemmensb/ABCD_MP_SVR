source("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/scripts/0x_svrfuncs.R")
source("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/scripts/0x_svrparsefuncs.R")

chunkdir<-"/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/svrbysamplesize/fulltrainsensitivity_20200401" ###resting-state
#chunkdir<-"/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/svrbysamplesize/fulltrainsensitivity_vertex"
Sens_PCAchunkdata<-chunklistaggregattion(chunkdir,PCA=TRUE)
Sens_unichunkdata<-chunklistaggregattion(chunkdir,unifeatselect=TRUE)

allsenschunk<-rbind(Sens_PCAchunkdata,Sens_unichunkdata)

plotsenschunk<-allsenschunk %>% dplyr::group_by(y,tune,type,param) %>% dplyr::summarize(outofsamplecor=cor(truey,testpred),outofsamplersq=rsquared_function(truey,truey-testpred))

plotsenschunk$motioncovary<-"no motion"
plotsenschunk$motioncovary[grep("resid_scaled_fullsample",plotsenschunk$y)]<-"full sample motion"
plotsenschunk$motioncovary[grep("resid_scaled_sepsample",plotsenschunk$y)]<-"sep. train test motion"

plotsenschunk$agecorrection<-"no age adjust"
plotsenschunk$agecorrection[grep("_t$|_t_resid|agecorrected",plotsenschunk$y)]<-"age adjust"

plotsenschunk$tunef<-"no tune"
plotsenschunk$tunef[plotsenschunk$tune==TRUE]<-"tune"
plotsenschunk$tuneparamf<-paste(plotsenschunk$type,plotsenschunk$param,sep=" ")

plotsenschunk$yplot<-as.character(plotsenschunk$y)
plotsenschunk$yplot<-sapply(plotsenschunk$y,function(y){strsplit(as.character(y),"_resid_scaled")[[1]][1]}) ###motion added to df remove from var name
plotsenschunk$yplot<-gsub("_r_log|_t$","",plotsenschunk$yplot)

plotsenschunk$yplot[plotsenschunk$yplot == "abcd_cbcls01_cbcl_scr_syn_external"]<-"cbcl externalizing"
plotsenschunk$yplot[plotsenschunk$yplot == "abcd_cbcls01_cbcl_scr_syn_internal"]<-"cbcl internalizing" 
plotsenschunk$yplot[plotsenschunk$yplot == "abcd_cbcls01_cbcl_scr_syn_totprob"]<-"cbcl total problems (p)"

plotsenschunk$yplot<-gsub("_uncorrected|_agecorrected","",plotsenschunk$yplot) ###age corrected added to df remove from var name

plotsenschunk$yplot[plotsenschunk$yplot=="abcd_tbss01_nihtbx_cryst"]<-"nihtbx crystalized"
plotsenschunk$yplot[plotsenschunk$yplot=="abcd_tbss01_nihtbx_fluidcomp"]<-"nihtbx fluid"
plotsenschunk$yplot[plotsenschunk$yplot=="abcd_tbss01_nihtbx_totalcomp"]<-"nihtbx total" 

plotsenschunk$yplot[plotsenschunk$yplot=="abcd_ps01_pea_wiscv_tss"]<-"wiscv prog. matrix (iq)" 
plotsenschunk$agecorrection[plotsenschunk$yplot=="wiscv prog. matrix (iq)"]<-"age adjust" ###IQ is age adjusted already

plotsenschunk$tuneparamf2<-plotsenschunk$tuneparamf
plotsenschunk$tuneparamf2[plotsenschunk$tuneparamf=="PCA 1"]<-"PCA all components"
plotsenschunk$tuneparamf2[plotsenschunk$tuneparamf=="PCA 5"]<-"PCA 50% of variance"
plotsenschunk$tuneparamf2[plotsenschunk$tuneparamf=="PCA 20"]<-"PCA 20 components"
#plotsenschunk$tuneparamf2[plotsenschunk$tuneparamf=="PCA 25"]<-"PCA 25 components"

plotsenschunk<-plotsenschunk[plotsenschunk$yplot!="age",]

plotsenschunk$tuneparamf2<-as.factor(plotsenschunk$tuneparamf2)
levels(plotsenschunk$tuneparamf2)<-c("PCA 20 components","PCA 50% of variance","PCA all components","unifeatselect 5000","unifeatselect 10000","unifeatselect 15000")

plotsenschunk %>% group_by(yplot,tunef,motioncovary,agecorrection) %>% summarise(n=n())

plotsenschunk$measuretype<-"cognitive"
plotsenschunk$measuretype[grep("cbcl",plotsenschunk$yplot)]<-"clinical"

plotsenschunk$colorvar<-paste(plotsenschunk$tuneparamf2,plotsenschunk$measuretype,sep=" ")

plotsenschunk$colorvarf<-factor(plotsenschunk$colorvar,levels(plotsenschunk$colorvar)<-c("PCA 20 components clinical","PCA 50% of variance clinical","PCA all components clinical","unifeatselect 5000 clinical","unifeatselect 10000 clinical","unifeatselect 15000 clinical","PCA 20 components cognitive","PCA 50% of variance cognitive","PCA all components cognitive","unifeatselect 5000 cognitive","unifeatselect 10000 cognitive","unifeatselect 15000 cognitive")) 


yplot<-ggplot(plotsenschunk,aes(y=outofsamplersq,x=type,fill=as.factor(colorvar)))+geom_bar(stat="identity",position = "dodge2",color="black")+facet_wrap(~measuretype*yplot*tunef*motioncovary*agecorrection)+scale_y_continuous(expand=c(0,0),limits=c(-.6,.3))+theme(axis.ticks.x=element_blank())#+scale_alpha_manual(values=c(.75,.5,.33,1))
#yplot<-yplot+scale_fill_manual(values=c(base::rep("#F8766D",4),base::rep("#00BFC4",4)))
yplot<-yplot+scale_fill_manual(values=c("#54278f","#756bb1","#9e9ac8","#bdc9e1","#bcbddc","#dadaeb","#f2f0f7","#006d2c","#31a354","#74c476","#a1d99b","#c7e9c0","#edf8e9"))
yplot<-LNCDR::lunaize(yplot)+theme(axis.ticks.x=element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),legend.title=element_blank(),axis.title=element_text(size=48),strip.text.x=element_text(size=12),legend.text=element_text(size=36),axis.text=element_text(size=24))
ggsave(file="/Volumes/Zeus/BTCfigs/fullsensitivityplot.2.pdf",plot=yplot,width=32,height=32)








