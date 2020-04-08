source("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/scripts/0x_svrfuncs.R")
source("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/scripts/0x_svrparsefuncs.R")
require(dplyr)
chunkdir<-"/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/svrbysamplesize_SC/PCA5_20200325/PCA5_20200325"

#Sens_PCAchunkdata<-chunklistaggregattion(chunkdir,PCA=TRUE,mc.cores=40)

#plotsenschunk<-Sens_PCAchunkdata %>% dplyr::group_by(y,itersamplesize,jobname) %>% dplyr::summarize(outofsamplecor=cor(truey,testpred),outofsamplersq=rsquared_function(truey,truey-testpred))

plotsenschunk$yplot<-as.character(plotsenschunk$y)

#plotsenschunk$yplot[plotsenschunk$y=="cbcl_scr_syn_totprob_r_log"]<-"cbcl total problems (p)"
plotsenschunk$yplot[plotsenschunk$y=="nihtbx_cryst_uncorrected"]<-"nihtbx crystalized"
plotsenschunk$yplot[plotsenschunk$y=="pea_wiscv_tss"]<-"wiscv prog. matrix (iq)" 

#write.csv(plotsenschunk,"/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/svrbysamplesize_SC/outputs/PCA5crystandIQ_20200327.csv")

plotsenschunk<-read.csv("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/svrbysamplesize_SC/outputs/PCA5crystandIQ_20200327.csv")

yplot<-ggplot(plotsenschunk,aes(y=outofsamplersq,x=itersamplesize,color=yplot,fill=yplot))+geom_point()
#yplot<-yplot+scale_fill_manual(values=c(base::rep("#F8766D",4),base::rep("#00BFC4",4)))

plotsenschunk_m<-plotsenschunk %>% dplyr::group_by(y,yplot,itersamplesize) %>% dplyr::summarize(outofsamplecor_m=mean(outofsamplecor),secor=sd(outofsamplecor)/sqrt(n()),outofsamplersq_m=mean(outofsamplersq),sersq=sd(outofsamplersq)/sqrt(n()))

scale0to1<-function(x){
scaled=(x-min(x))/(max(x)-min(x))
}

plotsenschunk_ms<-plotsenschunk_m %>% dplyr::group_by(y,yplot) %>% dplyr::mutate(rsqscaled=scale0to1(outofsamplersq_m))

yplot<-ggplot(plotsenschunk_m,aes(y=outofsamplersq_m,x=itersamplesize,color=yplot,fill=yplot))+geom_line()

yplots<-ggplot(plotsenschunk_ms,aes(y=rsqscaled,x=itersamplesize,color=yplot,fill=yplot))+geom_line()

#####

weightmat<-chunklistaggregattion_weights(chunkdir=chunkdir,PCA=TRUE,mc.cores=40)


crys<-weightmat[,grep("nihtbx_cryst_uncorrected",names(weightmat),value=TRUE)]
dfss<-crys
samplesizes<-unique(unlist(lapply(strsplit(names(dfss),"[.]"),"[[",2)))

corbysamplesize<-lapply(samplesizes,function(ss){
sscorsall<-cor(dfss[,grep(ss,names(dfss),value=TRUE)])
sscorsallm<-mean(sscorsall[upper.tri(x = cor(dfss[,grep(ss,names(dfss),value=TRUE)]), diag = FALSE)])
return(c(weightcorrelation=sscorsallm,itersamplesize=ss))
})


corbysamplesizemat<-data.frame(do.call(rbind,corbysamplesize))
yplot<-ggplot(corbysamplesizemat,aes(y=as.numeric(as.character(weightcorrelation)),x=as.numeric(as.character(itersamplesize))))+geom_point()+geom_line()
ggsave(yplot,file="weightcorrelationabcd_20200328.pdf",height=8,width=12)







