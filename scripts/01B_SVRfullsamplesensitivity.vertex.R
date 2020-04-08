#!/usr/bin/env Rscript
#
##load data##########################

load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/traindatadf.vertex.Rdata")
load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/trainyswithresidandscaledys.Rdata")
load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testdatadf.vertex.Rdata")
load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testyswithresidandscaledys.Rdata")

source("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/scripts/0x_svrfuncs.R")

traindf<-traindfvertex
testdf<-testdfvertex

traindf<-merge(traindf,trainysresidandscaledys,by="Subject")
testdf<-merge(testdf,testysresidandscaledys,by="Subject")

xcols<-grep("Vertex",names(traindf),value=TRUE)

traindf$abcd_cbcls01_cbcl_scr_syn_totprob_r_log<-log(1+traindf$abcd_cbcls01_cbcl_scr_syn_totprob_r)
traindf$abcd_cbcls01_cbcl_scr_syn_internal_r_log<-log(1+traindf$abcd_cbcls01_cbcl_scr_syn_internal_r)
traindf$abcd_cbcls01_cbcl_scr_syn_external_r_log<-log(1+traindf$abcd_cbcls01_cbcl_scr_syn_external_r)
traindf$age<-traindf$abcd_pgbi01_interview_age

testdf$abcd_cbcls01_cbcl_scr_syn_totprob_r_log<-log(1+testdf$abcd_cbcls01_cbcl_scr_syn_totprob_r)
testdf$abcd_cbcls01_cbcl_scr_syn_internal_r_log<-log(1+testdf$abcd_cbcls01_cbcl_scr_syn_internal_r)
testdf$abcd_cbcls01_cbcl_scr_syn_external_r_log<-log(1+testdf$abcd_cbcls01_cbcl_scr_syn_external_r)
testdf$age<-testdf$abcd_pgbi01_interview_age
####test run########

traindf<-traindf[complete.cases(traindf[,xcols]),]
testdf<-testdf[complete.cases(testdf[,xcols]),]

savedir<-"/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/svrbysamplesize/fulltrainsensitivity_vertex_20200401"

if (!dir.exists(savedir)){dir.create(savedir)}

######PCA feat select##############################################
#propvarretain<-c(.5,1,25,20)
propvarretain<-c(.5,1,20)
tunelist<-c(TRUE,FALSE)
ylist<-c("abcd_ps01_pea_wiscv_tss","abcd_cbcls01_cbcl_scr_syn_totprob_r_log","abcd_cbcls01_cbcl_scr_syn_external_r_log","abcd_cbcls01_cbcl_scr_syn_internal_r_log","abcd_tbss01_nihtbx_totalcomp_uncorrected","abcd_tbss01_nihtbx_fluidcomp_uncorrected","abcd_tbss01_nihtbx_cryst_uncorrected","abcd_cbcls01_cbcl_scr_syn_totprob_t","abcd_cbcls01_cbcl_scr_syn_external_t","abcd_cbcls01_cbcl_scr_syn_internal_t","abcd_tbss01_nihtbx_totalcomp_agecorrected","abcd_tbss01_nihtbx_fluidcomp_agecorrected","abcd_tbss01_nihtbx_cryst_agecorrected","age")
ylist<-c(ylist,grep("resid_scaled",names(traindf),value=TRUE))

allparams<-expand.grid(propvarretain=propvarretain,tune=tunelist,y=ylist)
require(parallel)
mclapply(1:nrow(allparams),mc.cores=8,function(pi){

   tunename<-"NOTUNE"
   if (allparams$tune[pi]){
   tunename<-"TUNE"
   }
   outname<-paste0(paste(savedir,paste("PCA",allparams$propvarretain[pi],tunename,"Sensitivityjob",allparams$y[pi],"n1957.chunk1",sep="."),sep="/"),".rdata")
   print(outname)
   if (!file.exists(outname)){
   svm_samplesizewrapper(traindf=traindf,testdf=testdf,y=as.character(allparams$y[pi]),xcols=xcols,tune=allparams$tune[pi],tunefolds=2,outerfoldcores=1,tunecores=1,weights=TRUE,samplesizes=nrow(traindf),niter=1,unifeatselect=FALSE,nfeatures=0,PCA=TRUE,propvarretain=allparams$propvarretain[pi],savedir=savedir,savechunksize=nrow(traindf),validationcores=1,jobname="Sensitivityjob")
   }else{print("file exists skipping")}
   
})

#######univariate feat select#######################################
#nfeaturelist<-c(1000,10000,15000,5000)
nfeaturelist<-c(10000,15000,5000)
tunelist<-c(TRUE,FALSE)
allparams<-expand.grid(nfeatures=nfeaturelist,tune=tunelist,y=ylist)

require(parallel)
mclapply(1:nrow(allparams),mc.cores=8,function(pi){

   tunename<-"NOTUNE"
   if (allparams$tune[pi]){
   tunename<-"TUNE"
   }
 outname<-paste0(paste(savedir,paste("unifeatselect",allparams$propvarretain[pi],tunename,"Sensitivityjob",allparams$y[pi],"n1957.chunk1",sep="."),sep="/"),".rdata") 
   print(outname)
   if (!file.exists(outname)){
   svm_samplesizewrapper(traindf=traindf,testdf=testdf,y=as.character(allparams$y[pi]),xcols=xcols,tune=allparams$tune[pi],tunefolds=2,outerfoldcores=1,tunecores=1,weights=TRUE,samplesizes=nrow(traindf),niter=1,unifeatselect=TRUE,nfeatures=allparams$nfeatures[pi],PCA=FALSE,propvarretain=.5,savedir=savedir,savechunksize=nrow(traindf),validationcores=1,jobname="Sensitivityjob")
   }else{print("file exists skipping")}
   
})

