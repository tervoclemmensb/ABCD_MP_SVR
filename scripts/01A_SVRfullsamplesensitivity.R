#!/usr/bin/env Rscript
#
##load data##########################

load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/traindatadf.Rdata")
load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testdatadf.Rdata")

source("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/scripts/0x_svrfuncs.R")
y<-"nihtbx_totalcomp_uncorrected"
xcols<-grep("edge",names(traindf),value=TRUE)

traindf<-traindf[,!grepl("Vertex",names(traindf))]
traindf$cbcl_scr_syn_totprob_r_log<-log(1+traindf$cbcl_scr_syn_totprob_r)
traindf$cbcl_scr_syn_internal_r_log<-log(1+traindf$cbcl_scr_syn_internal_r)
traindf$cbcl_scr_syn_external_r_log<-log(1+traindf$cbcl_scr_syn_external_r)

testdf<-testdf[,!grepl("Vertex",names(testdf))]
testdf$cbcl_scr_syn_totprob_r_log<-log(1+testdf$cbcl_scr_syn_totprob_r)
testdf$cbcl_scr_syn_internal_r_log<-log(1+testdf$cbcl_scr_syn_internal_r)
testdf$cbcl_scr_syn_external_r_log<-log(1+testdf$cbcl_scr_syn_external_r)
####test run########

savedir<-"/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/svrbysamplesize/fulltrainsensitivity"

if (!dir.exists(savedir)){dir.create(savedir)}

######PCA feat select##############################################
propvarretain<-c(.5,1,25)
tunelist<-c(TRUE,FALSE)
ylist<-c("pea_wiscv_tss","cbcl_scr_syn_totprob_r_log","cbcl_scr_syn_external_r_log","cbcl_scr_syn_internal_r_log",
         "FD","nihtbx_totalcomp_uncorrected","nihtbx_fluidcomp_uncorrected","nihtbx_cryst_uncorrected")
allparams<-expand.grid(propvarretain=propvarretain,tune=tunelist,y=ylist)
require(parallel)
mclapply(1:nrow(allparams),mc.cores=3,function(pi){

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
nfeaturelist<-c(1000,10000,15000,5000)
tunelist<-c(TRUE,FALSE)
ylist<-c("pea_wiscv_tss","cbcl_scr_syn_totprob_r_log","cbcl_scr_syn_external_r_log","cbcl_scr_syn_internal_r_log",
         "FD","nihtbx_totalcomp_uncorrected","nihtbx_fluidcomp_uncorrected","nihtbx_cryst_uncorrected")

allparams<-expand.grid(nfeatures=nfeaturelist,tune=tunelist,y=ylist)

require(parallel)
mclapply(1:nrow(allparams),mc.cores=3,function(pi){

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

