#!/usr/bin/env Rscript
#
##load data##########################

load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/traindatadf.Rdata")
load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testdatadf.Rdata")

source("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/scripts/0x_svrfuncs.R")
y<-"nihtbx_totalcomp_uncorrected"
xcols<-grep("edge",names(traindf),value=TRUE)

traindf<-traindf[,!grepl("Vertex",names(traindf))]
testdf<-testdf[,!grepl("Vertex",names(testdf))]

####test run########

savedir<-"/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/svrbysamplesize/fulltrainsensitivity"

if (!dir.exists(savedir)){dir.create(savedir)}

#######univariate#######################################

nfeaturelist<-c(1000,10000,length(xcols))
tunelist<-c(FALSE,TRUE)
ylist<-c("pea_wiscv_tss","cbcl_scr_syn_totprob_r","FD")

allparams<-expand.grid(nfeatures=nfeaturelist,tune=tunelist,y=ylist)

require(parallel)
mclapply(1:nrow(allparams),mc.cores=3,function(pi){

   tunename<-"NOTUNE"
   if (allparams$tune[pi]){
   tunename<-"TUNE"
   }
   outname<-paste0(paste(savedir,paste("unifeatselect",allparams$nfeatures[pi],tunename,allparams$y[pi],"n2350.chunk1",sep="."),sep="/"),".rdata")
   print(outname)
   if (!file.exists(outname)){
   svm_samplesizewrapper(traindf=traindf,testdf=testdf,y=as.character(allparams$y[pi]),xcols=xcols,tune=allparams$tune[pi],tunefolds=2,outerfoldcores=1,tunecores=1,weights=TRUE,samplesizes=nrow(traindf),niter=1,unifeatselect=TRUE,nfeatures=allparams$nfeatures[pi],PCA=FALSE,propvarretain=.5,savedir=savedir,savechunksize=nrow(traindf),validationcores=1)
   }else{print("file exists skipping")}
   
})

