#!/usr/bin/env Rscript
#
##load data##########################

if (!file.exists("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/traindatadf.Rdata") || !file.exists("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testdatadf.Rdata")){
###read and merge data#######
###disc###
Conn_disc<-data.table::fread("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/SubjbyLowerTri.disc.csv")
Conn_disc<-as.data.frame(Conn_disc)
names(Conn_disc)[grep("r",names(Conn_disc))]<-paste0("edge_",grep("r",names(Conn_disc),value=TRUE))

behav_disc<-data.table::fread("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/behaviordata.disc.csv")
behav_disc<-as.data.frame(behav_disc)
behav_disc$subj<-behav_disc$Subject

All_disc<-merge(Conn_disc,behav_disc,by="subj")
###rep####

Conn_rep<-data.table::fread("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/SubjbyLowerTri.rep.csv")
Conn_rep<-as.data.frame(Conn_rep)
names(Conn_rep)[grep("r",names(Conn_rep))]<-gsub("_rep","",grep("r",names(Conn_rep),value=TRUE))
names(Conn_rep)[grep("r",names(Conn_rep))]<-paste0("edge_",grep("r",names(Conn_rep),value=TRUE))

behav_rep<-data.table::fread("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/behaviordata.rep.csv")
behav_rep<-as.data.frame(behav_rep)
behav_rep$subj<-behav_rep$Subject

All_rep<-merge(Conn_rep,behav_rep,by="subj")

##variable set up####
traindf<-All_disc
testdf<-All_rep
rm(list=c("All_disc","All_rep","Conn_disc","behav_disc","Conn_rep","behav_rep"))
save(traindf,file="/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/traindatadf.Rdata")
save(testdf,file="/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testdatadf.Rdata")
}

load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/traindatadf.Rdata")
load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testdatadf.Rdata")

source("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/scripts/0x_svrfuncs.R")
y<-"nihtbx_totalcomp_uncorrected"
xcols<-grep("edge",names(traindf),value=TRUE)

traindf<-traindf[,!grepl("Vertex",names(traindf))]
testdf<-testdf[,!grepl("Vertex",names(testdf))]

####test run########

savedir<-"/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/svrbysamplesize/20200207"

svm_samplesizewrapper(traindf=traindf,testdf=testdf,y="pea_wiscv_tss",xcols=xcols,tune=FALSE,tunefolds=NA,outerfoldcores=8,tunecores=8,weights=TRUE,samplesizes=rev(c(25,50,100,250,500,1000)),niter=100,unifeatselect=TRUE,nfeatures=10000,PCA=FALSE,propvarretain=.5,savedir=savedir,savechunksize=10,validationcores=8)



