#!/usr/bin/env Rscript
#
##load data##########################

if (!file.exists("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/traindatadf.Rdata") || !file.exists("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testdatadf.Rdata")){
###read and merge data#######
###disc###
Conn_disc<-vroom::vroom("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/SubjbyLowerTri.disc.csv")
Conn_disc<-as.data.frame(Conn_disc)
names(Conn_disc)[grep("r",names(Conn_disc))]<-paste0("edge_",grep("r",names(Conn_disc),value=TRUE))

behav_disc<-vroom::vroom("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/behaviordata.disc.csv")
behav_disc<-as.data.frame(behav_disc)
behav_disc$subj<-behav_disc$Subject
behav_disc[,grep("Vertex",names(behav_disc))]<-NULL
All_disc<-merge(Conn_disc,behav_disc,by="subj")
###rep####

Conn_rep<-vroom::vroom("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/SubjbyLowerTri.rep.csv")
Conn_rep<-as.data.frame(Conn_rep)
names(Conn_rep)[grep("r",names(Conn_rep))]<-gsub("_rep","",grep("r",names(Conn_rep),value=TRUE))
names(Conn_rep)[grep("r",names(Conn_rep))]<-paste0("edge_",grep("r",names(Conn_rep),value=TRUE))

behav_rep<-vroom::vroom("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/behaviordata.rep.csv")
behav_rep<-as.data.frame(behav_rep)
behav_rep[,grep("Vertex",names(behav_rep))]<-NULL
behav_rep$subj<-behav_rep$Subject

All_rep<-merge(Conn_rep,behav_rep,by="subj")

##variable set up####
traindf<-All_disc
testdf<-All_rep
rm(list=c("All_disc","All_rep","Conn_disc","behav_disc","Conn_rep","behav_rep"))
save(traindf,file="/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/traindatadf.Rdata")
save(testdf,file="/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testdatadf.Rdata")
}

