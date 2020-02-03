#!/usr/bin/env Rscript
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

####executve svr funcs in "0x_svrfuncs.R"########
#source("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/scripts/0x_svrfuncs.R")

##variable set up####
traindf<-All_disc
testdf<-All_rep
All_disc<-NULL
All_rep<-NULL

y<-"nihtbx_totalcomp_uncorrected"
xcols<-grep("edge",names(Conn_disc),value=TRUE)



