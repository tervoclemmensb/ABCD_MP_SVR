#!/usr/bin/env Rscript
#
##load data##########################

if (!file.exists("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/traindatadf.vertex.Rdata") || !file.exists("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testdatadf.vertex.Rdata")){
###read and merge data#######
###disc###

Conn_disc<-vroom::vroom("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/SubjbyLowerTri.disc.csv")
Conn_disc<-as.data.frame(Conn_disc)
Conn_disc<-as.data.frame(Conn_disc[,"subj"])
names(Conn_disc)<-"subj"

disc<-vroom::vroom("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/behaviordata.disc.csv")
disc<-as.data.frame(disc)
disc$subj<-disc$Subject

All_disc<-merge(Conn_disc,disc,by="subj")
###rep####

rep<-vroom::vroom("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/behaviordata.rep.csv")
rep<-as.data.frame(rep)
rep$subj<-rep$Subject

Conn_rep<-vroom::vroom("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/SubjbyLowerTri.rep.csv")
Conn_rep<-as.data.frame(Conn_rep)
Conn_rep<-as.data.frame(Conn_rep[,"subj"])
names(Conn_rep)<-"subj"

All_rep<-merge(Conn_rep,rep,by="subj")
##variable set up####
traindfvertex<-All_disc
testdfvertex<-All_rep

save(traindfvertex,file="/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/traindatadf.vertex.Rdata")
save(testdfvertex,file="/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testdatadf.vertex.Rdata")
}

