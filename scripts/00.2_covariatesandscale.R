load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/traindatadf.Rdata")
load("/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testdatadf.Rdata")

traindf<-traindf[,!grepl("Vertex",names(traindf))]
traindf$abcd_cbcls01_cbcl_scr_syn_totprob_r_log<-log(1+traindf$abcd_cbcls01_cbcl_scr_syn_totprob_r)
traindf$abcd_cbcls01_cbcl_scr_syn_internal_r_log<-log(1+traindf$abcd_cbcls01_cbcl_scr_syn_internal_r)
traindf$abcd_cbcls01_cbcl_scr_syn_external_r_log<-log(1+traindf$abcd_cbcls01_cbcl_scr_syn_external_r)
traindf$type<-"train"

testdf<-testdf[,!grepl("Vertex",names(testdf))]
testdf$abcd_cbcls01_cbcl_scr_syn_totprob_r_log<-log(1+testdf$abcd_cbcls01_cbcl_scr_syn_totprob_r)
testdf$abcd_cbcls01_cbcl_scr_syn_internal_r_log<-log(1+testdf$abcd_cbcls01_cbcl_scr_syn_internal_r)
testdf$abcd_cbcls01_cbcl_scr_syn_external_r_log<-log(1+testdf$abcd_cbcls01_cbcl_scr_syn_external_r)
testdf$type<-"test"

ylist<-c("abcd_ps01_pea_wiscv_tss","abcd_cbcls01_cbcl_scr_syn_totprob_r_log","abcd_cbcls01_cbcl_scr_syn_external_r_log","abcd_cbcls01_cbcl_scr_syn_internal_r_log","abcd_tbss01_nihtbx_totalcomp_uncorrected","abcd_tbss01_nihtbx_fluidcomp_uncorrected","abcd_tbss01_nihtbx_cryst_uncorrected","abcd_cbcls01_cbcl_scr_syn_totprob_t","abcd_cbcls01_cbcl_scr_syn_external_t","abcd_cbcls01_cbcl_scr_syn_internal_t","abcd_tbss01_nihtbx_totalcomp_agecorrected","abcd_tbss01_nihtbx_fluidcomp_agecorrected","abcd_tbss01_nihtbx_cryst_agecorrected")

trainandtestys<-rbind(traindf[,c("Subject","type","FD",ylist)],testdf[,c("Subject","type","FD",ylist)])
trainandtestys_residscaled<-trainandtestys

trainandtestys_residscaled[,ylist]<-lapply(ylist,function(y){
yresidscale<-base::scale(resid(lm(as.formula(paste(y,"FD",sep="~")),data=trainandtestys_residscaled),na.action=na.pass))
})

yidx <- which(names(trainandtestys_residscaled) %in% ylist)
names(trainandtestys_residscaled)[yidx]<-paste(names(trainandtestys_residscaled[,yidx]),"resid_scaled_fullsample",sep="_")

traindfwithresidandscaledys<-merge(traindf,trainandtestys_residscaled,by=c("Subject","type","FD"))
testdfwithresidandscaledys<-merge(testdf,trainandtestys_residscaled,by=c("Subject","type","FD"))


trainys<-traindf[,c("Subject","type","FD",ylist)]

trainys_residscaled<-trainys
trainys_residscaled[,ylist]<-lapply(ylist,function(y){
yresidscale<-base::scale(resid(lm(as.formula(paste(y,"FD",sep="~")),data=trainys_residscaled)))
})

yidx <- which(names(trainys_residscaled) %in% ylist)
names(trainys_residscaled)[yidx]<-paste(names(trainys_residscaled[,yidx]),"resid_scaled_sepsample",sep="_")

testys<-testdf[,c("Subject","type","FD",ylist)]

testys_residscaled<-testys
testys_residscaled[,ylist]<-lapply(ylist,function(y){
yresidscale<-base::scale(resid(lm(as.formula(paste(y,"FD",sep="~")),data=testys_residscaled)))
})

yidx <- which(names(testys_residscaled) %in% ylist)
names(testys_residscaled)[yidx]<-paste(names(testys_residscaled[,yidx]),"resid_scaled_sepsample",sep="_")

traindfwithresidandscaledys<-merge(traindfwithresidandscaledys,trainys_residscaled,by=c("Subject","type","FD"))
testdfwithresidandscaledys<-merge(testdfwithresidandscaledys,testys_residscaled,by=c("Subject","type","FD"))

trainysresidandscaledys<-traindfwithresidandscaledys[,c("Subject",grep("resid_scaled",names(traindfwithresidandscaledys),value=TRUE))]
testysresidandscaledys<-testdfwithresidandscaledys[,c("Subject",grep("resid_scaled",names(testdfwithresidandscaledys),value=TRUE))]

save(trainysresidandscaledys,file="/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/trainyswithresidandscaledys.Rdata")
save(testysresidandscaledys,file="/Volumes/Hera/Datasets/ABCD/ABCD_MP_SVR/data/testyswithresidandscaledys.Rdata")


