#!/usr/bin/env Rscript
library(dplyr)

########################
##############################################################
###svm####
library(e1071)
library(foreach)
library(doParallel)
library(caret)

####error functions########
rmse_function <- function(error){
  rmse<-sqrt(mean(error^2))
   return(rmse)
}

rsquared_function<-function(origdata,error){
rsquared<-1-(sum(error^2))/(sum((origdata-mean(origdata))^2))
return(rsquared)
}

###tuning feature selction functions#####
SVRtuning_kfold_parallel<-function(tunedf,y,k,tunecores=NULL,epsilon=seq(.1,1,.1),cost=c(.01,.1,1,10,100,1000,10000)){
#df should be training data
#y is outcome variable to be predicted
#xcols columns to do predicting
#cores number of jobs (default is k)
#k n folds for tuning 
#epsiolon and cost sequences for grid search
require(foreach)
require(doParallel)
require(caret)
require(e1071)
if (is.null(tunecores)){tunecores=k}
registerDoParallel(cores=tunecores)
curd<-tunedf[complete.cases(tunedf),]
fold<-caret::createFolds(1:nrow(curd), k = k,list = FALSE)
params<-expand.grid(cost=cost,epsilon=epsilon)
fml<-as.formula(as.formula(paste0(y,"~.")))
result <- foreach(i = 1:nrow(params), .combine = rbind) %do% {
  c <- params[i, ]$cost
  e <- params[i, ]$epsilon
  ### K-FOLD VALIDATION ###
  #print(i)
  out <- foreach(j = 1:max(fold), .combine = rbind, .inorder = FALSE) %dopar% {
   #print(j)
   deve <- curd[fold != j, ]
    test <- curd[fold == j, ]
    mdl <- e1071::svm(fml, data = deve,kernel = "linear", cost = c, epsilon = e)
    pred <- predict(mdl,test)
    rmse_current<-rmse_function(test[,y]-pred)
    rsquared_current<-rsquared_function(test[,y],test[,y]-pred)
    data.frame(fold=j,rmse=rmse_current,rsq=rsquared_current)
  }
data.frame(c=c,e=e,meanrmse=mean(out$rmse),meanrsq=mean(out$rsq))
}
bestparameters<-result[result$meanrmse==min(result$meanrmse),]
bestparameters$rank<-(bestparameters$c==1)+(bestparameters$e==.1)
bestparameters<-bestparameters[which.max(bestparameters$rank),]

return(bestparameters)
}

svr.unifeatselect<-function(train,y,nfeatures){
featurecorrs<-data.frame(do.call(rbind,lapply(names(train)[names(train)!=y],function(ename){var<-c(ename,cor(train[,ename],train[,y]))})))
featurecorrs[,2]<-as.numeric(as.character(featurecorrs[,2]))
featurecorrs_r<-featurecorrs[rev(base::order(abs(featurecorrs[,2]))),]
returnfeatures<-as.character(featurecorrs_r[1:nfeatures,1])
returnfeaturesdf<-train[,returnfeatures]
return(returnfeatures)
}

svr.pca.featreduction<-function(train,y,propvarretain){
trainforpca<-train[,names(train)!=y]
require(Morpho)
trainforpcaz <- scale(trainforpca)
pcaout<-prcompfast(trainforpcaz,retx=FALSE,center=TRUE,scale.=TRUE)
prcomp.varex <-  pcaout$sdev^2/sum(pcaout$sdev^2)
prcomp.cumsum <-  cumsum(prcomp.varex)
compiatthreshold <- which.min(prcomp.cumsum <= propvarretain)
pcascoresreturn<-data.frame(scale(pcaout$x[,1:compiatthreshold]))
return(pcascoresreturn)
}

######SVR single model functions############
svr.single <- function(foldname=NA,test,train,y,tune=T,tunefolds,tunecores,weights=T) {
#foldname, string | number for naming output 
#test, test df
#train, train df
#y, yvarname
#tune, T/F to Tune via SVRtuning_kfold_parallel
#tunefolds, N folds for tuning (tuning occurs on train only)
#weights, T/F to include SVR weights in output
require(e1071)
print(foldname)
 svmmodel<-svm(as.formula(paste0(y,"~.")),kernel="linear",data=train)
 if (tune){
   bestparameters<-SVRtuning_kfold_parallel(tunedf=train,y=y,k=tunefolds,tunecores=tunecores)   
   svmmodel<-svm(as.formula(paste0(y,"~.")),cost=bestparameters$c,epsilon=bestparameters$e,kernel="linear",data=train)
 }
nw<-length(grep("vx",names(train)))
w <-rep(NA,nw)
if (weights){
w <- t(svmmodel$coefs) %*%svmmodel$SV 
}
 
 ####save model performance
 pred_train        <-predict(svmmodel,train)
 pred_test         <-predict(svmmodel,test)

 ######save model params
 
cvout<-as.data.frame(cbind(
   foldname     = foldname,
   cvpred   = pred_test,
   testyval = test[,y],
   traincor = cor(pred_train,train[,y]),
   kernel =svmmodel$kernel,
   epsilon=svmmodel$epsilon,
   cost   =svmmodel$cost,
   gamma  =svmmodel$gamma
 ))
savelist<-list(cvout,w)
return(savelist)
}

svr.trainonly <- function(foldname=NA,train,y,tune=T,tunefolds,tunecores,PCA,weights=T) {
#foldname, string | number for naming output
#test, test df
#train, train df
#y, yvarname
#tune, T/F to Tune via SVRtuning_kfold_parallel
#tunefolds, N folds for tuning (tuning occurs on train only)
#weights, T/F to include SVR weights in output
require(e1071)
print(foldname)
if (unifeatselect){

}
 svmmodel<-svm(as.formula(paste0(y,"~.")),kernel="linear",data=train)
 if (tune){
   bestparameters<-SVRtuning_kfold_parallel(tunedf=train,y=y,k=tunefolds,tunecores=tunecores)
   svmmodel<-svm(as.formula(paste0(y,"~.")),cost=bestparameters$c,epsilon=bestparameters$e,kernel="linear",data=train)
 }
nw<-length(grep("vx",names(train)))
w <-rep(NA,nw)
if (weights){
w <- t(svmmodel$coefs) %*%svmmodel$SV
}

 ####save model performance
 pred_train        <-predict(svmmodel,train)
 ######save model params

cvout<-as.data.frame(cbind(
   foldname     = foldname,
   traincor = cor(pred_train,train[,y]),
   kernel =svmmodel$kernel,
   epsilon=svmmodel$epsilon,
   cost   =svmmodel$cost,
   gamma  =svmmodel$gamma
 ))
savelist<-list(cvout,svmmodel,w)
return(savelist)
}

####svr validation functions#####

SVR_crossvalidation<-function(foldvar,df,y,xcols,verb=FALSE,tune=TRUE,tunefolds,permute=FALSE,yperm=NULL,outerfoldcores,tunecores,weights=FALSE){  
   df<-df[!(is.na(df[,y])),]
   ytrain<-y
   if (permute){
   ytrain<-yperm   
   }

   ####just data for current models#######
   jdcolstrain<-c(ytrain,xcols)
   jddatatrain<-df[,jdcolstrain]
   
   jdcolstest<-c(y,xcols)
   jddatatest<-df[,jdcolstest]
   
   names(jddatatest)[names(jddatatest)==y]<-ytrain   
   folds=df[,foldvar]

   #####
   print(length(unique(folds)))
   print("outer folds")
   if (tune){
   print(tunefolds)
   print("tuning folds")
   }
   #######leave one out loop##############
   outlist <- mclapply(unique(folds),mc.cores=outerfoldcores, function(lo){ 
                          svr.single(foldname=unique(df[df[,foldvar]==lo,foldvar]),test=jddatatest[folds==lo,],train=jddatatrain[folds!=lo,],y=ytrain,tune=tune,tunefolds=tunefolds,tunecores=tunecores,weights=weights)})  

   #####will help embedded list###
   modelfitouts<-lapply(outlist,"[[",1) 
   
   # list apparently needs a name
   #names(modelfitouts) <- unlist(lapply(modelfitouts,function(x) x[1]))
   # make into a outdf
   modeloutdf <- lapply(modelfitouts,function(x) { as.data.frame(x) } ) %>% bind_rows
   #########weights#############
   weightouts<-lapply(outlist,"[[",2)
   names(weightouts)<-names(modelfitouts)
   weightoutdf<- lapply(weightouts,function(x) { as.data.frame(x) } ) %>% bind_rows


   outlist<-list(modeloutdf=modeloutdf,weightoutdf=weightoutdf)
   return(outlist)
}

SVR_validationcommonleftout<-function(traindf,testdf,y,xcols,verb=FALSE,tune=TRUE,tunefolds,permute=FALSE,yperm=NULL,outerfoldcores,tunecores,weights=FALSE,itersamplesize,niter=1000){
   
   traindf<-traindf[!(is.na(traindf[,y])),]
   testdf<-testdf[!(is.na(testdf[,y])),]
   ytrain<-y
   if (permute){
   ytrain<-yperm
   }

   ####just data for current models#######
   jdcolstrain<-c(ytrain,xcols)
   jddatatrain<-traindf[,jdcolstrain]

   jdcolstest<-c(y,xcols)
   jddatatest<-testdf[,jdcolstest]

   names(jddatatest)[names(jddatatest)==y]<-ytrain

   ###create pulls#####

   pulls<-do.call(rbind,lapply(1:niter,function(x){sample(1:nrow(jddatatrain),itersamplesize,replace=TRUE)}))

   #####print run info#######
   print("iter sample size")
   print(itersamplesize)
   print ("with this many iterations")
   print(niter)
   if (tune){
   print("tuning folds")
   print(tunefolds)
   }
   #######parallel across iterations (pulls matrix)##############
   outlist <- mclapply(1:ncol(pulls),,mc.cores=outerfoldcores, function(p){
                          svr.trainonly(foldname=p,train=jddatatrain[pulls[p,],],y=ytrain,tune=tune,tunefolds=tunefolds,tunecores=tunecores,weights=weights)})

   #####will help embedded list###
   modelfitouts<-lapply(outlist,"[[",1)

   # list apparently needs a name
   #names(modelfitouts) <- unlist(lapply(modelfitouts,function(x) x[1]))
   # make into a outdf
   modeloutdf <- lapply(modelfitouts,function(x) { as.data.frame(x) } ) %>% bind_rows
   #########weights#############
   weightouts<-lapply(outlist,"[[",2)
   names(weightouts)<-names(modelfitouts)
   weightoutdf<- lapply(weightouts,function(x) { as.data.frame(x) } ) %>% bind_rows


   outlist<-list(modeloutdf=modeloutdf,weightoutdf=weightoutdf)
   return(outlist)
}



#######################################svm wrappers#############

svm_wrapper<-function(foldvar,df,ys,xcols,tune,tunefolds,outerfoldcores,tunecores,weights=TRUE,foldsummary=TRUE){
wrapperout<-NULL
weightsout<-NULL

for (cur_y in ys){
print(cur_y)

loocxlist <- SVR_crossvalidation(foldvar=foldvar,df,cur_y,xcols,tune=tune,tunefolds=tunefolds,outerfoldcores=outerfoldcores,tunecores=tunecores,weights=TRUE)
loocxdf<-loocxlist[["modeloutdf"]]
loocxdf$y<-cur_y
loocxdf$error<-loocxdf$testyval-loocxdf$cvpred
wrapperout<-rbind(wrapperout,loocxdf)

svmweights<-loocxlist[["weightoutdf"]]
svmweights_m<-as.data.frame(t(sapply(svmweights,mean, na.rm = T)))
svmweights_m$y<-cur_y
weightsout<-rbind(weightsout,svmweights_m)
}
wrapperreturn<-list(wrapperout=wrapperout,weightsout=weightsout)
return(wrapperreturn)
}

svm_wrapper_permute<-function(df,ys,xcols,tune,tunefolds,outerfoldcores,tunecores,nperms=1000,foldsummary=TRUE){
permwrapperout<-NULL
for (cur_y in ys){
curydf<-df
curydf<-curydf[!is.na(df[,cur_y]),]
print(cur_y)

for (p in 1:nperms){
print(p)
curydf$yperm<-sample(curydf[,cur_y])
locxlist <- SVR_crossvalidation(foldvar=foldvar,curydf,cur_y,xcols,tune=tune,tunefolds=tunefolds,outerfoldcores=outerfoldcores,tunecores=tunecores,permute=TRUE,yperm="yperm")

locx<-locxlist[["modeloutdf"]]
locx$error<-locx$testyval-locx$cvpred
permcor<-NA
permcor<-cor(as.numeric(locx$cvpred),curydf[,cur_y])
permptemp<-as.data.frame(permcor)
permptemp$y<-cur_y
permptemp$perm<-p
permptemp$rsquared<-rsquared_function(locx$testyval,locx$error)

if (foldsummary){
permptemp$foldname<-0
rsquaredfolds<-locx %>% group_by (foldname) %>% summarise(rsquared=rsquared_function(testyval,error),permcor=cor(testyval,cvpred))
rsquaredfolds$y<-cur_y
rsquaredfolds$perm<-p
permptemp<-rbind(permptemp,rsquaredfolds)
}

permwrapperout<-rbind(permwrapperout,permptemp)
}
}
return(permwrapperout)
}


