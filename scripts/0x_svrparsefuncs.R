load_obj <- function(f){
    env <- new.env()
    nm <- load(f, env)[1]
    env[[nm]]
}


chunklistaggregattion<-function(chunkdir,PCA=FALSE,unifeatselect=FALSE,mc.cores=1){
require(parallel)
files<-Sys.glob(paste0(chunkdir,"/*.rdata"))
if (PCA){
files<-files[grep("PCA.",files,)]
}
if (unifeatselect){
files<-files[grep("unifeatselect.",files,)]
}

chunkdatalist<-mclapply(files,mc.cores=mc.cores,function(cf){
               print(cf)
               currentchunk<-load_obj(cf)       
               if (PCA){
               if(length(strsplit(cf,"[.]")[[1]])==9){param<-strsplit(cf,"[.]")[[1]][3]}
               else{param<-strsplit(cf,"[.]")[[1]][2]}
               type<-"PCA"
               }

               if(unifeatselect){
               param<-strsplit(cf,"[.]")[[1]][2]
               type<-"unifeatselect"
               }

               jobname<-strsplit(cf,"[.]")[[1]][5]


               outdata<-data.frame(itersamplesize=currentchunk$itersamplesize,y=currentchunk$y,tune=currentchunk$tune,type,param=as.numeric(as.character(param)),truey=currentchunk$testyvals,testpred=currentchunk$testpreds,jobname=jobname)
})
chunkdatadf<-do.call(rbind,chunkdatalist)
return(chunkdatadf)
}


chunklistaggregattion_weights<-function(chunkdir,PCA=FALSE,unifeatselect=FALSE,mc.cores=1,namevars=c("y","itersamplesize","jobname")){
require(parallel)
files<-Sys.glob(paste0(chunkdir,"/*.rdata"))
if (PCA){
files<-files[grep("PCA.",files,)]
}
if (unifeatselect){
files<-files[grep("unifeatselect.",files,)]
}

chunkdatalist<-mclapply(files,mc.cores=mc.cores,function(cf){
               print(cf)
               currentchunk<-load_obj(cf)      
               if (PCA){
               if(length(strsplit(cf,"[.]")[[1]])==9){param<-strsplit(cf,"[.]")[[1]][3]}
               else{param<-strsplit(cf,"[.]")[[1]][2]}
               type<-"PCA"
               } 

               if(unifeatselect){
               param<-strsplit(cf,"[.]")[[1]][2]
               type<-"unifeatselect"
               } 

               jobname<-strsplit(cf,"[.]")[[1]][5]

               outdata<-data.frame(itersamplesize=currentchunk$itersamplesize,y=currentchunk$y,tune=currentchunk$tune,type,param=as.numeric(as.character(param)),svmweights=currentchunk$svmweights,svmweightnames=dimnames(currentchunk$svmweights)[[1]],jobname=jobname)
})

weightmat<-data.frame(do.call(cbind,lapply(1:length(chunkdatalist),function(li){
wm<-chunkdatalist[[li]]$svmweights
})))

weightlabels<-unlist(lapply(1:length(chunkdatalist),function(li){
thisnamevar<-lapply(namevars,function(nv){                        
unique(chunkdatalist[[li]][nv])})
nameout<-do.call(paste,c(c(thisnamevar[[1]],thisnamevar[[2]],thisnamevar[[3]]),sep=".")) #consider expanding to allow a flexible number of names
}))

names(weightmat)<-weightlabels
return(weightmat)
}


