load_obj <- function(f){
    env <- new.env()
    nm <- load(f, env)[1]
    env[[nm]]
}


chunklistaggregattion<-function(chunkdir,PCA=FALSE,unifeatselect=FALSE){

files<-Sys.glob(paste0(chunkdir,"/*.rdata"))

chunkdatalist<-lapply(files,function(cf){
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


               outdata<-data.frame(itersamplesize=currentchunk$itersamplesize,y=currentchunk$y,tune=currentchunk$tune,type,param=as.numeric(as.character(param)),truey=currentchunk$testyvals,testpred=currentchunk$testpreds)
})
chunkdatadf<-do.call(rbind,chunkdatalist)
return(chunkdatadf)
}
